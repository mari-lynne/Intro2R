#Install load packages ####

install.packages("ggpubr")
install.packages("tidylog")
install.packages("patchwork")

library(reshape2)
library(tidylog) #love this!
library(ggpubr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(scales)
library(viridis)

#Load data ####

setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial4")

#load data from previous tutoral
#Save by using save.image(file = name, "name.Rdata")

load(file = "tutorial3data.Rdata")

#Joins ####

#Join dataframes phago and age, by Sample_ID

str(age)
age <- age[,2:3] #select cols 2>3
phago_data <- phago_data[,2:12]

#left join, matches by IDs in left DF
#keeps observations in left DF as NAs if there's no data for that sample
phago_left <- left_join(phago_data, age, by = "Sample_ID")

#right join, matches by IDs in right DF, i.e adds phago data to age data
#keeps all obvservations in right DF, missing data recorded as NA
phago_right <- right_join(phago_data, age, by = "Sample_ID") 
#I think this has appended the rows to the main table/not ideal

#inner, needs matches in both rows
#unmatched rows (NAs) are filtered out
phago_inner <- inner_join(phago_data, age, by = "Sample_ID")

#full join, keeps all observations
#missing data from either DF is kept and given NAs
phago_full <- full_join(phago_data, age, by = "Sample_ID")

#Remove unnecessary objects
rm(phago_left, phago_right, phago_inner, phago_full)


#Use left join for merging age into orignal DF 
phago_data<- left_join(phago_data, age, by = "Sample_ID")



#HMWK ####
#If a fan of data.table/STATA approach see if you can recode these joins using the data.table syntax



#Adding ELISA data ####

IgA <- read.csv("ELISA_IgA.csv")

str(IgA)
IgA$Sample_ID <- as.character(IgA$Sample_ID)

#V1 V2 are in wide format, convert to long format so we can match data to our table

IgA <- melt(IgA, id.vars = c("Sample_ID"), measure.vars = c("IgA_V1","IgA_V2"), variable.name = "Visit", value.name = "IgA_titre")
#In this example I'm using the melt function but you could try using gather as per previous tutorial 2
#More details here https://ademos.people.uic.edu/Chapter8.html


#Recode factor levels 
levels(IgA$Visit) <- c("Pre-Vac", "Post-Vac")

#Repeat for IgG ###

IgG <- read.csv("ELISA_IgG.csv")

#Merging ####

#Add the ELISA data to our our phagocytosis data frame
#We will be using this data to see if titre is driving ADNP

#Age_data make new age groups ####
#Use dplyr mutate, case_when #could also use if_else

phago_data <- phago_data %>%
  mutate(Age_Group = case_when(Age <= 4 ~ "0-4", Age <=9 & Age >= 5 ~ "5-9", Age <=16 & Age >= 10 ~ "10-15") %>% as.factor())

#More on recoding next week :)

#GGPUBR####


#Plot a box plot comparing phagocytosis across age groups

#ggpubr version
 phago_data %>% filter(Rep == "1", Age_Group != "NA") %>%
  ggboxplot(x = "Age_Group", y = "mean_phago", outlier.shape = NA,  color = "Age_Group", add = "jitter") +
  labs(x = "Age Group (years)", y = "Phagocytic Score\n") +
  theme_minimal() + 
  facet_wrap(~Visit)
  

#age_box <-

#ggplot version
phago_data %>% filter(Rep == "1", Age_Group != "NA") %>%
  ggplot(aes(x = Age_Group, y = mean_phago, color=Age_Group)) +
  geom_boxplot(outlier.shape = NA)+
  labs(x = "Age Group (years)", y = "Phagocytic Score\n") +
  theme_minimal()+
  geom_jitter(size = 1.2, width = .25, height=0.2) +
  facet_wrap(~Visit)

#Reorder levels ####
library(forcats) #install if necessary
phago_data <- phago_data %>% mutate(Age_Group = fct_relevel(Age_Group, "0-4","5-9","10-15"))

#Statistical Significance #####

#ggpubr version
phago_data %>% filter(Rep == "1", Age_Group != "NA", Visit == "Pre-Vac") %>%
  ggboxplot(x = "Age_Group", y = "mean_phago", outlier.shape = NA,  color = "Age_Group", add = "jitter") +
  labs(x = "Age Group (years)", y = "Phagocytic Score\n") +
  theme_minimal() +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label = "p.signif")

#Use stat compare_means function
?stat_compare_means

my_comparisons <- list( c("0-4", "5-9"), c("0-4", "10-15"), c("5-9", "10-15"))#define the groups for comparison 

age_box +stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label = "p.signif") #chose method and label option


#Plot a bar chart of phagocytosis data using ggpubr ####

phago_data %>% filter(Rep == "1", Age_Group != "NA", Visit == "Post-Vac") %>%
  
  ggbarplot(x = "Sample_ID", y = "mean_phago",  fill = "Age_Group") +
  labs(x = "Visit", y = "Phagocytic Score\n") +
  theme_minimal()

#Improved bar chart:

#couldn't get sort.val to work, sort on phago not sample_ID
#sort orgiginal DF
phago_data <- phago_data[order(phago_data$mean_phago),]


phago_data %>% filter(Rep == "1", Age_Group != "NA", Visit != "Post-Vac") %>%
  ggbarplot(x = "Sample_ID", y = "mean_phago",  fill = "Age_Group") +
  labs(x = "Visit", y = "Phagocytic Score\n") +
  theme_minimal()+
  theme(axis.text.x = element_blank())


#Adding error bars #####

phago_data %>% filter(Age_Group != "NA", Visit != "Post-Vac", Age_Group == "5-9") %>% #Removed Rep filter
  
ggbarplot(x = "Sample_ID", y = "Phago_Score",  fill = "Age_Group",
          add = c("mean_se")) + #error bars
  labs(x = "Visit", y = "Phagocytic Score\n") +
  theme_minimal()+
  theme(axis.text.x = element_blank())

##ggplot version adds +geom_errorbar

#Combining Plots together ####

#https://patchwork.data-imaginist.com/articles/guides/layout.html

library(patchwork)

#Save your plots as objects
#Plot spacing is controlled by +, |, and /, i
#Plot_layout and Plot_annotation for further options

#Example

bar_plot <- phago_data %>% filter(Age_Group != "NA", Visit == "Pre-Vac") %>% #Removed Rep filter
  
  ggbarplot(x = "Sample_ID", y = "Phago_Score",  fill = "Age_Group",
            add = c("mean_se")) + #error bars
  labs(x = "Visit", y = "Phagocytic Score\n", ggtitle = "Pre-Vac ADNP") +
  theme_minimal()+
  theme(axis.text.x = element_blank(), legend.position = "none")

bar_plot

age_box + bar_plot

#update legend and add annotations
age_box + bar_plot + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")




