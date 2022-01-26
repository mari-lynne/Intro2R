#Data Visulation (Part 1)

#Install and load packages ####

install.packages("ggpubr")
install.packages("RColorBrewer")
install.packages("wesanderson")
install.packages("scales")
install.packages("viridis")


library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(scales)
library(viridis)

#Load data ####

setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial3")

phago_data <- read.csv("phago_data_recode.csv", fileEncoding="UTF-8-BOM")

#Data Preparation ####

#Steps:
#Here we will be preparing our data set for plotting
# 1)Recode Variables
# 2)Rename Variables
# 3)Mutate new columns
# 4)Join metadata to dataframe

# 1) recode variables into factor

str(phago_data)

phago_data$Rep <-as.factor(phago_data$Rep)
phago_data$Plate <-as.factor(phago_data$Plate)
phago_data$Sample_ID <- as.character(phago_data$Sample_ID)
phago_data$Visit <- as.factor(phago_data$Visit)

#2) Rename factor levels
levels(phago_data$Visit)
levels(phago_data$Visit) <- c("Pre-Vac", "Post-Vac")

#3)calculate duplicates, mean, sd and se
#Note how replicates and visits are organised on separate rows (long format)

#new variable(column) = function or calculation(old variables)

#replicates left out
 phago_data <- phago_data %>% group_by(Sample_ID, Plate, Visit) %>%
  mutate(mean_phago = mean(Phago_Score),
         sd = sd(Phago_Score),
         se = sd(Phago_Score) / sqrt(length(Phago_Score)))

#add percent CV column

phago_data <- phago_data %>% group_by(Sample_ID, Plate, Visit) %>%
  mutate(Percent_CV = sd/mean_phago * 100)

#filter data frame with high %CV
phago_data_CVhigh <- filter(phago_data, Percent_CV > 30)

#Filter main data frame
phago_data <- filter(phago_data, Percent_CV < 30)

phago_data <- distinct(phago_data) #removes duplicate rows

# Joining data frames #####

#4)Add age group data 

age <- read.csv(file = "age_recode", fileEncoding = "UTF-8-BOM")

#join by matching sample ID using dplyr's left_join function
#requires Joining IDs to be same data type, also check variable/level names match

str(age)
age$Sample_ID <- as.character(age$Sample_ID)

#Join dataframes a and b, by Sample_ID

phago_data <- left_join(phago_data, age, by = "Sample_ID")


#Plotting ####

#Visualise the differences in phagocytosis (ADNP) pre, and post-vaccination

#Box Plot ####
ggplot(phago_data, aes(x=Visit, y=mean_phago)) + geom_boxplot()

#plots median + IQR

#Bar Chart ####
ggplot(phago_data, aes(x=Visit, y=mean_phago)) + geom_bar(stat="identity")

#for bar charts we need an extra arguement within geom_bar, stat = identity
#This plots the data values on y axis. Modifies, default setting which is to plot the frequency/count of observations - therefore you wouldn't need to supply a y aes

#Plot Customisation ####

#Update colours: ##

#change colour in aes with by fill = (variable you want to colour by) # colour specifies outline colour

ggplot(phago_data, aes(x=Visit, y=mean_phago, fill = Visit)) + geom_boxplot()

#change colours manually within the geom_plot(fill=c("Color1", "Color2")
colours() #R has 657 built in color names. To see a list of names:

ggplot(phago_data, aes(x=Visit, y=mean_phago)) + geom_boxplot(fill=c("tan2", "springgreen3"))

#Update labels: ##
# + labs(x = "New x axis label", y = "New y axis label", title ="",subtitle = "",caption = "")

ggplot(phago_data, aes(x=Visit, y=mean_phago, fill = Visit)) + geom_boxplot() + labs(y = "Phagocytic Score (PU)\n", title = "ADNP in Nepalese Children (0-15 years)")
#use backslash \n to add line space between ylab and axis

#Update scales: ####

library(scales)
?scales #.format axis option

ggplot(phago_data, aes(x=Visit, y=mean_phago, fill = Visit)) +
  geom_boxplot() +
  labs(y = "Phagocytic Score (PU)", title = "ADNP in Nepalese Children (0-15 years)") +
  yscale("log10", .format = TRUE) + annotation_logticks()


#add limits ##

ggplot(phago_data, aes(x=Visit, y=mean_phago, fill = Visit)) +
  geom_boxplot() +
  labs(y = "Phagocytic Score (PU)\n", title = "ADNP in Nepalese Children (0-15 years)") +
  ylim(0, 5000)

#save plot ##

v1v2_box <- ggplot(phago_data, aes(x=Visit, y=mean_phago, fill = Visit)) +
  geom_boxplot() +
  labs(y = "Phagocytic Score (PU)\n", title = "ADNP in Nepalese Children (0-15 years)") +
  theme(legend.position = "none") #removes legend

v1v2_box

#Themes ####

v1v2_box + theme_grey() + ggtitle("theme_grey()")
v1v2_box + theme_bw() + ggtitle("theme_bw()")
v1v2_box + theme_linedraw() + ggtitle("theme_linedraw()")
v1v2_box + theme_light() + ggtitle("theme_light()")
v1v2_box + theme_dark() + ggtitle("theme_dark()")
v1v2_box + theme_minimal()  + ggtitle("theme_minimal()")
v1v2_box + theme_classic() + ggtitle("theme_classic()")
v1v2_box + theme_void() + ggtitle("theme_void()")

?theme #details on all the componenets of a theme you can modify, most everything about your plot can be customised] #once happy with a theme save as an object e.g: 

white_cor <- theme(axis.text=element_text(size=12),
                   axis.title = element_text(size = 13, face="bold"),
panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"),
panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey88"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey88"))

# TIP: I am keeping a record of any themes/functions in a separate Rscript which I open alongside analysis.

#Correlation Plot ####

#Often useful to filter data before plotting - i.e look at each time point separately, and also filter repeat replicate values

phago_data %>% filter(Visit == "Post-Vac", Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago)) + geom_point()

phago_data %>% filter(Visit == "Pre-Vac", Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago)) + geom_point()

#add regression line
phago_data %>% filter(Visit == "Pre-Vac", Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago))+
  geom_point() +
  geom_smooth(method=lm) +
  white_cor

#calculate R2 value

 phago_data %>% filter(Visit == "Pre-Vac", Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago)) +
   geom_point() +
   geom_smooth(method=lm) +
   stat_cor(method = "spearman")+
   white_cor

 #Check Normality of data ####
 
 #side note - check data distribution before deciding on stat method
 #plot histogram
 #x = measurement, y = ..density..
 #adjust binwidth
 
 phago_data %>% filter(Visit == "Pre-Vac", Rep == "1") %>%
   ggplot(aes(x = mean_phago, y = ..density..)) + 
   geom_histogram(binwidth = 35, fill = "purple", colour = "black")

#Faceting ####

phago_data %>% filter(Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago)) + geom_point() + geom_smooth(method=lm) + stat_cor(method = "spearman") +ylab("Phagocytic Score (PU)\n") +white_cor + facet_wrap(~ Visit)
 
#split by sex, vaccine type etc.

#Colour themes ####

#add colours to box/bar plots using +scale_fill_manual ##

v1v2_box + scale_fill_manual(values = c("red", "darkgrey"))

#chose colour from custom palette -wes anderson
v1v2_box + scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2))

display.brewer.all() #Rcolourbrewer
display.brewer.all(colorblindFriendly = TRUE)

v1v2_box + scale_fill_brewer(palette = "Dark2") + white_cor


#customise scatter plot using +scale_colour_ ##

phago_data %>% filter(Visit == "Pre-Vac", Rep == "1") %>%
  ggplot(aes(x=Age, y=mean_phago, colour = Age))+
  geom_point()+
  geom_smooth(method=lm)+
  stat_cor(method = "spearman")+
  ylab("Phagocytic Score (PU)\n")+ white_cor+
  scale_color_viridis(option = "D")


?viridis #options denote different palletes

#homework set your own gradient fill for the correlation plot


#Colour theory ####

#chose colours by hex code: google 'hex color picker' > complimentary colours/colour: https://htmlcolorcodes.com/color-picker/

#R version
install.packages("colortools")
library(colortools)
??colortools

splitComp("#EF671D")
complementary("#EF671D")

ggplot(phago_data, aes(x=Visit, y=mean_phago)) + geom_boxplot(fill=c("#EF671D", "#1DA5EF")) +labs(y = "Phagocytic Score (PU)\n", title = "ADNP") +theme_light()







