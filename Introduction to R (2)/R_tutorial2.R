#Set up your working Directory ####

#Wd is the Folder you have your data saved in #Change \'s to /'s

setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial2") #rename to your folder
getwd()

#load packages ###

install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2") #only need to install once (comment out in future)


library(ggplot2)
library(tidyr)
library(dplyr)


#Load data ####

ELISA <- read.csv(file = "ELISA_data.csv", fileEncoding = "UTF-8-BOM")
#Note: when naming your columns try to avoid using spaces or special characters $!%
#For separators _ or . works best
View(ELISA)

#reformat data ####

#wide to long = gather
#long to wide = spread


#gather . gather(data, key, value, ...)
#where
#data is the dataframe you are working with.
#key is the name of the key column to create (new grouping variable)
#value is the name of the value column to create (meaurement/observation variable)
#... is a way to specify what columns to gather from.

ELISA <- gather(ELISA, key = Time_Point, value = Conc, 2:3)

#Explore your Data ####
View(ELISA)
str(ELISA)

# use as. to change data types
#save over the original subsetted column using <- 
#Look out for factor recoding, factors variables is how ggplot groups data
ELISA$Time_Point <- as.factor(ELISA$Time_Point)

#rename levels 
levels(ELISA$Time_Point) #levels prints out a character vector
levels(ELISA$Time_Point) <- c("Post", "Pre") #write over original levels with new vector

str(ELISA)

#rename column
#ELISA <- ELISA %>% rename(Time_Point = purification_Time_Point) #New name from old name


#Summarise data ###

#base R #
sum(ELISA$Conc)
mean(ELISA$Conc)

mean(ELISA$Conc[ELISA$Time_Point == "Pre"])

#subset based on a logical condition - use square brackets [logical condition]


#dplyr version #
ELISA %>% summarise(average = mean(Conc))
?summarise #count, IQR, SD, min-max

#Instead of square brackets, use the filter function

ELISA %>% filter(Time_Point == "Pre") %>% summarise(average = mean(Conc))

#or use group_by
#Grouping itself doesn't change how the data looks!
#Just means that further operations will always be performed "by group"

ELISA %>% group_by(Time_Point) %>% summarise(average = mean(Conc))


#Mutate ####

#data %>% mutate(new variable name = (old variable*calculation))
#calculations done inside brackets #save into orignal df using <- 

ELISA <- ELISA %>% mutate(Conc_df = (Conc*100))                    
View(ELISA)

#Assay Calculations ####

#Steps ###
#Normally in excel we calculate across rows by selecting the cells we need.
# In R our data isn't formated as such - this is one of the biggest things to get used to so don't worry if it's confusing at first!

#Instead, use group_by function to ensure that data operations are done according to those with the same ID/vaccine group/time point etc.
#and/or use square brackets to subset by[logical condition]

ELISA %>% 
  group_by(Sample_ID) %>% #tells R to do this for matching sample IDs
  mutate(Amount = (Conc[Time_Point == "Pre"] - Conc[Time_Point == "Post"]))

#Tip - when writiting I sometimes like to figure out the calculation code first, then worry about the brackets later!

#work out grouping variables
#decide new col/variable name 
#work out calculation needed to get new variable
#add in logical conditions to calculation code
#put it all together with correct brackets

#Percent purified = (Amount purified/original concentration)*100

ELISA %>% 
  group_by(Sample_ID) %>%
  mutate(Percent_purified = (Amount/Conc[Time_Point == "Pre"])*100)

#GGPLOT ####

 #First element of ggplot is 'aes', these are the general rules of the graph so what variables you are using for x/y axis as well as grouping variables (indicated by fill)
 
 #Next, what sort of plot you would like is indicated by the + sign, e.g +geom_bar or +geom_box and add on any other details of the graph you want within the graph code element so spacing, position, width, color
 
 #Add ons such as labels, axis, and special colour schemes (themes) can be also added using the + sign
 
ELISA %>% ggplot(aes(x=Sample_ID, y=Conc, fill=Time_Point)) +
   geom_bar(stat="identity", position=position_dodge())+ scale_fill_manual(values=c('#E69F00','#BBF677')) +ylab("Vi-IgA Concentration (EU)") + xlab("Sample ID")

#by using fill as Time_Point, that splits the bar chart
#within the geom_bar() function we can add more detail, stat = identity, dodge (side by side instead of stacked)

#reorder levels
ELISA$Time_Point <- factor(ELISA$Time_Point, levels = c("Pre", "Post")) 
#Replot :) 
 