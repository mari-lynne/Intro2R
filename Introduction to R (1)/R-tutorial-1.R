#Set up your working Directory ####

#This is the folder you have your data saved in
#Change \'s to /'s

setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course") #rename to your folder

#load packages ###

#alongside base R language many packages have been developed to make code easier to write and can include specific functions for plotting/data analysis

library(dplyr)
library(ggplot2)

#Read data ####

ELISA <- read.csv(file = "ELISA_optimisation.csv", fileEncoding = "UTF-8-BOM") #File encoding helps read in unicode 

#Notice the data is saved in 'wide format' - Sample/Patient_IDs are rows, all other variables are columns #more in later tutorials. Extra info if you would like to change your data format in R https://ademos.people.uic.edu/Chapter8.html


?read.csv #help (examples are at the bottom!)
#you can specify if there's a header, the file deliminator etc.

#Explore your Data ####

str(ELISA)#my go to

head(ELISA)
tail(ELISA)
View(ELISA)

#Change data structre using as. ####

ELISA$purification_Time_Point <- as.factor(ELISA$purification_Time_Point)
ELISA$Sample_ID <- as.factor(ELISA$Sample_ID)

#as.numeric
#as.character

#Create vectors ####

Standards <- c(10, 20, 40, 80, 160) #operator 'c' returns a vector of whatever's in the brackets

Samples <- c("STD1", "STD2", "STD3", "STD4", "STD5")#character values have to be put in " ", otherwise R thinks it's an object

#make a small data frame
standards <- data.frame(Samples, Standards)
str(standards)

#create matrix
Num <- c(1:5) #generate numbers 
matrix <- data.matrix(Standards, Num)

#make list
R_list <- c(ELISA, matrix, standards)

#Subsetting data ####

#$ - To subset a specific column in a data.frame
ELISA$Sample_ID

#[ ] - To get a subset of elements 
#[row, column]
ELISA[2,]

ELISA["Percent_CV"]

#[[ ]] - To get a specific element from a list (or a column of a data.frame)
R_list[["Standards"]]

#subset data using dplyr package

#dplyr package uses pipe operator %>% 
#%>% feeds dataframe or object into a function, instead of always specifying within the function
# makes code easier to read and write when doing multiple steps

#select columns
ELISA %>% select("Amount") #same as
select(ELISA, "Amount")

#select rows
ELISA %>% slice(2:4) #selects rows 2-4


#Filtering ####

ELISA %>% filter(Percent_CV < 15)

ELISA_low <- ELISA %>% filter(Percent_CV < 15)

#find a sample

ELISA %>% filter(Sample_ID == "8478") #double equals!
ELISA %>% filter(Sample_ID != "8478") #does not equal


#Math operations ####

#assign variables, like you would in high school math

x <- 10
y <- 5

(x+y)
(x/y) + y

f <- x + y
f

#We can also use mathematical operations across rows to make new columns and values #I like to use dplyr function mutate

#Mutate new column ####
?mutate

#get actual concentration in EU by multiplying the dilution factor (in this case 100) 

ELISA <- ELISA %>% mutate(Concentration_df = (Conc*100))

#Mutate with subsetting ####

#Calculate the percentage purified serum ###

#ELISA measured unbound antibody in supernatent from my purification
#We know the original concentration of antibody and the unbound
#The difference between pre-purification conc and the conc, of unbound antibody left post purification in the supernatent is therefore the amount of Vi antibody bound to the plate
#(Pre_purification conc - Post conc.)/Pre conc.))*100

#Steps ###
#Normally in excell we can just caluculate across rows by selecting the cells we need

#In R our data isn't formated as such - this is one of the biggest things to get used to so don't worry if it's confusing at first!
View(ELISA)

#We can instead use group_by function to ensure that data operations are done according to those with the same participant ID/sample ID/vaccine group etc.

?group_by

#In addition to group_by, we also need to specify what condition we would like to use to perform the calculation.
#For this we can use the subset operator [], along with base logic, so == to specify a condtion

#rename column
ELISA <- ELISA %>% rename(Time_Point = purification_Time_Point) #New name from old name

#calculate percent purified
#(Pre_purification conc - Post conc.)/Pre conc.))*100 


ELISA %>% group_by(Sample_ID) %>% #tells R to do this for matching sample IDs
  mutate(Percent_Purified=((Conc[Time_Point == "Pre"]- Conc[Time_Point == "Post"])/(Conc[Time_Point == "Pre"])*100))

#Tip - when writiting I sometimes like to figure out the calculation code first, then worry about the brackets later!

#save as updated dataframe
ELISA <- ELISA %>% group_by(Sample_ID) %>%
  mutate(Percent_Purified=((Conc[Time_Point == "Pre"]- Conc[Time_Point == "Post"])/(Conc[Time_Point == "Pre"])*100))

#Extra tasks ####

#Calculate amount of serum bound to plate
#This has already been done previously and is saved in the 'amount' column but see if you can make a new column yourself :)


#bonus plotting ####

#what is a good way to plot the data?
#what do you think of my purification?
#More detailed ggplot tutorials to come! 

#make bar chart grouped by sample ID

#First element of ggplot is 'aes', these are the general rules of the graph so what variables you are using for x/y axis as well as grouping variables (indicated by fill)

#Next, what sort of plot you would like is indicated by the + sign, e.g +geom_bar or +geom_box and add on any other details of the graph you want within the graph code element so spacing, position, width, color

#Add ons such as labels, axis, and special colour schemes (themes) can be also added using the + sign


ELISA %>% ggplot(aes(x=Sample_ID, y=Conc, fill=Time_Point))+
  geom_bar(stat="identity", position=position_dodge())+ scale_fill_manual(values=c('#E69F00','#BBF677')) +ylab("Vi-IgA Concentration (EU)") + xlab("Sample ID") +
  geom_text(aes(label=Percent_Purified), vjust=1.7, color="black",
            position = position_dodge(0.9), size=3.5) + theme_minimal()

ELISA$Percent_Purified <- round(ELISA$Percent_Purified ,0)

#reorder levels
ELISA$Time_Point <- factor(ELISA$Time_Point, levels = c("Pre", "Post")) 

#Replot :)