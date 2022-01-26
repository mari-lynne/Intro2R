#Removing Duplicates ####

library(stringr)
library(stringi)
library(dplyr)
library(tidylog)

invo <- read.csv(file = "cov_invo_mini.csv", colClasses = "character")

#Approach 1 - stringi: ####

#Check for duplicates 
stri_duplicated(invo$Sample_ID) #stri_unique() also prints unique values

#make new column
invo_2 <- invo %>% mutate(dup_check = stri_duplicated(invo$Sample_ID))
#highlights first duplicate entry


#remove duplicates
invo_2 <- invo_2 %>% filter(dup_check == FALSE)

#rm dup check column to tidy
invo_2 <- invo_2 %>% select(-dup_check)

#Approach 2 - Dplyr: ####

#There is also the dplyr function distinct
#Can remove duplicate rows or just based on a column variable
#E.g

invo %>% distinct(Sample_ID)

#more info at 
#https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/#:~:text=DataScience%20Made%20Simple-,Remove%20Duplicate%20rows%20in%20R%20using%20Dplyr%20%E2%80%93%20distinct%20()%20function,variable%20or%20with%20multiple%20variable.


