#Set up  ####

setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial5")

install.packages("stringr")
library(stringr)
library(stringi)
library(dplyr)
library(tidylog)

meta <- read.csv(file = "meta_data.csv", colClasses = "character")



#Basic string manilpulation ####

#save a string ###
string1 <- "this_is_a_basic_string"

#count characters ###
str_length(string1)

#concatanate strings ###
#combine strings into character vector using str_c

subject_IDs <- c("VASP_6", "VAST_6","VAST_7", "VAST_8", "VAST_9", "VAST_9")

str_c("VASP",1:10, sep="-")

str_c("string1", "string2", sep = "_") #change separator
#string_c() has more functionallity than simple c()

#Change letter casing
str_to_lower(subject_IDs)
str_to_upper(subject_IDs)

#String split ###
str_split(subject_IDs, "_")


#Stingr functions continued ####

#Subsetting strings using str_detect, which and subset ###

#output shows which strings contain a match for the pattern/expression definied
str_detect(subject_IDs, "VAST_7")#equivalent to grepl
str_which(subject_IDs, "VAST_7") #returns row index, equivalent to grep


str_subset(subject_IDs, "VAST_7")
str_subset(subject_IDs, "VAST")
#subset prints the strings that match the expression


#Find and replace ###
?str_replace
str_replace(subject_IDs, "VAST", "VAST_01")

stri_duplicated(subject_IDs)

#Strings in dataframes ####
str(meta) #Inspect DF

#Each column from the data frame individually is a vector, containing strings! For example
meta$Ethnicity

#Apply stringr functions to column in dataframes E.g
str_split(meta$Ethnicity, "_")

#Find and replace in DFs ####
str_replace(meta$Ethnicity, "White_British", "White")
            
#Update separators
str_replace(meta$Ethnicity, "_", " ")

#Replace NA's ###
str_replace_na(meta$Study, replacement = "VASP")
#Save back into DF
meta$Study<- str_replace_na(meta$Study, replacement = "VASP")


#String detect and subset in DFs ####

str_detect(meta$Ethnicity, "White")
str_which(meta$Occupation, "Student")
#str_which function searches within the occupation column, and returns indicies (row locations) where a match is found, equivalent to grep

#These functions are useful for extracting data from our DF

#Use square bracket notation to subset data frame [rows,cols]
#Within the row, we are specifying that we want rows which satisfy our stringr conditions

meta[str_detect(meta$Occupation, "Student"), ]
#leave col index blank so to returns all the columns

student_data <- meta[str_detect(meta$Occupation, "Student"), ] #save DF
View(student_data)


#There are easier ways to subset as we've seen with filter
filter(meta, Occupation == "Student")
#or base R
meta[meta$Occupation == "Student",]


#But there are many operations we can do with stringr
#It also allows us to search based off of more complex pattern's and conditions...


#Regular Expressions ####

# The hat ,^, and dollar $

str_detect(meta$Ethnicity, "^Black")

str_detect(meta$Occupation, "Employed$") 

#subset DF (can be done using detect, which)
meta[str_detect(meta$Occupation, "Employed$"), ]

#save as a new DF
employed_data <-meta[str_detect(meta$Occupation, "Employed$"), ]

#add in extra search terms
#search for employed and student data
meta[str_detect(meta$Occupation, "Employed$|Student"), ]

#Regex rules continued ####
#Escape characters \\

artists <- c("A$AP Rocky", "Drake", "Eminem", "Jay-Z", "50 cent")

str_subset(artists, "A\\$AP")


str_subset(artists, "e") #returns all artists with e in their name
str_subset(artists, "\\d") #returns all artists with numbers in their name
str_subset(artists, "[DZ]") #returns all artists contiaing either D or Z 

#Regex Quantifiers ###


str_subset(artists, "A+") #needs at least one match of the left element
str_subset(artists, "x+")

str_subset(artists, "x?") #optional to match left element, needs zero or one occurances 
str_subset(artists, "x*") #optional to match left element, needs zero or more occurances 

# .* means select all occurances of match #E.g
str_subset(subject_IDs, "VAS.*")


#Learn these as you go :)

#You can combine quantifiers to determine your search pattern
#For example in your inventory you might want to return sample IDs that have to match the study ID VAST+ but then then also a specific set of numbers/samples
subject_IDs

str_subset(subject_IDs, "VAST+")
str_subset(subject_IDs, "6")

str_subset(subject_IDs, "[VAST+6]") #Matches everything either or in the brackets
str_subset(subject_IDs, "[VAST+].6")#find all matches with VAST then .anything with 6
#dots work to separate out search terms



#Inventory Example #####

invo <- read.csv(file = "cov_invo_mini.csv", colClasses = "character")
samples <- read.csv(file = "cov_samples.csv")

#Ideal world, left join based on Sample_IDs
new_invo <- left_join(invo, samples, by = "Sample_ID")
#But they currently aren't in the same format
samples

#Split IDs by delimeter  ####
  #str_split outputs list, bit tricky to work with!

str_extract(samples$Label, "[^_]+$")
#Extracts one or more characters after $ that dont match ^_

#Include - to remove
str_extract(samples$Label, "[^_-]+$")

#Include \s to remove any white space characters for future ref
str_extract("Testing things", "\\S.*")

#mutate new ID column
samples <- samples %>%
  dplyr::mutate(Sample_ID = str_extract(Label, "[^_-]+$"))

#mutate and check new study column
str_extract(samples$Label, "[^_-]+")

samples <- samples %>% mutate(Study_ID = str_extract(Label, "[^_-]+"))

#Check Study Codes ####
#In this example we are just looking for COV1 samples, all should be COV1
#Could use a list of accepted study codes if needing to expand

str_subset(samples$Study_ID, "[^1]+$")
#subset anything that doesn't [^] end in 1
str_subset(samples$Study_ID, "[^COV1]+") #equivalent


#Update mislables study codes ####
str_replace_all(samples$Study_ID, "[^1]+$", "COV1")

samples$Study_ID <- str_replace_all(samples$Study_ID, "[^1]+$", "COV1")


#Merge updated sample list with inventory to find sample locations
new_invo <- left_join(invo, samples, by = "Sample_ID")
#left join function keeps matching rows

#Make new labels
new_invo <- new_invo %>% mutate(New_Label = str_c(Study_ID, Sample_ID, sep = "_"))

#Clean DF
new_invo <- new_invo %>% dplyr::select(-Label, -Study_ID)

#Find Samples ####
#Sort by freezer then slot 

new_invo %>% arrange(desc(Freezer, Slot))
new_invo$Freezer <- as.numeric(new_invo$Freezer)
new_invo <- new_invo %>% arrange(desc(Freezer, Slot))

write.csv(new_invo, file = "cov_samplelist.csv")




#look arounds ####
#Samples that couldn't be located #use look around functions 
str_subset(subject_IDs, "(?=.*VAST)(?=.*67)") #search dependent on two conditions matching
