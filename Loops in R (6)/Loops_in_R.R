#Loops in R - Tutorial 6 ####


setwd("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial6")

install.packages("pacman")
library(pacman)

pacman::p_load(
  rio,         # import/export
  here,        # file locator
  purrr,       # iteration
  tidyverse,
  dplyr,
  ggplot2# data management and visualization
) #loads requires packages and installs if necessary



#For loop basic structure ####

for(variable in vector) {
  Perform function on variable
}


#Step 1: Define vector to iterate loop over##
Fc_names <- c("FCGRI", "FCGRII", "FCGRIII", "FCAR")

#Step 2: Write loop using print() ##





#Numeric Example 1 ####

#Step 1: Define vector ##

num <- (1:10)

#Write loop ##

for (i in num) {
  print(i +2)
}


#Numeric Example 2 - save into vector ####

#Calculate the R^2 values from a vector containing Pearson R coeficients
#Save the output of a loop in a new vector

#Step 1:
#Create vector to loop over - R_values
#Create empty vector to save output of loop to 

R_values <- c(0.8, 0.2, 0.53, 0.64, 0.5)
output_R <- vector(mode = "double", length = length(R_values))

#Step 2: Write loop
for (i in R_values) {
  output_R <- print(i ^2)
}

output_R

#Problem: the loop works but the output is only saving the last calculation
#We need to index our vectors so we can keep track of what row the loop is at, then the results can be saved to the corresponding place in the output vector

#Solution: create a row index to subset our vector and output from using seq along
seq_along(num)
seq_along(Fc_names)


#Step 2: Write loop
for (i in seq_along(R_values)) { #for every row in vector R_values, perform function
  output_R[i] <- R_values[i] ^ 2 #Subset our input and output vectors by each loop iteration [i]
}

output_R 

#The first iteration of the loop (first row) would be 0.8^2, this is also saved in the first row of our output vector #The next iteration would be 0.2^2 and so on...

#Note if we were subsetting a dataframe we would do so by df$colname[[i]]


#Useful loops ####

#Loop over files ####

#Get vector containing names of files to loop over
data_files <- list.files("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial6", pattern = "*csv")

#Read csv files
for(i in seq_along(data_files)) {                              
  assign(paste0("data", i), #pastes 'data' + row index i as file name to <- assign
  read.csv2(paste0(data_files[i])))#pastes each filename to the read.csv function,                                         subsetted by index
}

#Extension ###

#Search for string within multiple files in a directory, e.g a sample number
#See end of script


#Plots ####

#load data 
glyc <- read.csv(file = "glyc_data_postvac.csv", fileEncoding = "UTF-8-BOM")

#Basic plot loop structure:

for(each variable i in dataframe) { #Loop through variables in DF to plot
 plt <- plot variable i against y #save plot output as object
 print(plt) #Print plot output
}

#Translate to code :)

#Step 1: Define vector to iterate for loop over ##
#We can get a list of variable names to loop over from colnames

#Select Vi specific IgG1 data from dataframe
IgG1 <- glyc %>% dplyr::select(contains("Vi.IgG1"))

#Use names or colnames
col_names <- names(IgG1)

col_names
#character vector containing colnames with Vi glyc features of interest, plot on x axis

#Define data we want to plot on the y axis
Y= glyc$Foldchange

for(i in col_names){
  plot <- ggplot(glyc, aes_string(x=i, y= Y)) + #use each value of i as the x axis
    geom_point(color="#B20000", size=3.6, alpha=0.5)
  print(plot) #print outout after each iteration
  Sys.sleep(2)
}


#Tidy plot and add regression + correlation

for(i in col_names){
  plt <- ggplot(glyc, aes_string(x=i, y = Y)) +
    geom_point(color="black", size=3.1, alpha=0.9) + 
    geom_smooth(method=lm, alpha=0.25, color="black", fill="darkgreen")+
    ylab ("Phagocytosis Fold Change/n") +xlab(gsub("(\\.|\\-)", " ", i))+ #sub .and- with white space in every iteration of the x lablel
    stat_cor(method = "spearman") +theme_bw()
  print(plt)
  Sys.sleep(2) #pauses code execution
}






#If Else Loops ####

U.K_phago <- 4.5 # mean phagocytosis for U.K participants
Nepal_phago <- 4 # mean phago for Nepal

if (Nepal_phago > U.K_phago){
  print ("Higher ADNP observed in Nepalese Children")
}


#Add Else code
if (Nepal_phago > U.K_phago){
  print ("Higher ADNP observed in Nepalese Children")
  } else {
    print("Higher ADNP observed in U.K Adults")
  }#Note you need to close each if and else loop with {}


#Break the loop ###
#Use break within if statement

x <- 1:5 #define vector to loop through

for (val in x) {
  if (val == 3){
    break
  }
  print(val)
}


#If Else recoding Variables ####

#'ifelse' wrapper function loops through data for you :)

glimpse(glyc$Sex)

glyc$Sex <- ifelse(glyc$Sex == "Male", 1, ifelse(glyc$Sex == "Female", 2, NA))
glimpse(glyc$Sex)






#Loop over files, string search - Extension ####

fileNames <- Sys.glob("*.csv") #Read in all files, e.g inventory
# * wildcard matches all expressions that contain expression

#same as:
fileNames <- list.files("C:/Users/mjohnson/OneDrive - Nexus365/R_course/Tutorial6", pattern = "*csv")


files <- vector(mode = "character", length = length(fileNames))

#Use loop to print the names of all files that contain a string, we've searched for using str_detect (could also use grep)

for (f in fileNames) {
  if (length(grep("phago", readLines(f))) > 0) {
    {print(f)}
    }
} 


#save as a vector in loop, then feed into next loop that reads.csv files

for (f in fileNames) {
  if (length(grep("phago", readLines(f))) > 0) {
    {print(f)}
  }
} 

#To be figured out...







