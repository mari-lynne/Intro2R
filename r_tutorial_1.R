# Tutorial 1-2

# Set Up =======================================================================


# Install Packages
install.packages(c(
  "ggpubr",
  "ggplot2",
  "viridis",
  "ggvis",
  "data.table",
  "stringr",
  "forcats",
  "dplyr",
  "tidylog",
  "janitor"
))

# Or just install min packages for tutorial, then load these lines with library
install.packages(c("dplyr", "tidylog", "janitor", "ggplot2"))


### Load Packages --------------------------------------------------------------

# Visualization
library(ggpubr)
library(ggplot2)
library(viridis)
library(ggvis)

# Data manipulation
library(data.table)
library(stringr)
library(forcats)
library(dplyr)
library(tidylog)
library(janitor)


### Directories and variables --------------------------------------------------

# Update with your directory names
work_dir <- c("C:/Users/mjohnso5/Documents/teaching")
in_dir <- file.path(work_dir, "data") # Where input data is saved
plot_dir <- file.path(work_dir, "plots")
out_dir <- file.path(work_dir, "results") # file.path works similar to paste, but the separator is set to a /

setwd(work_dir) 
getwd()

# Read in data ----------------------------------------------------------------

data <- read.csv(file = file.path(in_dir, "GBD_enteric_subset.csv"))

data <- clean_names(data) # Function from Janitor package

# Inspect and subset -----------------------------------------------------------

View(data)
head(data, 5)
str(data)
class(data)
class(data$measure)

# Subset data (base R)

# Subset rows and columns
# Always rows x columns [], [row index/indices, column index], leave blank for all entries
data[1, 3]

# Use : to get sequence of numbers
data[1:5, 1:3]

# Subset rows
data[,6] # Leave row or column index blank to keep as is

# Subset columns
data$sex # or
data[ , c("cause")]
# Subset multiple columns
data[4, c("cause", "year")]

# Save subset
test <- data[1:4, ]

# dplyr version 

# Subset columns
hadley <- select(data, val, year)

# Subset rows
data %>% slice(1:4) # same as slice(data, 1:4) # just a style preference


# Filter data -----------------------------------------------------------------

# dplyr
data2 <- filter(data, location == "Southeast Asia")
# Filter using %in%, useful for filtering based on external chr vectors
data3 <- filter(data, location %in% c("Southeast Asia", "South Asia"))

# Filter for age just children, limit year past 1995 ...

# Filter duplicate data
data <- distinct(data)

# Rename variables
# measure, metric, cause, all the same as filtered from bigger data-set
data <- rename(data, enteric_deaths = val)

write.csv(data,
          file=file.path(out_dir, "GBD_enteric_subset2.csv"),
          row.names = F)

# Table summaries -------------------------------------------------------------

# Simple table
table(data$location)
  # Tidy factors
  data <- droplevels(data)

# Cross tabulation
table(data$location, data$age)

# Summary metrics (numeric variables)
summary(data$enteric_deaths)
mean(data$enteric_deaths)
IQR(data$enteric_deaths)

# Filter then inspect/summarize with dplyr
data %>%
  filter(location == "South Asia" & sex == "Both") %>%
  summarise(mean_enteric_deaths = mean(enteric_deaths, na.rm = TRUE))

# With base (row condition by column condition)
# Subset appropriate conditions, then summarize mean()
data[data$location == "South Asia" & data$sex == "Both", c("enteric_deaths")]


# Tidy variables ---------------------------------------------------------------

# Refer to columns as variables, rows are observations (long format)
str(data)
# dplyr mutate (write over or make new variables)
data <- data %>% mutate(range = upper-lower)

# Convert variables to appropriate data type (base)
data$location <- as.factor(data$location) # note save back to column (not data frame)
# Convert using mutate
data <- data %>% mutate(sex = as.factor(sex))

# Convert multiple factors at once (function iterates over all variables)
data <- data %>%
  mutate_if(is.character, as.factor)
# Other methods; lapply with if else, within read.csv(stringsAsFactors = TRUE)

# Rename variables
# measure, metric, cause, all the same as filtered from bigger data-set
data <- rename(data, enteric_deaths = val)

# Remove redundant columns
data <- select(data, !c("cause", "metric", "measure"))

# Reorder
data <- select(data, enteric_deaths, upper, lower, everything())


# Check for normality ---------------------------------------------------------

# Create a histogram with a normal distribution curve of variable of interest
# Decide what metric you might look at later e.g 
# Comparing deaths across time (within a country and age group)
sub <- data %>% filter(sex == "Both" & location == "South Asia" & age == "10-14 years")

ggplot(sub, aes(x = enteric_deaths)) +
  geom_histogram(aes(y = ..density..), binwidth = .005, fill = "lightblue", color = "black") + 
  geom_density(alpha = .2, fill = "darkgreen") + # Overlay a density plot
  labs(title ="Histogram of Enteric Deaths with Normal Curve")

# Binwidth is the x-axis units (ticks), will differ by data
# e.g if my range of deaths was 1-100 i might choose a bin of 10 

# QQ Plot
# Deviations from diagonal line may indicate non-parametric data
ggplot(sub, aes(sample = enteric_deaths)) + 
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot for Enteric Deaths")

shapiro.test(data$enteric_deaths) # Note sensitive to sample size


# Plotting ---------------------------------------------------------------------

# Hypothesis
# Were there differences in enteric deaths between regions in 2019
  # Did these deaths differ by age and sex?

# Tips:
# Think of the sort of plot you want, draw it out
# refresh on data frame structure
# Multiple time points, genders, countries
# Narrow down what we are interested in, as no filtering or faceting will just add up totals 

# No filtering
ggplot(data, aes(x = age, y = enteric_deaths, fill = age)) +
  geom_boxplot() +
  labs(title = "kids", y = "Percentage Enteric Deaths")

# Filter to just look at 2019, and facet by sex
data %>% filter(year == 2019) %>%
ggplot(aes(x = location, y = enteric_deaths, fill = location)) +
  geom_boxplot() +
  labs(title = "Enteric Deaths by Location and Sex", y = "Percentage Enteric Deaths") + facet_wrap(~sex)


# Deaths over time in south asia
data %>% filter(sex == "Both" & location == "South Asia") %>%
ggplot(aes(x = year, y = enteric_deaths, group = 1)) +
  geom_line(color = "lightblue") +
  geom_point(color = "darkgreen") +
  labs(title = "Trend of Enteric Deaths Over Time", x = "Year", y = "Enteric Deaths (%)") + facet_wrap(~age) + theme_bw()

# reorder age group factor
data$age <- fct_relevel(data$age, "<5 years", "5-9 years", "10-14 years")



# Part 2 
# Statistical tests on ggplot and plot modifications
# Add statistical tests








