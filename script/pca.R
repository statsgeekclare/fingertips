#load libraries

library(fingertipsR) ## for interacting with the Fingertips API
library(tidyverse, warn.conflicts = FALSE) ## packages for data manipulation and plotting
library(stringr) ## text manipulation
library(Hmisc) ## data description
library(corrr) ## calcuate correlation matrices
library(ggraph) ## plot as network charts
library(igraph, warn.conflicts = FALSE) ## network tools

#hopefully use this setting to stop the proxy servers blocking the API
Sys.setenv(no_proxy="*")

#This bit reads in the profiles

profiles <- profiles()
profiles %>% select(ProfileID, ProfileName) %>% distinct() %>%
  knitr::kable(align = "l")

#Select profile and download data
options(digits = 2)

profile_name <- filter(profiles, ProfileID == 26) %>% select(ProfileName) %>% distinct()

hp_data <- fingertips_data(ProfileID = 26, AreaTypeID = "All")

unique(hp_data$IndicatorName) 

#need to filter to most recent time period and just counties and UAs
#google how to do this

hp_data1 <- hp_data %>%
  filter(CategoryType == "", AreaType == "Counties & UAs (from Apr 2021)") %>%
  select(IndicatorName, AreaName, Age, Sex, TimeperiodSortable, Value) %>%
  group_by(IndicatorName, Sex, AreaName) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable))

# divides the data sets into training dataset and test datasets
set.seed(111)
ind <- sample(2, nrow(hp_data1),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- hp_data1[ind==1,]
testing <- hp_data1[ind==2,]

# Scatter Plot & Correlations
library(psych)

# First will check the correlation between independent variables. Let’s remove the factor variable from the dataset for correlation data analysis

pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$IndicatorName],
             pch=21)

