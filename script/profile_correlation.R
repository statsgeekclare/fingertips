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

#Normalise the data
#This part of the script:
  
#converts factor (categorical) variables to character variables
#simplifies column names
#filters the most recent data for each indicator
#creates a unique index for each indicator (indicator+age+sex+period)
#converts a long format table to a cross tab
#removes duplicates
#fills missing data with mean values
#scales (normalises) the dataset (i.e. calculates z-scores for each variable)

## scale the data

## this is not going to work because funs() was deprecated since this code was written
## need to work out how to rewrite this
## also the clean_names bit didn't work either so I commented that bit out
## and the timeperiod bit isn't returning anything - I can see it is trying to use just the most recent data but it doesn't appear to be working

library(janitor)
hp_data1 <- hp_data %>%
  mutate_if(is.factor, as.character) %>%
  filter(CategoryType == "", AreaType == "Counties & UAs (from Apr 2021)" ) %>%
  #janitor::clean_names() %>%
  select(IndicatorName, AreaName, Age, Sex, Timeperiod, Value) %>%
  group_by(IndicatorName, Sex) %>%
  filter(Timeperiod == max(Timeperiod)) %>%
  mutate(index = paste(Sex, "-",Age , "-",Timeperiod, "-", IndicatorName) ) %>%
  ungroup() %>%
  select(-c(IndicatorName, Sex, Age, Timeperiod)) %>%
  distinct() %>%
  spread(index, Value) %>%
  #janitor::clean_names() %>%
  mutate_if(is.numeric, funs(impute(., mean))) %>%
  mutate_if(is.numeric, funs(scale(.))) 

#Create correlation network map
#This code:
#  
#Calculates the correlation matrix between all variables
#Extracts well correlated variables (r > 0.7)
#Converts this to graph (network) format
#Plots the correlations as a network map
## see https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph

hp_cor <- hp_data1 %>%
  select(3:ncol(.)) %>%
  correlate() %>%
  stretch() 

graph_cors <- hp_cor %>% 
  filter(abs(r) > 0.7) %>% 
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors, layout = "igraph", algorithm = "nicely") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r, label = round(r,2)), label_size = 2) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "black", size = 3) +
  geom_node_point() +
  geom_node_text(aes(label = str_wrap(substring(name, 1,70), 30)), size  = 2, repel = TRUE) +
  theme_graph() +
  labs(title = paste("Correlation map of ", profile_name$ProfileName, "profile"))
