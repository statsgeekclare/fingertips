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
hp_data1 <- hp_data %>%
  mutate_if(is.factor, as.character) %>%
  filter(str_detect(CategoryType, "^$"), AreaType == "Counties & UAs (from Apr 2021)") %>%
  select(IndicatorName, AreaName, Age, Sex, Timeperiod, Value) %>%
  group_by(IndicatorName, Sex) %>%
  filter(Timeperiod == max(Timeperiod)) %>%
  mutate(index = paste(Sex, "-", Age, "-", Timeperiod, "-", IndicatorName)) %>%
  ungroup() %>%
  select(-c(IndicatorName, Sex, Age, Timeperiod)) %>%
  distinct() %>%
  spread(index, Value) %>%
  mutate_if(is.numeric, list(~ impute(., mean), scale))

#Create correlation network map
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
  geom_node_text(aes(label = str_wrap(substring(name, 1, 70), 30), size = ifelse(nchar(name) > 40, 1.5, 2)), repel = TRUE) +
  theme_graph() +
  labs(title = paste("Correlation map of ", profile_name$ProfileName, "profile"))