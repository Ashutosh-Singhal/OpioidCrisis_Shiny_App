#Ashutosh-app
# if (!"shiny" %in% installed.packages()) {install.packages("shiny")}
# if (!"shinydashboard" %in% installed.packages()) {install.packages("shinydashboard")}
# if (!"devtools" %in% installed.packages()) {install.packages("devtools")}
# if (!"fiftystater" %in% installed.packages()) {install.packages("fiftystater")}
# if (!"conflicted" %in% installed.packages()) {install.packages("conflicted")}
# if (!"mapproj" %in% installed.packages()) {install.packages("mapproj")}
# #if (!"ggmap" %in% installed.packages()) {install.packages("ggmap")}

#library(maps)
#library(mapdata)
#library(ggmap)
#library(fiftystater)

library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(devtools)

library(conflicted)

library(plotly)





#conflict betweeb packages and resolving issues using "conflicted" package


#declaring a preference with `conflict_prefer()
conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")
conflict_prefer("layout", "plotly")


# loading datset of prescription counts by state
state_allRx1 <- readRDS("./data/state_allRx1.RDS")
overdoses_lower <- readRDS('./data/overdoses_lower.RDS')
master_df1 <- readRDS('./data/master_df1.RDS')



state_allRx1 <- as.data.frame(state_allRx1)

overdoses_lower <- as.data.frame(overdoses_lower)





# DROPDOWN MENU

# 1. create states in alphabetical order for dropdown

states <- as.data.frame(master_df1) %>% 
  select(State) %>% 
  unique()

states <- sort(states$State)


# prescribers_by_states
prescribers_by_states <- master_df1%>%
  select(NPI, State, Specialty)%>%
  group_by(State, Specialty)%>%
  summarise(NPI = n_distinct(NPI))%>%
  filter(NPI >25)%>%
  arrange(desc(NPI))

# Rx_byGender2
Rx_byGender2 <- master_df1%>%
  select(Gender, State, Codeine:Tramadol, Morphine:Oxycodone)%>%
  group_by(State, Gender)%>%
  summarise(
    total_OpRx = sum(Codeine + Fentanyl + Hydrocodone + Hydromorphone + Methadone + Morphine + Oxycodone + Oxycontine + Tramadol), 
    Counts= n()) %>%
  arrange(desc(total_OpRx))
