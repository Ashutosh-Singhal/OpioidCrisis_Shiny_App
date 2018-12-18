#Ashutosh-app
if (!"devtools" %in% installed.packages()) {install.packages("devtools")}
if (!"mapproj" %in% installed.packages()) {install.packages("mapproj")}
if (!"maps" %in% installed.packages()) {install.packages("maps")}
if (!"mapdata" %in% installed.packages()) {install.packages("mapdata")}
devtools::install_github("wmurphyrd/fiftystater")
#
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
#
library(fiftystater)
library(maps)
library(mapdata)
library(devtools)
#conflict betweeb packages and resolving issues using "conflicted" package
devtools::install_github("r-lib/conflicted")
library(conflicted)
#declaring a preference with `conflict_prefer()
conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")



# loading datset of prescription counts by state
state_allRx1 <- readRDS("OpioidCrisis_Shiny_App/data/state_allRx1.RDS")
overdoses_lower <- readRDS('~/Documents/nss_data_science/OpioidCrisis_Shiny_App/data/overdoses_lower.RDS')
master_df1 <- readRDS('~/Documents/nss_data_science/OpioidCrisis_Shiny_App/data/master_df1.RDS')



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
