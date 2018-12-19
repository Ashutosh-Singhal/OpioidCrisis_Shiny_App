#Ashutosh app

shinyUI(
  dashboardPage(skin= "blue",
    dashboardHeader(title = 'Opioid Crisis in 2014', titleWidth = 400),
    dashboardSidebar(width = 200, tags$blockquote('Opioid Crisis in year 2014: Facts and Figures'),
                     selectInput("state", 
                                 label = "Select State:", 
                                 choices = states,
                                 selected = 'TN')),
                     
    dashboardBody(
      fluidRow(
        valueBoxOutput("value1"),
        valueBoxOutput("value2"),
        valueBoxOutput("value3")
      ),
        fluidRow(
          shinydashboard::box(
            title = "Year 2014: Fatal Opioid Overdose", status = "primary", solidHeader = TRUE,
            plotlyOutput("map_fatal_overdose", height = 400) 
          ),
          
          shinydashboard::box(
            title = "Number of Opioid Prescriptions", status = "primary", solidHeader = TRUE,
           plotOutput("Fig_OpRx_by_state", height = 400)
          ),
          
          shinydashboard::box(
            title = "Who prescribe more: Male or Female?", status = "primary", solidHeader = TRUE,
            plotOutput("fig_averageprescription_byGender", height = 400),
            textOutput("title_table1"),
            tableOutput("average_male_female")
          ),
         
          shinydashboard::box(
           title = "Which Specialty Has More Opioid Prescribers?", status = "primary", solidHeader = TRUE,
           plotOutput("fig_prescribers_by_states", height = 400),
           tableOutput("prescribers_by_states")
         ),
          
         tags$blockquote('Data Source: https://www.kaggle.com/apryor6/us-opiate-prescriptions')
          
        )#Closing fluidRow
      )#Closing dashboardBody    
    )# Closing dashboardPage
    )# Closing shinyUI
  
