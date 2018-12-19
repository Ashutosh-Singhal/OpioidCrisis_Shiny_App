#Ashutosh-app

#=====
shinyServer(function(input, output) {
  
  
  # Box.1. Overdose deaths MAP
  output$map_fatal_overdose <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_geo(overdoses_lower, locationmode = 'USA-states') %>%
      add_trace(
        z = ~death_rate_perM, locations = ~Abbrev,
        colors = 'Reds'
      ) %>%
      colorbar(title = "Overdose fatality per \n 100,000 population", x = -0.95, y = .25, xanchor = 'left', yanchor = 'middle') %>%
      layout(
        geo = g)
    
  })
  

  # Box.2. state_allRx1 dataset: Number of prescriptions by state selected  
  output$Fig_OpRx_by_state <- renderPlot({
    # browser()
    state_allRx1 %>% 
      filter(State == input$state) %>%
    
    ggplot(aes(x = reorder(State, total_OpRx_by_State), y = total_OpRx_by_State)) +
      geom_bar(stat = "identity", fill='gold', width = 0.4)+
      xlab('') + 
      ylab('Number of Prescriptions') +
      scale_y_continuous(limits = c(0,200000), expand=c(0,0))+
      theme_classic() +
      theme(legend.position="none",
            axis.title.y = element_text(color="brown", size=18, face="bold"),
            axis.text.x = element_text(size= 18, face="bold",angle=0),
            axis.text.y = element_text(size = 14, face="bold",angle=0)
      )
  })#Number of prescriptions
  
  
  # Box.3. Gender of prescriber
  
  output$fig_averageprescription_byGender <- renderPlot({
master_df1%>%
      filter(State == input$state)%>%
    select(Gender, State, Codeine:Tramadol, Morphine:Oxycodone)%>%
    group_by(State, Gender)%>%
    summarise(
      total_OpRx = sum(Codeine + Fentanyl + Hydrocodone + Hydromorphone + Methadone + Morphine + Oxycodone + Oxycontine + Tramadol), 
      Counts= n()) %>%
    arrange(desc(total_OpRx))%>%
    
  ggplot(aes(x= State, y= total_OpRx/Counts)) + 
    geom_col(aes(fill=Gender), width = 0.4)+
    xlab("")+
    ylab("Number of prescriptions / Prescriber")+
      scale_y_continuous(limits = c(0,500), expand=c(0,0))+
      theme_classic()+
      theme(legend.position = c(0.85, 0.8), 
            legend.background = element_rect(color = "black", 
                                             fill = "white", size = 1, linetype = "solid"),
            axis.title.y = element_text(color="brown", size=18, face="bold"),
            axis.text.x = element_text(size= 18, face="bold",angle=0),
            axis.text.y = element_text(size = 14, face="bold",angle=0)
      )
    
})#Gender of prescriber
  
# Number of Male & Femaile prescriber 
  output$average_male_female <- renderTable({
    Rx_byGender2 %>%
      filter(State == input$state)%>%
      select(State, Gender, Counts)%>%
      print(Rx_byGender2)
  })#closing Male-Female prescriber 
  
  # Title of Table in Box. 3
  output$title_table1 <- renderText("Number of Male/Female Prescribers")
  

# Box 4: Table: Specialty of major prescribers 
  
  output$prescribers_by_states <-renderTable({
    prescribers_by_states%>%
    filter(State == input$state)%>%
    print(prescribers_by_states)
  })# Closing Specialty 
  
# Plot: Specialty of major prescribers
  
  
  output$fig_prescribers_by_states <-renderPlot({
    master_df1%>%
      filter(State == input$state)%>%
      select(NPI, State, Specialty)%>%
      group_by(State, Specialty)%>%
      summarise(NPI = n_distinct(NPI))%>%
      filter(NPI >25)%>%
      arrange(desc(NPI))%>%
      
      ggplot(aes(x=State, y=NPI))+
      geom_col(aes(fill=Specialty), width = 0.4)+
      xlab('') + 
      ylab('Number of prescribers')+
      scale_y_continuous(expand=c(0,0))+
      theme_classic()+
      theme(legend.position = c(0.85, 0.5), 
            legend.background = element_rect(color = "black", 
                                             fill = "white", size = 1, linetype = "solid"),
            axis.title.y = element_text(color="brown", size=18, face="bold"),
            axis.text.x = element_text(size= 18, face="bold",angle=0),
            axis.text.y = element_text(size = 14, face="bold",angle=0)
      )
      
  })# Closing Specialty
  
#
  output$value1 <- renderValueBox({
    a <- sum(overdoses_lower$Deaths)
    valueBox("Total Deaths in 2014 ", a, color = "red") 
  })
  
  output$value2 <- renderValueBox({
    b <- overdoses_lower%>%
      select(State, Deaths)%>%
      arrange(desc(Deaths))%>%
      head(n=1)
    
    valueBox("#1 State in Deaths", b, color = "yellow")
      
  })
  
  output$value3 <- renderValueBox({
    c <- 90000
    valueBox("People at risk", c, color = "purple")
  })
  
  
})#closing shinyServer

