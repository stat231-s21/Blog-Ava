library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(fiftystater)
library(dplyr)
library(tidyr)
library(stringr)

options(scipen = 999)

###############
# import data #
###############
both <- read_csv("both.csv")
both[, "week"]<-as.Date(both$week, "%m/%d/%Y")
both <- both[order(both$"week"),] 


###################################################
# CHOICE VALUES AND LABELS#
###################################################
vacc_choice_values <- c("_m","_p","_both")
vacc_choice_names <- c("Moderna","Pfizer","Both Moderna and Pfizer")
names(vacc_choice_values) <- vacc_choice_names

dose_choice_values <- c("all_doses", "cumulative_doses")
dose_choice_names <- c("Total New Doses this Week", "Cumulative Doses to Date")
names(dose_choice_values) <- dose_choice_names

state_choice_values <- unique(both$jurisdiction)
state_choice_names <- sapply(state_choice_values, FUN = str_to_title)
names(state_choice_values) <- state_choice_names

week_choices <- unique(both$week)






############
#    ui    #
############
ui <- fluidPage(
  
  titlePanel(strong("Weekly Vaccine Allocations by State")),
  
  tabPanel(
    h1(style = "font-family:Impact",
       "Map"),
    sidebarLayout(
      sidebarPanel(
        #SELECT VACCINE BRAND
        selectInput(inputId = "vaccvar", 
                    label = "Choose a vaccine type to plot:", 
                    choices = vacc_choice_values, 
                    selected = "_p"),
        #SELECT DOSE NUMBER
        selectInput(inputId = "dosevar", 
                    label = "Choose a dose number to plot:", 
                    choices = dose_choice_values, 
                    selected = "first_"),
        #SELECT MULTIPLE STATES
        selectizeInput(inputId = "state_name", 
                       label = "Pick states to visualize:", 
                       choices = state_choice_values, 
                       selected = "california", 
                       multiple = TRUE),
        #SLIDER DATE
        sliderTextInput(inputId = "slider", 
                        label = h4(tags$b("Choose a week to plot:")), 
                        choices = week_choices, 
                        selected = "2020-12-21", 
                        grid = TRUE)),
      mainPanel(plotOutput(outputId = "distPlot"))
    )
  )
)
#~~~~~~~~~~~#
# END OF UI #
#~~~~~~~~~~~#







############
# server   #
############

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    dat_vacc <- both%>%
      select(c("jurisdiction", "week", contains(input$vaccvar)))%>%
      select(c("jurisdiction", "week", contains(input$dosevar)))%>%
      filter(week == input$slider)
    
    p <-ggplot(data = dat_vacc %>%filter(jurisdiction %in% input$state_name), aes(map_id = jurisdiction)) +    
      geom_map(aes(fill = get(paste(input$dosevar, input$vaccvar, sep = ""))), map = fifty_states) + 
      
      ###WANT A MAX VALUE FOR FIRST, SECOND, ALL, AND CUMULATIVE
      
      #scale_fill_gradient(low="white", high="dark green") +
      #scale_fill_gradient(low="blue", mid = "white", high="red") +
      scale_fill_gradientn(colors = rainbow(3)) +
      scale_fill_gradientn(colors = c("white", "blue", "red")) +
      #scale_fill_continuous(colors = c(rainbow(5))) +
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      
      coord_map() +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      
      labs(x = "", y = "", 
           title = str_wrap(
             paste(
               names(which(vacc_choice_values == input$vaccvar)), 
               names(which(dose_choice_values == input$dosevar)),
               " to", 
               paste(str_to_title(unlist(input$state_name)), collapse=', ')
             ), 
             80),
           subtitle = paste("Week of ", input$slider))+
      
      theme(legend.position = "left", 
            legend.direction = "vertical",
            panel.background = element_rect(fill = alpha("light blue", 0.3)), 
            legend.key.size = unit(1.5, "cm"),
            legend.key.width = unit(0.5,"cm"),
            text=element_text(size=16,  family="serrif"), 
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))+
      
      labs(fill = "Doses")
    
    
    plot_scale <- function(dosevar) {
      
      if(input$dosevar== "combined_doses" & input$vaccvar == "_both")
        scale_fill_gradient(low="blue", high="red", limits = c(0, 40000000))
      
      else if(input$vaccvar == "_both" & input$dosevar != "cumulative_doses")
        scale_fill_gradient(low="blue", high="red", limits = c(0, 2000000))
      
      else if(input$dosevar%in% c("first_dose", "second_dose"))
        scale_fill_gradient(low="blue", high="red", limits = c(0, 650000))
      
      else if(input$dosevar == "all_doses")
        scale_fill_gradient(low="blue", high="red", limits = c(0, 750000))
      
      else if(input$dosevar== "cumulative_doses" & input$vaccvar == "_both")
        scale_fill_gradient(low="blue", high="red", limits = c(0, 22000000))
      else if(input$dosevar== "cumulative_doses")
        scale_fill_gradient(low="blue", high="red", limits = c(0, 22000000))
    }
    
    p + plot_scale()
  })
  
}

shinyApp(ui = ui, server = server)
