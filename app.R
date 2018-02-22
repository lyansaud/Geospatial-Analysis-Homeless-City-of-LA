library(shiny)
library(leaflet)
library(readxl)
library(shinydashboard)
library(graphics)
library(dplyr)
library(ggplot2)
library(DT)


hcdata = read.csv("data/homelessdata.csv")
shelters = read.csv("data/sheltersCT.csv")
crimes = read.csv("data/crimeCT.csv")
calls = read.csv('data/call311CT.csv')

# Choices for drop-downs
hcvars = c(
  "Total Homeless" = "totPeople",
  "Total Unsheltered" = "totUnsheltPeople",
  "Total Sheltered" = "totSHPeople",
  "Homeless Density" = "density",
  "Total Encampments" = "totEncamp"
)

crimevars = c(
  "MO Codes" = "MO.CODES",
  "Victim Age" = "VICTIM.AGE",
  "Victim Gender" ="VICTIM.SEX",            
  "Victim Descent" = "VICTIM.DESCENT"
)

ui <- navbarPage(
  "DSO 545 Homeless Census", id="nav",
  tabPanel("Interactive Map", div(class = "outer",
                      tags$head(includeCSS("styles.css"),
                                includeScript("gomap.js")),
                      
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 70, left = "auto", right = 20, 
                                    bottom = "auto",
                                    width = 500, height = "auto",
                                    
                                    h2("Summaries"),
                                    
                                    selectInput("hchoice", "HC Population Filters", 
                                                hcvars, selected = "totPeople"),
                                    selectInput("crimechoice", "Crime Distribution Filters", 
                                                crimevars, selected = "Victim Age"),
                                    
                                    plotOutput("hchist", height = 200),
                                    
                                    plotOutput("crimehist", height = 250)
                      ))),
  tabPanel("Data explorer",
           fluidRow(
             
             DT::dataTableOutput("summarytable")
           ))
)


server <- function(input, output, session){
  output$map <- renderLeaflet({
    
    leaflet() %>% addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')  %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 10)%>%
      addCircles(lng = as.numeric(hcdata$long), lat = as.numeric(hcdata$lat), weight = 3, 
                 radius = sqrt(hcdata$totPeople)*10, 
                 popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                               "<dd>","Total Homeless",
                               round(as.numeric(hcdata$totPeople)),"</dd>",
                               "<dd>","Total Sheltered",
                               round(as.numeric(hcdata$totSheltPeople)),"</dd>",
                               "<dd>","Total Unsheltered",
                               round(as.numeric(hcdata$totUnsheltPeople)),"</dd>"), 
                 color = "#FFA500") %>%
      
      addCircles(lng = as.numeric(shelters$LONGITUDE), lat = as.numeric(shelters$LATITUDE), 
                 weight = 3, 
                 radius = 100, 
                 popup = paste("<dt>",shelters$NAME, "</dt>",
                               "<dd>","Hours",
                               shelters$HOURS,"</dd>"), 
                 color = "darkgreen")
  })
  
  output$hchist <- renderPlot({
    
    if (input$hchoice == "totPeople") {
      hccount = hcdata %>%
        group_by(Community_Name) %>%
        summarise(totHomeless = sum(totPeople)) %>%
        arrange(desc(totHomeless)) %>%
        slice(1:10)
    } else if (input$hchoice == "totUnsheltPeople") {
      hccount = hcdata %>%
        group_by(Community_Name) %>%
        summarise(totHomeless = sum(totPeople)) %>%
        arrange(desc(totHomeless)) %>%
        slice(1:10)
    } else if (input$hchoice == "totSHPeople") {
      hccount = hcdata %>%
        group_by(Community_Name) %>%
        summarise(totHomeless = sum(totSheltPeople)) %>%
        arrange(desc(totHomeless)) %>%
        slice(1:10)
    } else if (input$hchoice == "totEncamp") {
      hccount = hcdata %>%
        group_by(Community_Name) %>%
        summarise(totHomeless = sum(totEncamp)) %>%
        arrange(desc(totHomeless)) %>%
        slice(1:10)
    } else {
      hccount = hcdata %>%
        group_by(Community_Name) %>%
        summarise(totHomeless = sum(density)) %>%
        arrange(desc(totHomeless)) %>%
        slice(1:10)
    }
    
    hccount %>%
      ggplot(aes(y = totHomeless, 
                 x = reorder(Community_Name, totHomeless))) +
      geom_col() +
      coord_flip() +
      ggtitle('Top 10 Communities with Highest Homeless Count') +
      xlab('Community Name') +
      ylab('Homeless Population Count') +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text=element_text(size=12))
    
  })
  
  output$crimehist <- renderPlot({
    crimes %>%
      group_by(AREA.NAME) %>%
      summarise(totalcrime = n()) %>%
      arrange(desc(totalcrime)) %>%
      slice(1:10) %>%
      ggplot(aes(y = totalcrime, 
                 x = reorder(AREA.NAME, totalcrime))) +
      geom_col() +
      coord_flip() +
      ggtitle('Top 10 Communities with Highest Crime Counts') +
      xlab('Community Name') +
      ylab('Crime Count') +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text=element_text(size=12))
    
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
