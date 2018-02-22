library(knitr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(graphics)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(htmltools)
library(sp)
library(rgdal)

#rsconnect::deployApp('C:\\Users\\Fang\\Desktop\\USC MSBAN\\Fall 2017\\DSO 545\\Homeless Project\\Shiny v2')

# Loading in Data
hcdata = read.csv("data/homelessagg.csv") %>%
  mutate(density2017 = totHomeless2017/as.numeric(Land.Area)) %>%
  mutate(density2016 = totHomeless2016/as.numeric(Land.Area))
  
shelters = read.csv("data/sheltersCT.csv")
# Shelters filtering
lacities = c('Los Angeles',
             'Hollywood',
             'Sherman Oaks',
             'San Pedro',
             'Canoga Park',
             'Venice',
             'North Hills',
             'North Hollywood',
             'Winnetka',
             'Wilmington',
             'Van Nuys',
             'Reseda',
             'Chatsworth')
shelters = filter(shelters, CITY %in% lacities)

crimes = read.csv("data/crimesagg.csv")
calls = read.csv('data/callsagg.csv')

labounds = readOGR("shp/pit/Homeless_Count_2017_Results_CD.shp",
                   layer = "Homeless_Count_2017_Results_CD")
labounds@data = hcdata


# Choices for drop-downs
hcvars = c(
  "Total Homeless" = "totPeople",
  "Total Unsheltered" = "totUnsheltPeople",
  "Total Sheltered" = "totSHPeople",
  "Homeless Density" = "density",
  "Total Encampments" = "totEncamp"
)

# Choices for drop-downs
timeofday = c(
  "12:00 am to 6:00 am" = "1",
  "6:00 am to 12:00 pm" = "2",
  "12:00 pm to 6:00 pm" = "3",
  "6:00 pm to 12:00 am" = "4",
  "All" = "all"
)

weekday = c(
  "Monday" = "1",
  "Tuesday" = "2",
  "Wednesday" = "3",
  "Thursday" = "4",
  "Friday" = "5",
  "SaturdaY" = "6",
  "Sunday" = "7",
  "All" = "all"
)

ui = navbarPage(
  "DSO 545 Homeless Census", id="nav",
  tabPanel("Homeless Map 2017", div(class = "outer",
                                  tags$head(includeCSS("styles.css"),
                                            includeScript("gomap.js")),
                                  
                                  leafletOutput("map2017", width = "100%", height = "100%"),
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 70, left = "auto", right = 20, 
                                                bottom = "auto",
                                                width = 300, height = "auto",
                                                
                                                h3("Filters"),
                                                
                                                selectInput("hchoice", "HC Population Filters", 
                                                            hcvars, selected = "totPeople",
                                                            width = '100%')
                                  ))),
  tabPanel("Homeless Map 2016", div(class = "outer",
                                       tags$head(includeCSS("styles.css"),
                                                 includeScript("gomap.js")),
                                       
                                       leafletOutput("map2016", width = "100%", height = "100%"),
                                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                     draggable = TRUE, top = 70, left = "auto", right = 20, 
                                                     bottom = "auto",
                                                     width = 300, height = "auto",
                                                     
                                                     h3("Filters"),
                                                     
                                                     selectInput("hchoice", "HC Population Filters", 
                                                                 hcvars, selected = "totPeople",
                                                                 width = '100%')
                                       ))),
  tabPanel("Crime Map", div(class = "outer",
                                    tags$head(includeCSS("styles.css"),
                                              includeScript("gomap.js")),
                                    
                                    leafletOutput("crimemap", width = "100%", height = "100%"),
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                  draggable = TRUE, top = 70, left = "auto", right = 20, 
                                                  bottom = "auto",
                                                  width = 300, height = "auto",
                                                  
                                                  h3("Filters"),
                                                  
                                                  selectInput("timechoice", "Time of Day", 
                                                              timeofday, selected = "1",
                                                              width = '100%'),
                                                  selectInput("daychoice", "Weekday", 
                                                              weekday, selected = "1",
                                                              width = '100%')
                                    ))),
  tabPanel("311 Call Map", div(class = "outer",
                            tags$head(includeCSS("styles.css"),
                                      includeScript("gomap.js")),
                            
                            leafletOutput("callmap", width = "100%", height = "100%"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 70, left = "auto", right = 20, 
                                          bottom = "auto",
                                          width = 300, height = "auto",
                                          
                                          h3("Filters"),
                                          
                                          selectInput("timechoice", "Time of Day", 
                                                      timeofday, selected = "1",
                                                      width = '100%'),
                                          selectInput("daychoice", "Weekday", 
                                                      weekday, selected = "1",
                                                      width = '100%')
                            )))
  
  
)


server = function(input, output, session){
  
  output$map2017 = renderLeaflet({
    bins = quantile(hcdata$density2017, seq(0,1,0.2))
    pal <- colorBin("YlOrRd", domain = hcdata$density2017, bins = bins)
    
    leaflet(labounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(totHomeless2017),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Homeless",
                                round(as.numeric(hcdata$totHomeless2017)),"</dd>",
                                "<dd>","Total Sheltered",
                                round(as.numeric(hcdata$totShelt2017)),"</dd>",
                                "<dd>","Total Unsheltered",
                                round(as.numeric(hcdata$totUnshelt2017)),"</dd>")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addMarkers(lng = as.numeric(shelters$LONGITUDE), lat = as.numeric(shelters$LATITUDE),
                 popup = paste("<dt>",shelters$NAME, "</dt>",
                               "<dd>","Hours","</dd>","<dd>",
                               shelters$HOURS,"</dd>")) %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13)
  })
  
  output$map2016 = renderLeaflet({
    bins = quantile(hcdata$density2016, seq(0,1,0.2))
    pal <- colorBin("YlOrRd", domain = hcdata$density2016, bins = bins)
    
    leaflet(labounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(totHomeless2017),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Homeless",
                                round(as.numeric(hcdata$totHomeless2016)),"</dd>",
                                "<dd>","Total Sheltered",
                                round(as.numeric(hcdata$totShelt2016)),"</dd>",
                                "<dd>","Total Unsheltered",
                                round(as.numeric(hcdata$totUnshelt2016)),"</dd>")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addMarkers(lng = as.numeric(shelters$LONGITUDE), lat = as.numeric(shelters$LATITUDE),
                 popup = paste("<dt>",shelters$NAME, "</dt>",
                               "<dd>","Hours","</dd>","<dd>",
                               shelters$HOURS,"</dd>")) %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13)
  })
  
  output$crimemap = renderLeaflet({
    bins = c(0, 1, 2, 10, Inf)
    pal <- colorBin("YlOrRd", domain = hcdata$crime_counts, bins = bins)
    
    leaflet(labounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(crime_counts),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Crimes",
                                round(as.numeric(hcdata$crime_counts)),"</dd>")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addMarkers(lng = as.numeric(shelters$LONGITUDE), lat = as.numeric(shelters$LATITUDE),
                 popup = paste("<dt>",shelters$NAME, "</dt>",
                               "<dd>","Hours","</dd>","<dd>",
                               shelters$HOURS,"</dd>")) %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13)
  })
  
  output$callmap = renderLeaflet({
    bins = c(0, 5, 10, 15, 20, Inf)
    pal <- colorBin("YlOrRd", domain = hcdata$call_counts, bins = bins)
    
    leaflet(labounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(call_counts),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Calls",
                                round(as.numeric(hcdata$call_counts)),"</dd>")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addMarkers(lng = as.numeric(shelters$LONGITUDE), lat = as.numeric(shelters$LATITUDE),
                 popup = paste("<dt>",shelters$NAME, "</dt>",
                               "<dd>","Hours","</dd>","<dd>",
                               shelters$HOURS,"</dd>")) %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
