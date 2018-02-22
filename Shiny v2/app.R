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

setwd("~/Downloads/Shiny v2")

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
yearvars = c('2017' = '2017',
             '2016' = '2016')

hcvars = c(
  "Total Homeless" = "totHomeless",
  "Total Unsheltered" = "totUnshelt",
  "Total Sheltered" = "totShelt",
  "Homeless Density" = "density"
)

# Choices for drop-downs
timeofday = c(
  "All Times" = "all",
  "12:00 am to 6:00 am" = "1",
  "6:00 am to 12:00 pm" = "2",
  "12:00 pm to 6:00 pm" = "3",
  "6:00 pm to 12:00 am" = "4"
)

weekday = c(
  "All Days" = "all",
  "Monday" = "1",
  "Tuesday" = "2",
  "Wednesday" = "3",
  "Thursday" = "4",
  "Friday" = "5",
  "Saturday" = "6",
  "Sunday" = "7"
)

month = c(
  "All Months" = "all",
  "January" = "1",
  "February" = "2",
  "March" = "3",
  "April" = "4",
  "May" = "5",
  "June" = "6",
  "July" = "7",
  "August" = "8",
  "September" = "9",
  "October" = "10",
  "November" = "11",
  "December" = "12"
)

ui = navbarPage(
  "DSO 545 Homeless Census", id="nav",
  tabPanel("Interactive Map", div(class = "outer",
                                  tags$head(includeCSS("styles.css"),
                                            includeScript("gomap.js")),
                                  
                                  leafletOutput("map", width = "100%", height = "100%"),
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 70, left = "auto", right = 20, 
                                                bottom = "auto",
                                                width = 300, height = "auto",
                                                
                                                h3("Homeless Population Filters"),
                                                
                                                selectInput("yearchoice", "Year", 
                                                            yearvars, selected = "2017",
                                                            width = '100%'),
                                                
                                                selectInput("hchoice", "HC Population Filters", 
                                                            hcvars, selected = "totHomeless",
                                                            width = '100%'),
                                                
                                                h3("Crime and Calls Time Filters"),
                                                
                                                
                                                selectInput("timechoice", "Time of Day", 
                                                            timeofday, selected = "all",
                                                            width = '100%'),
                                                selectInput("daychoice", "Weekday", 
                                                            weekday, selected = "all",
                                                            width = '100%'),
                                                selectInput("monthchoice", "Month of Year", 
                                                            month, selected = "all",
                                                            width = '100%')
                                  )))
  
)


server = function(input, output, session){
  
  output$map = renderLeaflet({
    
    binsize = switch(input$hchoice,
                     "totHomeless" = c(0, 85, 170, 255, 340, 425, 510, 605, Inf),
                     "totUnshelt" = c(0, 75, 150, 225, 300, 375, 450, 525, Inf),
                     "totShelt" = c(0, 65, 130, 195, 260, 325, 390, 460, Inf),
                     "density" = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, Inf)
    )
    
    if (input$yearchoice == "2017") {
      totals = hcdata$totHomeless2017
      totalsh = hcdata$totShelt2017
      totalunsh = hcdata$totUnshelt2017
    } else {
      totals = hcdata$totHomeless2016
      totalsh = hcdata$totShelt2016
      totalunsh = hcdata$totUnshelt2016
    }
    
    if (input$yearchoice == "2017") {
      homelessdomain = switch(input$hchoice,
                              "totHomeless" = hcdata$totHomeless2017,
                              "totUnshelt" = hcdata$totShelt2017,
                              "totShelt" = hcdata$totShelt2017,
                              "density" = hcdata$density2017
      )
    } else {
      homelessdomain = switch(input$hchoice,
                              "totHomeless" = hcdata$totHomeless2016,
                              "totUnshelt" = hcdata$totShelt2016,
                              "totShelt" = hcdata$totShelt2016,
                              "density" = hcdata$density2016
      )
    }
    
    pal = colorBin("YlOrRd", domain = homelessdomain, bins = binsize)
    
    crimeicon = awesomeIcons(
      icon = 'times-circle',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'red'
    )
    
    sheltericon = awesomeIcons(
      icon = 'home',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'green'
    )
    
    callicon = awesomeIcons(
      icon = 'phone',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'blue'
    )
    
    leaflet(labounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(homelessdomain),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Homeless",
                                round(as.numeric(totals)),"</dd>",
                                "<dd>","Total Sheltered",
                                round(as.numeric(totalsh)),"</dd>",
                                "<dd>","Total Unsheltered",
                                round(as.numeric(totalunsh)),"</dd>"),
                  group = "Homeless") %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      
      # Adding Markers for Shelters
      addAwesomeMarkers(icon = sheltericon,
                        lng = as.numeric(shelters$LONGITUDE), 
                        lat = as.numeric(shelters$LATITUDE),
                        popup = paste("<dt>",shelters$NAME, "</dt>",
                                      "<dd>","Hours","</dd>","<dd>",
                                      shelters$HOURS,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "Shelters") %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13) %>%
      
      
      # Adding Markers for Crimes
      addAwesomeMarkers(icon = crimeicon,
                        lng=crimes$longitude, 
                        lat=crimes$latitude, 
                        popup = paste("<dt>",crimes$CRIME.DESCRIPTION.SHORT, "</dt>",
                                      "<dd>","Victim Age:",
                                      round(as.numeric(crimes$Victim.Age)),"</dd>",
                                      "<dd>","Victim Gender:",
                                      crimes$Victim.Sex,"</dd>",
                                      "<dd>","Victim Race:",
                                      crimes$Victim.Descent,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "Crimes") %>%
      
      # Adding Markers for Calls
      addAwesomeMarkers(icon = callicon,
                        lng=calls$LONGITUDE, 
                        lat=calls$LATITUDE, 
                        popup = paste("<dt>",calls$NCNAME, "</dt>",
                                      "<dd>","Request Source:",
                                      calls$REQUESTSOURCE,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "311 Calls") %>%
      
      # Layer Controls
      addLayersControl(
        overlayGroups = c("Shelters","Homeless","Crimes", "311 Calls"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomright"
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
