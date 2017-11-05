#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$map <- renderLeaflet({#Draw in a map location about Airports, Ports and Train Stations accross the world
    
    #Download the info
    url<-"https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports-extended.dat"
    download.file(url,"airportsandmore.csv")
    
    #Create a proper dataframe
    airportsandmore<-read.csv("airportsandmore.csv",header=FALSE)
    columnNames<-c("Airport ID",
                   "Name",
                   "City",
                   "Country",
                   "IATA",
                   "ICAO",
                   "Latitude",
                   "Longitude",
                   "Altitude",
                   "Timezone",
                   "DST",
                   "Tz database time zone",
                   "Type",
                   "Source")
    
    #adding column names 
    #source: https://openflights.org/data.html
    
    colnames(airportsandmore)[] <- columnNames
    
    #dropping records whose type=="unknown"
    
    airportsandmore<-subset(airportsandmore,!Type=="unknown")
    
    #rename data column names
    colnames(airportsandmore)[which(names(airportsandmore) == "Latitude")] <- "lat"
    colnames(airportsandmore)[which(names(airportsandmore) == "Longitude")] <- "lng"
    
    
    
    
    # GRAPHICAL PART
    
    #Generate Icons
    airportIconUrl="http://en.wikipilipinas.org/images/archive/3/3f/20070629015340%21Airport_symbol.png"
    airportIcon<- makeIcon(iconUrl=airportIconUrl,iconWidth = 30,iconHeight=30)
    
    portIconUrl="https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Ship.svg/600px-Ship.svg.png"
    portIcon<- makeIcon(iconUrl=portIconUrl,iconWidth = 30,iconHeight=30)
    
    stationIconUrl="https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Q55488_noun_19262_ccPierreLucAuclair_train-station.svg/2000px-Q55488_noun_19262_ccPierreLucAuclair_train-station.svg.png"
    stationIcon<-makeIcon(iconUrl=stationIconUrl,iconWidth = 30,iconHeight=30)
    
    
    # Select of lat and long columns
    airportsLocation<-airportsandmore[,7:8]
    
    #Create BaseMap
    
    map<-airportsLocation %>% 
      leaflet() %>% 
      addTiles(group="OpenStreetMap") %>%
      addProviderTiles('Esri.WorldImagery',group="SatelliteView") 
    
    #Add just airports  
    map<-map %>% 
      addMarkers(,lng=airportsLocation$lng[airportsandmore$Type=="airport"],lat=airportsLocation$lat[airportsandmore$Type=="airport"],icon=airportIcon, clusterOptions=markerClusterOptions(), popup =airportsandmore$Name[airportsandmore$Type=="airport"],group="Airports")
    
    
    #Add just ports  
    map<-map %>% 
      addMarkers(,lng=airportsLocation$lng[airportsandmore$Type=="port"],lat=airportsLocation$lat[airportsandmore$Type=="port"],icon=portIcon, clusterOptions=markerClusterOptions(), popup =airportsandmore$Name[airportsandmore$Type=="port"], group="Ports")
    
    #Add just stations 
    map<-map %>% 
      addMarkers(,lng=airportsLocation$lng[airportsandmore$Type=="station"],lat=airportsLocation$lat[airportsandmore$Type=="station"],icon=stationIcon, clusterOptions=markerClusterOptions(), popup =airportsandmore$Name[airportsandmore$Type=="station"], group="Train Stations")
    
    #Add control layer
    
    
    map<-map %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "SatelliteView"),
        overlayGroups = c("Airports","Ports", "Train Stations"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
    
    
    
    
    
    
    
    
    
  })
  
})
