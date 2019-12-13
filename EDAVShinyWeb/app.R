library(tidyverse)
library(sp)
library(lubridate)
library(dplyr)
library(shiny)
library(shinythemes)
library(eeptools)
library(ggmap)
library(sf)

# load data
citibikedata <- read.csv("https://raw.githubusercontent.com/harish-cu/edav_fall_2019_citibike_weather/master/data/clean/2018-citibike-summarydata.csv")
#stations <- read.csv("/Users/venkatgangireddy/Documents/GitHub/edav_fall_2019_citibike_weather/data/otherdata/All_NYC_Bike_stations.csv")
weatherdata <- read_csv('https://raw.githubusercontent.com/harish-cu/edav_fall_2019_citibike_weather/master/data/raw/2018_Weather_NYC.csv')

ggmap_credentials()
routeQueryCheck()
API_KEY <- "AIzaSyDHwy7O0EOi3PxtDUI01zOo5PDuRQMoQA0"    # Replace with the Actual google cloud account Key
register_google( key=API_KEY, account_type="premium", day_limit=10000 )
ggmap_credentials()

routeQueryCheck()

#NYC_Neighborhood <- geojson_read( "/Users/venkatgangireddy/Documents/GitHub/edav_fall_2019_citibike_weather/data/otherdata/NYC_Neighborhood.geojson", what="sp" )

#citibikedata$start.station.longitude
plotRides <- function(nyc.bike.data, weatherdata=weatherdata, station.size=0.5,includeWeather = 'F', max.count=1000, Selecteddate = '2018-08-08' ){
    #print(Selecteddate)
    nyc.bike.data <- subset(nyc.bike.data, nyc.bike.data$Date==Selecteddate)
    #weatherdata <- subset(weatherdata, weatherdata$DATE==Selecteddate)
    #weatherdata <- weatherdata[1:1,]
    # print(nrow(nyc.bike.data))
    # starttrip.count <- as.data.frame( table( nyc.bike.data$start.station.id ) )
    # This need to be adjusted based on the daily or monthly
    
    keep.these.vars <- c("start.station.id","start.station.latitude","start.station.longitude","trip_count")
    stations.data <- unique( nyc.bike.data[ keep.these.vars ] )
    names( stations.data ) <- c("ID","LAT","LON","COUNT")
    stations.data <- stations.data[ order( stations.data$ID ) , ]
    rownames( stations.data ) <- NULL
    #print(head(stations.data,100))
    station.size = .5 * nyc.bike.data$trip_count
    
    
    par( mar=c(0,0,0,0) )
    #plot( stations.data$LON, stations.data$LAT, col="darkred", pch=19, cex=.5 )
    
    nyc <- qmap( 'east village, ny', zoom = 13, color = 'bw' )
    nyc + geom_point( aes(x = LON, y = LAT, size=COUNT ),alpha = 0.001*stations.data$COUNT, colour="red", data = stations.data )
    
    
}

plotRides(nyc.bike.data=citibikedata,weatherdata=weatherdata)

ui <- fluidPage (
    #theme = shinytheme("cyborg"),
    titlePanel("Weather Impact on NYC Citi Bike Ridership"),
    sidebarLayout(
        sidebarPanel(
            h2( helpText("First Map") ),
            
            dateInput('date',
                      label = 'Date input: yyyy-mm-dd',
                      value = "2018-08-10"
            ),
            
            #h2( helpText("Second Map") ),
            
            # sliderInput( inputId="Decile", 
            #              label="Temperature Range", 
            #              min=0, max=100,
            #              value=9 
            # ),
            
            
        ),
        
        mainPanel(  
            #verbatimTextOutput("dateText"),
            htmlOutput("text"),
            plotOutput( "tripPlot" )
        )
    )
)


server <- function(input,output) {
    
    output$text <- renderUI({
        
        weatherdata$DATE <- mdy(weatherdata$DATE)
        weatherdata <- filter(weatherdata, DATE==as.character(input$date))
        str1 <- paste("Date :", as.character(input$date))
        str2 <- paste("Average Temeprature :", weatherdata$TAVG)
        str3 <- paste("Average Wind :", weatherdata$AWND)
        str4 <- paste("Precipitation :", weatherdata$PRCP)
        str5 <- paste("  ")
        
        HTML(paste(str1, str2,str3,str4, sep = '<br/>'))
        
    })
    
    
    
    output$tripPlot <- renderPlot({
        dat.sub1 <- subset(citibikedata, citibikedata$Date==as.character(input$date))
        #dat.sub2 <- citibikedata
        par( mfrow=c(1,2) )
        plotRides(nyc.bike.data=dat.sub1,weatherdata=weatherdata, Selecteddate=as.character(input$date))
        #plotRides(dat.sub1,stations)
        
    } )
}

shinyApp(server = server, ui = ui)
