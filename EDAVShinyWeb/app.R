library(tidyverse)
library(gridExtra)
library(sp)
library(lubridate)
library(dplyr)
library(shiny)
library(shinythemes)
library(eeptools)
library(ggmap)
library(sf)


weather_nyc <- read_csv('https://raw.githubusercontent.com/harish-cu/edav_fall_2019_citibike_weather/master/data/raw/2018_Weather_NYC.csv')
weather_nyc$DATE <- mdy(weather_nyc$DATE)
# Filter for the time period of the analysis
weather_nyc <- weather_nyc %>% 
    filter(DATE>='2018-01-01', DATE<'2019-01-01')
# Select the necessary variables
weather_nyc <- weather_nyc %>% 
    select('DATE', 'PRCP', 'TAVG', 'TMAX' , 'TMIN', 'SNOW', 'SNWD', 'AWND')

citibike <- read.csv('https://raw.githubusercontent.com/harish-cu/edav_fall_2019_citibike_weather/master/data/clean/2018-citibike-summarydata.csv')
citibike <- citibike %>% 
    mutate(avg_duration = avg_duration/60,
           min_duration = min_duration/60,
           max_duration = max_duration/60)
citibike <- citibike %>% 
    mutate(Date = as_date(Date))
citibike_agg <- citibike %>% 
    group_by(Date) %>% 
    summarise(total_trip_count = sum(trip_count),
              avg_duration = sum(avg_duration*trip_count)/sum(trip_count))
citibike_weather <- citibike_agg %>% 
    inner_join(weather_nyc, by=c("Date"="DATE"))

weather_buckets <- weather_nyc %>% 
    mutate(temp_bucket = cut(x=TAVG, breaks=seq(0,100,5), include.lowest=TRUE, right=FALSE)) %>% 
    group_by(temp_bucket) %>% 
    summarise(freq= n())




citibike_buckets <- citibike_agg %>% 
    mutate(ride_bucket = cut(x=total_trip_count, breaks=seq(0,100000,5000), include.lowest=TRUE, right=FALSE)) %>%
    group_by(ride_bucket) %>% 
    summarise(freq= n())



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
    
    nyc.bike.data <- subset(nyc.bike.data, nyc.bike.data$Date==Selecteddate)
    
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
    
    navbarPage("",
               tabPanel("Histogram buckets" ,
                tags$div(
                            h5("This interactive visualization has been designed to allow the user explore the effect of temperature on citibike ridership. Please select a date from the dropdown in order to highlight the temperature bucket and the citibike ridership bucket for that date. For example, when you select January 3rd (which has an avg temp of 21F and total citibike trips of 24,299), the 4th bar (20-25F) in the weather histogram and the 5th bar(20-25k Trips) in the ridership histogram will be selected."),
                            hr()
                ),
               sidebarLayout(
                       fluidRow(
                           column(8,
                                  dateInput('date_hist',label = 'Date Input',value = "2018-01-03")
                           )
                       ),
                       fluidRow(
                           column(8,plotOutput(outputId="plot_nice", width="1100px",height="500px"))
                       )
                   ),
               tags$div(
                   hr(),
                   p("The idea here is to select a few dates across different seasons of the year (a day in January, a day in April, a day in June, a day in November, etc) to check the relationship between temperature and ridership. What you will notice is that similar areas of the two graphs are highlighted on selection of any date (i.e.) if a low temperature day is selected, low ridership will be observed and if a high temperature day is selected, high ridership will be observed. However, the highest ridership will not be observed during the hottest day (as being too hot is not considered the best day for ridership)")
               )
               ),
               tabPanel("Geospatial Exploration" ,
                        tags$div(
                            h5("This plot will shows the volume of rides at each station for the selected date. The size and color of the bubble is proportional to the volume of rides at that station. Generally, for a cold day, there will not be much activity on the map except for some busy stations while warmer days will see much more activity. This map allows the user to understand whether there are differences in reactions to weather changes across different areas of NYC."),
                            hr()
                        ),
                        sidebarLayout(
                            fluidRow(
                                column(8,
                                       dateInput('date',label = 'Date Input',value = "2018-01-03")
                                )
                            ),
                            fluidRow(
                                column(8,htmlOutput(outputId="text", width="400px",height="200px")),
                                column(8,plotOutput(outputId="tripPlot", width="600px",height="500px"))
                            )
                        ),
                        tags$div(
                            hr(),
                            p("Note that it was getting cumbersome to get and parse the NYC neighborhoods shapefiles and then create a choropleth by mapping the stations to neighborhoods. So we went with a less optimum visual of using bubbles with size and color indicating ridership volume.")
                        )
                    )


))


server <- function(input,output) {
    
    output$text <- renderUI({
        
        weatherdata$DATE <- mdy(weatherdata$DATE)
        weatherdata <- filter(weatherdata, DATE==as.character(input$date))
        str0 <- paste("  ")
        str1 <- paste("Statistics :")
        str2 <- paste("Average Temeprature :", weatherdata$TAVG)
        str3 <- paste("Average Wind :", weatherdata$AWND)
        str4 <- paste("Precipitation :", weatherdata$PRCP)
        str5 <- paste("  ")
        
        HTML(paste( str1, str2,str3,str4, str5, str0, sep = '<br/>'))
        
    })
    
    output$tripPlot <- renderPlot({
        dat.sub1 <- subset(citibikedata, citibikedata$Date==as.character(input$date))
        #dat.sub2 <- citibikedata
        par( mfrow=c(1,2) )
        plotRides(nyc.bike.data=dat.sub1,weatherdata=weatherdata, Selecteddate=as.character(input$date))
        #plotRides(dat.sub1,stations)
        
    } )
    

    
    output$plot_nice <- renderPlot({
        
        
        # weather_buckets$Specific <- ifelse((weather_buckets$temp_bucket == "[25,30)"), 1,0)
        selected_temp <- citibike_weather %>% 
            filter(Date==input$date_hist) %>%
            select(TAVG)
        
        selected_bucket <- selected_temp %>% 
            mutate(temp_bucket = cut(x=TAVG, breaks=seq(0,100,5), include.lowest=TRUE, right=FALSE)) %>% 
            select("temp_bucket")
        
        weather_buckets <- weather_buckets %>% 
            mutate(selected = ifelse(temp_bucket==selected_bucket$temp_bucket, 1,0))
        
        
        pt1 <- ggplot(data=weather_buckets, aes(x=temp_bucket, y=freq)) +
            geom_col(aes(fill=selected)) +
            scale_x_discrete(labels = c("5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50","50-55", "55-60","60-65", "65-70","70-75", "75-80", "80-85")) +
            labs(title = "Average Temperature Buckets Distribution",
                 x = "\n Average Temperature Bucket",
                 y = "Number of Days") +
            theme(text =element_text(size=13),
                axis.text.x = element_text(angle = 90),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "none")
        
        
        selected_ridership <- citibike_weather %>% 
            filter(Date==input$date_hist) %>%
            select(total_trip_count)
        
        selected_ride_bucket <- selected_ridership %>% 
            mutate(ride_bucket = cut(x=total_trip_count, breaks=seq(0,100000,5000), include.lowest=TRUE, right=FALSE)) %>% 
            select("ride_bucket")
        
        citibike_buckets <- citibike_buckets %>% 
            mutate(selected = ifelse(ride_bucket==selected_ride_bucket$ride_bucket, 1,0))
        
        pt2 <- ggplot(data=citibike_buckets, aes(x=ride_bucket, y=freq)) +
            geom_col(aes(fill=selected)) +
            scale_x_discrete(labels = c("0-5K", "5-10K", "10-15K", "15-20K", "20-25K", "25-30K", "30-35K", "35-40K", "40-45K", "45-50K","50-55K", "55-60K","60-65K", "65-70K","70-75K", "75-80K", "80-85K")) +
            labs(title = "Ridership Buckets Distribution",
                 x = "\n Ridership Bucket",
                 y = "") +
            theme(text =element_text(size=13),
                 axis.text.x = element_text(angle = 90),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = "none")
        
        grid.arrange(pt1, pt2,  nrow=1, ncol=2)
        
    })
    
}

shinyApp(server = server, ui = ui)
