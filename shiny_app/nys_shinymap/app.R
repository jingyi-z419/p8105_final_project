#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(leaflet)
library(RColorBrewer)
library(DT)
library(rgdal)
library(gpclib)
library(maptools)
library(R6)
library(raster)
library(broom)
library(scales)
library(reshape2)
library(tidyverse)
library(data.table)
library(highcharter)



#Read in dataset
Data = 
    read_csv("./data/ny_local.csv")


#Specify shinyApp(ui, server)
shinyApp(options = list(height = 1000), 
         
         #Define the user interface element
         ui = fluidPage(
             titlePanel("Health Indicators in New York State"),
             fluidRow(
                 column(8
                        
                        #Create element to allow user input. 
                        , selectInput('category', 'Select Category'
                                      , choices = unique(Data$category)
                        )
                        #render ui elements within the server function. 
                        , uiOutput('measures'))
                 
                 , column(4, uiOutput('slider')
                          
                 )
             ),
             
             fluidRow(column(12, leafletOutput('mymap', height = 550))
             )
         )
         
         #Define functionality
         ,server = function(input, output, session){
             
             
             #Read the data into a reactive function. 
             df1 = reactive({
                 df = Data
                 df = subset(df, select = c('city_name','state_abbr', 'geo_location', 'year'
                                            , 'measure', 'data_value', 'population_count', 'geographic_level'
                                            , 'short_question_text', 'category', 'data_value_type_id'))
                 
                 #Removes NA values
                 df = df[!is.na(df$data_value),]
                 
                 df = subset(df, category == input$category)
                 #df = subset(df, GeographicLevel == as.character(input$geoLevel))
             })
             
             #renderUI to dynamically generate ui elements. 
             output$measures = renderUI({
                 selectInput('measures', 'Select Measure', choices = unique(df1()$measure))
             })
             
             #Filter the data again based on the measure chosen by the user. 
             df2 = reactive({
                 x = df1()
                 x = subset(x, measure == as.character(input$measures))
             })
             
             #Create a slider to filter the map markers. 
             output$slider = renderUI({
                 sliderInput('slider', 'Filter Based on Prevalence', 0, 100, value = c(0,100))
             })
             
             #Build the leaflet map
             output$mymap = renderLeaflet({
                 df = df2()
                 
                 #Filter the data set based on values from the slider input
                 df = subset(df, data_value > input$slider[1] & data_value < input$slider[2])
                 
                 #Define color pallete
                 Colors = brewer.pal(8,"Set2")
                 
                 #Apply pallete to values from data set
                 binpal = colorBin(Colors, df$data_value, 6, pretty = FALSE)
                 
                 #Separate GeoLocation column into latitude and longitude columns. 
                 lat = vector()
                 lng = vector()
                 for (i in 1:nrow(df)) {
                     x = unlist(strsplit(df$geo_location[i], ",")) 
                     lat[i] = substr(x[1],2,8) 
                     lng[i] = substr(x[2],2,9) 
                     
                 }
                 
                 df$lat = as.numeric(lat)
                 df$lng = as.numeric(lng)
                 
                 #Build leaflet map
                 leaflet() %>%
                     
                     #Adds state borders to the map
                   addProviderTiles(providers$CartoDB.Positron) %>% 
          
                     
                     #Add the markers for each location
                     addCircleMarkers(lat = df$lat, 
                                      lng = df$lng,
                                      data = df,
                                      label = paste(df$data_value),
                                      color = ~binpal(data_value),
                                      radius = 5,
                                      fillColor = ~binpal(data_value),
                                      fill = TRUE,
                                      opacity = 0.8
                                      
                     ) %>%
                     addLegend(position = 'bottomleft', pal = binpal, values = df$data_value, title = "Prevalence"
                     )
                 
             })
             
             
             
         }
)


