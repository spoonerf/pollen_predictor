#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(PostcodesioR)
library(rgdal)
library(cplm)
library(ggplot2)
library(dplyr)
library(gganimate)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grass Pollen Density Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          dateInput("date",
                    "Date:",
                    value = Sys.Date()),
            
            textInput("postcode",
                      "Postcode:",
                      "SW1A 2AA"),
            
            sliderInput("max_temp",
                        "Daily Maximum Temperature",
                        min = -5,
                        max = 40,
                        value = 20),
            
            sliderInput("total_precip",
                        "Total Daily Precipitation (mm)",
                        min = 0,
                        max = 100,
                        value = 30),
            
            sliderInput("mean_rel_hum",
                        "Daily Mean Relative Humidity",
                        min = 0,
                        max = 100,
                        value = 50),
            
            sliderInput("mean_windsp",
                        "Daily Mean Wind Speed",
                        min = 0,
                        max = 40,
                        value = 10)
      
        ),
        
         mainPanel(
             imageOutput("pollenPlot", width = "590px", height = "590px")
        )
        
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


     output$pollenPlot <- renderPlot({
         
         model <- readRDS("pollen_model.RDS")
         hs_model <- readRDS("heat_sum_model.RDS")
         grass <- raster("grass_grains_mean.tif")
         dist2c_ras  <- raster("distance_to_sea.tif")
         ann_mean_temp_ras <- raster("annual_mean_temp_1981_2010.tif")
         
         
         ll <- SpatialPoints(postcode_lookup(input$postcode)[,c("eastings", "northings")])
         
         doty <- yday(input$date)
         ann_mean_temp <- raster::extract(ann_mean_temp_ras, ll)
         dist2c <- raster::extract(dist2c_ras, ll)
         
         new_data <- data.frame(doty = doty, 
                                   ann_mean_temp = ann_mean_temp,
                                   dist2c = dist2c)
         
         heat_sum <- predict(hs_model, new_data)
         
         
         ggrm <- raster::extract(grass, ll, buffer = 5000, fun = mean)/cellStats(grass, mean)
         
         
         new_data <- data.frame(max_temp = input$max_temp, 
                                total_precip = input$total_precip,
                                mean_rel_hum = input$mean_rel_hum,
                                ann_mean_temp = ann_mean_temp,
                                heat_sum = heat_sum,
                                season = ifelse(doty >= 154 & doty <= 214, 1,0), #pollen season days for poacae
                                mean_windsp = input$mean_windsp,
                                ggrm = ggrm)
         
         pollen <- round(predict(model, new_data), 2)

         risk <- case_when( pollen < 30 ~ "Low risk of hayfever.",
                             pollen >= 30 & pollen <= 49 ~ "Moderate risk of hayfever.",
                            pollen >= 49 & pollen <= 149 ~ "High risk of hayfever.",
                            pollen > 149 ~ "Very high risk of hayfever.")
         n <- floor(pollen)
         x <- runif(n, 0,1)
         y <- runif(n, 0,1)
         
         df <- data.frame(x, y)
         
         ggplot(df, aes(x = x, y = y))+
             geom_point(color = "orange", shape = 8)+
             theme_void()+
             ggtitle(paste0(pollen, " grams per cubic metre.\n ", risk))+
             coord_equal()+
             theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
                   panel.border = element_rect(colour = "black", fill=NA, size=1))+
             xlim(0,1)+
             ylim(0,1)#+
          # transition_states(day,transition_length = .1, state_length = 4) +
        #  ease_aes('linear')


         })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
