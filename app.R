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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grass Pollen Density Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
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
            
            sliderInput("mean_tempq",
                        "Mean Temperature First Quarter",
                        min = 0,
                        max = 10,
                        value = 6),
            
            sliderInput("heat_sum",
                        "Cumulative Daily Mean Temperature of the Year",
                        min = 0,
                        max = 4000,
                        value = 2000),
            
            sliderInput("mean_windsp",
                        "Daily Mean Wind Speed",
                        min = 0,
                        max = 15,
                        value = 5),
            
            radioButtons("season", "Day Previously in Pollen Season:",
                         c("Yes" = "1",
                           "No" = "0"))
      
        ),
        
         mainPanel(
             plotOutput("pollenPlot", width = "700px", height = "700px")
        )
        
       
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


     output$pollenPlot <- renderPlot({
         
         model <- readRDS("pollen_model.RDS")
         grass <- raster("grass_grains_mean.tif")
         
         ll <- SpatialPoints(postcode_lookup(input$postcode)[,c("eastings", "northings")])
         ggrm <- raster::extract(grass, ll, buffer = 5000, fun = mean)/cellStats(grass, mean)
         
         new_data <- data.frame(max_temp = input$max_temp, 
                                total_precip = input$total_precip,
                                mean_rel_hum = input$mean_rel_hum,
                                mean_tempq = input$mean_tempq,
                                heat_sum = input$heat_sum,
                                season = input$season,
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
         df <- data.frame(x,y)
         ggplot(df, aes(x = x, y = y))+
             geom_point(color = "orange", shape = 8)+
             theme_void()+
             ggtitle(paste0(pollen, " grams per cubic metre.\n ", risk))+
             coord_equal()+
             theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
                   panel.border = element_rect(colour = "black", fill=NA, size=1))+
             xlim(0,1)+
             ylim(0,1)
     })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
