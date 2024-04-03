

#---------------------------------------------------------------------------#
# Libraries
#---------------------------------------------------------------------------#

library(shiny)
library(tidyverse)
library(readr)

library(data.table)
library(dplyr)

#---------------------------------------------------------------------------#
# data loading, inspection and formatting
#---------------------------------------------------------------------------#

# airbnb <- read_csv("data/airbnb_listings.csv")
# 
# airbnb_complete <- read_csv("data/listings.csv")
# 
# 
# #no missing values
# 
# any(is.na("airbnb_listings.csv"))
# any(is.na("listings.csv"))
# 
# #adding interesting values from the detailed listing to the dataset "airbnb"
# 
# beds <- airbnb_complete$beds
# 
# 
# airbnb <- airbnb %>%
#   mutate(beds = beds)
# 
# accommodates <- airbnb_complete$accommodates
# 
# airbnb <- airbnb %>%
#   mutate(accommodates = accommodates)
# 
# host_is_superhost <- airbnb_complete$host_is_superhost
# 
# airbnb <- airbnb %>%
#   mutate(host_is_superhost  = host_is_superhost)
# 
# 
# airbnb<- airbnb %>%
#   mutate(beds = case_when(
#     beds == 1 ~ "1",
#     beds == 2 ~ "2",
#     beds == 3 ~ "3",
#     beds == 4 ~ "4",
#     
#     TRUE ~ "other"
#   ))
# 
# 
# airbnb$beds <- as.character(airbnb$beds)
# 
# str(airbnb)
# 
# ###availability_365 divided in ranges
# 
 availability_ranges <- c("0-30", "31-60", "61-90", "91-120", "121-200", "201-365" )
# 
# airbnb$availability_range <- cut(airbnb$availability_365,
#                                  breaks = c(0, 30, 60, 90, 120, 200, 365),
#                                  labels = availability_ranges,
#                                  right = FALSE)
# 
# 
# 
# #create new csv.-file
# write.csv(airbnb, "airbnb_data_2023.csv", row.names = FALSE)

airbnb <- read_csv("airbnb_data_2023.csv")

#---------------------------------------------------------------------------#
# UI
#---------------------------------------------------------------------------#

### 1. Title
### 2. Sidebarlayout
#### a) SelectInput
#### b) RadioButtons
#### c) SliderInput
#### d) CheckBoxen
### 3. MainPanel
#### a) Text
#### b) Plot
#### c) Dataframe



ui <- fluidPage(
  # Title
  titlePanel("Airbnb Data Viz and Manipulation"),
  
  # Sidebarlayout
  
  sidebarLayout(
    sidebarPanel(
      # SelectInput
      selectInput("neighbourhood_group",
                  "Kiez/Neighbourhood", # title
                  choices = unique(airbnb$neighbourhood_group),
                  selected = "Pankow"), # pre-selection
      
      
      
      # Radiobutton
      radioButtons("license",
                   "License:",
                   choices = list("No" = "NA",
                                  "Yes"= "notNA"),
                   selected = "notNA"),
      
      
      # SliderInput
      sliderInput("price",
                  "Price in [Euro]: ",
                  min = min(airbnb$price),
                  max = 1000,
                  value = c(min(airbnb$price), 
                            1000)),
      
      
      
      # Checkbox
      
      checkboxGroupInput("availability_365",
                         "Availability [days/year]:",
                         choices = availability_ranges,
                         selected = "201-365"),
      
      
      # Radiobutton
      radioButtons("beds",
                   "Number of beds:",
                   choices = list("1 bed"= "1", "2 beds"= "2", "3 beds"= "3",
                                  "4 beds" = "4", "> 4 beds" = "other"),
                   selected = "1"),
      
      
      # Checkbox
      checkboxGroupInput("host_is_superhost",
                         "Superhost:",
                         choices = list("Yes" = "TRUE",
                                        "No" = "FALSE"),
                         selected = "TRUE")
      
    ),
    
    # Mainpanel
    mainPanel(
      
      # Download buttons
      downloadButton("downloadData", "Download Filtered Data"),
      downloadButton("downloadPlot", "Download Box Plot"),
      
      # Display number of listings
      textOutput("listingCount"),
      
      # Plot
      plotOutput("Boxplot"),
      
      # Table
      tableOutput("Table"),
      
      
    )
    
    
  ))



#---------------------------------------------------------------------------#
# Server
#---------------------------------------------------------------------------#

server <- function(input, output){
  
  # Reactive input
filtered_data <- reactive(

  airbnb %>%
    filter(neighbourhood_group == input$neighbourhood_group) %>%
    filter(if (input$license == "NA")is.na(license) else !is.na(license)) %>%
    filter(price >= input$price[1] & price <= input$price[2]) %>%
    filter(availability_range %in% input$availability_365) %>%
    filter(beds == input$beds) %>%
    filter(host_is_superhost %in% input$host_is_superhost)
  )
  
  # Number of listings
output$listingCount <- renderText({
  paste("Number of Listings:", nrow(filtered_data())) 
})

 
 
# Boxplot  
 output$Boxplot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = room_type, y = price)) +
      geom_boxplot(color = "#325263", fill = "#5891B0", alpha = 0.5) +
      labs(x = "Type of accommodation", y = "Price in [Euro]") +
      theme_minimal()
  })
  
  # Table
  output$Table <- renderTable({
    filtered_data()
  })
  
  # Download-Button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("boxplot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(ggplot(filtered_data(), aes(x = room_type, y = price)) +
              geom_boxplot(color = "#325263", fill = "#5891B0", alpha = 0.5) +
              labs(x = "Type of accommodation", y = "Price in [Euro]") +
              theme_minimal())
      dev.off()
    }
  )
}


#---------------------------------------------------------------------------#
# Start Server
#---------------------------------------------------------------------------#

shinyApp(ui, server)
