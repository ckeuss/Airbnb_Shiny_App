

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

airbnb <- read_csv("data/airbnb_listings.csv")

airbnb_detailed <- read_csv("data/listings.csv")

# no missing values

any(is.na("airbnb_listings.csv"))
any(is.na("listings.csv"))

#adding interesting values from the detailed listing to the dataset "airbnb"

# no of beds
no_beds <- airbnb_detailed$beds
no_beds = as.integer(no_beds)

airbnb <- airbnb %>%
  mutate(no_beds = no_beds)

# no of persons
no_persons <- airbnb_detailed$accommodates

airbnb <- airbnb %>%
  mutate(no_persons = as.integer(no_persons))

# no of host listings
host_listings_count <- airbnb_detailed$host_listings_count

airbnb <- airbnb %>%
  mutate(host_listings_count = as.integer(host_listings_count))

# no of reviews
number_of_reviews <- airbnb_detailed$number_of_reviews

airbnb <- airbnb %>%
  mutate(number_of_reviews = as.integer(number_of_reviews))

# superhost
super_host <- airbnb_detailed$host_is_superhost

airbnb <- airbnb %>%
  mutate(super_host = super_host)

#availability_365
airbnb <- airbnb %>%
  mutate(availability_365 = as.integer(availability_365))

# no of beds groups
airbnb<- airbnb %>%
  mutate(no_beds = case_when(
    no_beds == 1 ~ "1",
    no_beds == 2 ~ "2",
    no_beds == 3 ~ "3",
    no_beds == 4 ~ "4",
    
    TRUE ~ "other"
  ))


airbnb$no_beds <- as.character(airbnb$no_beds)

str(airbnb)

# availability_365 divided in ranges

availability_ranges <- c("0-30", "31-60", "61-90", "91-120", "121-200", "201-365" )

airbnb$availability_range <- cut(airbnb$availability_365,
                                 breaks = c(0, 30, 60, 90, 120, 200, 365),
                                 labels = availability_ranges,
                                 right = FALSE)


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
  #Theme
  theme = shinytheme("simplex"),
  # Title
  titlePanel("Airbnb Data"),
  
  # Sidebarlayout
  
  sidebarLayout(
    sidebarPanel(
      # SelectInput
      selectInput("neighbourhood_group",
                  "District", # title
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
      radioButtons("no_beds",
                   "Number of beds:",
                   choices = list("1 bed"= "1", "2 beds"= "2", "3 beds"= "3",
                                  "4 beds" = "4", "> 4 beds" = "other"),
                   selected = "1"),
      
      
      # Checkbox
      checkboxGroupInput("super_host",
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
      
      # add space
      br(" "),
      
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
    filter(no_beds == input$no_beds) %>%
    filter(super_host %in% input$super_host)%>%
    select(id, name, host_id, host_name, neighbourhood_group, room_type,price, number_of_reviews, reviews_per_month, host_listings_count, availability_365, no_beds, no_persons, super_host)
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

   data <- filtered_data()
   # Rename columns
   colnames(data) <- c("Id", "Name", "Host Id", "Host Name", "District", "Room Type", "Price (Euro)", "No. of Reviews", "Reviews per Month", "No. of Host listings","Availability (days/year)", "No. of Beds", "No. of Persons", "Superhost")
   
   data
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
