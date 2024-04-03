# Airbnb Data viz and manipulation
## R Shiny-App - learning to build an app with R

### Project Overview
This interactive data viz and exploratory analysis project aims to provide insight into the price ranges and number of airbnb accommodation listings depending on various factors such as neighbourhood, type of accommodation, number of beds etc. The chart of choice is the boxplot to show the range as well as the median. It gives potential Airbnb accommodation providers an understanding of the market in Berlin for the year 2023.

The analysis can answer key questions such as:
- How many accommodations are listed in the different neighbourhoods depending on different factors?
- What are the price ranges in the neighbourhoods depending on e.g. the type of accommodation or number of beds?
- What do listings of super hosts have in common?

### Data Sources
The datasets used for this analysis are listings.csv and listings.csv.gz from the website insideairbnb.com/get-the-data.  
[download here](http://insideairbnb.com/get-the-data/)

### Tools
- R for data inspection and formatting
- shiny package for interactive app

### Access to the app
```
library(shiny)
runGitHub(repo = "Airbnb_Shiny_App", username = "ckeuss", ref = "main")
```


### Data Analysis
Some interesting code worked with:
```
airbnb<- airbnb %>%
  mutate(beds = case_when(
    beds == 1 ~ "1",
    beds == 2 ~ "2",
    beds == 3 ~ "3",
    beds == 4 ~ "4",
    TRUE ~ "other"
  ))
```
```
filtered_data <- reactive(

  airbnb %>%
    filter(neighbourhood_group == input$neighbourhood_group) %>%
    filter(if (input$license == "NA")is.na(license) else !is.na(license)) %>%
    filter(price >= input$price[1] & price <= input$price[2]) %>%
    filter(availability_range %in% input$availability_365) %>%
    filter(beds == input$beds) %>%
    filter(host_is_superhost %in% input$host_is_superhost)
  )
```
```
 output$Boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = room_type, y = price)) +
      geom_boxplot(color = "#325263", fill = "#5891B0", alpha = 0.5) +
      labs(x = "Type of accommodation", y = "Price in [Euro]") +
      theme_minimal()
  })
```

