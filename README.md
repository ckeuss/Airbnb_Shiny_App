# Airbnb Data viz and manipulation
## R Shiny-App

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

### Insights on airbnb listings in Berlin in 2023
- Looking at the licensed superhosts with accommodations available 201-365 days a year, no price restrictions and having 2 beds it becomes clear that the most accommodations that fit that criteria are in Mitte (62), followed by Friedrichshain-Kreuzberg (45) and Pankow (38). The median price for entire apartments lie at around 240 Euro for Mitte, 140 Euro for Friedrichshain-Kreuszberg and also around 140 Euro for Pankow. However, it should be mentioned that there are several serviced apartments in Mitte that might have a higher price due to the additional service.
- Further, most superhosts in these three neighbourhoods with the availability of 201-365 days a year and no price restriction offered an entire apartment instead of e.g. a hotel room or private room. The most listings in Freidrichshain-Kreuzberg and Pankow offer 2 beds, in Mitte the most superhost listings have 1 bed (80) and accommodate 2 people.
- Based on the given data there were 128 airbnb superhost listings in total that offered accommodation with more than 4 beds, with all kinds of availabilities throughout the year, again with the most being in Mitte.

