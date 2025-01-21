

#---------------------------------------------------------------------------#
# Libraries
#---------------------------------------------------------------------------#

library(shiny)
library(tidyverse)

#---------------------------------------------------------------------------#
# inspecting and formatting data: airbnb_listings.csv and listings.csv
#---------------------------------------------------------------------------#

airbnb <- read_csv("data/airbnb_listings.csv")

airbnb_complete <- read_csv("data/listings.csv")

#no missing values

any(is.na("airbnb_listings.csv"))
any(is.na("listings.csv"))

#adding interesting values from the detailed listing to the dataset "airbnb"
beds <- airbnb_complete$beds

airbnb <- airbnb %>%
   mutate(beds = beds)

accommodates <- airbnb_complete$accommodates

airbnb <- airbnb %>%
  mutate(accommodates = accommodates)

host_is_superhost <- airbnb_complete$host_is_superhost

airbnb <- airbnb %>%
   mutate(host_is_superhost  = host_is_superhost)

airbnb<- airbnb %>%
   mutate(beds = case_when(
     beds == 1 ~ "1",
     beds == 2 ~ "2",
     beds == 3 ~ "3",
     beds == 4 ~ "4",

     TRUE ~ "other"
   ))

airbnb$beds <- as.character(airbnb$beds)

str(airbnb)

###availability_365 divided in ranges

availability_ranges <- c("0-30", "31-60", "61-90", "91-120", "121-200", "201-365" )

airbnb$availability_range <- cut(airbnb$availability_365,
                                  breaks = c(0, 30, 60, 90, 120, 200, 365),
                                  labels = availability_ranges,
                                  right = FALSE)



#create new csv.-file
write.csv(airbnb, "airbnb_data_2023.csv", row.names = FALSE)
