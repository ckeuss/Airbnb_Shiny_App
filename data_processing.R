

#---------------------------------------------------------------------------#
# Libraries
#---------------------------------------------------------------------------#

library(shiny)
library(tidyverse)

#---------------------------------------------------------------------------#
# inspecting and formatting data: airbnb_listings.csv and listings.csv
#---------------------------------------------------------------------------#

airbnb <- read_csv("data/airbnb_listings.csv")

airbnb_detailed <- read_csv("data/listings.csv")

# no missing values

any(is.na(airbnb))
any(is.na(airbnb_detailed))

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

availability_ranges <- c("0-30", "31-60", "61-90", "91-120", "121-200", "201-365" )

airbnb$availability_range <- cut(airbnb$availability_365,
                                 breaks = c(0, 30, 60, 90, 120, 200, 365),
                                 labels = availability_ranges,
                                 right = FALSE)
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

#create new csv.-file
write.csv(airbnb, "airbnb_data_2023.csv", row.names = FALSE)

