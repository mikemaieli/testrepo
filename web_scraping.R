#####
#WEB SCRAPING TUTORIAL FROM RFORTHERESTOFUS.COM
#https://rfortherestofus.com/2021/04/how-to-scrape-data-with-r/
#####


###########################################################################
# Load Packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(purrr)



###########################################################################
# Create a function to pull all data from one page ------------------------

# Data from https://www.foodpantries.org/ci/ca-san_francisco

# This line creates the function
import_food_pantry_data <- function(pantry_url) {

  # Get HTML from main page
  pantries_page_html <- read_html(pantry_url)
  
  #identify how to get the text from this page
  pantries_page_text <- pantries_page_html %>% 
    # look for class (.) called span8
    html_elements(".span8") %>% 
    html_text2()
  
  # Use regex to pull data from ONE page
  # Name
  pantry_name <- pantries_page_html %>%
    html_elements("h1") %>%
    html_text() %>%
    as_tibble() %>%
    filter(value != "Food Pantries") %>%
    mutate(value = str_remove(value, " Details Page")) %>%
    pull(value)
  
  # Zip Code
  zip_codes <- pantries_page_text %>% 
    str_extract("\\b\\d{5}\\b")
  pantry_zip_code <- zip_codes[1]
  
  #tribble creates rowWise tibble
  pantry_data <- tribble(
    ~name, ~zip_code,
    pantry_name, pantry_zip_code)
  
  pantry_data
}
  


###########################################################################
# Identify the links for each page that needs to be scraped ---------------

# Get HTML from main page
food_pantries_html <- read_html("https://www.foodpantries.org/ci/ca-san_francisco")

# create a vector of each food panty page URLs
pantries_urls <- food_pantries_html %>%
  #this will pull all links
  html_elements("a") %>%
  html_attr("href") %>%
  as_tibble() %>%
  #the links we want all have "li" in their url
  filter(str_detect(value, "/li/")) %>%
  filter(!str_detect(value, " ")) %>% 
  distinct() %>%
  #mutate(value = str_replace(value, " ", "%20")) %>%
  pull(value)



###########################################################################
# Run the function and store all results in one data frame- ---------------

# this uses purr

pantries_data <- map_df(pantries_urls, import_food_pantry_data)


