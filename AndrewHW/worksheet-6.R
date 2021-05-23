## Web Scraping

library(httr)

response <- GET('http://research.jisao.washington.edu/pdo/PDO.latest')
response

library(rvest) 

pdo_doc <- read_html(response)
pdo_doc

pdo_node <- html_node(pdo_doc, "p")
pdo_text <- html_text(pdo_node)

library(stringr)
pdo_text_2017 <- str_match(pdo_text, "(?<=2017).*.(?=\\n2018)")

str_extract_all(pdo_text_2017[1], "[0-9-.]+")

##Exercise 1: Extract country pop from wikipedia
library(rvest)
url <- 'https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population'
doc <- read_html(url)
table_node <- html_node(doc, xpath='//*[@id="mw-content-text"]/div/table[1]')
pop_table <- html_table(table_node)

## HTML Tables

census_vars_doc <- read_html('https://api.census.gov/data/2017/acs/acs5/variables.html')

table_raw <- html_node(census_vars_doc, 'table')

census_vars <- html_table(table_raw, fill = TRUE) 

library(tidyverse)

census_vars %>%
  set_tidy_names() %>%
  select(Name, Label) %>%
  filter(grepl('Median household income', Label))

## Web Services

path <- 'https://api.census.gov/data/2018/acs/acs5'
query_params <- list('get' = 'NAME,B19013_001E', 
                     'for' = 'county:*',
                     'in' = 'state:24')

response = GET(path, query = query_params)
response

response$headers['content-type']

## Response Content

library(jsonlite)

county_income <- response %>%
  content(as = 'text') %>%
  fromJSON()

## Specialized Packages

library(tidycensus)
source('~/SENSummer2020/AndrewHW/census_api_key.R')
variables <- c('NAME', 'B19013_001E')

county_income <- get_acs(geography = 'county',
                         variables = variables,
                         state = 'MD',
                         year = 2018,
                         geometry = TRUE)

ggplot(county_income) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  coord_sf() + 
  theme_minimal() + 
  scale_fill_viridis_c()


##EXERCISE 2: identify variable names based on concept
library(tidyverse)
library(tidycensus)
source('census_api_key.R')

# Using the previously created census_vars table, find the variable ID for population count.
census_vars <- set_tidy_names(census_vars)
population_vars <- census_vars %>%
  filter(grepl('COUNT OF THE POPULATION', Concept))
pop_var_id <- population_vars$Name[1]

# Use tidycensus to query the API.
county_pop <- get_acs(geography = 'county',
                      variables = pop_var_id,
                      state = 'MD',
                      year = 2018,
                      geometry = TRUE)

# Map of counties by population
ggplot(county_pop) + 
  geom_sf(aes(fill = estimate), color = NA) + 
  coord_sf() + 
  theme_minimal() + 
  scale_fill_viridis_c()

## Paging & Stashing

api <- 'https://api.nal.usda.gov/fdc/v1/'
source('~/SENSummer2020/AndrewHW/datagov_api_key')
path <- 'foods/search'

query_params <- list('api_key' = Sys.getenv('DATAGOV_KEY'),
                     'query' = 'fruit')

doc <- GET(paste0(api, path), query = query_params) %>%
  content(as = 'parsed')

fruit<-doc$foods[[1]]

nutrients <- map_dfr(fruit$foodNutrients, 
                     ~ data.frame(name = .$nutrientName, 
                                  value = .$value))

library(DBI) 
library(RSQLite)

fruit_db <- dbConnect(SQLite(), '~/SENSummer2020/AndrewHW/fruits.sqlite') 

query_params$pageSize <- 100

for (i in 1:10) {
  # Advance page and query
  query_params$pageNumber <- i
  response <- GET(paste0(api, path), query = query_params) 
  page <- content(response, as = 'parsed')

  # Convert nested list to data frame
  values <- tibble(food = page$foods) %>%
    unnest_wider(food) %>%
    unnest_longer(foodNutrients) %>%
    unnest_wider(foodNutrients) %>%
    filter(grepl('Sugars, total', nutrientName)) %>%
    select(fdcId, description, value) %>%
    setNames(c('foodID', 'name', 'sugar'))
  
  # Stash in database
  dbWriteTable(fruit_db, name = 'Food', value = values, append = TRUE)
  
}

fruit_sugar_content <- dbReadTable(fruit_db, name = 'Food')

dbDisconnect(fruit_db)

##EXERCISE 3:ACESS API FOR SPECIFIC DATA
library(httr)
library(DBI) 
library(RSQLite)

source('datagov_api_key.R')

api <- 'https://api.nal.usda.gov/fdc/v1/'
path <- 'foods/search'

query_params <- list('api_key' = Sys.getenv('DATAGOV_KEY'),
                     'query' = 'cheese',
                     'pageSize' = 100)

# Create a new database
cheese_db <- dbConnect(SQLite(), 'cheese.sqlite') 

for (i in 1:3) {
  # Advance page and query
  query_params$pageNumber <- i
  response <- GET(paste0(api, path), query = query_params) 
  page <- content(response, as = 'parsed')
  
  # Convert nested list to data frame
  values <- tibble(food = page$foods) %>%
    unnest_wider(food) %>%
    unnest_longer(foodNutrients) %>%
    unnest_wider(foodNutrients) %>%
    filter(grepl('Protein', nutrientName)) %>%
    select(fdcId, description, value) %>%
    setNames(c('foodID', 'name', 'protein'))
  
  # Stash in database
  dbWriteTable(cheese_db, name = 'Food', value = values, append = TRUE)
  
}

dbDisconnect(cheese_db)

