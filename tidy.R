library(tidycensus)
library(tidyverse)
library(crsuggest)
library(maps)
library(sf)
library(gridExtra)
library(plotly)
library(shiny)
library(rgdal)


# source('data_clean.R')

shapefile <- st_read("data/counties/us_counties_acs_2021.shp")
st_write(geojs, "data/counties/us_acs_2021.geojson")

gjs <- st_read("src/data/counties/us_acs_2021.geojson")
st_write(gjs, "src/data/counties/us_acs_2021.geojson")

# Later addendums

# 1. Added carper for quicker handling on app-side
# gjs$carper <- gjs$car / gjs$ttl * 100 
# st_write(gjs, "src/data/counties/us_acs_2021.geojson")

# 2. Added column for highest, non-car means, amount, and percentage

# selected_col_names <- colnames(gjs)[c(22, 34, 36, 38, 40, 42)]
# 
# tpnoncar <- apply(gjs, 1, function(x) {
#   max_col <- which.max(x[c(22, 34, 36, 38, 40, 42)])
#   selected_col_names[max_col]
# })
# 
# numnocr <- apply(gjs, 1, function(x) {
#    (x[c(22, 34, 36, 38, 40, 42)] %>% unlist %>% max)
# })
# 
# pernocr <- apply(gjs, 1, function(x) {
#   (x[c(22, 34, 36, 38, 40, 42)] %>% unlist %>% max) / x$ttl * 100
# })
# 
# gjs <- cbind(gjs, tpnoncar)
# gjs <- cbind(gjs, numnocr)
# gjs <- cbind(gjs, pernocr)


get_states <- function(){
  if(exists("state.name")){
    return(state.name)
  } else if(file.exists("data/state_names.csv")){
    states <- read.csv("data/state_names.csv")
    state.name <<- states$x
  } else {
    data(state)
    write.csv(state.name, "data/state_names.csv")
    get_states()
  }
}

# B08301	MEANS OF TRANSPORTATION TO WORK	Workers 16 years and over
means_col_names = c("ttl", "car", "csol", "cpl", "cpl2", "cpl3", 
                    "cpl4", "cpl5", "cpl7", "pt", "ptbus",
                    "pttrl", "ptsb", "ptrr", "ptfr", "taxi", "mtc",
                    "bik", "wlk", "othr", "wfh")

select_col_names = c("geoid", "name", "ttl", "car", "pt", "taxi", 
                     "mtc", "bik", "wlk", "othr", "wfh", "geometry")



# For now, ignoring margins of error, but should return to that
pull_reshape_data = function(geography = "county", var = "B08301", var_names = means_col_names, select_names = select_col_names, 
                             year = 2021, survey="acs5", keep_error = F) {
  # Pull data
  df <- get_acs(geography = geography, table = var, year = year, output = "wide", geometry = T, survey = survey, 
                cache_table = T)
  
  # Reshape columns for legibility and add percentages
  cols = c()
  for(i in var_names) {
    cols = append(cols, c(i, paste0(i, "er")))
  }
  var_names = c("geoid", "name", cols, "geometry")
  colnames(df) = var_names
  
  if(keep_error){
    return(df)
  } else {
    return(df[,colnames(df) %in% select_names])
  }
}

us <- get_acs(geography = "state", table = "B08301", year = 2021, geometry = F, survey = "acs1", 
              cache_table = T)
pattern <- c(1, 2, 10, 16, 17, 18, 19, 20, 21)

create_pattern = function(i, n, last, pattern, acc){
  if(i == n){
    return(acc)
  } else {
    return (create_pattern(i+ 1, n, last, pattern, c(acc, pattern + (last * i))))
  }
}

us_slice = create_pattern(1, 52, 21, pattern, pattern)

us <- us %>% slice(us_slice)
us$means <- rep(c("Total", "Car", "Public Transportation", "Taxi", "Motorcycle", "Bicycle", "Walked", "Other", "Worked From Home"), 52)


us <- us %>% mutate(means = get_var_labels(variable)) %>% select(!c(variable, GEOID, NAME))
us <- us %>% relocate(means)

us <- us %>% slice(c(1, 2, 10, 16, 17, 18, 19, 20, 21))


us$means <- c("Total", "Car", "Public Transportation", "Taxi", "Motorcycle", "Bicycle", "Walked", "Other", "Worked From Home")
colnames(us) <- c("Means", "People", "Error")
write.csv(us, "~/proj/biking-in-the-us/src/data/means_by_state.csv")

load_us_data <- function(){
  if(file.exists("data/counties/us_counties_acs_2021.shp")){
    counties <<- st_read("data/counties/us_counties_acs_2021.shp")
  } else {
    key = readLines("key.txt")
    census_api_key(key)
    counties <<- pull_reshape_data(keep_error = T)
    counties_dir <- paste0(getwd(), "/data/counties")
    if(!dir.exists(counties_dir)) { dir.create(counties_dir)}
    st_write(counties, "data/counties/us_counties_acs_2021.shp")
  }
}

add_percent_col = function(df, var) {
  var_er = paste0(var, "er")
  var_per = paste0(var, "_percentage")
  var_per_moe = paste0(var, "_percentage_moe")
  df = df %>% mutate({{var_per}} := 100 * .data[[var]]/ttl)
  df = df %>% mutate({{var_per_moe}} := 100 * .data[[var_er]]/ttl)
  return(df)
}

####
counties_car <- add_percent_col(counties, "car")
counties_car <- counties_car %>% select(c("name", "ttl", "car", "carer", "car_percentage",
                                          "car_percentage_moe"))

mississippi <- counties_car %>% filter(grepl("*, Mississippi", name), car_percentage_moe < 10) %>% arrange(-car_percentage) 
mass <- counties_car %>% filter(grepl("*, Massachusetts", name), car_percentage_moe < 10) %>% arrange(-car_percentage) 

ggplot() + 
  geom_sf(data = mass, aes(fill = car_percentage), color="#FFFFFF") +
  coord_sf(crs = 6492) +
  labs(title= paste0("Driving", " in ", "Mass"),
       subtitle= paste0("Percentage of people who ", "drive", " to work")) +
  scale_fill_continuous(low ="#F0E442", high ="#234d0d") +
  labs(fill=paste0("Percentage of ", "Car", " Commuters"))

# car, pt, walk, bike, other, wfh


# convenience function to select only 1 var from df, arrange according to that var
slice_arrange = function(summary_df, var, is_desc = T){
  var_per = paste0(var, "_percentage")
  new_slice = select(summary_df, all_of(c("name", "total", var, var_per)))
  if(is_desc){
    arrange(new_slice, desc(.data[[var_per]]))
  } else {
    arrange(new_slice, .data[[var_per]])
  }
}

## 2. Create df of CRSs to save time

get_geography_crs = function(df, fil) {
  new_geo = filter(df, grepl(paste0("*, ", fil), name))
  crs_opts = suggest_crs(new_geo$geometry)
  as.numeric(crs_opts$crs_code[1])
}

create_crs_df = function(df) {
  if(!exists("state.name")){
    data(state) }
  
    crs = c()
    
    for(i in state.name){
      new_crs = get_geography_crs(df, i)
      crs = c(crs, new_crs)
    }
  
    return(data.frame(state.name, crs))
  
}

## 3. Plots

plot_state = function(df, state, transport) {
  if(!exists("crs_df")){
    crs_df = create_crs_df(df)
  }
  
  state_crs = filter(crs_df, state.name == state)$crs
  
  map <- ggplot() + 
    geom_sf(data = new_state, aes(fill = .data[[transport_per]]),color="#FFFFFF") +
    coord_sf(crs = state_crs) +
    labs(title= paste0(transport, " in ", state),
         subtitle= paste0("Percentage of people who ", transport, " to work")) +
    scale_fill_continuous(low ="#F0E442", high ="#234d0d") +
    labs(fill=paste0("Percentage of ", transport, " Commuters"))
  
  return(map)
}

filter_state = function(df, state, transport) {
  transport_per = paste0(transport, "_percentage")

  
  new_state = filter(df, grepl(paste0("*, ", state), name))
  
  plot_state(df, state, transport)
  
  # return(ggplotly(map))
  
  # data(us.cities)
  # alaska_cities = us.cities[us.cities$country.etc %in% c("AK"),]
  # alaska_cities <- st_as_sf(alaska_cities, coords =c("long", "lat"), crs = 6393, remove = F)
  #geom_sf(data = alaska_cities, size = 4, shape = 23, fill = "white", inherit.aes =F) +
  # + geom_text(data = alaska_cities, aes(x= long, y = lat, label = name), fontface = "bold")
}

top_locations_transports = function(df, transport, num = 10, is_desc = T){
  new_slice = slice_arrange(df, transport, is_desc)
  top_n = head(new_slice, n = num)
  transport_per = paste0(transport, "_percentage")
  
  min_x = top_n[[transport_per]] %>% min(na.rm =T) %>% floor()
  max_x = top_n[[transport_per]] %>% max(na.rm =T) %>% ceiling()
  
  if(max_x > 95) {
    max_x = 100
  }
  
  if(min_x < 5) {
    min_x = 0
  }

  ggplot() +
    geom_col(data = top_n, aes(x = .data[[transport_per]], y = reorder(name, .data[[transport_per]]))) +
    coord_cartesian(xlim = c(min_x, max_x)) 
}

## 4. Shiny App

ui <- fluidPage(
  
  fluidRow(
    column(12, titlePanel("How People Across the US Commute"))
  ), 
  
  fluidRow(
    column(12, p("The majority of people (X%) in the US drive to work."))
  ), 
  
  fluidRow(
    column(3, selectInput(inputId = "state", label = "State",
                          choices = state.name,
                          selected = "Alaska"),
           
            selectInput(inputId = "transport", label = "Transport",
                       choices = select_col_names[4:11],
                       selected = select_col_names[4])),
    column(9,  plotOutput(outputId = "state"))
  ),
  
  fluidRow(
    column(3, selectInput(inputId = "top_transport", label = "Transport",
                       choices = select_col_names[4:11],
                       selected = select_col_names[4])),
    column(9,  plotOutput(outputId = "top_transport"))
  ),
  
  fluidRow(
    column(12,  plotOutput(outputId = "federal_funding"))
  )
    

  
)

server <- function(input, output, session) {
  output$state <- renderPlot ({ filter_state(acs_df, input$state, input$transport) })
  output$top_transport <- renderPlot ({ top_locations_transports (acs_df, input$top_transport) })
  output$federal_funding <- renderPlot ({ plot_federal_funding() })
}


acs_df = pull_reshape_data()

crs_df = create_crs_df(acs_df)

for(c in select_col_names[4:11]){
  acs_df = add_percent_col(acs_df, c)
}


all_us <- pull_reshape_data(geography="us", survey="acs1")

shinyApp(ui = ui, server = server)
