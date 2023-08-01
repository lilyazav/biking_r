library(pdftools)
library(magrittr)
library(tidyverse)
library(plotly)
library(gghighlight)

get_state_data <- function(){
  state_obligations <- pdf_text("./data/obligations_state.pdf") %>% str_split("\n")
  
  df_names <- state_obligations[[1]][2] %>% str_squish() %>% strsplit(" ")
  df_names1 <- c("state", df_names[[1]][-1])
  df_names2 <- state_obligations[[2]][1] %>% str_squish() %>% strsplit(" ")
  df_names2 <- c("state", df_names2[[1]][-1])
  
  yrs_2020_2010 <- state_obligations[[1]][c(-1, -2, -60:-79)]
  yrs_2009_2001 <- state_obligations[[2]][c(-1, -59)]
  
  clean_years <- function(lst, names) {
    state_obligations_df <- data.frame()
    
    for(i in 1:length(lst)) {
      state_obligations_df <- rbind(state_obligations_df, lst[i] %>% str_squish %>% str_replace_all("-\\$", "$-") %>% 
                                      str_replace_all(",", "") %>%
                                      strsplit(" \\$") %>% extract2(1))
    }
    
    colnames(state_obligations_df) = names
    return(state_obligations_df)
  }
  
  y20_21_df <- clean_years(yrs_2020_2010, df_names1)
  y09_01_df <- clean_years(yrs_2009_2001, df_names2)
  
  bike_ped_infrastructure <- merge(y20_21_df, y09_01_df, by="state")
  # Remove summary statistics
  bike_ped_infrastructure <- bike_ped_infrastructure[-48,-2]
  
  
  bike_ped_long <- pivot_longer(bike_ped_infrastructure, cols = names(bike_ped_infrastructure[2:23]))
  names(bike_ped_long) <- c("state", "year", "value")
  
  bike_ped_long$year <- as.Date(bike_ped_long$year, "%Y")
  bike_ped_long$value <- as.numeric(bike_ped_long$value)
  
  #convert value to millions
  bike_ped_long$value <- bike_ped_long$value / 1000000
  
  return(bike_ped_long)
}


plot_federal_funding <- function(){
  bike_ped_long <- get_state_data()
  states_plot <- ggplot(data = bike_ped_long, aes(x=year, y= value, color = state)) +  
    geom_point() + geom_line() + gghighlight(state %in% c("California", "Massachusetts")) + 
    labs( x = "Year", y = "Millions of Dollars, USD",
          title ="Federal Funding for Bike and Pedestrian Infrastructure",
          caption = "Source: US DOT FHWA") 
  
  return(ggplotly(states_plot))
}

# alabama <- bike_ped_long %>% filter(state == "Alabama")
# 
# ggplot(data = alabama, aes(x=year, y= value)) + 
#   geom_point() + geom_line()
# 
# states_plot <- ggplot(data = bike_ped_long, aes(x=year, y= value, color=state)) + 
#   geom_point() + geom_line()
# 
# ## Using gghighlight
# states_plot <- ggplot(data = bike_ped_long, aes(x=year, y= value, color = state, group = state)) +  
#   geom_point() + geom_line() + gghighlight(state %in% c("California", "Massachusetts")) + 
#   labs( x = "Year", y = "Millions of Dollars, USD",
#         title ="Federal Funding for Bike and Pedestrian Infrastructure",
#         caption = "Source: US DOT FHWA") 
# 
# 
# ggplotly(states_plot)
