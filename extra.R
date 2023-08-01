

##################################

# Questions using this data set. 
# Looking at commuting habits through the following vars:
# How far is work?
# what type of work?
# might make sense to look at census tracts, because 

us_county_means <- get_acs(geography="county",
                           table = "B08301", 
                           year = 2021, output="wide", cache_table = T)

median_earning_ctv <- get_acs(geography="county",
                              variables = c("B08006_001", "B08006_002", "B08121_001"),
                              output = "wide", 
                              year = 2021, cache_table = T)

col_names <- c("geoid", "name", "total", "total_error", "ctv", "ctv_error", "median_earnings", "median_earnings_error")

names(median_earning_ctv) <- col_names

median_earning_ctv <- median_earning_ctv %>% mutate(ctv_percentage = 100 * ctv/total)

ggplot(data = median_earning_ctv, aes(x=ctv_percentage, y = median_earnings)) +
  geom_point() +
  geom_smooth(method="lm") + 
  labs(title="Do Car Commmuters Earn Less Money?", 
       subtitle="Median Earnings in a County compared to the Percent of Workers who Drive to work") + 
  xlab("Percent of Commuters Who Drive to Work") +
  ylab("Median Earnings, US Dollars") 

# what is the probability of driving to work, given median earnings
ggplot(data = median_earning_ctv) + 
  geom_histogram( aes(x=median_earnings)) +
  geom_vline(aes(xintercept = mean(median_earnings, na.rm=T)), color="blue", linetype = "dashed" ) + 
  geom_vline(aes(xintercept = median(median_earnings, na.rm=T)), color ="red") +
  labs(title="Median Earnings by US County") + 
  xlab("Median Earnings, US Dollars") + 
  ylab("Number of Counties") 


#######