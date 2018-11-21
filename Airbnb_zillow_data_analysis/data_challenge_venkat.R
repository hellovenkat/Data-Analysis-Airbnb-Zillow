# The real estate company has engaged your firm to build out a data product 
# and provide your conclusions to help them understand 
# which zip codes would generate the most profit on short term rentals within New York City.
# Author: Venkat Kotha
# Setting the working directory after extracting the downloaded zip file
setwd("G:/Data_Challenge/airbnb-zillow-data-challenge-master")

# Installing the required packages
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(scales)


# Step 1: Loading the Datasets
extract_airbnbr_url <- function(textfileName){
  airbnb_fileName <- textfileName
  con <- file(airbnb_fileName,"r")
  first_two_lines <- readLines(con,n=2)
  close(con)
  return(first_two_lines)
}
first_two_lines<-extract_airbnbr_url(textfileName = 'AirBnB Dataset Link.txt')
input_airbnb_file<-first_two_lines[2]
# Downloading the data from the given link
download.file(input_airbnb_file,destfile="input_airbnb_file.gz",method="libcurl")
# Unzipping it
unzipped_airbnb_file=gzfile('input_airbnb_file.gz','rt') 
# read input - airbnb data 
airbnb_data = read.csv(unzipped_airbnb_file)
# Unzipping zillow file
unzipped_zillow_file=gzfile('Zip_Zhvi_2bedroom.csv','rt') 
# read input - zillow data 
zillow_data = read.csv(unzipped_zillow_file)

# Removing unnecessary values
rm(first_two_lines)
rm(input_airbnb_file)

# Step 2: Examining the data and filtering the unnecesary data
# Checking the dimensions
dim(airbnb_data)
# [1] 40753    95
dim(zillow_data)
# [1] 8946  262

# There are so many columns in both of the datasets. We will only use the relevant data for our problem statement without over complicating things.
# Zillow_data dataset contains the cost estimate of the property from as early as 1996 to 2017.
# We keep the data of only last 12 months, as the previous data would not be impacting the present cost estimate in any way. 
# Creating a new dataset with the first seven columns and only the data of last one year
zillow_req<- cbind(zillow_data[,c(1:7, (ncol(zillow_data)-11):ncol(zillow_data))])
# Deleting the original dataset after subsetting the data
rm(zillow_data)
# Except that previous cost estimates data, everything else seems relevant to the problem:
# The columns of zillow_req dataset are:
colnames(zillow_req)

# The airbnb_data dataset contains so many columns that would not be of any help for our problem.
# For example, it contains the metadata like listing_url, scrape_id which was used by airbnb internally. Also, there are so many columns containing the landlord information.
# We should drop those columns. We create a vector of all the irrelevant columns and then pass it to a function that will drop from the dataset.
# We consider only those columns which are useful.
airbnb_req_cols <- c('id', 'street', 'neighbourhood',
                     'neighbourhood_cleansed',
                     'neighbourhood_group_cleansed',
                     'city',
                     'state',
                     'zipcode',
                     'market',
                     'bedrooms',
                     'price',
                     'number_of_reviews',
                     'review_scores_rating',
                     'review_scores_location',
                     'review_scores_value',
                     'reviews_per_month')
# Using the dplyr package, we create the new data frame airbnb_req by subsetting the airbnb_data with the required cols i.e., 'airbnb_req_cols'
# Also, we know that the real estate company has already concluded that 2 Bedrooms are most profitable.
# So, applying the filter which returns the dataframe whose bedrooms are 2.

airbnb_req <- select(filter(airbnb_data, bedrooms == 2), airbnb_req_cols)

# We now have two datsets with the useful columns. The airbnb_req has data from only New york location(which is what we want.)
# But the zillow_req has data of all the locations.
# let us join the two datasets into one, so that it will be easy to observe and perform manipulations.
# We join the two datasets by zipcodes, we have the 'zipcode' column in airbnb_req and 'RegionName' column in zillow_req which are zicodes.
joined_airbnb_zillow <- merge(airbnb_req, zillow_req, by.x="zipcode", by.y="RegionName", sort = FALSE)


# Checking the dimensions of the joined dataset
dim(joined_airbnb_zillow)
# [1] 1238   36
# By checking the dimensions we can see that the rows in the merged data reduced drastically. This is because the unavailability of data in the zillow dataset for the remaining zipcodes.

# Now we got all the required data into one dataset - 'joined_airbnb_zillow'

# The next step is ---------- Quality Check -------------
# It is rightly said that the bad data is worse than no data at all.
# We now check for bad data and correct it.

# Before that, convert the price to proper format.We observe that price starts with '$'.
# Also converting it into numeric values so that calculations can be performed.
joined_airbnb_zillow$price <- as.numeric(gsub('[$,]','', joined_airbnb_zillow$price))

# Checking if any of the rows contain incorrect data. The values should not negative for price, SizeRank and Monthly cost estimates.

# We use lapply to check if any of the columns contain negative values.
# The result if a list with the column names and their corresponding total number of negative values.
neg_count <- lapply(joined_airbnb_zillow, function(y) length(which(y < 0)))
# Displaying the output list neg_count
neg_count
# Checking if any of the columns has negative values.
length(which(neg_count>0))
# [1] 0
# As the output is zero, we can confirm that there are no negative values in any of the columns.

# We now check for missing values in the data in the same way
missing_count <- lapply(joined_airbnb_zillow, function(y) length(which(is.na(y) | y == '')))

# The missing_count is a list which contains the number of missing values for each column.
# We can apply dimensionality reduction and remove or drop the column which has more than certain percentage of missing values, because we could not attain significant information through reliable number of sources for that column.
# The minimum percentage value totally varies by each scenario and the importance of the particular column, for our case we can remove if we miss atleast 50 percent.
joined_airbnb_zillow <- joined_airbnb_zillow[,missing_count<(0.5*nrow(joined_airbnb_zillow))]

# We find by checking the dimensions that no column had to be deleted.
dim(joined_airbnb_zillow)
# [1] 1238   34

# We now check for any duplicate rows present in the dataset
joined_airbnb_zillow <- joined_airbnb_zillow %>% distinct()
# ==== When creating meta data - follow the guidelines of the document

# Next step ------------ Data Munging ------------
# We now have clean data with no known inappropriate data. We can proceed to calculate the mean price.
joined_airbnb_zillow$mean_zillow_cost <- rowMeans(joined_airbnb_zillow[,(ncol(joined_airbnb_zillow)-11):ncol(joined_airbnb_zillow)])

# We now have the estimated cost for each property to buy.
# We have some info: Occupancy is 75%.
occupancy <- 0.75
# We also have price for one day stay.
# Using that information, we can calculate the revenue that can be obtained each year by renting the property.
joined_airbnb_zillow$revenue_per_year <- occupancy * joined_airbnb_zillow$price * 365

# With the revenue per year and price of the property available, we can easily calculate the breakeven period for profit
joined_airbnb_zillow$breakeven <- joined_airbnb_zillow$mean_zillow_cost/joined_airbnb_zillow$revenue_per_year
# We now have the breakeven period for each property.
# Forming the required columns into a seperate dataset 'final_data_req'.
final_data_req_columns <- c('zipcode', 'id','neighbourhood_group_cleansed', 'mean_zillow_cost', 'revenue_per_year', 'breakeven')

final_data_req <- select(joined_airbnb_zillow, final_data_req_columns)

# Converting the suitable columns to factors as they are categorical values, not continuous.
final_data_req$zipcode <- as.factor(final_data_req$zipcode)
final_data_req$id <- as.factor(final_data_req$id)
final_data_req$neighbourhood_group_cleansed <- as.factor(final_data_req$neighbourhood_group_cleansed)
#- === refine the zipcode...remove dash
# As we have to find the zipcodes where we can invest, grouping by zipcodes. 
final_data <- final_data_req %>%
              select(zipcode, neighbourhood_group_cleansed,mean_zillow_cost, revenue_per_year, breakeven) %>%
              group_by(zipcode, neighbourhood_group_cleansed) %>%
              summarise(breakeven = mean(breakeven), mean_zillow_cost = mean(mean_zillow_cost),
                        revenue_per_year = mean(revenue_per_year))
# Sorting the dataset by the break even period and displaying the top 10 zipcodes.
zipcodes_earliest_profit <- (arrange(final_data, breakeven)[1:10,])$zipcode
zipcodes_earliest_profit
# These are the zipcodes which would start generating the profit at the earliest.
# But we want the zipcodes which would generate most profit for a certain amount of time.
# As the time period is not specified, let us calculate the profitablity for several time periods in the future.
# We can obtain the most profitable zipcodes by giving the years as input.

get_most_profitable <- function(final_data, num_years, num_zipcodes){
  gmp_profits <- (final_data$revenue_per_year*num_years) - final_data$mean_zillow_cost
  temp_zip_data<-as.character(final_data$zipcode)
  temp_mat <- cbind(gmp_profits, temp_zip_data)
  gmp_res<-temp_mat[order(-as.numeric(temp_mat[, 1])), ][1:num_zipcodes,]
  colnames(gmp_res) <- c("Profit/Loss in USD","Zipcode")
  return(gmp_res)
}
# By specifying the required number of years, how many number of zipcodes that we want to find, we can get the result by the above function.
# Depending upon the time period we estimate, corresponding zipcodes can be shown by the following function.
# For example if we want to see the 5 zipcodes with high profitability after 8 years, we just plug in those values in the function call below.
most_profitable_zipcodes_and_revenue <- get_most_profitable(final_data, num_years = 8, num_zipcodes = 5)


final_data$profits_after_5_years <-  (final_data$revenue_per_year*5)  - final_data$mean_zillow_cost
final_data$profits_after_10_years <- (final_data$revenue_per_year*10) - final_data$mean_zillow_cost
final_data$profits_after_15_years <- (final_data$revenue_per_year*15) - final_data$mean_zillow_cost
final_data$profits_after_20_years <- (final_data$revenue_per_year*20) - final_data$mean_zillow_cost
final_data$profits_after_25_years <- (final_data$revenue_per_year*25) - final_data$mean_zillow_cost
final_data$profits_after_30_years <- (final_data$revenue_per_year*30) - final_data$mean_zillow_cost


# Next Step ------------- Craft a Visual Narrative -------------
# a.	Visualize metrics for profitability on short term rentals by zip code
# let us observe the distribution of properties across neighbourhoods in the given data

png("data_vis_1_histograms_properties_neigh.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes(neighbourhood_group_cleansed)) + 
  geom_bar(aes(), width = 0.5) + 
  labs(title="Properties across Neighbourhoods", 
       x = "Neighbourhood", 
       y ="Number of Properties")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
# We can see that most of the properties are in Brooklyn and Manhattan.
png("data_vis_1_box_plot_prices_neigh.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes(neighbourhood_group_cleansed, price)) + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Understanding prices - Box plot", 
       subtitle="Prices grouped by Neighbourhoods",
       caption="Dataset: joined_airbnb_zillow",
       x="Neighbourhood",
       y="Price for one night(in USD)")
dev.off()
png("data_vis_1_violin_plot_prices_neigh.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes(neighbourhood_group_cleansed, price)) + geom_violin() + 
  labs(title="Understanding prices - Violin plot", 
       subtitle="Prices grouped by Neighbourhoods",
       caption="Dataset: joined_airbnb_zillow",
       x="Neighbourhood",
       y="Price for one night(in USD)")
dev.off()
# By the above plots, we come to know that there is huge variations in the price per night across different neighbourhoods.
# Insights: 1. Manhattan region has more number of zipcodes followed by Brooklyn, Staten Island and Queens.
# 2. The prices of houses in the Manhattan region have more outliers than the remaining regions.

# Next we try to find if there is any correlation between the rating of the location of the property and the price per night.
# Plotting the review_score vs prices in Scatter plot
# As we are concerned with only the location rating, we dont consider the unnecesary data like review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication etc

png("data_vis_2_scatter_plot_ratingsNeigh_prices.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes(x=review_scores_location, y=price)) + geom_point()+ 
  labs(title="Rating of Location vs Price", 
       caption="Dataset: joined_airbnb_zillow",
       x="Rating of Location",
       y="Price for one night(in USD)")
dev.off()
# Observing the graph, we can see that if the review about the location of the property is good(rating is high) then the price is high.
# Insights: There is correlation between Location Rating and price.

# Next we try to find if there is any correlation between SizeRank(population of the area) and price.
joined_airbnb_zillow$SizeRank<- as.factor(joined_airbnb_zillow$SizeRank)
png("data_vis_3_plot_sizerank_prices.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes(x=SizeRank, y=price)) + 
  geom_bar(stat="identity") + 
  labs(title="SizeRank vs Price", 
       caption="Dataset: joined_airbnb_zillow",
       x="SizeRank",
       y="Price for one night(in USD)")
dev.off()
# Observing the graph, we can see that there is no relation between the SizeRank and price variables.
# For instance, the prices of SizeRank 32, 52, 71, contrast the correlation in any way.
# Insights: No correlation between SizeRank and price.

# Next we observe the realation between mean_zillow_cost and neighbourhood.
png("data_vis_4_scatter_plot_mean_prices.png", width = 543, height = 365)

ggplot(joined_airbnb_zillow, aes( mean_zillow_cost, zipcode)) +
  #geom_boxplot() +
  geom_point(aes(col=neighbourhood_group_cleansed), size = 3)+ 
  labs(title="Understanding Neighbourhoods-Costs better",
       caption="Dataset: joined_airbnb_zillow",
       x="Cost of the Property(in USD)",
       y="Zipcodes") + 
  scale_x_continuous(labels = comma)
dev.off()
# ----
# As we can see from the above graph, the cost to buy a house in Manhattan is much higher than the other neighbourhoods. The neighbourhoods of Queens and Staten Island have much lesser house prices.
# Let us now compare the revenues generated in those neighbourhoods by plotting the graph in the same way.
png("data_vis_4_scatter_plot_neigh_revenue.png", width = 543, height = 365)
ggplot(joined_airbnb_zillow, aes( revenue_per_year, zipcode)) +
  geom_point(aes(col=neighbourhood_group_cleansed), size = 3)+ 
  labs(title="Understanding Neighbourhoods-Revenues better", 
       caption="Dataset: joined_airbnb_zillow",
       x="Annual Revenue(in USD)",
       y="Zipcodes") + 
  scale_x_continuous(labels = comma)
dev.off()
# By the above plot, we can assess that the Manhattan and Brooklyn neighbourhoods generate more revenue annually than the other two.
# Insights: 1. The properties in both the areas: Manhattan and Brooklyn are significantly pricier(mean cost) than the other two.
# 2. The revenues generated by Manhattan and Brooklyn are understandably higher than Queens and Staten Island.



# We can also visualize the breakeven periods of the properties.
png("data_vis_5_breakeven.png", width = 543, height = 365)
ggplot(data=final_data, aes(y=breakeven, x=zipcode, fill=neighbourhood_group_cleansed)) +
  geom_bar(stat="identity")+ coord_flip()+
  labs(title="BreakEven Periods vs Zipcodes", 
       caption="Dataset: final_data",
       y="BreakEven Period (in years)",
       x="Zipcodes") 
dev.off()
# Insights: 1. By the above graph plot, we can observe that the breakeven periods for the properties of the zipcodes that belong to Manhattan region are very high.
# This can be attributed to the high costs of the properties in that area.
# 2. However the break even periods for properties in the zipcodes that belong to Staten Island are noticebly lower. That makes the region a good option for investing.


# ----------- Conclusions ----------

# The answer to the main question to find the zipcodes that would generate most profit depends on the time period considered.
# For instance, if the time period is 5 years(displaying the top 5):
get_most_profitable(final_data, num_years = 5, num_zipcodes = 5)
'Profit/Loss in USD  Zipcode
[1,] "-41585.4166666667" "10312"
[2,] "-166439.583333333" "10304"
[3,] "-201956.25"        "10306"
[4,] "-222852.083333333" "11434"
[5,] "-238872.916666667" "10305"'

#If the time period is 10 years(displaying the top 5):
get_most_profitable(final_data, num_years = 10, num_zipcodes = 5)
'Profit/Loss in USD  Zipcode
[1,] "252695.833333333"  "10312"
[2,] "-33670.8333333333" "10304"
[3,] "-74662.5"          "10306"
[4,] "-86637.5"          "10305"
[5,] "-86737.5"          "11434"'

#If the time period is 30 years(displaying the top 5):
get_most_profitable(final_data, num_years = 30, num_zipcodes = 5)
'     Profit/Loss in USD Zipcode
[1,] "1429820.83333333" "10312"
[2,] "1245635.41666667" "10036"
[3,] "1189723.13596491" "10022"
[4,] "984850"           "10025"
[5,] "642585.171568628" "10011"'

# We can see the variations of zipcodes depending on the time period.
# 1. The real estate company should invest in those zipcodes according to their future planning i.e. how many years they can wait.

# 2. Overall, the breakeven period of the properties in Manhattan are so high compared to the other regions. The real estate company should avoid investing in Manhattan neighbourhood.

# 3. The breakeven period of the properties in Staten Island is much lower than other regions. So, the company should prefer to invest in Staten Island.

# 4. The zipcode 10312 generates the profit at the earliest compared to all the other zipcodes. 
# The client should try to invest in this zipcode.

# ----------- What's Next - Future Steps --------
# 1. Some zipcodes in the airbnb_data are missing. Though we have their neighbourhood group, it is very inappropriate to fill the zipcodes with the most occurred zip code in that particular neighbourhood. Doing so would decrease the data quality and induce the bad data. Therefore it is avoided. 
# Another possibility of filling the missing zipcodes is through the lattitude and longitude information that we have. If the lat and long information given is identified as correct then we can use free reverse geocoding R packages like ggmap and find the zipcodes.
# This can be pursued in future work. Having more data refines the results and provides more insights.

# 2. We observe that though the original data is large , we had to do analysis on much smaller data due to the unavailability of zillow data for those zipcodes.
# We can try to incorporate multiple data sources like zillow, property registration records in the future.

# removing the datasets, variables and functions from the environment
rm(list=ls())
package_listing <- c('', 'tidyverse','DT' , 'leaflet','plotly','ggthemes', 'ggplot', 'ggplot2','dplyr')
remove.packages(package_listing)