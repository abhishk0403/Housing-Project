#Part 1 - Data analysis
# Load required libraries
library(tidyverse)
# install.packages(c("ggplot2", "dplyr","car"))
library(ggplot2)
library(dplyr)
library(car)
# 1. Load the data
house_df=read_csv("G:/Data Science/My notes/365 DATA SCIENCE/R Programming/housing_data.csv")
house_df
# 2. Summary statistics
summary(house_df)
tail(house_df[,1])
# 3. Calculate mean, mode and median, as well as the standard deviation for each variable
# Mode function

for (name in colnames(house_df)){
  mean=mean(house_df[[name]],na.rm = TRUE)
  print(mean)
}
for (name in colnames(house_df)){
  median=median(house_df[[name]],na.rm = TRUE)
  print(median)
}
#mode function
get_mode <- function(x) {
  unique_values <- unique(x)
  counts <- table(x)
  mode_values <- unique_values[counts == max(counts)]
  
  if (length(mode_values) == length(unique_values)) {
    # If all values occur equally, there is no mode
    return(NULL)
  } else {
    return(mode_values)
  }
}

# Print results
colnames(house_df)

for (name in colnames(house_df)) {
  mode_values <- get_mode(house_df[[name]])
  
  # Check if mode_values is not NULL before printing
  if (!is.null(mode_values)) {
    print(paste("Mode for", name, ":", mode_values))
  } else {
    print(paste("No mode for", name))
  }
}
# 4. Compute the correlation between each pair of variables
house_df_no_na <- na.omit(house_df)
correlation_matrix <- cor(house_df_no_na)
correlation_matrix

library(dplyr)
# Replace missing values with the median for each variable
house_df_imputed <- house_df %>%
  mutate_all(~ ifelse(is.na(.), median(., na.rm = TRUE), .))
house_df_imputed
#Check for missing values in the imputed dataset
print(colSums(is.na(house_df_imputed)))
correlation_matrix <- cor(house_df_imputed)
correlation_matrix

# Part 2 - Data visualization
hist<-ggplot(data=house_df_imputed,aes(x='Median Home Value'))
ggplot(house_df_imputed, aes(x = `Median Home Value`)) +
  geom_histogram(binwidth = 9, color = "white", fill = "darkcyan", alpha = 0.7) +
  labs(x = 'Median Home Value', y = 'Frequency', title = 'Distribution of Median Home Values') +
  theme_minimal()
str(house_df_imputed)

#Part 3 - Hypothesis testing
# 1. Define "high" and "low" crime rates based on the median of the Crime.Rate variable
median_crime_rate <- median(house_df_imputed$`Crime Rate`)
# Define a new variable indicating high or low crime areas
house_df_imputed <- mutate(house_df_imputed, 
                           Crime_Category = ifelse(`Crime Rate` > median_crime_rate, "High", "Low"))
summary(house_df_imputed$Crime_Category)
house_df_imputed$Crime_Category
# 2. Additional: Check for assumptions
# For a t-test, it's essential to check for normality & homogeneity of variance.
# Subset the data into two groups based on Crime_Category
high_crime_values <- house_df_imputed$`Median Home Value`[house_df_imputed$Crime_Category == "High"]
low_crime_values <- house_df_imputed$`Median Home Value`[house_df_imputed$Crime_Category == "Low"]
# 3. Conduct an independent two-sample t-test
# Conduct a two-sample independent t-test
t_test_result <- t.test(high_crime_values, low_crime_values)

# Print the result
print(t_test_result)
# Check normality for each group
# Create a function to plot histograms for both groups
plot_histograms <- function(values, group_name) {
  ggplot(data.frame(values), aes(x = values)) +
    geom_histogram(binwidth = 5, color = "white", fill = "lightblue", alpha = 0.7) +
    labs(x = 'Median Home Value', y = 'Frequency', title = paste('Histogram of', group_name)) +
    theme_minimal()
}
plot_histograms(high_crime_values, "High Crime Areas")
plot_histograms(low_crime_values, "Low Crime Areas")

levene_test_result <- leveneTest(`Median Home Value` ~ Crime_Category, data = house_df_imputed)
print(levene_test_result)



# Part 4 - Linear regression
# for predicting the median home value with the average rooms variable

linmod <- lm(`Median Home Value` ~ `Average Rooms`, data = house_df_imputed)

ggplot(house_df_imputed,aes(`Average Rooms`,`Median Home Value`)) + geom_point()+
  theme_light()+labs(x="Average Rooms",y="Median Home Value",title="Average Rooms and Median Home Value")+
  geom_smooth(method="lm",col="red",se=FALSE)+theme_minimal()
summary(linmod)
colnames(house_df_imputed)
