###############################################################################################
#   Bike Data Insights generation                                                             #
#   Date: 17th May 2023                                                                       #
###############################################################################################

# Preparations
rm(list = ls(all.names = TRUE))

# data set loading
df <- read.csv('dataset/transformed_clean_data.csv')

head(df)

# attach the variables
attach(df)

# get null Counts
null_counts <- sapply(df, function(x) sum(is.na(x)))

print(null_counts)
