
# __________________________________________________________________
# //////////////////////////////////////////////////////////////////
#
# Author - Anupama Rajaram
# Program Description - Marketing related functions, including
#       a. Compute RFM (recency, frequency and monetary value)
#       b. Dendograms & Statistical/ hierarchical segmentation
#       c. Managerial (customer) segmentation into categories 
#           like inactive, warm, cold etc. based on recency
# __________________________________________________________________
# //////////////////////////////////////////////////////////////////


# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


#===================================================================#
#=========== Section 1: Import data and clean/re-arrange============#

# Load text file into local variable called 'data'
# this dataset had 51243 rows and 3 columns
data = read.delim(file = 'purchases.txt', header = FALSE, 
                  sep = '\t', dec = '.')

# Rename the column headers from generic V1, V2,.. to meaningful titles
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
# this commnad 

# interpret the last column as a date, and extract year of purchase
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
# the as.Date specifies date in a particular format


# now we can calculate individual components (date, month or year) 
# to be used at a later time. 
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)


#===================================================================#
#=========== Section 2: Compute RFM, with SQL help==================#

# Compute key marketing indicators - recency, frequency and monetary
# value, using SQL queries within R

# please make sure the slqdf library is installed already
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")
# Note, new dataset has 18417 rows and 4 columns, since some users 
# purchased products more than once


# Explore the data - optional 
head(customers)
summary(customers)

# Graphical visualization of RFM
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


#===================================================================#
#=========== Section 3: Statistical segmentation ===================#

# Preparing the data 
# First, Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# Second, run statistical segmentation 

# most laptops do not have enough memory to compute distance metrics
# for the entire dataset.
# to check try the command below (commented here) 
# d = dist(new_data)


# If your computer throws an error message use the 30% sample below
# If not, replace "by = 3" with "by = 1"

# We take a 30% sample - hence dividing the original set by 3
# For 10% sample, divide by 10
# For 50% sample, divide by 2, and so on.
sample = seq(1, 18417, by = 3)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot dendogram
plot(c)

# Divide customers into 15 segments
members = cutree(c, k = 15)

# Show 100 first customers and their segment number
# this is an exploratory step, hence optional
members[1:100]
table(members)

# Show average/ aggregrated profile (average revenue) of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)



#===================================================================#
#=========== Section 4: Managerial/ customer segmentation ==========#

# statistical segmentations are not viable for businesses since they 
# become obsolete very quickly, with changes in data

# Managerial segmentation is a much better alternative.

# First copy customers data
customers_2015 = customers

# ------------Simple 2-segment solution based on recency alone------------#
# adding a column called segment.
# if customers bought products >3 years, then mark as inactive.
customers_2015$segment = ifelse(test = customers_2015$recency > 365*3, 
                                yes = "inactive", no = "NA")
# exploratory step (optional)
table(customers_2015$segment)
# note, table() uses cross-classifying factors to build a contingency table 
# of the counts at each combination of factor levels.
# In this case, the classification factor = segment => inactive or NA


# ------------ Alternative 6-segment solution based on ----------#
# ------------ recency & monetary value -------------------------#
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & 
                                 customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & 
                                 customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & 
                                 customers_2015$amount < 100)] = 
    "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & 
                                 customers_2015$amount >= 100)] = 
    "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" 
                             & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & 
                                 customers_2015$amount < 100)] = 
    "active low value"
customers_2015$segment[which(customers_2015$segment == "active" &
                                 customers_2015$amount >= 100)] = 
    "active high value"

# Re-order factor in a way that makes sense
customers_2015$segment = factor(x = customers_2015$segment, 
                                levels = c("inactive", "cold",
                                           "warm high value", 
                                           "warm low value", 
                                           "new warm", "active high value",
                                           "active low value", "new active"
                                ))

# exploring these new segments
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:4], by = list(customers_2015$segment), mean)


# using a colored pie chart to view the segments
pie(table(customers_2015$segment), col = rainbow(24))
aggregate(x = customers_2015[, 2:4], by = list(customers_2015$segment), 
          mean)


