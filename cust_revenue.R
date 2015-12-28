
# __________________________________________________________________
# //////////////////////////////////////////////////////////////////
#
# Author - Anupama Rajaram
# Filename - cust_revenue.R
#
# Program Description - Marketing related functions, including
#       a. revenue per customer & revenue per segment
#       b. Customer Scoring and revenue prediction
#
# Attribution - Some of the code, and the dataset is from a marketing 
# analytics course from Essec Business school, freely available 
# on Coursera Platform.
# __________________________________________________________________
# //////////////////////////////////////////////////////////////////


# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


#===================================================================#
#=========== Section 1: Import data and clean/re-arrange============#

# Load text file into local variable called 'data' - 51243 rows and 3 columns
data = read.delim(file = 'purchases.txt', header = FALSE, 
                  sep = '\t', dec = '.')

# Rename the column headers from generic V1, V2,.. to meaningful titles
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')

# interpret the last column as a date, and extract year of purchase
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")

# the as.Date specifies date in a particular format
# so we can calculate individual components (day, month or year) 
# to be used at a later time. 
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Data exploration - Display the data after transformation
head(data)
summary(data)

# all the snippets titled 'Data exploration' are optional,
# although they are useful to view the data & check our 
# program works as expected.


#===================================================================#
#=========== Section 2: Compute average revenue per customer =======#

# First Compute recency, frequency and average purchase amount

library(sqldf)  # please make sure this slqdf library is installed already

customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data GROUP BY 1")
# Note, this dataframe has 18417 rows and 4 columns, since some users 
# purchased products more than once


# Data exploration 
head(customers)
summary(customers)


# Divide customers into segment based on recency and purchase amount at end of 2015
# customer segments are inactive, cold, warm high, warm low, new warm, active high,
# active low & new active
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & 
                                 customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & 
                                 customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & 
                                 customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & 
                                 customers_2015$amount < 100)] = 
    "warm low"
customers_2015$segment[which(customers_2015$segment == "warm" & 
                                 customers_2015$amount >= 100)] = 
    "warm high"
customers_2015$segment[which(customers_2015$segment == "active" 
                             & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & 
                                 customers_2015$amount < 100)] = 
    "active low"
customers_2015$segment[which(customers_2015$segment == "active" &
                                 customers_2015$amount >= 100)] = 
    "active high"

# Next, re-order factor in a way that makes sense, then view segmentation details
customers_2015$segment = factor(x = customers_2015$segment, 
                                levels = c("inactive", "cold",
                                           "warm high", 
                                           "warm low", 
                                           "new warm", "active high",
                                           "active low", "new active"
                                )) 
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:4], by = list(customers_2015$segment), mean)


# Retrospective database segmenting (for 2014)
# helps us compute differences and predict values for the future
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")


# Segmenting customers for 2014
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & 
                                 customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & 
                                 customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & 
                                 customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" 
                             & customers_2014$amount < 100)] = "warm low"
customers_2014$segment[which(customers_2014$segment == "warm" 
                             & customers_2014$amount >= 100)] = "warm high"
customers_2014$segment[which(customers_2014$segment == "active" & 
                                 customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 
                                 100)] = "active low"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 
                                 100)] = "active high"

# Re-order 2014 customers & Show segmentation results
customers_2014$segment = factor(x = customers_2014$segment, 
                                levels = c("inactive", "cold",
                                           "warm high", "warm low", "new warm", 
                                           "active high", "active low", 
                                           "new active"))

# Data exploration - viewing details of this table
table(customers_2014$segment)
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), 
          mean)

# calculate revenue - Notice that people with no revenue in 2015 do NOT appear here
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) 
                     AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")

# Merge 2015 customers and 2015 revenue tables(correct)
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

# =========== Show average revenue per customer ====================#
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), 
          mean)

# Merge 2014 customers and 2015 revenue tables(correct)
forward = merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

# ========== Show average revenue per segment ======================#
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment),
              mean)
print(r)

# Re-order and display results in descending order of amount
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)



#======================================================================#
#=========== Section 3: Customer Scoring and revenue prediction =======#

# Calibrate the "probability" model
in_sample = forward
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)

library(nnet)
prob.model = multinom(formula = active_2015 ~ recency + 
                          first_purchase + frequency + amount + 
                          max_amount, data = in_sample)

coef = summary(prob.model)$coefficients
std  = summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std)

# For the monetary model, select only those who made a purchase
z = which(in_sample$active_2015 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])


# Calibrate the "monetary" model, using a log-transform 
amount.model = lm(formula = log(revenue_2015) ~ log(amount) + log(max_amount), data = in_sample[z, ])
summary(amount.model)

# Plot the results of this new monetary model
plot(x = log(in_sample[z, ]$revenue_2015), y = amount.model$fitted.values)


# --- APPLY THE MODELS TO TODAY'S DATA ---------------------

# Predict the target variables based on today's data
customers_2015$prob_predicted    = predict(object = prob.model, 
                                           newdata = customers_2015, 
                                           type = "probs")
customers_2015$revenue_predicted = exp(predict(object = amount.model, 
                                               newdata = customers_2015))
customers_2015$score_predicted   = customers_2015$prob_predicted * customers_2015$revenue_predicted
summary(customers_2015$prob_predicted)
summary(customers_2015$revenue_predicted)
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)

# How many customers have an expected revenue of more than $50
z = which(customers_2015$score_predicted > 50)
print(length(z))

# Data exploration - print details of customers with expected revenue > $100
# only among the first 1000 customers
# we basically append each chosen customer into a new dataframe 
z4 <- data.frame( "customer_id" = numeric(), "amount" = numeric(), 
                  "revenue_predicted" = numeric())

for(i in 1:50) {  
    if(customers_2015$score_predicted[i] > 50) {
        newrow4 <- (customers_2015[i,c("customer_id", 
                                                "amount", 
                                                "revenue_predicted")])
        z4 <- rbind(z4, newrow4)
    }
}

colnames(z4) = c('customer_id', 'amount', 'revenue')
print(z4)
