# Load necessary packages
library(arules)
library(arulesViz)
library(tidyverse)

# Set the working directory dinamically
wd <- readline(prompt = "Enter the path to your working directory: ")
setwd(wd)

# Read the csv file into a data frame
df <- read.csv("german_credit.csv")

# Remove the first attribute from the data frame
df <- df %>% select(-default)

# Convert the data frame into a transactions data set
dfT <- as(df, "transactions")

# Discretize the numerical attributes according to:
# duration_in_month: 4 equal-with intervals with labels "short", "med-short", "med-long", "long"
# credit_amount: 4 equal-with intervals with labels "small", "med-small", "med-high", "high"
# age: 4 equal-with intervals with labels "young adult", "adult", "senior", "golden"
# to the rest of numerical attributes, simply use the function as.factor 

df <- df %>%
  mutate_if(is.character, as.factor)
df <- df %>% 
  discretizeDF(methods=list(duration_in_month = list(method = "interval", 4, labels = c("short", "med-short", "med_long", "long")), 
                            credit_amount = list(method = "interval", 4, labels = c("small", "med-small", "med-high", "high")),
                            age = list(method = "interval", 4, labels = c("young adult", "adult", "senior", "golden"))),
                            default = list(method = "interval"))

# Convert the data frame into a data set of transactions. Use the function itemInfo to see what each item represents 
dfT <- as(df, "transactions")
item_dfT <- itemInfo(dfT)
head(item_dfT)
subset(item_dfT, variables == "duration_in_month")

# Obtain the association rules from the data set using apriori. Plot the rules
rules <- apriori(dfT)
plot(rules)

# Select the rules with confidence equal to 1 and lift higher than 1.1. Plot them using engine="htmlwidget"
rules_new <- subset(rules, confidence == 1 & lift > 1.1)
plot(rules_new, engine="htmlwidget")

# Run apriori using a minimum confidence of 0.6, minimum length of 2 and focusing only on attributes sex, age, job, housing and purpose of credit. Plot the rules with method="graph" and engine="htmlwidget"
myItems <- subset(item_dfT, variables %in% c("age", "personal_status_sex", "job", "housing", "purpose"))$labels
rules <- apriori(dfT, 
                 parameter = list(conf=0.6, minlen=2),
                 appearance = list(both = myItems, default="none"))
plot(rules, engine="htmlwidget", method="graph")

# Obtain the rules that relate the purpose of credit with age, job and housing using apriori. Impose a minimum support of 0.05, minimum confidence of 0.25 and a minimum length of 2
my.lhs <- subset(item_dfT, variables %in% c("age", "job", "housing"))$labels
my.rhs <- subset(item_dfT, variables == "purpose")$labels
rules1 <- apriori(dfT, parameter = list(confidence = 0.25, minlen = 2, support = 0.05),
                  appearance = list(lhs = my.lhs, rhs = my.rhs, default = "none"))

# Plot the obtained rules 
plot(rules1, method = "graph", engine = "htmlwidget")

# Plot the previous set of rules using the method grouped 
plot(rules1, method = "grouped")
