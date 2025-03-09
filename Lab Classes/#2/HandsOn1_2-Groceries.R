# Load packages and dataset to use
library(arules)
library(arulesViz)
data(Groceries)

# Inspect the dataset and its type 
Groceries
class(Groceries)

# Get information on the dataset 
summary(Groceries)

# Get the size of the dataset
head(size(Groceries))

# Get the first five transactions
inspect(Groceries[1:5])

# Check if there are duplicated transactions
length(which(duplicated(Groceries)))

# See the relative frequency of each item
head(itemFrequency(Groceries))

# Plot the top 5 more frequent items
itemFrequencyPlot(Groceries, topN = 5)

# Plot the items that have a support value of at least 0.1
itemFrequencyPlot(Groceries, support = 0.1)

# Obtain the frequent itemsets for a minimum support of 0.01 and check the class of the returned object
itemsets <- apriori(Groceries, parameter = list(supp = 0.1, target = "frequent itemsets"))
class(itemsets)

# Inspect the 5 most frequent itemsets
inspect(sort(itemsets)[1:5])

# Select the subset of closed frequent itemsets and the subset of maximal frequent itemsets from the frequent itemsets obtained
itemsets[is.closed(itemsets)]
itemsets[is.maximal(itemsets)]

# Generate the rules from the dataset using apriori and check its class
rules <- apriori(Groceries)
class(rules)

# Generate new rules with updated minimum support and confidence
rules <- apriori(Groceries, parameter = list(supp = 0.1, conf = 0.3))

# Obtain the rules with minsup = 0.01 and minconf = 0.25
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.25))
summary(rules)
quality(rules[1:5])
plot(rules[1:5])
inspect(rules[1:5])

# Select the rules with a lift value above 2
rules.sub <- subset(rules, subset = lift > 2)
inspect(rules.sub[1:5])

# Select the rules that have lift value above 2 and the items "whole milk" or "yogurt" on the consequent
rules.sub <- subset(rules, subset = rhs %in% c("yogurt", "whole milk") & lift > 2)
rules.sort <- sort(rules.sub, by = "lift")
inspect(rules.sort[1:5])
plot(rules.sub)
plot(rules.sub, method = "graph")
