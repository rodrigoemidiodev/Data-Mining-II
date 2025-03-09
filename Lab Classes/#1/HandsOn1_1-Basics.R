library(arules)

# Set the working directory dinamically
wd <- readline(prompt = "Enter the path to your working directory: ")
setwd(wd)

# Checking the working directory
getwd()

# Load the set of transactions into an object and inspect it
dfT <- read.transactions("trans-ex2.csv", sep = ",")
print(dfT)
inspect(dfT)

# Derive the set of frequent itemsets with minimum support of 40% using apriori algorithm
itemsets <- apriori(dfT, parameter = list(supp = 0.4, target = "frequent itemsets"))
inspect(itemsets)

# Identify the set of relevant association rules with minimum support of 40% and minimum confidence of 60% using the apriori algorithm
rules <- apriori(dfT, parameter= list(supp = 0.4, conf = 0.6, minlen = 2, target = "rules"))
inspect(rules)
