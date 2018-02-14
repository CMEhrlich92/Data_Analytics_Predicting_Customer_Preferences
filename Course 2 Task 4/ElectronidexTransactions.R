#Install package for analyzing transactional data
install.packages("arules")
library(arules)
require(arules)
??arules

#Install package that provides visual techniques for the arules package
install.packages("arulesViz")
install.packages("TSP")
install.packages("caTools")
install.packages("whisker")
install.packages("fpc")
library(arulesViz)
require(arulesViz)
??arulesViz

#Upload Electronidex Transaction Data
ETrans <- read.transactions("ElectronidexTransactions2017.csv", sep = ",")
summary(ETrans)

#Get to know Transactional Data
inspect(ETrans) #All individual transactions
length(ETrans) #Total number of transactions
size(ETrans) #Number of items per transaction
LIST(ETrans) #Lists the transactions by conversion
itemLabels(ETrans)# To see the item labels

#Visualize Data
itemFrequency(ETrans[,1:10])
itemFrequencyPlot(ETrans, support = .10)
itemFrequencyPlot(ETrans, topN = 10) #Top 10 items of highest frequency
image(ETrans[1:100])
image(sample(ETrans, 100))

#Support = Percentage of times item appears in transaction
#Confidence = Accuracy of the rules. Measure of the proportion of transactions where the presence of an item/itemset results in the presence of another.
#Lift = Likelihood of RHS being purchased with LHS, taking into account the popularity of RHS

#Apriori Algorithm
help(apriori)
APRule <- apriori(ETrans, parameter = list(support=0.01, confidence=.50, minlen=2))
summary(APRule)
APRuleInspect <- inspect(APRule[1:19])
inspect(sort(APRule, by="lift")[1:19]) #can sort by support, confidence, lift, or count

#Seeing a specific item's rules using subset function
help(subset)
APRuleSS <- subset(APRule, subset = lhs %in% "HP Laptop") #change "item name" to see specific rules for said item
APRuleSS
inspect(APRuleSS)

#Testing for redundant rules
is.redundant(APRule)
is.redundant(APRuleSS)

#Visualize Results
help(plot)
plot(APRule, method="graph", control=list(type="p"))
.libPaths()
getwd()

WriteTable <- write.table(APRuleInspect, "APRuleInspect.txt")
write.csv(WriteTable, "APRuleInspect.csv")
