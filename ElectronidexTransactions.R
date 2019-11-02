# install and call packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
---
# Upload and inspect the transactional dataset
transdata <-read.transactions("ElectronidexTransactions2017.csv", 
                              format = "basket", 
                              rm.duplicates=TRUE, sep=",")
inspect (transdata) 
inspect (transdata[1000]) 
length (transdata) 
size (transdata) 
summary(size(transdata)) 
LIST(transdata) 
LIST(transdata[1:10], decode=FALSE)
itemLabels(transdata)
---
# Vidusalize your dataset
# Check item Frequency Plot & Bar Plot 
itemFrequencyPlot(transdata, type = "absolute", topN=10)
itemFrequencyPlot(transdata, support = 0.10, type = c("relative"), col = "steelblue3", 
                  main="Top 10 items, Relative Frequency", ylab ="")
itemFrequencyPlot(transdata, type =c("absolute"), topN =10, col = "steelblue3", 
                  main="Top 10 Products Frequency", ylab = "")
itemFrequencyPlot(transdata, type =c("relative"), topN =10, col = "steelblue3", 
                  main="Top 10 Products Sold", ylab = "% of Transactions")

itemFrequency(transdata, type=c("relative"))
head(sort(itemFrequency(transdata), decreasing=TRUE), n=10)------# TOP 10
View(head(sort(itemFrequency(transdata), decreasing=TRUE), n=10))---# TOP 10
head(sort(itemFrequency(transdata), decreasing=FALSE), n=10)---# Bottom 10
View(head(sort(itemFrequency(transdata), decreasing=FALSE), n=10))---# Bottom 10
---
image(transdata) 
image(sample(transdata, 50)) 
barplot(sort(itemFrequency(transdata), decreasing=TRUE))
barplot(sort(itemFrequency(transdata), decreasing=FALSE))
barplot(sort(table(unlist(LIST(transactions))))[1:10],horiz=TRUE, 
        las = 1, col=rainbow(4), main = "Low Frequency")
---
# Apply the Apriori algorithm to find association rules.
apriori(transdata)
rule1<- apriori (transdata, parameter = list(supp = 0.1, conf = 0.8))
# To view your rules, use the inspect() function
inspect(rule1) 
summary(rule1) # set of 0 rule

rule2 <- apriori(transdata, parameter = list(supp = 0.3, conf = 0.8))
inspect(rule2)
summary(rule1) # set of 0 rule
  
rule3 <- apriori(transdata, parameter = list(supp = 0.5, conf = 0.8))
inspect(rule3) # set of 0 rule

rule4 <- apriori(transdata, parameter = list(supp = 0.01, conf = 0.9, target = "rule", minlen=10))
inspect(rule4) # set of 0 rule

rule5 <- apriori(transdata, parameter = list(supp = 0.03, conf = 0.15))
inspect(rule5) # set of 46 rules
---
# Below code lift is 1
rule6 <- apriori(transdata, parameter = list(supp = 0.1, conf = 0.2))
inspect(rule6) # just 1 rule
plot(rule6)
---
rule7 <- apriori(transdata, parameter = list(supp = 0.01, conf = 0.2))
summary(rule7)  # 288 rules
plot(rule7)
---
# All 3 parameters appeared ( supp, conf, lift)
rule8 <- apriori(transdata, parameter = list(supp = 0.015, conf = 0.5))
inspect(rule8)   # 5 rules
plot(rule8)
---
rule9 <- apriori(transdata, parameter = list(supp = 0.003, conf = 0.5))
inspect(rule9)   # only 535 rules

---
# Rule but low support - rules appear around 10 times in the entire data set
rule10 <- apriori(transdata, parameter = list(supp = 0.001, conf = 0.95))
inspect(rule10)  ---# 43 rules
summary(rule10)
---
rule11 <- apriori(transdata, parameter = list(supp = 0.001, conf = 0.8))
inspect(rule11)----# 635 rules
summary(rule11)
---
rule12 <- apriori(transdata, parameter = list(supp = 0.001, conf = 0.6))
inspect(rule12)----# 3969 rules
summary(rule12)
inspect(rule9[1:20])
---
inspect(sort(rule12, decreasing = TRUE, by = "confidence"))
inspect(sort(rule12, decreasing = TRUE, by = "support"))
inspect(sort(rule12, decreasing = TRUE, by = "lift"))
---
# Sorting 
# To Print out the top 10 rules sorted by support:
top.support <- sort(rule12, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10)) # or inspect(sort(top.support)[1:10])

# Print out the top 10 rules sorted by confidence:
top.confidence <- sort(rule11, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))
 ## Print out the top 10 rules sorted by lift:
top.lift <- sort(rule12, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))

---
## An example using the subset () function:
ItemRules <- subset(rule12, items %in% "Apple MacBook Air")
inspect(ItemRules[2])
?subset
---
#  is redundant() function:
redundant_rules <- is.redundant(rule11)
rule11
summary(redundant_rules)  
# Remove redundant rules
rule11 <- rule11[!redundant_rules]  
rule11
inspect(rule11[1:10])

## Redundant rules
inspect(rule10[is.redundant(rule10)])
## non-redundant rules
inspect(rule10[!is.redundant(rule10)])
---
# Visualize the result
plot(rule12, jitter = 0)  

plot(rule12, method="graph", control=list(type="items", interactive = TRUE, max = 10)) 


