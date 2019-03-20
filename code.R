library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(knitr)
library(arules)
library(arulesViz)
library(arulesCBA)
library(arulesSequences)
options(scipen=999)

# Nice colors
cYellow = '#FADA5E'
cBlue = '#378CC7'


# Read the data
dataImport <- read.csv('data/teaBasket_DMS.csv')

# Remove transactions with NONE and Adjustment
dataExport <- dataImport %>% filter(dataImport$Item != "NONE" & dataImport$Item != "Adjustment")

# Remove two most frequent items 
dataExportNoCoffee <- dataImport %>% filter(!(dataImport$Item %in% c("NONE", "Adjustment", "Coffee", "Bread", "Postcard")))

# Write only the relevant columns
write.csv(dataExport[, c(3:4)], './data/transactions.csv')
write.csv(dataExportNoCoffee[, c(3:4)], './data/transactionsNoCoffee.csv')

# Read the data as transactions
coffee <- read.transactions('data/transactions.csv', 
                           format="single", 
                           cols = c('Transaction', 'Item'), 
                           sep = ',', 
                           header = TRUE)

tea <- read.transactions('data/transactionsNoCoffee.csv', 
                            format="single", 
                            cols = c('Transaction', 'Item'), 
                            sep = ',', 
                            header = TRUE)

# With coffee
coffee

# Without coffee and bread
tea




############## Cross tables
teaCount <- tea %>% crossTable(measure="count", sort=TRUE)
coffeeCount <- coffee %>% crossTable(measure="count", sort=TRUE)

# Coffee in most of transactions
coffeeCount[1,1]
teaCount[1,1]

teaSupport <- tea %>% crossTable(measure="support", sort=TRUE)
teaLift <- tea %>%crossTable(measure="lift", sort=TRUE)
teaChi <- tea %>% crossTable(measure="chiSquared", sort=TRUE)

# p-value of test, H0:independent rows and columns
teaCount[1:8,1:6]
print(teaSupport[1:8,1:6], digits = 1)
print(teaLift[1:8,1:6], digits = 2)
print(teaChi[1:8,1:6], digits = 1)

############# Eclat

# Low support still gives items that have at least ~ 8 occurences
teaFreqItems <- tea %>% eclat(list(supp=0.0003, maxlen=4)) 
median(teaFreqItems@quality$count)

teaFreqRules <- teaFreqItems %>% ruleInduction(tea, confidence=0.4)
teaFreqRules
inspect(teaFreqRules)



########### Apriori
teaRules <- tea %>% apriori(list(supp=0.0004, conf=0.3)) %>% filter(Lift < 50)

teaRules %>% 
  sort(by="lift", decreasing=TRUE) %>% 
  inspect()


# induction – forces that drive to given purchase
# what forces make people buying a banana

coffee %>% 
  apriori(list(supp=0.001,conf = 0.10),
          appearance=list(default="lhs", rhs="Coffee"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(15) %>%
  inspect(ruleSep = ">>>", itemSep = " + ", setStart = "", setEnd ="") 

coffee %>% 
  apriori(list(supp=0.001,conf = 0.10),
          appearance=list(default="lhs", rhs="Bread"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect(ruleSep = ">>>", itemSep = " + ", setStart = "", setEnd ="")

# what people buy when they have coffee
coffee %>% 
  apriori(list(supp=0.001,conf = 0.05),
          appearance=list(default="rhs", lhs="Coffee"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect(ruleSep = ">>>", itemSep = " + ", setStart = "", setEnd ="")

coffee %>% 
  apriori(list(supp=0.001,conf = 0.05),
          appearance=list(default="rhs", lhs="Bread"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect(ruleSep = ">>>", itemSep = " + ", setStart = "", setEnd ="")

########### Visulalization

# Frequency of items
coffee %>% itemFrequencyPlot(topN=15, type="absolute", main="Item Absolute Frequency", col = cYellow, border = NA) 
coffee %>% itemFrequencyPlot(topN=15, type="relative", main="Item Relative Frequency", col = cBlue, border = NA) 

tea %>% itemFrequencyPlot(topN=15, type="relative", main="Item Relative Frequency", col = cBlue, border = NA) 

# A sample
image(coffee[1000:1200])
image(tea[1000:1200])

plot(teaFreqRules, method="matrix", measure="confidence")
plot(teaFreqRules) 
plot(teaFreqRules, measure=c("support","lift"), shading="confidence")
plot(teaFreqRules, shading="order", control=list(main="Two-key plot"))
plot(teaFreqRules, method="graph")
plot(teaFreqRules, method="paracoord", control=list(reorder=TRUE))



############## Dissimilarity

# most are dissimilar - 90%
coffee[,itemFrequency(coffee)>0.05] %>%
  dissimilarity(which="items") %>%
  round(2) 

