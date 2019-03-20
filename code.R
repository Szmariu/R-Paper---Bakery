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
dataImport <- read.csv('data/BreadBasket_DMS.csv')

# Remove transactions with NONE and Adjustment
dataExport <- dataImport %>% filter(dataImport$Item != "NONE" & dataImport$Item != "Adjustment")
dataExportNoCoffee <- dataImport %>% filter(dataImport$Item != "NONE" & dataImport$Item != "Adjustment" & dataImport$Item != "Coffee")

# Write only the relevant columns
write.csv(dataExport[, c(3:4)], './data/transactions.csv')
write.csv(dataExportNoCoffee[, c(3:4)], './data/transactionsNoCoffee.csv')

# Read the data as transactions
coffee <- read.transactions('data/transactions.csv', 
                           format="single", 
                           cols = c('Transaction', 'Item'), 
                           sep = ',', 
                           header = TRUE)

bread <- read.transactions('data/transactionsNoCoffee.csv', 
                            format="single", 
                            cols = c('Transaction', 'Item'), 
                            sep = ',', 
                            header = TRUE)

# With coffee
coffee

# Without coffee
bread




############## Cross tables
breadCount <- bread %>% crossTable(measure="count", sort=TRUE)
coffeeCount <- coffee %>% crossTable(measure="count", sort=TRUE)

# Coffee in most of transactions
coffeeCount[1,1]
breadCount[1,1]


breadSupport <- bread %>% crossTable(measure="support", sort=TRUE)
breadLift <- bread %>%crossTable(measure="lift", sort=TRUE)
breadChi <- bread %>% crossTable(measure="chiSquared", sort=TRUE)

# p-value of test, H0:independent rows and columns
breadCount[1:8,1:6]
print(breadSupport[1:8,1:6], digits = 1)
print(breadLift[1:8,1:6], digits = 2)
print(breadChi[1:8,1:6], digits = 1)

############# Eclat

# Low support still gives items that have at least ~ 8 occurences
breadFreqItems <- bread %>% eclat(list(supp=0.0003, maxlen=2)) 
median(breadFreqItems@quality$count)

breadFreqRules<- breadFreqItems %>% ruleInduction(bread, confidence=0.4)
breadFreqRules
inspect(breadFreqRules)



########### Apriori
breadRules<-bread %>% apriori(list(supp=0.0005, conf=0.5)) 

breadRules %>% 
  sort(by="lift", decreasing=TRUE) %>% 
  inspect()


# induction – forces that drive to given purchase
# what forces make people buying a banana

coffee %>% 
  apriori(list(supp=0.001,conf = 0.10),
          appearance=list(default="lhs", rhs="Coffee"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect()

coffee %>% 
  apriori(list(supp=0.001,conf = 0.10),
          appearance=list(default="lhs", rhs="Bread"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect()

# what people buy when they have coffee
coffee %>% 
  apriori(list(supp=0.001,conf = 0.05),
          appearance=list(default="rhs", lhs="Coffee"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect()

coffee %>% 
  apriori(list(supp=0.001,conf = 0.05),
          appearance=list(default="rhs", lhs="Bread"),
          control=list(verbose=F)) %>%
  sort(by="confidence", decreasing=TRUE) %>%
  head(20) %>%
  inspect()

########### Visulalization

coffee %>% itemFrequencyPlot(topN=15, type="absolute", main="Item Absolute Frequency", col = cYellow, border = NA) 
coffee %>% itemFrequencyPlot(topN=15, type="relative", main="Item Relative Frequency", col = cBlue, border = NA) 


