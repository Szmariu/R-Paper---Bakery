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
a <- read.csv('data/BreadBasket_DMS.csv')

# Remove transactions with NONE and Adjustment
a <- a %>% filter(a$Item != "NONE" & a$Item != "Adjustment" )

# Write only the relevant columns
write.csv(a[, c(3:4)], './data/transactions.csv')

# Read the data as transactions
bread <- read.transactions('data/transactions.csv', 
                           format="single", 
                           cols = c('Transaction', 'Item'), 
                           sep = ',', 
                           header = TRUE)

bread
bread.Items <- bread@itemInfo
# inspect(bread)
size(bread) 
