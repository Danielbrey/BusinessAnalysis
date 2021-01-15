library(tidyverse)
library(rvest)


#Store spreadsheets in data frames
prod.info.data <- read_csv("School Supplies.xlsx - Product Info.csv")
hist.orders <- read_csv("School Supplies.xlsx - Historical Orders.csv")
carrier.rates<- read_csv("School Supplies.xlsx - Carrier Rates.csv")


#Modify orders to be equivalent to 2015 predictions
new.orders <- hist.orders
new.orders$`Order Qty` <- new.orders$`Order Qty` * 1.15
new.orders$`Order Weight` <- new.orders$`Order Weight`* 1.15
new.orders$`Order Dt` <- str_replace(new.orders$`Order Dt`, "14$", "15")


#Create potential glue stick orders
gluestick.orders15 <- new.orders %>% 
  filter(`Product Id` == 1)
gluestick.orders15$`Product Id` <- 4
gluestick.orders15$`Product Weight`<- 2
gluestick.orders15$`Order Weight` <- gluestick.orders15$`Order Qty` * gluestick.orders15$`Product Weight`


#Find transportation costs if we acquired glue sticks from Supplier A
gluestick.orders15$Supplier <- "A"
all.data <- full_join(gluestick.orders15,
                      new.orders)
week.data.A <- all.data %>% 
  group_by(`Order Dt`, `Str Nbr`, Supplier) %>% 
  summarize(store.pounds = sum(`Order Weight`))

week.data.A <- left_join(week.data.A, carrier.rates, by = c("Supplier", "Str Nbr"))
modify.cost <- str_replace(week.data.A$Cost,
                           ",", "")
week.data.A$Cost <- as.numeric(str_replace(modify.cost,
                                           "\\$", ""))

week.data.A <- week.data.A %>% 
  mutate(carrier.is.Y = as.integer((Carrier == "Y"))) 

week.data.A <- week.data.A %>% 
  mutate(trans.costs = as.integer(carrier.is.Y) * Cost * store.pounds + (1 - carrier.is.Y) * Cost * ceiling(store.pounds/45000))

week.data.A2 <- week.data.A %>% 
  group_by(`Order Dt`, `Str Nbr`, Supplier) %>% 
  summarize(best.trans.cost = min(trans.costs))


#Find transportation costs if we acquire glue sticks from Supplier B
gluestick.orders15B <- gluestick.orders15
gluestick.orders15B$Supplier <- "B"
all.dataB <- full_join(gluestick.orders15B,
                       new.orders)
week.data.B <- all.dataB %>% 
  group_by(`Order Dt`, `Str Nbr`, Supplier) %>% 
  summarize(store.pounds = sum(`Order Weight`))

week.data.B <- left_join(week.data.B, carrier.rates, by = c("Supplier", "Str Nbr"))
modify.costB <- str_replace(week.data.B$Cost,
                            ",", "")
week.data.B$Cost <- as.numeric(str_replace(modify.costB,
                                           "\\$", ""))

week.data.B <- week.data.B %>% 
  mutate(carrier.is.Y = as.integer((Carrier == "Y"))) 

week.data.B <- week.data.B %>% 
  mutate(trans.costs = as.integer(carrier.is.Y) * Cost * store.pounds + (1 - carrier.is.Y) * Cost * ceiling(store.pounds/45000))

week.data.B2 <- week.data.B %>% 
  group_by(`Order Dt`, `Str Nbr`, Supplier) %>% 
  summarize(best.trans.cost = min(trans.costs))


#Summarize Results
Total.gluesticks <- sum(gluestick.orders15$`Order Qty`)

gluestick.costsA <- Total.gluesticks * 0.80
Total.Trans.costA <- sum(week.data.A2$best.trans.cost)
TotalCostA <- Total.Trans.costA + gluestick.costsA

gluestick.costsB <- Total.gluesticks * 0.77
Total.Trans.costB <- sum(week.data.B2$best.trans.cost)
TotalCostB <- Total.Trans.costB + gluestick.costsB

SummaryData <- data.frame("Values" = c("gluestick Product Cost - A", "gluestick Product Cost - B", "Transportation Cost - A", "Transportation Cost - B", "Total Cost - A", "Total Cost - B"), "Answers" = c(gluestick.costsA, gluestick.costsB, Total.Trans.costA, Total.Trans.costB, TotalCostA, TotalCostB))

SummaryData





