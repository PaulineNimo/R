library(readxl)
library(dplyr)

data <- read_xlsx("E:/Data Analyst Test.xlsx", sheet = 2)
prod_tbl <- read_excel("E:/Data Analyst Test.xlsx", sheet = 1)
data2 <- data[14:nrow(data),]
colnames(data2) <- data[13,]
colnames(data2)[10] <- "INTEREST"
prod_tbl2 <- prod_tbl[27:nrow(prod_tbl),]
colnames(prod_tbl2) <- prod_tbl[26,]
data3 <- merge(data2, prod_tbl2, by.x = "Product", by.y = "Loan Name", all.x = T, sort = F)
data_clients <- data3$`Client ID`[data3$Cycle > 1]
data4 <- data3[data3$`Client ID` %in% data_clients,]
#order clients that bought more than one product make their purchases 
client_prod <- data4 %>%
                group_by(Cycle) %>%
                count(`Product associated with loan`) %>%
                arrange(Cycle,desc(n))

data_clients2 <- data3$`Client ID`[data3$`Product associated with loan` == "LPG cylinder" & data3$Cycle == 1]
data5 <- data3[data3$`Client ID` %in% data_clients2,]
client_prod <- data5 %>%
    group_by(Cycle) %>%
    count(`Product associated with loan`) %>%
    arrange(Cycle,desc(n))

#correlation/pattern between age of client and choice of product bought
data3$BirthDate <- (as.Date(as.numeric(data3$DOB), origin = "1900-01-01")-2)
data3$Age <- ifelse(as.Date(paste0(year(Sys.Date()),substr(data3$BirthDate,5,10)))>Sys.Date(),year(Sys.Date())-year(data3$BirthDate)-1,year(Sys.Date())-year(data3$BirthDate))
#mod1 <- lm(as.factor(data3$`Product associated with loan`) ~ data3$Age)
#plot(data3$Age)
hist(data3$Age)
data6 <- data3
data6$Category_Vals <- as.numeric(factor(data3$Category))
#plot(data6$Age)
plot(data6$Age,data6$Category_Vals)
#mod2 <- lm(data6$Category_Vals ~ data6$Age)
#summary(mod2)
mod2 <- lm(data6$Age ~ factor(data6$Category))
summary(mod2)
data7 <- data6[data6$Category == "Cooking",]
plot(data7$Age)

#plot(data3$Age,factor(data3$Category))

#repayment behaviour measured with TRP and choice of product bought
plot(factor(data3$Category),data3$TRP)
