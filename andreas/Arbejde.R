library(tidyverse)
library(caret)


#Loading data
load(file="generated_data/fATM.Rdata")

#Divid clock
sATM$Morning <- ifelse((sATM$hour>=4 & sATM$hour<=9), TRUE, FALSE)
sATM$Midday  <- ifelse((sATM$hour>=10 & sATM$hour<=15), TRUE, FALSE)
sATM$Evening <- ifelse((sATM$hour>=16 & sATM$hour<=21), TRUE, FALSE)
sATM$Night   <- ifelse((sATM$hour>=22 | sATM$hour<=3), TRUE, FALSE)

#Divid the days
sATM$Weekday <- ifelse(sATM$weekday == "Monday" | sATM$weekday == "Tuesday" | sATM$weekday == "Wednesday" | sATM$weekday == "Thursday" | sATM$weekday == "Friday", TRUE, FALSE)
sATM$Weekend <- ifelse(sATM$weekday == "Saturday" | sATM$weekday == "Sunday", TRUE, FALSE)

#Valuta
sATM$Euro <- as.factor(ifelse(sATM$currency=="DKK",FALSE,TRUE))

#Customer
sATM$Customer <- str_detect(sATM$card_type, pattern = "on-us")

#Cardtype
sATM$Card <- sapply(strsplit(sATM$card_type, split=' - on-us', fixed=TRUE), function(x) (x[1]))
sATM$Card <- as.factor(sATM$Card)
