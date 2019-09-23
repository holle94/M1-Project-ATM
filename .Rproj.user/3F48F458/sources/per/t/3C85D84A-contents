
library(geosphere)
library(tidyverse)

# Features for the data ---------------------------------------------------

load(file="generated_data/sATM.Rdata")
#sATM <- bATM

# Holidays ----------------------------------------------------------------

# Denmark
# Ferie i Nordjylland (http://skoleferie-dk.dk/skoleferie-nordjylland/)

Jul    <- ifelse(((sATM$month=="December" & 20<=sATM$day) | (sATM$month=="January" & sATM$day<=4)), TRUE,FALSE)
Vinter <- ifelse((sATM$month=="February" & 11<=sATM$day & sATM$day<=26), TRUE,FALSE)
Paaske <- ifelse((sATM$month=="April" & 8 <= sATM$day & sATM$day<=17), TRUE,FALSE)
Sommer <- ifelse(((sATM$month=="June" & 23<=sATM$day) | (sATM$month=="July" & sATM$day>=1)| (sATM$month=="August" & sATM$day<=13)), TRUE,FALSE)
Efter  <- ifelse((sATM$month=="October" & 14<=sATM$day & sATM$day<=22), TRUE,FALSE)
sATM$Holliday <- Jul | Vinter | Paaske | Sommer | Efter

# Distance to other ATMs --------------------------------------------------


mat <- as.matrix(data.frame(lon = sATM$atm_lon, 
                            lat = sATM$atm_lat))

Aal.luft <- c(9.842829962 , 57.088999644)
Bil.luft <- c(9.150999396 , 55.73749705)
Køb.luft <- c(12.650462   , 55.620750)

#Afstand er i meter og i luftlinje:
sATM$Aal.luft <- distHaversine(mat, Aal.luft, r=6378137)
sATM$Bil.luft <- distHaversine(mat, Bil.luft, r=6378137)
sATM$Køb.luft <- distHaversine(mat, Køb.luft, r=6378137)

#Nærmeste lufthavn i km:
sATM <- sATM %>% 
  mutate(Airportdist=round(pmin(sATM$Aal.luft,sATM$Bil.luft,sATM$Køb.luft)/1000,2))


#Divid clock
sATM$Time <- ifelse((sATM$hour>=4 & sATM$hour<=9),   "Morning", NA)
sATM$Time <- ifelse((sATM$hour>=10 & sATM$hour<=15), "Midday", sATM$Time)
sATM$Time <- ifelse((sATM$hour>=16 & sATM$hour<=21), "Evening", sATM$Time)
sATM$Time <- ifelse((sATM$hour>=22 | sATM$hour<=3),  "Night", sATM$Time)

#Divid the days
sATM$Weekend <- ifelse(sATM$weekday == "Saturday" | sATM$weekday == "Sunday", TRUE, FALSE)

#Valuta
sATM$Euro <- as.factor(ifelse(sATM$currency=="DKK", FALSE, TRUE))

#Payout
sATM$Payout <- ifelse((sATM$day <=4 | sATM$day >=27), TRUE, FALSE)

#Customer
sATM$Customer <- str_detect(sATM$card_type, pattern = "on-us")

#Cardtype
sATM$Card <- sapply(strsplit(sATM$card_type, split=' - on-us', fixed=TRUE), function(x) (x[1]))
sATM$Card <- as.factor(sATM$Card)

#Region
library(sf)

loc <- sATM %>% group_by(atm_id) %>% 
  summarize(x=first(atm_lon), y=first(atm_lat))

reg <- NA
for (i in 1:nrow(loc)) {
  aa <- read_csv(paste0("http://dawa.aws.dk/kommuner/reverse?x=",loc$x[i] ,"&y=",loc$y[i]))
  reg[i] <- as.character(aa[10,])
}

loc$Region <- reg

sATM <- merge(sATM,loc[,c(1,4)],by ="atm_id")
sATM$Region <- sapply(strsplit(sATM$Region, split='": "', fixed=TRUE), function(x) (x[2]))
sATM$Region <- as.factor(sATM$Region)

sATM$Id       <- as.factor(sATM$atm_id)
sATM$Time     <- as.factor(sATM$Time)
sATM$Holliday <- as.factor(sATM$Holliday)
sATM$Customer <- as.factor(sATM$Customer)
sATM$Weekend  <- as.factor(sATM$Weekend)
sATM$Payout   <- as.factor(sATM$Payout)

#Final dataset
fsATM <- sATM %>% select(Id, Holliday, Airportdist, Time, 
                         Euro, Customer, Card, Weekend, Payout, Region)


save(fsATM, file="generated_data/fsATM.Rdata")

