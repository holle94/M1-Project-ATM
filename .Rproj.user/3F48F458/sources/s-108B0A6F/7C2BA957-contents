

library(tidyverse)
library(lubridate)

set.seed(19092019)

# Data --------------------------------------------------------------------

temp <- tempfile()
download.file("https://sparnordopenbanking.com/downloads/open_dataset.zip", temp)
data1 <- read_csv(unz(temp, "atm_data_part1.csv"))
data2 <- read_csv(unz(temp, "atm_data_part2.csv"), 
                  col_types = cols(rain_3h = col_double()))
unlink(temp)



# Binding the data --------------------------------------------------------

data <- bind_rows(data1, data2)

data$date <- ymd(paste0(data$year, "-", data$month, "-", data$day))



# Save sample -------------------------------------------------------------

sATM <- sample_n(data, size=10000, replace = F)
bATM <- sample_n(data, size=250000, replace = F)


save(sATM, file = "generated_data/sATM.Rdata")
save(bATM, file = "generated_data/bATM.Rdata")

