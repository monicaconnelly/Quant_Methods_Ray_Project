
## Import Data

install.packages("tidyverse")
library(tidyverse)
library(readxl)
install.packages("pillar")

rdata_gen <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                        sheet = "General", col_types = c("date", 
                                                         "date", "blank", "blank", "text", "blank"))

rdata_cb <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "C - breach", col_types = c("date", 
                                                           "date", "blank", "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric"))

rdata_cp <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "C - peek", col_types = c("date", 
                                                         "date", "blank", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_cw <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "C - wave", col_types = c("date", 
                                                         "date", "blank", "text", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_sb <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "S - breach", col_types = c("date", 
                                                           "date", "blank", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_sp <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "S - peek", col_types = c("data", 
                                                         "data", "blank", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_sw <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                       sheet = "S - wave", col_types = c("date", 
                                                         "date", "blank", "text", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_prsnt <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                          sheet = "Present", col_types = c("date", 
                                                           "date", "blank", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_intact <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                           sheet = "Interacting", col_types = c("date", 
                                                                "date", "blank", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_mxdb <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                         sheet = "Max db", col_types = c("date", 
                                                         "date", "blank", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

rdata_feed <- read_excel("Quant_Methods_Ray_Project/The Shallows Data.xlsx", 
                         sheet = "Feed", col_types = c("date", 
                                                       "date", "blank", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))


## Create summary cols and cbind in new table

cb_tot <- rowSums(rdata_cb[, -c(1, 2)])
cp_tot <- rowSums(rdata_cp[, -c(1, 2)])
sb_tot <- rowSums(rdata_sb[, -c(1, 2)])
sp_tot <- rowSums(rdata_sp[, -c(1, 2)])
feed_tot <- rowSums(rdata_feed[, -c(1, 2)])
cw_avg <- rowMeans(rdata_cw[, -c(1, 2, 3)])
sw_avg <- rowMeans(rdata_sw[, -c(1, 2, 3)])
intact_avg <- rowMeans(rdata_intact[, -c(1, 2)])
mxdb_avg <- rowMeans(rdata_mxdb[, -c(1, 2)])
prsnt_avg <- rowMeans(rdata_prsnt[, -c(1, 2)])

rdata <- cbind(rdata_gen$Date, rdata_gen$Time, prsnt_avg, intact_avg, mxdb_avg, feed_tot, cw_avg, cp_tot, cb_tot, sw_avg, sp_tot, sb_tot)


## Run Models









