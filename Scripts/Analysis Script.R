
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


prsnt_by_wind_lm <- lm(prsnt_avg ~ V3, data = rdata.df)
prsnt_by_wind_cor <- cor.test(rdata.df$prsnt_avg, rdata.df$V3, method="pearson")
summary(prsnt_by_wind_lm)
intact_by_prsnt_lm <- lm(intact_avg ~ prsnt_avg, data = rdata.df)
intact_by_prsnt_cor <- cor.test(rdata.df$intact_avg, rdata.df$prsnt_avg, method="pearson")
summary(intact_by_prsnt_lm)
mxdb_by_prsnt_lm <- lm(mxdb_avg ~ prsnt_avg, data = rdata.df)
mxdb_by_prsnt_cor <- cor.test(rdata.df$mxdb_avg, rdata.df$prsnt_avg, method="pearson")
summary(mxdb_by_prsnt_lm)
feed_by_mxdb_lm <- lm(feed_tot ~ mxdb_avg, data = rdata.df)
feed_by_mxdb_cor <- cor.test(rdata.df$feed_tot, rdata.df$mxdb_avg, method="pearson")
summary(feed_by_mxdb_lm)

#PLOT EACH INTERACTION

cb_by_prsnt <- lm(cb_tot ~ prsnt_avg, data = rdata.df)
cp_by_prsnt <- lm(cp_tot ~ prsnt_avg, data = rdata.df)
cw_by_prsnt <- lm(cw_avg ~ prsnt_avg, data = rdata.df)
#PLOT THE ABOVE HERE IN ONE GRAPH

sb_by_prsnt <- lm(sb_tot ~ prsnt_avg, data = rdata.df)
sp_by_prsnt <- lm(sp_tot ~ prsnt_avg, data = rdata.df)
sw_by_prsnt <- lm(sw_avg ~ prsnt_avg, data = rdata.df)
#PLOT THE ABOVE HERE IN ONE GRAPH

cb_by_mxdb <- lm(cb_tot ~ mxdb_avg, data = rdata.df)
cp_by_mxdb <- lm(cp_tot ~ mxdb_avg, data = rdata.df)
cw_by_mxdb <- lm(cw_avg ~ mxdb_avg, data = rdata.df)
#PLOT THE ABOVE HERE IN ONE GRAPH

sb_by_mxdb <- lm(sb_tot ~ mxdb_avg, data = rdata.df)
sp_by_mxdb <- lm(sp_tot ~ mxdb_avg, data = rdata.df)
sw_by_mxdb <- lm(sw_avg ~ mxdb_avg, data = rdata.df)
#PLOT THE ABOVE HERE IN ONE GRAPH

cb_by_feed <- lm(cb_tot ~ feed_tot, data = rdata.df)
cp_by_feed <- lm(cp_tot ~ feed_tot rdata.df)
cw_by_feed <- lm(cw_avg ~ feed_tot, data = rdata.df)
#PLOT THE ABOVE HERE

#ALSO DO BEHAVIOR BY MULTIFACTORS HERE!!!
cb_by_all <- 
cp_by_all <- 
cw_by_all <- 
  
  
  







