
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

cb_tot <- rowSums(rdata_cb[, -c(rdata_cb$Date, rdata_cb$Time)])











