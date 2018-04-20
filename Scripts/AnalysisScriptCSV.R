
## Import Data

install.packages("tidyverse")
install.packages("pillar")
install.packages("zoo")
install.packages("car")
install.packages("psych")
library(tidyverse)
library("zoo")
library("car")
library("psych")


# read in all CSV files in Data folder and rename

rdata_gen <- ShallowsGeneral
rdata_mxdb <- ShallowsMaxDB
rdata_prsnt <- ShallowsPresent
rdata_intact <- ShallowsInteracting
rdata_feed <- ShallowsFeed
rdata_cb <- ShallowsCownoseBreach
rdata_cp <- ShallowsCownosePeak
rdata_cw <- ShallowsCownoseWave
rdata_sb <- ShallowsSouthernBreach
rdata_sp <- ShallowsSouthernPeak
rdata_sw <- ShallowsSouthernWave



# Create summary cols and cbind in new table

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

rdata <- cbind(rdata_gen$Date, rdata_gen$Time, rdata_gen$`Wind Speed`, prsnt_avg, intact_avg, mxdb_avg, feed_tot, cw_avg, cp_tot, cb_tot, sw_avg, sp_tot, sb_tot)
rdata.df <- as.data.frame(rdata)

## Run Models

# These models look to investigate the relationship
# between the observed factors that could potentially
# contribute to the ray's patterns of behavior.

prsnt_by_wind_lm <- lm(prsnt_avg ~ V3, data = rdata.df)
cor.test(rdata.df$prsnt_avg, rdata.df$V3, method="pearson")
summary(prsnt_by_wind_lm) ## p = 0.03 *significant
intact_by_prsnt_lm <- lm(intact_avg ~ prsnt_avg, data = rdata.df)
cor.test(rdata.df$intact_avg, rdata.df$prsnt_avg, method="pearson")
summary(intact_by_prsnt_lm) ## p < 0.001 *significant
mxdb_by_prsnt_lm <- lm(mxdb_avg ~ prsnt_avg, data = rdata.df)
cor.test(rdata.df$mxdb_avg, rdata.df$prsnt_avg, method="pearson")
summary(mxdb_by_prsnt_lm) ## p = 0.47
feed_by_mxdb_lm <- lm(feed_tot ~ mxdb_avg, data = rdata.df)
cor.test(rdata.df$feed_tot, rdata.df$mxdb_avg, method="pearson")
summary(feed_by_mxdb_lm) ## p = 0.50

# From this analysis, we see the strongest correlation
# between the factors of people present at the side of the
# tank and the number of people interacting with the rays.
# Surprisingly, the smallest correlations were those
# involving the maximum decibel level recorded within the
# interval, indicating that the number of people present
# and whether or not they were feeding the rays at the
# time are not good predictors of the noise levels around
# the tank.


# This next set of models aims to look at the main effect
# of number of people present at the side of the tank had
# on each behavior by the Cownose and Southern Rays.  Most
# significant interaction will be plotted below.

cb_by_prsnt <- lm(cb_tot ~ prsnt_avg, data = rdata.df)
summary(cb_by_prsnt)
cp_by_prsnt <- lm(cp_tot ~ prsnt_avg, data = rdata.df)
summary(cp_by_prsnt)
cw_by_prsnt <- lm(cw_avg ~ prsnt_avg, data = rdata.df)
summary(cw_by_prsnt)

sb_by_prsnt <- lm(sb_tot ~ prsnt_avg, data = rdata.df)
summary(sb_by_prsnt)
sp_by_prsnt <- lm(sp_tot ~ prsnt_avg, data = rdata.df)
summary(sp_by_prsnt)
sw_by_prsnt <- lm(sw_avg ~ prsnt_avg, data = rdata.df)
summary(sw_by_prsnt)

plot(cw_by_prsnt)


# This next set of models aims to look at the main effect
# of average maximum decibel level recorded within each trial
# had on each behavior by the Cownose and Southern Rays.  Most
# significant interaction will be plotted below.

cb_by_mxdb <- lm(cb_tot ~ mxdb_avg, data = rdata.df)
summary(cb_by_mxdb)
cp_by_mxdb <- lm(cp_tot ~ mxdb_avg, data = rdata.df)
summary(cp_by_mxdb)
cw_by_mxdb <- lm(cw_avg ~ mxdb_avg, data = rdata.df)
summary(cw_by_mxdb)

sb_by_mxdb <- lm(sb_tot ~ mxdb_avg, data = rdata.df)
summary(sb_by_mxdb)
sp_by_mxdb <- lm(sp_tot ~ mxdb_avg, data = rdata.df)
summary(sp_by_mxdb)
sw_by_mxdb <- lm(sw_avg ~ mxdb_avg, data = rdata.df)
summary(sw_by_mxdb)

plot(cw_by_mxdb)


# This next set of models aims to look at the main effect
# of number of people interacting with the rays had on
# each behavior by the Cownose and Southern Rays.  Most
# significant interaction will be plotted below.

cb_by_intact <- lm(cb_tot ~ intact_avg, data = rdata.df)
summary(cb_by_intact)
cp_by_intact <- lm(cp_tot ~ intact_avg, data = rdata.df)
summary(cp_by_intact)
cw_by_intact <- lm(cw_avg ~ intact_avg, data = rdata.df)
summary(cw_by_intact)

sb_by_intact <- lm(sb_tot ~ intact_avg, data = rdata.df)
summary(sb_by_intact)
sp_by_intact <- lm(sp_tot ~ intact_avg, data = rdata.df)
summary(sp_by_intact)
sw_by_intact <- lm(sw_avg ~ intact_avg, data = rdata.df)
summary(sw_by_intact)

plot(sw_by_intact)


# This next set of models aims to look at the main effect
# of number of people feeding the rays had on each
# behavior by the Cownose and Southern Rays.  Most
# significant interaction will be plotted below.

cb_by_feed <- lm(cb_tot ~ feed_tot, data = rdata.df)
summary(cb_by_feed)
cp_by_feed <- lm(cp_tot ~ feed_tot, data = rdata.df)
summary(cp_by_feed)
cw_by_feed <- lm(cw_avg ~ feed_tot, data = rdata.df)
summary(cw_by_feed)

sb_by_feed <- lm(sb_tot ~ feed_tot, data = rdata.df)
summary(sb_by_feed)
sp_by_feed <- lm(sp_tot ~ feed_tot, data = rdata.df)
summary(sp_by_feed)
sw_by_feed <- lm(sw_avg ~ feed_tot, data = rdata.df)
summary(sw_by_feed)

plot(cw_by_feed)

# From this analysis, we see that each individual main
# effect has little impact on the behavior of either
# ray species.  This could be due to the fact that they
# occured at such frequent intervals with such little
# variation that the measures were not sufficient to
# detect a true causal relationship.  Further work should
# aim to find a better way to measure the impact that
# each form of human interaction has on the behavior 
# (both nautral and captivity-created) of the rays.


# Additionally, this multilevel analysis aims to look at
# the interaction effect between the previous
# contributing factors and the resulting behavior by the
# Cownose and Southern Rays.

cb_by_all_lm <- lm(cb_tot ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(cb_by_all_lm, type = "II")
cp_by_all_lm <- lm(cp_tot ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(cp_by_all_lm, type = "II")
cw_by_all_lm <- lm(cw_avg ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(cw_by_all_lm, type = "II")
sb_by_all_lm <- lm(sb_tot ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(sb_by_all_lm, type = "II")
sp_by_all_lm <- lm(sp_tot ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(sp_by_all_lm, type = "II")
sw_by_all_lm <- lm(sw_avg ~ feed_tot:mxdb_avg + feed_tot:prsnt_avg + feed_tot:intact_avg
                   + mxdb_avg:prsnt_avg + mxdb_avg:intact_avg + prsnt_avg:intact_avg, data = rdata.df)
Anova(sw_by_all_lm, type = "II")

# In this case, the multilevel analysis proved more
# useful in examining the impact that each factor had on
# the behavior of the rays.  By including how each
# factor interacts with one another to influence the 
# behavior of each species, more variables are accounted
# for and therefore the findings are more externally
# applicable.

