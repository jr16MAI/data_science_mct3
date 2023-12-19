### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc", "Rmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")

# install packages in list
# lapply(pkgs, install.packages, character.only = T)
install.packages(pkgs)
# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("/Users/llin/Desktop/ds_for_buz/Day2")

# load cleaned and prepared datasets
data <- read.csv("day2-data.csv", header=TRUE, sep=",")
data$X <- NULL
data$unique_id <- as.factor(data$unique_id)

# pull descriptive stats
summarySE(data, "d1_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d1_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d7_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d7_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

# add winsorized revenue and repeat purchases for day 14
d <- .98
data$d14_rev_denoised <- data$d14_revenue
data$d14_rev_denoised[data$d14_revenue > quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))] <- quantile(filter(data,d14_revenue > 0)$d14_revenue, c(d))

data$d14_repeat_purchases <- data$d14_purchases-1
data$d14_repeat_purchases[data$d14_repeat_purchases<0] <- 0

summarySE(data, "d14_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d14_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

summarySE(data, "d30_offer_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_offer_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_conversion", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_revenue", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rev_denoised", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_repeat_purchases", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_minutes", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_sessions", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_rounds_played", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_retention", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
summarySE(data, "d30_ad_views", groupvars = c("test_bucket"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)

### visualize treatment effects
install.packages("gplots")
library(gplots)
# using test_bucket
plotmeans(d30_revenue ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Gross Revenue",
          ylim=c(1,3.5),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

plotmeans(d30_rev_denoised ~ interaction(test_bucket, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Gross Revenue",
          ylim=c(1,2.5),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

# using the prettier action2
data$action2 <- as.factor(data$action2)
data$treatment_group <- ordered(data$action2,
                        levels = c("2.99 USD", "4.99 USD", "29.99 USD", "Control"))

plotmeans(d30_rev_denoised ~ interaction(treatment_group, sep ="   "),
          connect=list(1:4),
          barwidth=2,
          col="dark green",
          barcol="dark green",
          ccol="dark green",
          #data=analysis_data3,
          data=data,
          xlab="",
          ylab="Day 30 Revenue denoised",
          ylim=c(1.2,2.2),
          text.n.label="",
          #n.label=FALSE,
          main=c(""),
          p=0.95)

### assess treatment effects using linear regression
# set the reference category for our independent variable / predictor
data$action2 <- relevel(data$action2, ref = "Control")

# estimate linear regression and show results
lm_treat = lm(d30_rev_denoised ~ action2, data = data)

summary(lm_treat) 

'Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.7224     0.0616   27.94   <2e-16 *** # the control
  action22.99 USD   -0.1504     0.1069   -1.41     0.16    
action229.99 USD   0.0140     0.1068    0.13     0.90    
action24.99 USD    0.0768     0.1067    0.72     0.47    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.5 on 363436 degrees of freedom
Multiple R-squared:  1.01e-05,	Adjusted R-squared:  1.86e-06 
F-statistic: 1.22 on 3 and 363436 DF,  p-value: 0.299'

# not significant result, means the offers are doesnt have an significant effect 
# on the revenue (people don't spend money)

##########

data$action2 <- relevel(data$action2, ref = "Control")

# estimate linear regression and show results
lm_treat2 = lm(d30_conversion ~ action2, data = data)

summary(lm_treat) 


'Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.025552   0.000411   62.17  < 2e-16 ***
  action22.99 USD   0.003741   0.000713    5.25  1.5e-07 ***
  action229.99 USD -0.006410   0.000712   -9.00  < 2e-16 ***
  action24.99 USD   0.001037   0.000712    1.46     0.15    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.157 on 363436 degrees of freedom
Multiple R-squared:  0.000452,	Adjusted R-squared:  0.000444 
F-statistic: 54.8 on 3 and 363436 DF,  p-value: <2e-16'

# increase share of buyer of 0.3% when company offer 2.99 USD

lm_treat_ram = lm(d30_conversion ~ device_ram, data = data)

summary(lm_treat_ram)

data$device_ram_gb <- data$device_ram/1000

lm_treat_ram = lm(d30_rev_denoised ~ device_ram_gb, data = data)

summary(lm_treat_ram)

'Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -1.2211     0.1084   -11.3   <2e-16 ***
device_ram_gb   1.1384     0.0393    29.0   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.5 on 363438 degrees of freedom
Multiple R-squared:  0.00231,	Adjusted R-squared:  0.0023 
F-statistic:  840 on 1 and 363438 DF,  p-value: <2e-16'

# for every one gb increase in ram, the revenue increase by 1.14 usd.
install.packages("fastDummies")
library(fastDummies)
data <- dummy_cols(data, select_columns = "country_tier")

lm_treat_ctry = lm(d30_rev_denoised ~ data$`country_tier_Country tier 1`
                   + `country_tier_Country tier 2`
                   + `country_tier_Country tier 3`
                   + `country_tier_Country tier 4`
                   + `country_tier_Country tier 5`, data = data)
summary(lm_treat_ctry)

tier1_countries <- unique(data %>%
  filter(country_tier == "Country tier 1") %>%
  pull(country))

tier1_countries

# "CH" "HK" "KW" "NO" "SE" "SG" "US"

tier2_countries <- unique(data %>%
                            filter(country_tier == "Country tier 2") %>%
                            pull(country))
tier2_countries
# "AE" "AT" "AU" "BE" "CA" "CY" "DE" "DK" "DO" "FR" "GB" "LU" "NL" "TH" "TW"

tier3_countries <- unique(data %>%
                            filter(country_tier == "Country tier 3") %>%
                            pull(country))
tier3_countries

dependency <- c(data$`country_tier_Country tier 1`,
                data$`country_tier_Country tier 2`,
                data$`country_tier_Country tier 3`,
                data$`country_tier_Country tier 4`,
                data$`country_tier_Country tier 5`,
                as.integer(data$device_ram))
cor_data <- df[dependency]

# create correlation matrix
# fill all missings with 0 to allow for correlation calculation
cor_data[is.na(cor_data) == TRUE] <- 0
cormat <- round(cor(cor_data),2)

# helper function to get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# label levels of factors to make heatmap nice to read
#levels(melted_cormat$Var1)[levels(melted_cormat$Var1)=="years_since_first_spend"] <- "Years active on platform"
#levels(melted_cormat$Var2)[levels(melted_cormat$Var2)=="years_since_first_spend"] <- "Years active on platform"

cor_heat <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  theme(axis.text.y = element_text(size = 12))+
  coord_fixed()