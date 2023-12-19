### set working environment
# add packages to the list as needed
pkgs <- list("glmnet", "reticulate", "stringr", "rstudioapi", "data.table", "parallel", "minpack.lm", "doParallel",
             "foreach", "pROC", "gplots", "pwr", "dplyr", "caret", "sm", "ggplot2", "scales", "reshape2", "Hmisc",
             "bayesAB", "gridExtra", "plotly", "flux", "RColorBrewer", "plm", "xts", "pdp", "vip", "ranger", "vioplot",
             "randomForest")
# for parquet
library(arrow)

# for data cleaning
library(tidyr)
# install packages in list
# lapply(pkgs, install.packages, character.only = T)

# load packages in list
lapply(pkgs, require, character.only = T)

# set wd
setwd("/Users/llin/Desktop/ds_for_buz/Day1")
data <- read_parquet("day1-data.parquet")

# impute 0 to deal with missings
data <- data %>% 
  mutate_if(is.numeric, list(~replace_na(., 0)))

# create outcome variable as d30 - d14 spend
data$y <- data$d30_spend - data$d14_spend
data$y[data$y<0] <- 0

### correlation heatmap
# select variables used in analysis for heatmap
cor_vars <- c("y","d30_spend","d14_spend","d7_spend","d3_spend","d1_spend",
              "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7",
              "count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
              "count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

cor_data <- data[cor_vars]

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

cor_heat

### fit random forest
# subset data
#data_sub <- filter(data,data$d14_spend>20)
data_sub <- data[sample(nrow(data), 30000), ]

# select x vars
x1_vars <- c("d14_spend","d7_spend","d3_spend","d1_spend",
             "count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7")
             #"count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
             #"count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")
x2_vars <- c("count_p_1","max_p_2","p_4","p_5","sum_p_6","count_p_7")
             #"count_p_8","count_p_9","count_p_10","count_p_11","count_p_12",
             #"count_p_13","len_p_14","count_p_15","count_p_19","count_p_20")

# set seed and specify cross-validation
set.seed(825)
cv_fold <- trainControl(method = "cv",
                        number = 5,
                        search = "grid")

# set number of variables to randomly sample at each split
# mtry <- round(sqrt(ncol(data_sub)),0) ### not needed

# train random forests
rf_x1 <- train(as.formula(paste("y", paste(x1_vars, collapse = " + "), sep = " ~ ")), 
               data = data_sub,
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=30)
rf_x2 <- train(as.formula(paste("y", paste(x2_vars, collapse = " + "), sep = " ~ ")), 
               data = data_sub, 
               method = "ranger",
               #method = "rpart",
               trControl = cv_fold,
               metric = "RMSE",
               importance = "impurity",
               tuneLength=30)

print(rf_x1)
print(rf_x2)
plot(rf_x1)
plot(rf_x2)
rf_x1$results
rf_x2$results

### look at variable importance
print(varImp((rf_x1)))
plot(varImp((rf_x1)))
print(varImp((rf_x2)))
plot(varImp((rf_x2)))

rf_x1_var_imp <- vip(rf_x1$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15)

rf_x2_var_imp <- vip(rf_x2$finalModel,
                     #bar = FALSE,
                     size = 1.5,
                     num_features = 15)

vip_compare <- grid.arrange(rf_x1_var_imp,
                            rf_x2_var_imp,
                            nrow = 1,
                            ncol = 2)

### generate partial dependency plots with main x variable
rf_x1_par <- pdp::partial(rf_x1,
                     pred.var = c("d14_spend"),
                     chull = TRUE)
rf_x1_plot <- ggplot(rf_x1_par, aes(x=d14_spend, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("y = D30 Spend - D14 Spend")

rf_x1_plot

rf_x2_par <- pdp::partial(rf_x2,
                     pred.var = c("count_p_1"),
                     chull = TRUE)
rf_x2_plot <- ggplot(rf_x2_par, aes(x=count_p_1, y=yhat)) + 
  geom_line() +
  geom_smooth(method="auto") +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P1") +
  ylab("y = D30 Spend - D14 Spend")

rf_x2_plot

### generate partial dependency plots with two main x variables
rf_x1_par2 <- pdp::partial(rf_x1,
                      pred.var = c("d14_spend","d7_spend"),
                      chull = TRUE)
rf_x1_plot2 <- ggplot(rf_x1_par2, aes(x=d14_spend, y=d7_spend, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "y = D30 Spend - D14 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=d14_spend, y=d7_spend, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("D14 Spend") +
  ylab("D7 Spend")

rf_x1_plot2

rf_x2_par2 <- pdp::partial(rf_x2,
                      pred.var = c("count_p_1","sum_p_6"),
                      chull = TRUE)
rf_x2_plot2 <- ggplot(rf_x2_par2, aes(x=count_p_1, y=sum_p_6, z=yhat)) +
  geom_tile(aes(fill=yhat)) +
  scale_fill_gradientn(colours=rev(brewer.pal(9,"YlGnBu")), name = "y = D30 Spend - D14 Spend", labels = NULL) +
  stat_contour(bins=10,aes(x=count_p_1, y=sum_p_6, z=yhat), color="black", size=0.2) +
  #xlim(0,.5) +
  #ylim(0,10) +
  xlab("Count P1") +
  ylab("Sum P6")
