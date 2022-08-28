library(tidyverse)
library(sf)
library(geodist)
library(data.table)
library(glmm)
library(lme4)
library(qplot)
library(lattice)
library(jtools)
library(ggstance)
library(broom.mixed)
library(huxtable)
#read in the data
data <- read.csv(file = 'Desktop/Dissertation/Data/GLMM_final_check.csv')
head(data)
store_type=read.csv(file = 'Desktop/Dissertation/Data/clean_data/store_type.csv')
head(store_type)
store_type <- store_type[c("fk_sgplaces","sub_category")]
store_type
data <- merge(data,store_type,by.x="dest_store",by.y="fk_sgplaces",type = "inner")
head(data)
# public transport factors/ add zero
# transfer ori_zip to character
data$orig_zip = as.character(data$orig_zip)

data$visit_each[data$visit_each == 0] <- 1
data$visit_each <- as.integer(data$visit_each)
data$log_visit_each = log(data$visit_each)
data$log_visit_each
library(ICC)


control = glmerControl(optimizer = "nloptwrap",optCtrl = list(maxfun = 1000000))
SIM_prod1 <- glmer( visit_each ~ orig_zip + log_distance_m ++ log_public_transport+ log_dest_flr_sq_ft + log_competition_f + (1|log_high_income)+(1|log_medium_high_income)+(1|log_medium_low_income), family=poisson(link = "log"), control=control, data=data)
data$log_distance_m
log_distance_center 
summary(SIM_prod1)
summ(SIM_prod1)
#(1|log_low_income)+(1|log_medium_low_income)+(1|log_medium_high_income)+(1|log_high_income)
vcov(SIM_prod1)

data$SIM_prod1_coef<-coef(SIM_prod1)

data$SIM_prod1_fitted <- fitted(SIM_prod1)

data$SIM_prod1_resid <- resid(SIM_prod1)
#check R Square
CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(data$visit_each,data$SIM_prod1_fitted)
#check RMSE
sqrt(mean((data$visit_each-data$SIM_prod1_fitted)^2))

#visualize of GLMM
plot_model(SIM_prod1)
effect_plot(SIM_prod1, pred = log_high_income   , interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)
plot_summs(SIM_prod1,SIM_prod2)
plot_summs(SIM_prod1,SIM_prod2, plot.distributions = TRUE)

export_summs(SIM_prod1,SIM_prod2, scale = TRUE)

dotchart(sort(data(~ log_high_income)), cex=0.7)


plot(data$SIM_prod1_resid ~ data$SIM_prod1_fitted, xlab='predicted values', ylab='normalized residuals')
plot(data$SIM_prod2_resid ~ data$SIM_prod2_fitted, xlab='predicted values', ylab='normalized residuals')
plot(data$log_competition_f ~ data$SIM_prod1_fitted, xlab='predicted values', ylab='normalized residuals')

plot(resid(SIM_prod2) ~ fitted(SIM_prod2), xlab='predicted values', ylab='normalized residuals')
plot((data$ ~ data$visit_each), xlab='competiton', ylab='visits')
hist(data$SIM_prod1_resid)
hist(resid(SIM_prod2))
plot(ranef(SIM_prod1))


dd=coef(SIM_prod1)$log_medium_high_income

xqplot(SIM_prod1)

qplot(data$visit_each) + geom_histogram()
qplot(log(data$visit_each)) + geom_histogram()
qplot(data$distance_m) + geom_histogram()
qplot(log(data$distance_m)) + geom_histogram()
qplot(data$log_competition_f) + geom_histogram()
qplot(data$log_medium_high_income) + geom_histogram()
qplot(data$log_medium_low_income) + geom_histogram()
qplot(data$log_high_income) + geom_histogram()
qplot(data$log_public_transport) + geom_histogram()
qplot(log(data$distance_to_center)) + geom_histogram()
qplot(log(data$dest_flr_sq_ft)) + geom_histogram()

#####################################################################################
# GLM model for comparison
SIM_prod2 <- glm(visit_each ~ orig_zip + log_distance_m +log_dest_flr_sq_ft + log_distance_center + log_public_transport+log_competition_f+log_medium_low_income+log_medium_high_income+log_high_income,na.action = na.exclude, family = poisson(link = "log"), data=data)

summary(SIM_prod2)
summ(SIM_prod2)
data$SIM_prod2_fitted <- fitted(SIM_prod2)
data$SIM_prod2_resid <- resid(SIM_prod2)
#check R square
CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(data$visit_each,data$SIM_prod2_fitted)

sqrt(mean((data$visit_each-data$SIM_prod2_fitted)^2))


effect_plot(SIM_prod1, pred =log_high_income, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)



# build model for convenience store
convenience <- read.csv(file = 'Desktop/Dissertation/Data/convenience_store_glm.csv')
head(convenience)
convenience=convenience['fk_sgplaces']
convenience$orig_zip = as.character(convenience$orig_zip)
convenience$visit_each[convenience$visit_each == 0] <- 1
convenience$log_visit_each = log(convenience$visit_each)
convenience$log_visit_each
# convenience store glm
SIM_conv <- glm(visit_each ~ orig_zip + log_distance_m +log_dest_flr_sq_ft + log_distance_center + log_public_transport+log_competition_f,na.action = na.exclude, family = poisson(link = "log"), data=convenience)
summary(SIM_conv)
summ(SIM_conv)

convenience$SIM_conv_fitted <- fitted(SIM_conv)

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(convenience$visit_each,convenience$SIM_conv_fitted)
 #supermarket glm
supermarket <- read.csv(file = 'Desktop/Dissertation/Data/convenience_store_glm.csv')
head(supermarket)
supermarket$orig_zip = as.character(supermarket$orig_zip)
SIM_sup <- glm(visit_each ~ orig_zip + log_distance_m +log_dest_flr_sq_ft + log_distance_center + log_public_transport+log_competition_f,na.action = na.exclude, family = poisson(link = "log"), data=supermarket)
summary(SIM_sup)
summ(SIM_sup)
supermarket$SIM_sup_fitted <- fitted(SIM_sup)

icc(data)

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(supermarket$visit_each,supermarket$SIM_sup_fitted)
############


#############GLM for income clusters

cluster0 <- read.csv(file = 'Desktop/Dissertation/Data/cluster0_model.csv')
cluster1 <- read.csv(file = 'Desktop/Dissertation/Data/cluster1_model.csv')
cluster2 <- read.csv(file = 'Desktop/Dissertation/Data/cluster2_model.csv')
cluster0$orig_zip = as.character(cluster0$orig_zip)
cluster1$orig_zip = as.character(cluster1$orig_zip)
cluster2$orig_zip = as.character(cluster2$orig_zip)
SIM_x <- glm(visit_each ~ orig_zip + log_distance_m +log_dest_flr_sq_ft + log_distance_center + log_public_transport+log_competition_f,na.action = na.exclude, family = poisson(link = "log"), data=cluster2)
summary(SIM_x)
summ(SIM_x)
cluster2$SIM_fitted <- fitted(SIM_x)

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(cluster2$visit_each,cluster2$SIM_fitted)

icc(data, model = "twoway", type = "agreement", unit = "single")

write_csv(data, "data_SIM_visualize.csv")

vis <- read.csv(file = 'Desktop/Dissertation/Data/data_SIM_visualize.csv')
head(vis)
vis

plot(vis$visit_each ~ vis$SIM_prod1_fitted, xlab='predicted flows', ylab='obserived flows',geom_smooth())

ggplot(vis, aes(x=SIM_prod2_fitted,y=visit_each))+geom_point()+geom_smooth()

plot(vis$visit_each ~ vis$SIM_prod2_fitted, xlab='predicted flows', ylab='obserived flows', trendline=TRUE)
