#Packages
library(lubridate)
library(glmmTMB)

#Read in data
#pneumonia and influenza deaths obtained from: https://data.cdc.gov/api/v3/views/pp7x-dyj2/
#Flu hospitalizations:CDc Flu-Survnet data, accessed via CMU Delphu Epiportal 11/4/2026
flu_pneu_weekly<-readRDS("./Data/flu_pneumonia_weekly.rds")
flu_pneu_weekly<-flu_pneu_weekly[order(flu_pneu_weekly$date),]

#Data preparation
pneumo<-flu_pneu_weekly$deaths_from_pneumonia
flu<-flu_pneu_weekly$rate_overall
year<-year(flu_pneu_weekly$date)
month<-month(flu_pneu_weekly$date)
time<-seq_along(flu_pneu_weekly$date)
mean(duplicated(flu_pneu_weekly$date))  #Checking for duplicates

#Model fitting
time_cat<-factor(time)
group<-factor(rep(1, nrow(flu_pneu_weekly)))
fit<-glmmTMB(pneumo ~ as.factor(year) + 
		                  as.factor(month) + 
		                  flu + 
		                  ar1((time_cat + 0) | group)
						  ,
             family = nbinom2(link = 'identity'),
             REML = TRUE)
summary(fit)
AIC(fit)

#Simulating "real" data
x<-model.matrix(~as.factor(year) +
		             as.factor(month))
n<-length(pneumo)

beta_full<-fixef(fit)$cond
beta_true<-beta_full[names(beta_full) != "flu"]
theta_true<-beta_full[names(beta_full) == "flu"]
alpha_true<-unlist(ranef(fit)$cond$group)
delta_true<-sigma(fit) 

lambda_true<-x%*%beta_true +
	           flu*theta_true +
	           alpha_true
p_true<-delta_true/(delta_true + (lambda_true))

set.seed(1568)
y_sim<-rnbinom(n = n,
	             size = delta_true,
	             prob = p_true)

#Defining the true attributable fraction
lambda_true_cf<-x%*%beta_true +
	              0.00*theta_true +
	              alpha_true

af_true<-100.00*(1.00 - sum(lambda_true_cf)/sum(lambda_true))

save(y_sim, af_true,
     file = "./Data/Simulated_Data.RData")