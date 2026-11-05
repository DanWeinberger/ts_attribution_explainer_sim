#Packages
library(lubridate)
library(glmmTMB)
library(splines)

#Read in data
flu_pneu_weekly<-readRDS("./Data/flu_pneumonia_weekly.rds")
flu_pneu_weekly<-flu_pneu_weekly[order(flu_pneu_weekly$date), ]

#Data preparation
year<-year(flu_pneu_weekly$date)
month<-month(flu_pneu_weekly$date)
time<-seq_along(flu_pneu_weekly$date)
mean(duplicated(flu_pneu_weekly$date))  #Checking for duplicates

#Load simulated data 
load("./Data/Simulated_Data.RData")
pneumo<-y_sim
n<-length(pneumo)
flu<-flu_pneu_weekly$rate_overall

#Misspecified model applied to simulated data
time_cat<-factor(time)
group<-factor(rep(1, nrow(flu_pneu_weekly)))
year_spline<-ns(year, df = 3)
month_spline<-ns(month, df = 3)
x<-model.matrix(~year_spline +
		             month_spline)
fit_sim_miss<-glmmTMB(pneumo ~ year_spline + 
		                           month_spline + 
		                           flu + 
		                           ar1((time_cat + 0) | group),
                      family  = poisson(link = 'log'),  
                      REML    = TRUE)
summary(fit_sim_miss)
AIC(fit_sim_miss)

#Attributable fraction estimate
beta_full<-fixef(fit_sim_miss)$cond
beta_est<-beta_full[names(beta_full) != "flu"]
theta_est<-beta_full[names(beta_full) == "flu"]
alpha_est<-unlist(ranef(fit_sim_miss)$cond$group)

lambda_est<-exp(x%*%beta_est +
	              flu*theta_est +
	              alpha_est)

lambda_est_cf<-exp(x%*%beta_est +
	                 0.00*theta_est +
	                 alpha_est)

af_est<-100.00*(1.00 - sum(lambda_est_cf)/sum(lambda_est))

#Inference for the attributable fraction: parametric bootstrap
b<-1000
af_bs_est<-rep(NA,
	             times = b)
set.seed(3787)

for(j in 1:b){
	
	 pneumo_bs<-rpois(n = n,
	                  lambda = lambda_est)
	 fit_bs_miss<-glmmTMB(pneumo_bs ~ year_spline + 
		                                month_spline + 
		                                flu + 
		                                ar1((time_cat + 0) | group),
                        family  = poisson(link = 'log'),  
                        REML    = TRUE)
   beta_bs_full<-fixef(fit_bs_miss)$cond
   beta_bs_est<-beta_bs_full[names(beta_bs_full) != "flu"]
   theta_bs_est<-beta_bs_full[names(beta_bs_full) == "flu"]
   alpha_bs_est<-unlist(ranef(fit_bs_miss)$cond$group)
   
   lambda_bs_est<-exp(x%*%beta_bs_est +
	                    flu*theta_bs_est +
	                    alpha_bs_est)
   
   lambda_bs_est_cf<-exp(x%*%beta_bs_est +
	                       0.00*theta_bs_est +
	                       alpha_bs_est)
   
   af_bs_est[j]<-100.00*(1.00 - sum(lambda_bs_est_cf)/sum(lambda_bs_est))

   print(j)
   
   }

#Summary table
af_summary<-data.frame(True_AF  = round(af_true, 2),
                       Est_AF   = round(af_est,  2),
                       CI_Lower = round(quantile(af_bs_est, 0.025, na.rm = TRUE), 2),
                       CI_Upper = round(quantile(af_bs_est, 0.975, na.rm = TRUE), 2))
print(af_summary, row.names = FALSE)

