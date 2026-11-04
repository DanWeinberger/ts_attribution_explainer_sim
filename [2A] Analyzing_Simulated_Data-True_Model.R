#Packages
library(lubridate)
library(glmmTMB)

#Read in data
flu_pi_weekly<-readRDS("./Data/flu_pneumonia_weekly.rds")
flu_pi_weekly<-flu_pi_weekly[order(flu_pi_weekly$date), ]

#Data preparation
year<-year(flu_pi_weekly$date)
month<-month(flu_pi_weekly$date)
time<-seq_along(flu_pi_weekly$date)
mean(duplicated(flu_pi_weekly$date))  #Checking for duplicates

#Load simulated data 
load("Simulated_Data.RData")
pneumo<-y_sim
x<-model.matrix(~as.factor(year) +
		             as.factor(month))
n<-length(pneumo)
flu<-flu_pi_weekly$rate_overall

#True applied to simulated data
time_cat<-factor(time)
group<-factor(rep(1, nrow(flu_pi_weekly)))
fit_sim_true<-glmmTMB(pneumo ~ as.factor(year) + 
		                           as.factor(month) + 
		                           flu + 
		                           ar1((time_cat + 0) | group),
                      family = nbinom2(link = 'identity'),
	                    REML = TRUE)
summary(fit_sim_true)
AIC(fit_sim_true)

#Attributable fraction estimate
beta_full<-fixef(fit_sim_true)$cond
beta_est<-beta_full[names(beta_full) != "flu"]
theta_est<-beta_full[names(beta_full) == "flu"]
alpha_est<-unlist(ranef(fit_sim_true)$cond$group)
delta_est<-sigma(fit_sim_true) 

lambda_est<-x%*%beta_est +
	          flu*theta_est +
	          alpha_est
p_est<-delta_est/(delta_est + (lambda_est))

lambda_est_cf<-x%*%beta_est +
	             0.00*theta_est +
	             alpha_est

af_est<-100.00*(1.00 - sum(lambda_est_cf)/sum(lambda_est))

#Inference for the attributable fraction: parametric bootstrap
b<-1000
af_bs_est<-rep(NA,
	             times = b)
set.seed(1879)

for(j in 1:b){
	
	 pneumo_bs<-rnbinom(n = n,
	                    size = delta_est,
	                    prob = p_est)
	 fit_bs_true<-glmmTMB(pneumo_bs ~ as.factor(year) + 
		                                as.factor(month) + 
		                                flu + 
		                                ar1((time_cat + 0) | group),
                        family = nbinom2(link = 'identity'),
	 	                    REML = TRUE)
   beta_bs_full<-fixef(fit_bs_true)$cond
   beta_bs_est<-beta_bs_full[names(beta_bs_full) != "flu"]
   theta_bs_est<-beta_bs_full[names(beta_bs_full) == "flu"]
   alpha_bs_est<-unlist(ranef(fit_bs_true)$cond$group)
   
   lambda_bs_est<-x%*%beta_bs_est +
	                flu*theta_bs_est +
	                alpha_bs_est
   
   lambda_bs_est_cf<-x%*%beta_bs_est +
	                   0.00*theta_bs_est +
	                   alpha_bs_est
   
   af_bs_est[j]<-100.00*(1.00 - sum(lambda_bs_est_cf)/sum(lambda_bs_est))

   print(j)
   
   }

#Summary table
af_summary<-data.frame(True_AF  = round(af_true, 2),
                       Est_AF   = round(af_est,  2),
                       CI_Lower = round(quantile(af_bs_est, 0.025, na.rm = TRUE), 2),
                       CI_Upper = round(quantile(af_bs_est, 0.975, na.rm = TRUE), 2))
print(af_summary, row.names = FALSE)

