### 0. Load Required Packages and Utility Functions
suppressPackageStartupMessages({
  library(rstan)
  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = TRUE)
  Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
  
  library(brms)
  library(lme4)
  library(here)
  library(tidyverse)
  library(tidybayes)
  library(bayesplot)
  library(lubridate)
  library(zoo)
})

source("Packages/Utility_Functions_MMM.R")



### 1. Use Pre-Processed Media Mix Data
load("RData/pp_MMM_email_cntrl.RData")



### 2. Train Models by Pooling Type
#------------------------------------------------------------#
# MCMC Conditions                                            #
#------------------------------------------------------------#
num_warmups     <- 1000
num_iterations  <- 2000
num_chains      <- 4
num_cores       <- 4
num_refresh     <- num_iterations/10
thin_num        <- 1
seed_num        <- 12345
num_treedepth   <- 15
acceptance_rate <- 0.95



#------------------------------------------------------------#
# Target:              True Gross Adds                       #
# Media Normalization: across dma per media normalization    #
#------------------------------------------------------------#
var_target      = 'gross_add'
var_control        = c('seasonality_gross_add','MA_4','price_ARPU_fprint','comp_index','weather1','weather5','promotion','mega_holiday','covid_daily_deaths','AR2','AR4','SNOW','covid_dummy','email_sent_w')

pp_MMM_ls = pp_MMM_email_cntrl

# Print media variables
print_media_names(pp_MMM_ls = pp_MMM_ls)




#------------------------------------------------------------#
# Partial Pooling                                            #
#------------------------------------------------------------#
# One can choose one of ("partial", "no", "complete") pooling types.
pooling_type   = "no"

# Choose the beginning of test date.
# test_date      = "2019-07-01"
# test_date      = NULL
test_date      = NULL      # 70% Train vs 30% Test

# Specify prior values for media parameters
# Provide (1) median/mean and (2) standard deviation of each of the media parameters
media_priors   = list(median = rep(0, 7), std = rep(1, 7))


### Create stan_data from `make_stan_data_MMM`.
stan_data_NP      = make_stan_data_MMM(pp_MMM_ls          = pp_MMM_ls,
                                       target_norm_method = 2,
                                       media_norm_method  = 2,
                                       var_target         = var_target,
                                       var_control        = var_control,
                                       df_geo             = NULL,
                                       test_date          = test_date,
                                       pooling_type       = pooling_type,
                                       media_priors       = media_priors)


### Fit the MMM.
t0 <- Sys.time()
fit_NP_22_00_C12_v2 = stan(file    = here::here("stanmodels", "stan_NP_22_00_C12_v2.stan"),
                 data    = stan_data_NP$stan_data$train,
                 chains  = num_chains,
                 warmup  = num_warmups,
                 iter    = num_iterations,
                 cores   = num_cores,
                 refresh = num_refresh,
                 thin    = thin_num,
                 seed    = seed_num,
                 control = list(max_treedepth = num_treedepth,
                                adapt_delta   = acceptance_rate))
t1 <- Sys.time()
Runtime <- t1 - t0
print(Runtime)
# Time difference of 3.910124 hours

# print(fit_NP_22, pars = c("beta_media"))
# print(fit_NP_22, pars = c("beta_base", "beta_media", "beta_control"))
# print(fit_NP_22, pars = c("alpha", "S", "K"))

save(fit_NP_22_00_C12_v2, file = "RData/fit_NP_22_00_C12_v2.RData")

# library(googleCloudStorageR)
# gcs_auth("alticeusa-am-b639e404289b.json")
# gcs_upload(segment_filename, bucket = "alticeusa-am", name = paste('cloud_abc_data/manoj/',segment_filename, sep = ''),predefinedAcl ="default")
