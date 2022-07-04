/*
#-------------------------------------------------------------------------------#
#                                                                               #
#                              Media Mix Molding                                #
#                                (No Pooling)                                   #
#                                                                               #
#-------------------------------------------------------------------------------#

Pooling Type
--------------------------
- No pooling: theta vary by geo but are not related across geos
- Parameterization: centered parameterization


# Author: Taesun Kim
# Last Revision: 07/07/2020

*/


functions {
  // the Adstock Function for Carryover Effect
  real Adstock(vector t, vector w) {
    return dot_product(t, w) / sum(w);
    } 
  
  // the Hill Function for Response Effect
  real Hill(real t, real K, real S) {
    return 1 / (1 + (t / K)^(-S));
  }
}


data {
  // Note: some data may not be used in modleing.
  
  // Dimensions
  int<lower=1> num_obs;                            // # of observations
  int<lower=1> num_geo;                            // # of geos
  int<lower=1> num_media;                          // # of media variables
  int<lower=1> num_control;                        // # of control variables
  int<lower=1> num_grp_var;                        // # of geo-level covariates
  int<lower=1> num_lag;                            // # of max lags  

  // Data
  vector<lower=0>[num_obs] y;                      // the response variable
  vector[num_lag] X_media[num_obs, num_media];     // the media variables
  matrix[num_obs, num_control] X_control;          // the control variables
  matrix[num_geo, num_grp_var] Z_geo;              // the geo-level covariates
  int<lower=1, upper=num_geo> id_geo[num_obs];     // the geo-id vector  

  // Priors
  row_vector[num_media] prior_location;
  vector[num_media] prior_spread;
}


transformed data {

}


parameters {
  // Geo-Level Parameters
  vector[num_geo] beta_base;                       // the intercept by geo
  matrix<lower=0>[num_geo, num_media] beta_media_raw;// the media parameters by geo-media
  matrix[num_geo, num_control] beta_control;       // the control parameters by geo-control
 
  // Population-Level Parameters
  vector<lower=0.5,upper=0.9>[num_media] K;        // the saturation parameter
  vector<lower=1.5,upper=3.5>[num_media] S;        // the slope parameter
  vector<lower=0.2,upper=0.75>[num_media] alpha;   // the retention rate of media effect

  // Standard Deviations
  real<lower=0> sigma_y;                           // the standard deviation of y
}


transformed parameters {
  // The Mean of Each Observation
  vector[num_obs] mu_y;

  // Carryover and Shape Effects
  //Specifying weights as a matrix rather than a vector
  matrix<lower=0>[num_media,num_lag] weights;
  //vector<lower=0>[num_lag] weights;
  real X_Adstock;
  matrix[num_obs, num_media] X_Hill;

  // beta_media with lower bounds
  // Use `prior_location` as lower bounds for media parameters
  matrix<lower=0>[num_geo, num_media] beta_media;  // the media parameters by geo-media
  
  for (geo in 1:num_geo) { // How geo-level parameters be genearated.
    beta_media[geo, ]   = prior_location + beta_media_raw[geo, ];
  }
  
  //As the lags is not dependent on each observation, refactoring the code to reduce the number of computations and loops
  for (media in 1:num_media) {
      for (lag in 1:num_lag){
        weights[media,lag] = pow(alpha[media], (lag - 1));
      }
   }

  // X_media is transformed via (1) Adstock and (2) Hill functions.
  for (obs in 1:num_obs) {// How each observation be genearated.
    for (media in 1:num_media) {
      //extracting the weight vector from the matrix by media
      X_Adstock = Adstock(X_media[obs, media], weights[media,]');
      
      X_Hill[obs, media] = Hill(X_Adstock, K[media], S[media]);
    }
    mu_y[obs] = beta_base[id_geo[obs]] + X_Hill[obs,]*beta_media[id_geo[obs],]' + 
                  X_control[obs,]*beta_control[id_geo[obs],]';
  }
}


model {
  // Likelihood
  y ~ normal(mu_y, sigma_y);

  // Note: prior values will change later.
  // Prior for STD of y 
  sigma_y ~ normal(0, 1);
  
  // Priors for carryover & shape parameters
  alpha ~ beta(3, 3);
  S ~ gamma(1, 0.5);
  K ~ beta(2, 2);
  
  // Priors for model parameters
  beta_base ~ normal(0, 1);                 // Intercept  
  to_vector(beta_control) ~ normal(0, 1);   // Control variables
  to_vector(beta_media_raw) ~ normal(0, 1); // Media variables
}


generated quantities {
  //Model estimation and prediction
  vector[num_obs] y_fitted;               // fitted y
  vector[num_obs] y_pred;                 // predicted y
  vector[num_obs] log_lik;                // log-likelihood

  // Compute fit meterics
  for (obs in 1:num_obs) {     
    y_fitted[obs] = mu_y[obs];
    y_pred[obs]   = normal_rng(y_fitted[obs], sigma_y);
    log_lik[obs]  = normal_lpdf(y[obs] | mu_y[obs], sigma_y);
  }
}
