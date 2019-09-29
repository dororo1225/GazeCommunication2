data{
  int N;                                        // number of EC bouts
  int N_pair;                                   // number of infant-mother pair
  int N_obs;                                    // number of observation day
  int N_session;                                // number of EC session
  int<lower=0> Y[N];                            // number of objects on the floor
  real X2[N];                                   // proportion of infant's walking
  real X3[N];                                   // infant-parent distance
  int<lower=1, upper=N_pair> ID_pair[N];        // infant-parent pair id
  int<lower=1, upper=N_obs> ID_obs[N];          // observation id
  int<lower=1, upper=N_session> ID_session[N];  // session id
  
  int N_predict;
  int<lower=1, upper=N_pair> ID_pair_predict[N_predict];
  int<lower=1, upper=N_obs> ID_obs_predict[N_predict];
  real X2_predict[N_predict];
  real X3_predict[N_predict];
}

parameters{
  real beta1;                      // effect of age in months
  real beta2;                      // effect of walking
  real beta3;                      // effect of interpersonal distance
  real y[N_pair, N_obs];
  real<lower=0> sigma_sys[N_pair]; // system noise
  real r_session[N_session];       // random intercepts for EC session
  real<lower=0> sigma_session;     // hyper parameter
}

model{
  for(i in 1:N){
    Y[i] ~ poisson_log(y[ID_pair[i], ID_obs[i]] +  beta2 * X2[i] + beta3 * X3[i] + r_session[ID_session[i]]);
  }
  
  for (j in 1:N_pair){
    for (k in 2:N_obs){
      y[j, k] ~ normal(y[j, k - 1] + beta1, sigma_sys[j]);
    }
  }

  // prior distribution
  for (j in 1:N_pair){
    sigma_sys[j] ~ student_t(4, 0, 1);
  }
  
  // random effects
  r_session ~ normal(0, sigma_session);
}

generated quantities{
  real<lower=0> lambda[N_predict];
  real b1_over;
  real b2_over;
  real b3_over;
  
  for (i in 1:N_predict){
    lambda[i] = exp(y[ID_pair_predict[i], ID_obs_predict[i]] + beta2 * X2_predict[i] + beta3 * X3_predict[i]);
  }
  
  b1_over = step(beta1);
  b2_over = step(beta2);
  b3_over = step(beta3);
}
