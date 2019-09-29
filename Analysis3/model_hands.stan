data{
  int N;                                        // number of EC bouts
  int N_pair;                                   // number of infant-mother pair
  int N_obs;                                    // number of observation day
  int<lower=0> Y[N];                            // number of objects in infant's hands
  real X2[N];                                   // proportion of infant's walking
  real X3[N];                                   // infant-parent distance
  int<lower=1, upper=N_pair> ID_pair[N];        // infant-mother pair id
  int<lower=1, upper=N_obs> ID_obs[N];          // observation id
}

parameters{
  real beta1;                      // effect of age in months
  real beta2;                      // effect of walking
  real beta3;                      // effect of interpersonal distance
  real y[N_pair, N_obs];
  real<lower=0> sigma_sys[N_pair]; // system noise
}

model{
  for(i in 1:N){
    Y[i] ~ poisson_log(y[ID_pair[i], ID_obs[i]] +  beta2 * X2[i] + beta3 * X3[i]);
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
}

generated quantities{
  real b1_over;
  real b2_over;
  real b3_over;
  
  b1_over = step(beta1);
  b2_over = step(beta2);
  b3_over = step(beta3);
}
