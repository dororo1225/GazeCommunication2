data{
  int N;                                       // number of EC bouts
  int N_pair;                                  // number of infant-mother pairs 
  int N_obs;                                   // number of observations 
  int N_session;                               // number of session
  real Y[N];                                   // EC Bout Distance
  real X2[N];                                  // percent walker
  int<lower=1, upper=N_pair> ID_pair[N];       // infant-mother pair id 
  int<lower=1, upper=N_obs> ID_obs[N];         // observation id
  int<lower=1, upper=N_session> ID_session[N];
  real X2_pre[N_pair, N_obs];                  // walker or not for predictions
}

parameters{
  real beta1;                                  // effect of age
  real beta2;                                  // effect of Walking
  real y[N_pair, N_obs];
  real r_session[N_session];
  real<lower=0> sigma_session;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_sys[N_pair];
}

model{
  for (i in 1:N){
    Y[i] ~ lognormal(y[ID_pair[i], ID_obs[i]] + beta2 * X2[i] + r_session[ID_session[i]], sigma_obs); 
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
  
  for (k in 1:N_session){
    r_session[k] ~ normal(0, sigma_session);
  }
}

generated quantities{
  real mu[N_pair, N_obs];   // 
  real b1_over;
  real b2_over;

  for (j in 1:N_pair){
    for (k in 1:N_obs){
      mu[j, k] = y[j, k] + beta2 * X2_pre[j, k];
    }
  }
  
  b1_over = step(beta1);
  b2_over = step(beta2);
}
