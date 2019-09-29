data {
  int<lower=1> N_total;
  int<lower=1> N_pair;
  int<lower=1> N_obs;
  int<lower=1, upper=N_pair> ID_pair[N_total];// id repersenting infant-parent pair
  int<lower=1, upper=N_obs> ID_obs[N_total];  // id repersenting observation day
  int<lower=1> K;
  real X2[N_total];
  int<lower=0, upper=1> Miss[N_total];        // missing data
  int<lower=1,upper=K> d[N_total];            // distance category
}

parameters {
  real beta1;            // age in months
  real beta2;            // percent of walking
  ordered[K - 1] c;      // cut off value
  real y[N_pair, N_obs];
  real<lower=0> sigma_sys[N_pair]; // system noise
}

model {
  // Observational model
  for (i in 1:N_total) {
    if (Miss[i] != 1) {
      d[i] ~ ordered_logistic(y[ID_pair[i], ID_obs[i]] + beta2 * X2[i], c);
    }
  }
        
  // System Model
  for (j in 1:N_pair){
    for(k in 2:N_obs){
      y[j, k] ~ normal(y[j, k - 1] + beta1, sigma_sys[j]); 
    }
  }

 // prior distribution
  for (j in 1:N_pair){
    sigma_sys[j] ~ student_t(4, 0, 1);
  }
  
 c ~ normal(0, 10);
 beta1 ~ normal(0, 100);
 beta2 ~ normal(0, 100);
}

generated quantities{
  real b1_over;
  real b2_over;
  b1_over = step(beta1);
  b2_over = step(beta2);
}
