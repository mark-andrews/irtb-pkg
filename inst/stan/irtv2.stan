data {
  int<lower=1> J; // number of persons (students/subjects/paricipants/respondents)
  int<lower=1> K; // number of items
  int<lower=1> N; // total number of observations

  int<lower=1, upper=J> person[N]; // array of integers giving person IDs
  int<lower=1, upper=K> item[N];  // array of intgers giving items IDs
  int<lower=0, upper=1> y[N]; // accuracy of each observation (0: incorrect, 1: correct)

  real<lower=0, upper=1> chi;   // probability of correct guess when alpha = -oo
}

parameters {
  vector<lower=0>[K] gamma; // discriminability of item
  vector[J] alpha;          // person abilities
  vector[K] beta;           // item difficulty
  real<lower=0> sigma_beta;  // sd of population model of betas
  real<lower=0> sigma_gamma; // sd of population model of gammas
}

transformed parameters {
  vector[N] p; // probability
  p = chi + (1-chi) * inv_logit(gamma[item] .* (alpha[person] - beta[item]));
}

model {

  alpha ~ std_normal();
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);

  y ~ bernoulli(p);

}



