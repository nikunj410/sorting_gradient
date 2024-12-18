data {
  int N_unflooded;
  int N_unflooded_macro;
}

parameters {
  real phi_unflooded;
}

transformed parameters{
  real <lower = 0, upper = 1> p_unflooded_macro = 0.5*(1+erf(phi_unflooded/sqrt(2)));
}

model {
  N_unflooded_macro ~ binomial(N_unflooded, p_unflooded_macro);
  phi_unflooded ~ normal(0, 5);
}

generated quantities{
  real std_sorting_differential = inv(p_unflooded_macro*sqrt(2*pi()))*exp(-square(phi_unflooded)/2);
}

