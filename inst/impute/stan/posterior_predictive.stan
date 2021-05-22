data {
  int N;
  int N_samples;
  vector[N] x_test;
  vector[N_samples] alpha;
  vector[N_samples] beta;
}
parameters {
}
model {
}
generated quantities {
  matrix[N_samples, N] y_test;
  for(n in 1:N) {
    for(i in 1:N_samples) {
      y_test[i, n] = bernoulli_rng(inv_logit(alpha[i] + beta[i]*x_test[n]));
    }
  }
}
