source('hmm.r')

M = 3

mu_ref <- c(4, 4.5, 4.25)
sigma_mu_ref <- c(0.25, 0.35, 0.3)

eta_ref <- c(150000, 60000, 100000)
sigma_eta_ref <- c(50000, 20000, 25000)

A_ref <-matrix(c(0.3, 0.2, 0.5, 0.3, 0.3, 0.4, 0.15, 0.2, 0.65), nrow = M, ncol = M, byrow = TRUE)

N = 140
T = 40

p_increment <- matrix(0, ncol = T, nrow = N)
volume <- matrix(0, ncol = T, nrow = N)

for (n in 1:N){
  state_prob <- c(0.3, 0.3, 0.4)
  for (t in 1:T){
    state <- rmultinom(1, 1, state_prob)
    mu <- mu_ref[state == 1]
    sigma_mu <- sigma_mu_ref[state == 1]
    eta <- eta_ref[state == 1]
    sigma_eta <- sigma_eta_ref[state == 1]
    
    p_increment[n,t] = rnorm(1, mean = mu, sd = sigma_mu)
    volume[n,t] = rnorm(1, mean = eta, sd=sigma_eta)
    
    state_prob = t(state) %*% A_ref
    
  }
}


system.time({estimate <- EM(p_increment, volume)})


