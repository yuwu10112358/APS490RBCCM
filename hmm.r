
num_states <- 4

error_bound <- 0.001

state_log_lik <- function(p_increment, mu, sigma_mu, volume, eta, sigma_eta){
  return (-0.5 * (((p_increment - mu)/sigma_mu)^2 + ((volume - eta)/sigma_eta)^2) - log(2 * pi * sigma_mu * sigma_eta))
}

EM <- function (p_increment, volume){
  #format of p_increment: N X T matrix, N = number of days in training data,
  # T = number of steps in a day
  #format of volume: N X T matrix, same as above
  #format of prob matrix for each time: m x T x N matrix, m is the number of possible states
  #Prob. matrix initialized so that each state has equal prob.
  N = dim(p_increment)[1]
  Tnum = dim(p_increment)[2]
  P = array(rep(1 / num_states, num_states * N * Tnum), c(num_states, Tnum, N))
  
  #transition prob. matrix is m x m matrix, initialized such that each state goes to itself with prob ~1.
#   A = matrix(0, num_states, num_states)
#   for (i in 1: num_states){
#     for (j in 1 : num_states){
#         if (i == j) A[i,j] = 0.9
#         else A[i,j] = (1 - 0.9)/(num_states - 1)
#     }
#   }
  A = matrix(1/num_states, nrow = num_states, ncol = num_states)
  
  #all mu's initialized to avg of p_increment, all sigma)mu's initialized to std of p_increment
  qt0 <- seq(0, 1, length.out = num_states + 1)
  qt <- (qt0[1:num_states] + qt0[2:(num_states+1)]) / 2
  mu = quantile(as.vector(p_increment), qt)
  sigma_mu = rep(sqrt(var(as.vector(p_increment))), num_states)
  #same for eta and sigma_eta
  eta = quantile(as.vector(volume), qt)
  sigma_eta = rep(sqrt(var(as.vector(volume))), num_states)
  
  max_iter_num = 500
  for (i in 1:max_iter_num){
    old_mu <- mu
    old_eta <- eta
    old_sigma_mu <- sigma_mu
    old_sigma_eta <- sigma_eta
    old_A <- A
    
    log_alpha = calc_forward(N, Tnum, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta)
    log_beta = calc_backward(N, Tnum, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta)
    P <- calc_P(N, Tnum, log_alpha, log_beta)
    mu = update_mu(P, p_increment)
    sigma_mu = update_sigma_mu(P, p_increment, mu, N, Tnum)
    eta = update_eta(P, volume)
    sigma_eta = update_sigma_eta(P, volume, eta, N, Tnum)
    if (i == 5){
      l1 <- 1
    }
    A = update_A(N, Tnum, P, log_alpha, log_beta, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta)
    
    diff_mu <- sum(abs(old_mu - mu))
    diff_eta <- sum(abs(old_eta - eta))
    diff_sigma_mu <- sum(abs(old_sigma_mu - sigma_mu))
    diff_sigma_eta <- sum(abs(old_sigma_eta - sigma_eta))
    diff_A <- sum(abs(old_A - A))
    if (diff_mu < error_bound && diff_eta < error_bound && diff_sigma_mu < error_bound &&
        diff_sigma_eta < error_bound && diff_A < error_bound){
      break
    }
  }
  data_A <- data.frame(A)
  data_mu <- data.frame(mu)
  data_sigma_mu <- data.frame(sigma_mu)
  data_eta <- data.frame(eta)
  data_sigma_eta <- data.frame(sigma_eta)
  
  params <- list(data_A, data_mu, data_sigma_mu, data_eta, data_sigma_eta)
  names(params) <- c('A', 'mu', 'sigma_mu', 'eta', 'sigma_eta')
  
  return(params)
}

calc_forward <- function(N, Tnum, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta){
  log_alpha <- array(0, c(num_states, Tnum, N))
  #get the initial prob
  log_alpha[, 1, ] <- state_log_lik(matrix(p_increment[,1], nrow = num_states, ncol = dim(p_increment)[1], byrow = TRUE),
                               mu, sigma_mu,
                               matrix(volume[,1], nrow = num_states, ncol = dim(volume)[1], byrow = TRUE),
                               eta, sigma_eta)
  
  for (t in 2: dim(p_increment)[2]){
    
    log_diff <- exp(log_alpha[, t-1, ] - matrix(apply(log_alpha[, t-1, ], 2, max), nrow = num_states, ncol = dim(volume)[1], byrow = TRUE))
    log_alpha[, t, ] <- t(log(t(log_diff) %*% A)) + matrix(apply(log_alpha[, t-1, ], 2, max), nrow = num_states, ncol = dim(volume)[1], byrow = TRUE) + 
      state_log_lik(matrix(p_increment[,t], nrow = num_states, ncol = dim(p_increment)[1], byrow = TRUE),
                    mu, sigma_mu,
                    matrix(volume[,t], nrow = num_states, ncol = dim(volume)[1], byrow = TRUE),
                    eta, sigma_eta)
  }
  return (log_alpha)
}

calc_backward <- function(N, Tnum, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta){
  log_beta <- array(0, c(num_states, Tnum, N))
  log_beta[, Tnum, ] <- 0
  for (t in (Tnum - 1) : 1){
    log_diff <- exp(log_beta[, t+1, ] - matrix(apply(log_beta[, t+1, ], 2, max), nrow = num_states, ncol = dim(volume)[1], byrow = TRUE))
    tt <- state_log_lik(matrix(p_increment[,t+1], nrow = num_states, ncol = dim(p_increment)[1], byrow = TRUE),
                        mu, sigma_mu,
                        matrix(volume[,t+1], nrow = num_states, ncol = dim(volume)[1], byrow = TRUE),
                        eta, sigma_eta)
    log_beta[, t, ] <- log(A %*% exp(log_diff * state_log_lik(matrix(p_increment[,t+1], nrow = num_states, ncol = dim(p_increment)[1], byrow = TRUE),
                                                mu, sigma_mu,
                                                matrix(volume[,t+1], nrow = num_states, ncol = dim(volume)[1], byrow = TRUE),
                                                eta, sigma_eta))) + matrix(apply(log_beta[, t+1, ], 2, max), nrow = num_states, ncol = dim(volume)[1], byrow = TRUE)
  }
  return (log_beta[,1:Tnum,])
}

calc_P <- function(N, Tnum, log_alpha, log_beta){
  temp <- log_alpha + log_beta
  temp2 <- array(0, c(num_states, Tnum, N))
  P <- array(0, c(num_states, Tnum, N))
  for (t in 1: Tnum){
    temp2[,t,] = exp(temp[,t,] - matrix(apply(temp[,t,], 2, max), nrow = num_states, ncol = N, byrow = TRUE))
    P[,t,] <- temp2[,t,] / matrix(colSums(temp2[,t,]), nrow = num_states,ncol = N, byrow = TRUE)
  }
  return (P)
}

update_A <- function(N, Tnum, P, log_alpha, log_beta, A, p_increment, mu, sigma_mu, volume, eta, sigma_eta){
  conditional_tp <- array(0, c(N, (Tnum - 1), num_states, num_states))
  for (n in 1:N){
    if (n == 71){
      l2 <- 1
    }
    for(t in 1:(Tnum - 1)){
      temp_a <- matrix(rep(log_alpha[, t, n], num_states), ncol = num_states, nrow = num_states)
      temp_b <- log_beta[,t+1,n] + state_log_lik(p_increment[n, t+1], mu, sigma_mu, volume[n, t+1], eta, sigma_eta)
      temp_bb <- matrix(t(temp_b), nrow = num_states, ncol = num_states, byrow = TRUE)
      temp_c <- temp_a + temp_bb + log(A)
      temp_c = exp(temp_c - temp_c[which.max(temp_c)])
      conditional_tp[n,t,,] <- temp_c / sum(temp_c)
    }
  }
  new_A <- matrix(0, nrow = num_states, ncol = num_states)
  for (i in 1:num_states){
    for (j in 1:num_states){
      new_A[i,j] <- sum(conditional_tp[,,i,j])/sum(conditional_tp[,,i,])
    }
  }
  return (new_A)
}

update_mu <- function(P, p_increment){
  new_mu <- rep(0, num_states)
  for (i in 1: num_states){
    new_mu[i] <- sum(P[i,,] * t(p_increment)) / sum(P[i,,])
  }
  return(new_mu)
}

update_sigma_mu <- function(P, p_increment, mu, N, Tnum){
  new_sigma_mu <- rep(0, num_states)
  for(i in 1: num_states){
    new_sigma_mu[i] <- sqrt(sum(t(p_increment - mu[i])^2 * P[i,,]) * N * Tnum / ((Tnum * N - 1) * sum(P[i,,])))
  }
  return(new_sigma_mu)
}

update_eta <- function(P, volume){
  new_eta <- rep(0, num_states)
  for (i in 1: num_states){
    new_eta[i] <- sum(P[i,,] * t(volume)) / sum(P[i,,])
  }
  return(new_eta)
}

update_sigma_eta <- function(P, volume, eta, N, Tnum){
  new_sigma_eta <- rep(0, num_states)
  for(i in 1: num_states){
    new_sigma_eta[i] <- sqrt(sum(t(volume - eta[i])^2 * P[i,,]) * N * Tnum / ((N * Tnum - 1) * sum(P[i,,])))
  }
  return(new_sigma_eta)
}
