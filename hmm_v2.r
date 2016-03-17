library(MASS)
#num_states <- 4

error_bound <- 0.001

state_log_lik <- function(dat, mu, cov_mat, cov_mat_inv,cov_mat_det, N, num_var){
  #dat is a matrix of dimension num_var X N
  #mu is a vector with num_var elements
  #cov_mat is the covanriance matrix with dim num_var X num_var, its determinant is cov_mat_det,
  #its inverse is cov_mat_inv
  # returns a vector 1 x N of log-likelihoods
  diff <- dat - matrix(rep(mu, N), nrow = num_var, ncol = N)
  return (diag(-0.5 * t(diff) %*% cov_mat_inv %*% diff) - 0.5 * num_var * log(2 * pi) - 0.5 *log(abs(cov_mat_det)))
}

EM <- function (test_data, num_states){
  #format of test_data: dim X T X N matrix, N = number of days in training data,
  # T = number of steps in a day, dim = number of parameters to consider for each state
  #format of prob matrix for each time: m x T x N matrix, m is the number of possible states
  #Prob. matrix initialized so that each state has equal prob.
  N = dim(test_data)[3]
  Tnum = dim(test_data)[2]
  num_var = dim(test_data)[1]
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
  
  #mu is initialized to the mid points of test_data, all var initialized to std of test_data
  #covariance initialized to 0
  qt0 <- seq(0, 1, length.out = num_states + 1)
  qt <- (qt0[1:num_states] + qt0[2:(num_states+1)]) / 2
  
  mu <- matrix(0, nrow = num_var, ncol = num_states)
  cov_mat <- array(0, c(num_var, num_var, num_states))
  for (i in 1:num_var){
    mu[i,] = quantile(as.vector(test_data[i,,]), qt)
    cov_mat[i,i,] = rep(var(as.vector(test_data[i,,])), num_states)
  }
  
  max_iter_num = 500
  for (i in 1:max_iter_num){
    old_mu <- mu
    old_cov_mat <- cov_mat
    old_A <- A
    
    log_alpha = calc_forward(num_states, N, Tnum, num_var, A, mu, cov_mat, test_data)
    log_beta = calc_backward(num_states, N, Tnum, num_var, A, mu, cov_mat, test_data)
    P <- calc_P(num_states, N, Tnum, log_alpha, log_beta)
    mu = update_mu(num_states, num_var, P, test_data)
    cov_mat = update_cov_mat(num_states, num_var, N, Tnum, P, test_data, mu)
    A = update_A(num_states, N, Tnum, num_var, P, log_alpha, log_beta, A, mu, cov_mat, test_data)
    
    diff_mu <- sum(abs(old_mu - mu)) / sum(abs(old_mu))
    diff_cov_mat <- sum(abs(old_cov_mat - cov_mat)) / sum(abs(old_cov_mat))
    diff_A <- sum(abs(old_A - A)) / sum(abs(old_A))
    if (diff_mu < error_bound && diff_cov_mat < error_bound && diff_A < error_bound){
      break
    }
  }
  data_A <- data.frame(A)
  data_mu <- data.frame(mu)
  data_cov_mat <- data.frame(cov_mat)
  
  params <- list(data_A, data_mu, data_cov_mat)
  names(params) <- c('A', 'mu', 'covariance matrix')
  
  return(params)
}

calc_forward <- function(num_states, N, Tnum, num_var, A, mu, cov_mat, test_data){

  log_alpha <- array(0, c(num_states, Tnum, N))
  #get the initial prob
  for (s in 1:num_states){
    cov_mat_inv <- solve(cov_mat[,,s])
    cov_mat_det <- det(cov_mat[,,s])
    log_alpha[s, 1, ] = state_log_lik(test_data[,1,], mu[,s], cov_mat[,,s], cov_mat_inv,cov_mat_det, N, num_var)
  }
  
  if (Tnum > 1){
    for (t in 2:Tnum){
      temp1 <- matrix(0, nrow = num_states, ncol = N)
      for (s in 1:num_states){
        cov_mat_inv <- solve(cov_mat[,,s])
        cov_mat_det <- det(cov_mat[,,s])
        temp1[s,] <- state_log_lik(test_data[,t,], mu[,s], cov_mat[,,s], cov_mat_inv,cov_mat_det, N, num_var)
      }
      log_diff <- exp(log_alpha[, t-1, ] - matrix(apply(matrix(log_alpha[, t-1, ], nrow = num_states, ncol = N), 2, max), nrow = num_states, ncol = N, byrow = TRUE))
      log_alpha[, t, ] <- t(log(t(log_diff) %*% A)) + 
        matrix(apply(matrix(log_alpha[, t-1, ], nrow = num_states, ncol = N), 2, max), nrow = num_states, ncol = N, byrow = TRUE) + 
        temp1
        
    }
  }
  
  return (log_alpha)
}

calc_backward <- function(num_states, N, Tnum, num_var, A, mu, cov_mat, test_data){
  log_beta <- array(0, c(num_states, Tnum, N))
  log_beta[, Tnum, ] <- 0
  for (t in (Tnum - 1) : 1){
    log_diff <- exp(log_beta[, t+1, ] - matrix(apply(log_beta[, t+1, ], 2, max), nrow = num_states, ncol = N, byrow = TRUE))
    temp1 <- matrix(0, nrow = num_states, ncol = N)
    for (s in 1:num_states){
      cov_mat_inv <- solve(cov_mat[,,s])
      cov_mat_det <- det(cov_mat[,,s])
      temp1[s,] <- state_log_lik(test_data[,t+1,], mu[,s], cov_mat[,,s], cov_mat_inv,cov_mat_det, N, num_var)
    }

    log_beta[, t, ] <- log(A %*% exp(log_diff * temp1)) + 
      matrix(apply(log_beta[, t+1, ], 2, max), nrow = num_states, ncol = N, byrow = TRUE)
  }
  return (log_beta[,1:Tnum,])
}

calc_P <- function(num_states, N, Tnum, log_alpha, log_beta){
  temp <- log_alpha + log_beta
  temp2 <- array(0, c(num_states, Tnum, N))
  P <- array(0, c(num_states, Tnum, N))
  for (t in 1: Tnum){
    temp2[,t,] = exp(temp[,t,] - matrix(apply(temp[,t,], 2, max), nrow = num_states, ncol = N, byrow = TRUE))
    P[,t,] <- temp2[,t,] / matrix(colSums(temp2[,t,]), nrow = num_states,ncol = N, byrow = TRUE)
  }
  return (P)
}

update_A <- function(num_states, N, Tnum, num_var, P, log_alpha, log_beta, A, mu, cov_mat, test_data){
  conditional_tp <- array(0, c(N, (Tnum - 1), num_states, num_states))
  for (n in 1:N){
    for(t in 1:(Tnum - 1)){
      temp_a <- matrix(rep(log_alpha[, t, n], num_states), ncol = num_states, nrow = num_states)
      temp_log_lik <- matrix(0, nrow = num_states, ncol = 1)
      for (s in 1:num_states){
        cov_mat_inv <- solve(cov_mat[,,s])
        cov_mat_det <- det(cov_mat[,,s])
        temp_log_lik[s,] <- state_log_lik(test_data[,t+1,n], mu[,s], cov_mat[,,s], cov_mat_inv,cov_mat_det, 1, num_var)
      }
      temp_b <- log_beta[,t+1,n] + temp_log_lik
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

update_mu <- function(num_states, num_var, P, test_data){
  new_mu <- matrix(0, nrow = num_var, ncol = num_states)
  for (v in 1: num_var){
    for (i in 1: num_states){
      new_mu[v,i] <- sum(P[i,,] * test_data[v,,]) / sum(P[i,,])
    }
  }
  return(new_mu)
}

update_cov_mat <- function(num_states, num_var, N, Tnum, P, test_data, mu){
  new_cov_mat <- array(0, c(num_var, num_var, num_states))
  for(s in 1: num_states){
    for (i in 1:num_var){
      for (j in 1:num_var){
        new_cov_mat[i,j,s] <- sum((test_data[i,,] - mu[i,s]) * (test_data[j,,] - mu[j,s]) * P[s,,]) * N * Tnum / ((Tnum * N - 1) * sum(P[s,,]))
      }
    }
  }
  return(new_cov_mat)
}

