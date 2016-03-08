source('hmm_v2.r')
library(MASS)

M = 3
dim_num = 2

avg_1_ref <- c(4, 4.5, 4.25)
avg_2_ref <- c(150000, 60000, 100000)

std_1_ref <- c(0.25,0.35,0.3)
std_2_ref <- c(50000, 20000, 25000)


mu_ref <- matrix(c(avg_1_ref, avg_2_ref), nrow = dim_num, ncol = M, byrow = TRUE)
std_ref <- matrix(c(std_1_ref, std_2_ref), nrow = dim_num, ncol = M, byrow = TRUE)
cov_ref <- array(0, dim = c(dim_num,dim_num,M))

for (s in 1:M){
  for (i in 1:dim_num){
    for (j in i:dim_num){
      if (i == j){
        cov_ref[i,j,s] = std_ref[i,s]^2
      }
      else{
        cov_ref[i,j,s] = std_ref[i,s] * std_ref[j,s] * runif(1,min = -1, max = 1)
        cov_ref[j,i,s] = cov_ref[i,j,s]
      }
    }
  }
}

A_ref <-matrix(c(0.3, 0.2, 0.5, 0.3, 0.3, 0.4, 0.15, 0.2, 0.65), nrow = M, ncol = M, byrow = TRUE)

N = 140
T = 40

test_data <- array(0, dim = c(dim_num, T, N))

p_increment <- matrix(0, ncol = T, nrow = N)
volume <- matrix(0, ncol = T, nrow = N)

for (n in 1:N){
  state_prob <- c(0.3, 0.3, 0.4)
  for (t in 1:T){
    state <- rmultinom(1, 1, state_prob)
    mu <- mu_ref[,state == 1]
    cov_m <- cov_ref[,,state == 1]

    
    test_data[,t,n] = mvrnorm(1, mu = mu, Sigma = cov_m)
    
    state_prob = t(state) %*% A_ref
    
  }
}


system.time({estimate <- EM(test_data, M)})


