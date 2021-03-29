rm(list=ls())
library(randtoolbox)
source("scripts/benchmark.R")

n <- 1e7
output <- data.frame()
for (i in 1:25){
  samp <- sobol(n, problem[[i]]$p)
  samp.gval <- t(apply(samp, 1, problem[[i]]$g))
  samp.out <- apply(samp.gval, 1, function(x) any(x>0))
  ratio <- 1 - sum(samp.out)/n
  output <- rbind(output,
    data.frame(
      number = i,
      name = problem[[i]]$name,
      dimension = problem[[i]]$p,
      ratio = ratio)
  )
}
 
write.csv(output, "results/feasible_probability.csv", row.names = F)

# Generate Monte Carlo Samples for computing the fill distance
n <- 1e4
N <- 1e7
prob <- read.csv("results/feasible_probability.csv")
newProb <- prob[-c(2,9,10),]
newProb$new_ratio <- NA
for (t in 1:nrow(newProb)){
  idx <- newProb$number[t]
  name <- as.character(newProb$name[t])
  p <- newProb$dimension[t]
  init <- TRUE
  feasible <- NULL
  cnt <- 0
  while (TRUE){
    cnt <- cnt + 1
    samp <- sobol(N, p, init = init)
    samp.gval <- t(apply(samp, 1, problem[[idx]]$g))
    samp.out <- apply(samp.gval, 1, function(x) any(x>0))
    feasible <- rbind(feasible, samp[!samp.out,])
    if (cnt %% 100 == 0){
      write.table(feasible, sprintf("results/feasible/%s.txt", name), row.names = F, col.names = F)
    }
    ratio <- nrow(feasible) / N / cnt
    if (nrow(feasible) > n){
      feasible <- feasible[c(1:n),]
      newProb$new_ratio[t] <- ratio
      break
    }
    init <- FALSE
  }
  write.table(feasible, sprintf("results/feasible/%s.txt", name), row.names = F, col.names = F)
  write.csv(newProb, "results/feasible_probability_new.csv", row.names = F) # store intermediate results
}

