rm(list=ls())
source("../lib.R")
source("../benchmark.R")
library(pdist)

######################################################################
# SCMC 2D PROBLEMS
set.seed(20210228)
# PARAMETERS
tau <- c(0,exp(c(1:7)),1e6)
runs <- 50

mecd <- read.csv("results/mecd_2d.csv")

for (t in 1:nrow(mecd)){
  i <- mecd$number[t]
  name <- mecd$name[t]
  p <- mecd$p[t]
  s <- mecd$s[t]
  n <- mecd$n[t]
  nAug <- mecd$nAug[t]
  autoScale <- as.logical(mecd$autoScale[t])
  # obtain constraint function
  g <- problem[[i]]$g
  # compute big N, number of samples per iteration
  N <- max(n*nAug, mecd$noEval[t]/length(tau))
  N <- ceiling(N)
  # store output files
  output.df <- data.frame()
  for (r in 1:runs){
    start.time <- Sys.time()
    cand <- scmc(n = N, p = p, tau = tau, 
                 constraint = g, auto.scale=autoScale)
    end.time <- Sys.time()
    # compute fill distance
    M <- as.matrix(read.table(sprintf("results/feasible/%s.txt",name)))
    caFillDist <- max(apply(M,1,function(x) min(as.matrix(pdist(matrix(x,nrow=1),cand)))))
    M <- NULL # free memory
    # compute design measure
    if (nrow(cand) < n){
      meMaximin <- meanMaximin <- maxMaximin <- NA
      meMaxpro <- meanMaxpro <- minMaxpro <- NA
    } else {
      msMaximin <- rep(NA,10)
      msMaxpro <- rep(NA,10)
      for (q in 1:10){
        msMaximin[q] <- maximin.seq(n, cand, return.obj=T)$obj
        msMaxpro[q] <- maxpro.seq(n, cand, return.obj=T)$obj
      }
      meMaximin <- msMaximin[1]
      meanMaximin <- mean(msMaximin)
      maxMaximin <- max(msMaximin)
      meMaxpro <- msMaxpro[1]
      meanMaxpro <- mean(msMaxpro)
      minMaxpro <- min(msMaxpro)
    }
    output.df <- rbind(output.df,
                       data.frame(
                         run = r,
                         time = as.numeric((end.time - start.time), units = "secs"),
                         noEval = N*length(tau),
                         noFeaCand = nrow(cand),
                         caFillDist = caFillDist,
                         meMaximin = meMaximin,
                         meanMaximin = meanMaximin,
                         maxMaximin = maxMaximin,
                         meMaxpro = meMaxpro,
                         meanMaxpro = meanMaxpro,
                         minMaxpro = minMaxpro)
    )
  }
  output.file <- sprintf("results/scmc/%s_%d_%d_%d_%d.csv",
                         name,n,nAug,as.numeric(autoScale),s)
  write.csv(output.df, output.file, row.names = F)
}
