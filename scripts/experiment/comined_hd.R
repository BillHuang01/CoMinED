rm(list=ls())
source("../lib.R")
source("../benchmark.R")
library(pdist)

######################################################################
# CoMinED HD PROBLEMS
set.seed(20210307)

n <- c(109)
n.aug <- c(27)
tau <- c(0,exp(c(1:7)),1e6)
auto.scale <- c(TRUE)
s <- 2

# remove problem 2, 9, and 10, feasible region is too large
# 18-24 are ran in 2d setting above
# 1,5,17,25 are ran in hd setting above
idx <- c(1,5,17,25)

output.df <- data.frame()
for (t in 1:length(idx)){
  i <- idx[t]
  p <- problem[[i]]$p
  g <- problem[[i]]$g
  for (j in 1:length(n)){
    for (k in 1:length(n.aug)){
      for (l in 1:length(auto.scale)){
        start.time <- Sys.time()
        output <- comined(n = n[j], p = p, tau = tau, constraint = g, 
                          n.aug = n.aug[k], auto.scale = auto.scale[l],
                          s = s, visualization = F)
        end.time <- Sys.time()
        cand <- output$cand[output$feasible.idx,]
        # compute fill distance
        M <- as.matrix(read.table(sprintf("results/feasible/%s.txt",problem[[i]]$name)))
        caFillDist <- max(apply(M,1,function(x) min(as.matrix(pdist(matrix(x,nrow=1),cand)))))
        M <- NULL # free memory
        # compute design measure
        if (nrow(cand) < n[j]){
          meMaximin <- meanMaximin <- maxMaximin <- NA
          meMaxpro <- meanMaxpro <- minMaxpro <- NA
        } else {
          meMaximin <- min(dist(output$med))
          msMaximin <- rep(NA,10)
          msMaxpro <- rep(NA,10)
          for (q in 1:10){
            msMaximin[q] <- maximin.seq(n[j], cand, return.obj=T)$obj
            msMaxpro[q] <- maxpro.seq(n[j], cand, return.obj=T)$obj
          }
          meanMaximin <- mean(msMaximin)
          maxMaximin <- max(msMaximin)
          meMaxpro <- msMaxpro[1]
          meanMaxpro <- mean(msMaxpro)
          minMaxpro <- min(msMaxpro)
        }
        output.df <- rbind(output.df,
                           data.frame(
                             number = i,
                             name = problem[[i]]$name,
                             p = p,
                             s = s,
                             n = n[j],
                             nAug = n.aug[k],
                             autoScale = as.numeric(auto.scale[l]),
                             time = as.numeric((end.time - start.time), units = "secs"),
                             noEval = nrow(output$cand),
                             noFeaCand = nrow(cand),
                             caMinDist = output$cand.min.dist,
                             caMinDDist = output$cand.min.ddist,
                             caFillDist = caFillDist,
                             meMaximin = meMaximin,
                             meanMaximin = meanMaximin,
                             maxMaximin = maxMaximin,
                             meMaxpro = meMaxpro,
                             meanMaxpro = meanMaxpro,
                             minMaxpro = minMaxpro)
        )
      }
    }
  }
}

write.csv(output.df, "results/comined_hd.csv", row.names = F)