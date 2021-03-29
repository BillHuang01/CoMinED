#############################################################################
# motivation
rm(list = ls())
constraint <- function(x){
  c1 <- (x[1] - sqrt(50 * (x[2] - 0.52)^2 + 2) + 1)
  c2 <- (sqrt(120 * (x[2] - 0.48)^2 + 1) - 0.75 - x[1])
  c3 <- (0.65^2 - x[1]^2 - x[2]^2)
  return (c(c1,c2,c3))
}

# constraint contour
plot(NULL, type = 'n', xlim = c(0,1), ylim = c(0,1), ylab = "", xlab = "")
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)
polygon(x = c(c1.x1[383:536], c2.x1[536:529], c3.x1[529:412], c2.x1[412:383]),
        y = c(x2[383:536], x2[536:529], x2[529:412], x2[412:383]),
        col = "red")

# latin hypercube samples
library(lhs)
set.seed(20210320)
lhs <- randomLHS(4500, 2)
lhs.gval <- t(apply(lhs, 1, constraint))
lhs.out.idx <- apply(lhs.gval, 1, function(x) return(any(x>0)))
lhs.out <- lhs[lhs.out.idx,]
lhs.in <- lhs[!lhs.out.idx,]
nrow(lhs.in) # 22
# visulization
plot(lhs, col = "green", pch = 18, cex = 0.75, 
     xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "")
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)

plot(lhs.in, col = "red", pch = 16, cex = 0.75, 
     xlim = c(0.3,0.8), ylim = c(0.35,0.55), xlab = "", ylab = "")
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)

# doubling
lhs <- randomLHS(9000, 2)
lhs.gval <- t(apply(lhs, 1, constraint))
lhs.out.idx <- apply(lhs.gval, 1, function(x) return(any(x>0)))
lhs.out <- lhs[lhs.out.idx,]
lhs.in <- lhs[!lhs.out.idx,]
nrow(lhs.in) # 31


# scmc
setwd("~/gatech/research/sampling/CoMinED/scripts/")
source("lib.R")
set.seed(20210320)
tau <- c(0,exp(c(1:7)),1e6)
samp <- scmc(500, 2, tau, constraint, auto.scale = F, return.all = T)
# visualization
samp.all <- samp$samp.all
nrow(samp.all) # 4500
plot(samp.all[1:500,], col = "red", pch = 16, cex = 1, 
     xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "")
points(samp.all[501:nrow(samp.all),], col = "green", pch = 18, cex = 0.75)
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)
# feasible sample only
candF <- samp$samp.feasible
nrow(candF) # 2246
plot(candF, col = "red", pch = 16, cex = 0.75, 
     xlim = c(0.3,0.8), ylim = c(0.35,0.55), xlab = "", ylab = "")
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)

#############################################################################
# Adaptive Lattice Grid Refinement
# One Step ALGR Versus Maximin LHDs Augmentation
library(mined)
n <- 53
p <- 2
n.aug <- 11
ini <- Lattice(n,p)
# ALGR
set.seed(20210320)
min.dist <- min(dist(ini))/2
no.decimal <- attr(regexpr("(?<=\\.)0+", min.dist, perl = TRUE), "match.length") + 2
ini.dist <- as.matrix(dist(ini))
aug <- NULL
for (i in 1:n){
  nn.idx <- order(ini.dist[i,])[2:(n.aug+1)]
  aug <- rbind(aug, 0.5*(ini[nn.idx,]+rep(1,n.aug)%*%t(ini[i,])))
  aug <- rbind(aug, 0.5*(3*ini[nn.idx,]-rep(1,n.aug)%*%t(ini[i,])))
}
# remove repeated samples
aug.rep <- round(aug, digits = no.decimal)
aug.dp <- duplicated(aug.rep)
aug <- aug[!aug.dp,]
# remove samples outside of boundary
aug.out <- apply(aug, 1, function(x) (any(x<0)|any(x>1)))
aug <- aug[!aug.out,]
# remove samples in candidate set already
no.aug <- nrow(aug)
all.rep <- rbind(aug, ini)
all.rep <- round(all.rep, digits = no.decimal)
all.rep.dp <- duplicated(all.rep, fromLast = TRUE)
aug <- aug[!(all.rep.dp[1:no.aug]),]
# visualization
plot(ini, col = "red", pch = 16, cex = 1, xlim = c(0,1), ylim = c(0,1),
     xlab = "", ylab = "")
points(aug, col = "green", pch = 18, cex = 1)
nrow(aug) # 163
min(dist(rbind(ini,aug))) # 0.06868028

# maximin LHDs
set.seed(20210320)
library(lhs)
D.aug <- maximinLHS(n.aug, p)
D.aug <- 2 * (D.aug - 0.5)
ini.dist <- as.matrix(dist(ini))
diag(ini.dist) <- NA
rads <- apply(ini.dist, 1, min, na.rm=T) / sqrt(p)
aug <- NULL
for (i in 1:n){
  aug <- rbind(aug, (rep(1,n.aug)%*%t(ini[i,])+rads[i]*D.aug[,sample(1:p,p)]))
}
# visualization
plot(ini, col = "red", pch = 16, cex = 1, xlim = c(0,1), ylim = c(0,1),
     xlab = "", ylab = "")
points(aug, col = "green", pch = 18, cex = 1)
nrow(aug) # 583
min(dist(rbind(ini,aug))) # 0.00734764

# FULL Approach
rm(list = ls())
constraint <- function(x){
  c1 <- (x[1] - sqrt(50 * (x[2] - 0.52)^2 + 2) + 1)
  c2 <- (sqrt(120 * (x[2] - 0.48)^2 + 1) - 0.75 - x[1])
  c3 <- (0.65^2 - x[1]^2 - x[2]^2)
  return (c(c1,c2,c3))
}

x1 <- x2 <- matrix(NA, nrow = 3, ncol = 1001)
x2.seq <- seq(0,1,length.out = 1001)
x2[1,] <- x2.seq
x1[1,] <- sqrt(50 * (x2.seq - 0.52)^2 + 2) - 1
x2[2,] <- x2.seq
x1[2,] <- sqrt(120 * (x2.seq - 0.48)^2 + 1) - 0.75
x2[3,] <- x2.seq
x1[3,] <- sqrt(0.65^2 - x2.seq^2)
contour <- list(x1 = x1, x2 = x2)

setwd("~/gatech/research/sampling/CoMinED/scripts/")
source("lib.R")
set.seed(20210320)
tau <- c(0,exp(c(1:7)),1e6)
output <- comined(n = 53, p = 2, tau = tau, constraint = constraint,
                  n.aug = 5, auto.scale = F, s = 2, visualization = T,
                  visualization.params = list(unit.scale=TRUE,contour=contour))
min(dist(output$cand)) # 0.0004126795
nrow(output$cand) # 2155
# visualization
plot(output$cand[1:263,], col = "red", pch = 16, cex = 1, 
     xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "")
points(output$cand[264:nrow(output$cand),], col = "green", pch = 18, cex = 0.75)
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)
# feasible sample only
candF <- output$cand[output$feasible.idx,]
nrow(candF) # 915
plot(candF, col = "red", pch = 16, cex = 0.75, 
     xlim = c(0.3,0.8), ylim = c(0.35,0.55), xlab = "", ylab = "")
x2 <- seq(0,1,length.out = 1001)
c1.x1 <- sqrt(50 * (x2 - 0.52)^2 + 2) - 1
lines(c1.x1, x2, lty = 2)
c2.x1 <- sqrt(120 * (x2 - 0.48)^2 + 1) - 0.75
lines(c2.x1, x2, lty = 2)
c3.x1 <- sqrt(0.65^2 - x2[x2<=0.65]^2)
lines(c3.x1, x2[x2<=0.65], lty = 2)