library(mined)
library(randtoolbox)

# log to handle numerical underflow
logaddexp <- function(logv){
  logv.max <- max(logv)
  logv.sum <- log(sum(exp(logv - logv.max))) + logv.max
  return(logv.sum)
}

# sequentially constrained monte carlo
scmc <- function(n, p, tau, constraint, auto.scale = F, return.all = F){
  ################################################################
  # sequentially constrained monte carlo
  # n: number of initial samples
  # p: number of dimensions
  # tau: rigidity parameter
  # constraint: contraint parameter
  # auto.scale: is scaling applied to the constraint values
  # return.all: is returning all samples
  ################################################################
  
  samp <- sobol(n, p)
  samp.logq <- rep(0,n)
  samp.gval <- matrix(t(apply(samp, 1, constraint)), nrow = n)
  # store all samples
  samp.all <- samp
  samp.all.gval <- samp.gval
  # scale parameter if auto-scale is on
  scale <- rep(1, ncol(samp.all.gval))
  if (auto.scale) scale <- apply(samp.all.gval,2,mad,center=0)
  # smc iterations
  for (k in 2:length(tau)){
    # weighting
    samp.logf <- apply(samp.gval, 1, function(x) sum(pnorm(-tau[k]*x/scale,log.p=T)))
    samp.logwts <- samp.logf - samp.logq
    samp.wts <- exp(samp.logwts - logaddexp(samp.logwts))
    # resample
    idx <- sample(1:n, n, replace = T, prob = samp.wts)
    samp <- samp[idx,]
    samp.gval <- matrix(samp.gval[idx,], nrow = n)
    samp.logf <- samp.logf[idx]
    # estimate sigma
    samp.unique <- unique(samp)
    if (nrow(samp.unique)==1){
      sigma <- sigma / 10
    } else {
      samp.dist <- as.matrix(dist(samp.unique))
      diag(samp.dist) <- NA
      sigma <- as.numeric(quantile(apply(samp.dist,1,min,na.rm=T),0.75))/sqrt(p)
    }
    # sampling
    samp.mh <- samp + matrix(rnorm(n*p, 0, sigma), nrow = n)
    samp.mh.gval <- matrix(t(apply(samp.mh, 1, constraint)), nrow = n)
    # add new samples to all samples
    samp.all <- rbind(samp.all, samp.mh)
    samp.all.gval <- rbind(samp.all.gval, samp.mh.gval)
    if (auto.scale) scale <- apply(samp.all.gval,2,mad,center=0)
    # metropolis hasting 
    samp.mh.logf <- apply(samp.mh.gval, 1, function(x) sum(pnorm(-tau[k]*x/scale,log.p=T)))
    samp.mh.out <- apply(samp.mh, 1, function(x) (any(x<0)|any(x>1)))
    samp.mh.logf[samp.mh.out] <- log(0)
    samp.mh.ap <- exp(samp.mh.logf - samp.logf)
    samp.mh.u <- runif(n)
    samp.mh.ac <- (samp.mh.u < samp.mh.ap)
    samp[samp.mh.ac,] <- samp.mh[samp.mh.ac,]
    samp.gval[samp.mh.ac,] <- samp.mh.gval[samp.mh.ac,]
    samp.logf[samp.mh.ac] <- samp.mh.logf[samp.mh.ac]
    samp.logq <- samp.logf
  }
  samp.ifb.idx <- apply(samp.all.gval, 1, function(x) any(x>0))
  samp <- samp.all[!samp.ifb.idx,]
  samp.out <- apply(samp, 1, function(x) (any(x<0)|any(x>1)))
  samp <- samp[!samp.out,]
  samp <- unique(samp)
  if (return.all){
    return(list(samp.all = samp.all, samp.feasible=samp))
  } else {
    return (samp)
  }
}

# log distance function
logdis <- function(x, s=2){
  if (s > 0) length(x)/s*log(sum(abs(x)^s)) else sum(log(abs(x)))
}

# mined by one-point-at-a-time greedy algorithm
mined.seq <- function(n, cand, cand.lf, s = 2, return.obj = F){
  N <- nrow(cand)
  if (n > N) stop(message("Not enough candidate points!"))
  p <- ncol(cand)
  idx <- which.max(cand.lf)
  xi <- c(idx)
  val <- NULL
  for (i in 2:n){
    cand.dist <- cand - rep(1,N) %*% t(cand[idx,])
    val <- cbind(val, 0.5*cand.lf[idx] + 0.5*cand.lf + apply(cand.dist, 1, logdis, s=s))
    idx <- which.max(apply(val, 1, min))
    xi <- c(xi, idx)
  }
  if (return.obj){
    val <- val[xi,]
    val <- cbind(val, c(val[n,],-Inf))
    obj <- min(val[upper.tri(val)])
    return (list(idx = xi, obj = obj))
  }
  return (xi)
}

# constrained minimum energy design
comined <- function(n, p, tau, constraint, n.aug, auto.scale = F, 
                    s = 2, visualization = F, visualization.params = list()){
  # initialize sample
  samp <- Lattice(n*n.aug, p)
  min.dist <- min(dist(samp))
  min.ddist <- Inf # dimensional minimum
  for (i in 1:p){
    ddist <- min(dist(samp[,p]))
    if (ddist < min.ddist) min.ddist <- ddist
  }
  samp.gval <- matrix(t(apply(samp,1,constraint)), nrow=nrow(samp))
  # scale parameter if auto-scale is on
  scale <- rep(1, ncol(samp.gval))
  if (auto.scale) scale <- apply(samp.gval,2,mad,center=0)
  # find first set of minimum energy design points
  samp.lf <- apply(samp.gval,1,function(x) sum(pnorm(-tau[2]*x/scale,log.p=T)))
  med.op <- mined.seq(n, samp, samp.lf, s = s, return.obj = TRUE)
  samp.med <- samp[med.op$idx,]
  if (visualization){
  	vparams <- list(unit.scale = FALSE, contour = NULL)
  	vparams[(nm<-names(visualization.params))] <- visualization.params
  	xlim <- ylim <- NULL
  	if (vparams$unit.scale) xlim <- ylim <- c(0,1)
    plot(samp, col = "green", pch = 18, cex = 0.75, 
    	xlim = xlim, ylim = ylim, xlab = "", ylab = "")
    points(samp.med, col = "red", pch = 16, cex = 1)
    if (!is.null(vparams$contour)){
    	contours <- vparams$contour
    	for (l in 1:nrow(contour$x1)){
    	  lines(contour$x1[l,], contour$x2[l,], lty = 2)
    	}
    }
  }
  
  for (k in 3:length(tau)){
    min.dist <- min.dist / 2
    min.ddist <- min.ddist / 2
    no.decimal <- attr(regexpr("(?<=\\.)0+", format(min.ddist, scientific = F), 
                               perl = TRUE), "match.length") + 1
    # sample augmentation
    samp.med.dist <- as.matrix(dist(samp.med))
    samp.aug <- NULL
    for (i in 1:n){
      nn.idx <- order(samp.med.dist[i,])[2:(n.aug+1)]
      samp.aug <- rbind(samp.aug, 0.5*(samp.med[nn.idx,]+rep(1,n.aug)%*%t(samp.med[i,])))
      samp.aug <- rbind(samp.aug, 0.5*(3*samp.med[nn.idx,]-rep(1,n.aug)%*%t(samp.med[i,])))
    }
    # remove repeated samples
    samp.aug.rep <- round(samp.aug, digits = no.decimal)
    samp.aug.dp <- duplicated(samp.aug.rep)
    samp.aug <- samp.aug[!samp.aug.dp,]
    # remove samples outside of boundary
    samp.aug.out <- apply(samp.aug, 1, function(x) (any(x<0)|any(x>1)))
    samp.aug <- samp.aug[!samp.aug.out,]
    # remove nearby point due to numerical issue
    no.aug <- nrow(samp.aug)
    samp.rep <- rbind(samp.aug, samp)
    samp.rep <- round(samp.rep, digits = no.decimal)
    samp.rep.dp <- duplicated(samp.rep, fromLast = TRUE)
    samp.aug <- samp.aug[!(samp.rep.dp[1:no.aug]),]
    if (nrow(samp.aug) > 0){
      # compute constraint value for augmented samples
      samp.aug.gval <- matrix(t(apply(samp.aug,1,constraint)), 
                              nrow=nrow(samp.aug))
      # combine augmented samples with original samples
      samp <- rbind(samp, samp.aug)
      samp.gval <- rbind(samp.gval, samp.aug.gval)
    }
    # find minimum energy design points
    if (auto.scale) scale <- apply(samp.gval,2,mad,center=0)
    samp.lf <- apply(samp.gval,1,function(x) sum(pnorm(-tau[k]*x/scale,log.p=T)))
    if (s == 0){
      hp <- floor(p/2)
      nl <- sqrt((min.dist^2-min.ddist^2*hp)/(p-hp))
      min.logdis <- logdis(c(rep(min.ddist,hp),rep(nl,(p-hp))),s=s)
    } else {
      min.logdis <- logdis(rep(min.dist/sqrt(p),p),s=s)
    }
    samp.lf.cv <- sort(samp.lf, decreasing = T)[n] + 2.5 * min.logdis
    samp.cand.idx <- (samp.lf > samp.lf.cv)
    samp.cand <- samp[samp.cand.idx,]
    samp.cand.lf <- samp.lf[samp.cand.idx]
    med.op <- mined.seq(n, samp.cand, samp.cand.lf, s = s, return.obj = TRUE)
    samp.med <- samp.cand[med.op$idx,]
    if (visualization){
      plot(samp.cand, col = "green", pch = 18, cex = 0.75, 
           xlim = xlim, ylim = ylim, xlab = "", ylab = "")
      points(samp.med, col = "red", pch = 16, cex = 1)
      if (!is.null(vparams$contour)){
    	  contour <- vparams$contour
    	  for (l in 1:nrow(contour$x1)){
    	    lines(contour$x1[l,], contour$x2[l,], lty = 2)
    	  }
      }
    }
    
  }
  
  samp.med.lf <- samp.cand.lf[med.op$idx]
  feasible.idx <- !(apply(samp.gval, 1, function(x) any(x>0)))
  
  return (list(med = samp.med, 
               med.lf = samp.med.lf,
               cand = samp, 
               cand.lf = samp.lf,
               cand.min.dist = min.dist,
               cand.min.ddist = min.ddist,
               feasible.idx = feasible.idx))
  
}

# maximin by one-point-at-a-time greedy algorithm
maximin.seq <- function(n, cand, return.obj = F){
  N <- nrow(cand)
  if (n > N) stop(message("Not enough candidate points!"))
  p <- ncol(cand)
  idx <- sample(1:N,1)
  xi <- c(idx)
  val <- NULL
  for (i in 2:n){
    cand.dist <- cand - rep(1,N) %*% t(cand[idx,])
    val <- cbind(val, apply(cand.dist,1,function(x) sqrt(sum(x^2))))
    idx <- which.max(apply(val,1,min))
    xi <- c(xi, idx)
  }
  if (return.obj){
    val <- val[xi,]
    val <- cbind(val, c(val[n,],0))
    obj <- min(val[upper.tri(val)])
    return (list(idx = xi, obj = obj))
  }
  return (xi)
}

# maxpro by one-point-at-a-time greedy algorithm
maxpro.seq <- function(n, cand, return.obj = F){
  N <- nrow(cand)
  if (n > N) stop(message("Not enough candidate points!"))
  p <- ncol(cand)
  idx <- sample(1:N,1)
  xi <- c(idx)
  val <- NULL
  for (i in 2:n){
    cand.dist <- cand - rep(1,N) %*% t(cand[idx,])
    val <- cbind(val, apply(cand.dist,1,function(x) 1/(prod(x^2))))
    idx <- which.min(apply(val,1,sum))
    xi <- c(xi, idx)
  }
  if (return.obj){
    val <- val[xi,]
    val <- cbind(val, c(val[n,],Inf))
    obj <- mean(val[upper.tri(val)])^(1/p)
    return (list(idx = xi, obj = obj))
  }
  return (xi)
}

# maxpro measure
maxpro.measure <- function(D){
  n <- nrow(D)
  p <- ncol(D)
  val <- 0
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      val <- val + 1/(prod((D[i,]-D[j,])^2))
    }
  }
  val <- (val/(n*(n-1)/2))^(1/p)
  return (val)
}
