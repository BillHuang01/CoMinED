problem <- list()

problem[[1]] <- list(
  # Liu 2017
  # G01
  # range of parameters
  # 0 <= x_i <= 1 for i = 1-9,13
  # 0 <= x_i <= 100 for i = 10-12
  name = "G01",
  p = 13,
  f = function(x){
    x[10:12] <- x[10:12]*100
    val <- 5*sum(x[1:4]) - 5*sum(x[1:4]^2) - sum(x[5:13])
    return (val)
  },
  g = function(x){
    x[10:12] <- x[10:12]*100
    val <- rep(NA,9)
    val[1] <- 2*x[1] + 2*x[2] + x[10] + x[11] - 10
    val[2] <- 2*x[1] + 2*x[3] + x[10] + x[12] - 10
    val[3] <- 2*x[2] + 2*x[3] + x[11] + x[12] - 10
    val[4] <- -8*x[1] + x[10]
    val[5] <- -8*x[2] + x[11]
    val[6] <- -8*x[3] + x[12]
    val[7] <- -2*x[4] - x[5] + x[10]
    val[8] <- -2*x[6] - x[7] + x[11]
    val[9] <- -2*x[8] - x[9] + x[12]
    return (val)
  }
)

problem[[2]] <- list(
  # Liu 2017
  # G02
  # range of parameters
  # 0 <= x_i <= 10
  name = "G02",
  p = 20,
  f = function(x){
    x <- x * 10
    numer <- sum(cos(x)^4) - 2*prod(cos(x)^2)
    denom <- sqrt(sum(c(1:length(x))*x^2))
    val <- abs(numer/denom)
    return (val)
  },
  g = function(x){
    x <- x * 10
    val <- rep(NA,2)
    val[1] <- 0.75 - prod(x)
    val[2] <- sum(x) - 7.5*length(x)
    return (val)
  }
)

problem[[3]] <- list(
  # Liu 2017
  # G04
  # range of parameters
  # 78 <= x_1 <= 102 
  # 33 <= x_2 <= 45
  # 27 <= x_i <= 45 for i = 3,4,5
  name = "G04",
  p = 5,
  f = function(x){
    x[1] <- 78 + x[1] * (102-78)
    x[2] <- 33 + x[2] * (45-33)
    x[3:5] <- 27 + x[3:5] * (45-27)
    val <- 5.3578547*x[3]^2 + 0.8356891*x[1]*x[5] + 37.293239*x[1] - 40792.141
    return (val)
  },
  g = function(x){
    x[1] <- 78 + x[1] * (102-78)
    x[2] <- 33 + x[2] * (45-33)
    x[3:5] <- 27 + x[3:5] * (45-27)
    val <- rep(NA,6)
    val[1] <- 85.334407 + 0.0056858*x[2]*x[5] + 0.0006262*x[1]*x[4] - 0.0022053*x[3]*x[5] - 92
    val[2] <- -85.334407 - 0.0056858*x[2]*x[5] - 0.0006262*x[1]*x[4] + 0.0022053*x[3]*x[5]
    val[3] <- 80.51249 + 0.0071317*x[2]*x[5] + 0.0029955*x[1]*x[2] + 0.0021813*x[3]^2 - 110
    val[4] <- -80.51249 - 0.0071317*x[2]*x[5] - 0.0029955*x[1]*x[2] - 0.0021813*x[3]^2 + 90
    val[5] <- 9.300961 + 0.0047026*x[3]*x[5] + 0.0012547*x[1]*x[3] + 0.0019085*x[3]*x[4] - 25
    val[6] <- -9.300961 - 0.0047026*x[3]*x[5] - 0.0012547*x[1]*x[3] - 0.0019085*x[3]*x[4] + 20
    return (val)
  }
)

problem[[4]] <- list(
  # Liu 2017
  # G06
  # range of parameters
  # 13 <= x_1 <= 100
  # 0 <= x_2 <= 100
  name = "G06",
  p = 2,
  f = function(x){
    x[1] <- 13 + x[1] * (100-13)
    x[2] <- x[2] * 100
    val <- (x[1]-10)^3 + (x[2]-20)^3
    return (val)
  },
  g = function(x){
    x[1] <- 13 + x[1] * (100-13)
    x[2] <- x[2] * 100
    val <- rep(NA,2)
    val[1] <- -(x[1]-5)^2 - (x[2]-5)^2 + 100
    val[2] <- (x[1]-6)^2 + (x[2]-5)^2 - 82.81
    return (val)
  }
)

problem[[5]] <- list(
  # Liu 2017
  # G07
  # range of parameters
  # -10 <= x_i <= 10
  name = "G07",
  p = 10,
  f = function(x){
    x <- -10 + x * 20
    val <- x[1]^2 + x[2]^2 + x[1]*x[2] - 14*x[1] - 16*x[2] + (x[3]-10)^2 + 
      4*(x[4]-5)^2 + (x[5]-3)^2 + 2*(x[6]-1)^2 + 5*x[7]^2 + 7*(x[8]-11)^2 + 
      2*(x[9]-10)^2 + (x[10]-7)^2 + 45
    return (val)
  },
  g = function(x){
    x <- -10 + x * 20
    val <- rep(NA,8)
    val[1] <- -105 + 4*x[1] + 5*x[2] - 3*x[7] + 9*x[8]
    val[2] <- 10*x[1] - 8*x[2] - 17*x[7] + 2*x[8]
    val[3] <- -8*x[1] + 2*x[2] + 5*x[9] - 2*x[10] - 12
    val[4] <- 3*(x[1]-2)^2 + 4*(x[2]-3)^2 + 2*x[3]^2 - 7*x[4] - 120
    val[5] <- 5*x[1]^2 + 8*x[2] + (x[3]-6)^2 - 2*x[4] - 40
    val[6] <- x[1]^2 + 2*(x[2]-2)^2 - 2*x[1]*x[2] + 14*x[5] - 6*x[6]
    val[7] <- 0.5*(x[1]-8)^2 + 2*(x[2]-4)^2 + 3*x[5]^2 - x[6] - 30
    val[8] <- -3*x[1] + 6*x[2] + 12*(x[9]-8)^2 - 7*x[10]
    return (val)
  }
)

problem[[6]] <- list(
  # Liu 2017
  # G08
  # range of parameters
  # 0 <= x_i <= 10
  name = "G08",
  p = 2,
  f = function(x){
    x <- x * 10
    numer <- sin(2*pi*x[1])^3 * sin(2*pi*x[2])
    denom <- x[1]^3 * (x[1]+x[2])
    val <- numer / denom
    return (val)
  },
  g = function(x){
    x <- x * 10
    val <- rep(NA,2)
    val[1] <- x[1]^2 - x[2] + 1
    val[2] <- 1 - x[1] + (x[2]-4)^2
    return (val)
  }
)

problem[[7]] <- list(
  # Liu 2017
  # G09
  # range of parameters
  # -10 <= x_i <= 10
  name = "G09",
  p = 7,
  f = function(x){
    x <- -10 + x * 20
    val <- (x[1]-10)^2 + 5*(x[2]-12)^2 + x[3]^4 + 3*(x[4]-11)^2 + 
      10*x[5]^6 + 7*x[6]^2 + x[7]^4 - 4*x[6]*x[7] - 10*x[6] - 8*x[7]
    return (val)
  },
  g = function(x){
    x <- -10 + x * 20
    val <- rep(NA,4)
    val[1] <- -127 + 2*x[1]^2 + 3*x[2]^4 + x[3] + 4*x[4]^2 + 5*x[5]
    val[2] <- -282 + 7*x[1] + 3*x[2] + 10*x[3]^2 + x[4] - x[5]
    val[3] <- -196 + 23*x[1] + x[2]^2 + 6*x[6]^2 - 8*x[7]
    val[4] <- 4*x[1]^2 + x[2]^2 - 3*x[1]*x[2] + 2*x[3]^2 + 5*x[6] - 11*x[7]
    return (val)
  }
)

problem[[8]] <- list(
  # Liu 2017
  # G10
  # range of parameters
  # 100 <= x_1 <= 10,000
  # 1,000 <= x_i <= 10,000 for i = 2,3
  # 10 <= x_i <= 1,000 for i = 4-8
  name = "G10",
  p = 8,
  f = function(x){
    x[1] <- 100 + x[1] * (10000-100)
    x[2:3] <- 1000 + x[2:3] * (10000-1000)
    x[4:8] <- 10 + x[4:8] * (1000-10)
    val <- x[1] + x[2] + x[3]
    return (val)
  },
  g = function(x){
    x[1] <- 100 + x[1] * (10000-100)
    x[2:3] <- 1000 + x[2:3] * (10000-1000)
    x[4:8] <- 10 + x[4:8] * (1000-10)
    val <- rep(NA,6)
    val[1] <- -1 + 0.0025*(x[4]+x[6])
    val[2] <- -1 + 0.0025*(x[5]+x[7]-x[4])
    val[3] <- -1 + 0.01*(x[8]-x[5])
    val[4] <- -x[1]*x[6] + 833.33252*x[4] + 100*x[1] - 83333.333
    val[5] <- -x[2]*x[7] + 1250*x[5] + x[2]*x[4] - 1250*x[4]
    val[6] <- -x[3]*x[8] + 1250000 + x[3]*x[5] - 2500*x[5]
    return (val)
  }
)

problem[[9]] <- list(
  # Liu 2017
  # G12
  # range of parameters
  # 0 <= x_i <= 10
  name = "G12",
  p = 3,
  f = function(x){
    x <- x * 10
    val <- (100-sum((x-5)^2))/100
    return (val)
  },
  g = function(x){
    x <- x * 10
    val <- (x[1]-1)^2 - (x[2]-2)^2 - (x[3]-9)^2 - 0.0625
    return (val)
  }
)

problem[[10]] <- list(
  # Chaiyotha 2020
  # G24
  # range of parameters
  # 0 <= x_1 <= 3
  # 0 <= x_2 <= 4
  name = "G24",
  p = 2,
  f = function(x){
    x[1] <- x[1] * 3
    x[2] <- x[2] * 4
    val <- -x[1] - x[2]
    return (val)
  },
  g = function(x){
    x[1] <- x[1] * 3
    x[2] <- x[2] * 4
    val <- rep(NA,2)
    val[1] <- -2*x[1]^4 + 8*x[1]^3 - 8*x[1]^2 + x[2] - 2
    val[2] <- -4*x[1]^4 + 32*x[1]^3 - 88*x[1]^2 + 96*x[1] + x[2] - 36
    return (val)
  }
)

problem[[11]] <- list(
  # Dong 2018
  # Tensor/Compression Spring Design Problem
  # range of parameters
  # 0.05 <= x_1 <= 2.00
  # 0.25 <= x_2 <= 1.30
  # 2 <= x_3 <= 15
  name = "TSD",
  p = 3,
  f = function(x){
    x[1] <- 0.05 + x[1] * (2.00 - 0.05)
    x[2] <- 0.25 + x[2] * (1.30 - 0.25)
    x[3] <- 2 + x[3] * (15 - 2)
    val <- x[1]^2 * x[2] * (x[3]+2)
    return (val)
  },
  g = function(x){
    x[1] <- 0.05 + x[1] * (2.00 - 0.05)
    x[2] <- 0.25 + x[2] * (1.30 - 0.25)
    x[3] <- 2 + x[3] * (15 - 2)
    val <- rep(NA,4)
    val[1] <- 1.0 - (x[2]^3*x[3])/(71875*x[1]^4)
    val[2] <- (4*x[2]^2-x[1]*x[2])/(12566*x[1]^3*(x[2]-x[1])) + 
      1/(5108*x[1]^2) - 1.0
    val[3] <- 1.0 - 140.45*x[1]/(x[3]*x[2]^2)
    val[4] <- (x[1]+x[2])/1.5 - 1.0
    return (val)
  }
)

problem[[12]] <- list(
  # Chaiyotha 2020, Dong 2018
  # Pressure Vessel Design
  # range of parameters
  # 0.0625 <= x_i <= 6.1875 for i = 1,2
  # 10 <= x_i <= 200 for i = 3,4
  name = "PVD",
  p = 4,
  f = function(x){
    x[1:2] <- 0.0625 + x[1:2] * (6.1875 - 0.0625)
    x[3:4] <- 10 + x[3:4] * (200 - 10)
    val <- 0.6224*x[1]*x[3]*x[4] +1.7781*x[2]*x[3]^2 + 
      3.1661*x[1]^2*x[4] + 19.84*x[1]^2*x[3]
    return (val)
  },
  g = function(x){
    x[1:2] <- 0.0625 + x[1:2] * (6.1875 - 0.0625)
    x[3:4] <- 10 + x[3:4] * (200 - 10)
    val <- rep(NA,4)
    val[1] <- -x[1] + 0.0193*x[3]
    val[2] <- -x[2] + 0.00954*x[3]
    val[3] <- -pi*x[3]^2*x[4] - (4/3)*pi*x[3]^3 + 1296000
    val[4] <- x[4] - 240
    return (val)
  }
)

problem[[13]] <- list(
  # Liu 2017
  # NASA Speed Reducer Design
  # range of parameters
  # 2.6 <= x_1 <= 3.6
  # 0.7 <= x_2 <= 0.8
  # 17 <= x_3 <= 28
  # 7.3 <= x_4 <= 8.3
  # 7.8 <= x_5 <= 8.3
  # 2.9 <= x_6 <= 3.9
  # 5.0 <= x_7 <= 5.5
  name = "SRD",
  p = 7,
  f = function(x){
    x[1] <- 2.6 + x[1] * (3.6 - 2.6)
    x[2] <- 0.7 + x[2] * (0.8 - 0.7)
    x[3] <- 17 + x[3] * (28 - 17)
    x[4] <- 7.3 + x[4] * (8.3 - 7.3)
    x[5] <- 7.8 + x[5] * (8.3 - 7.8)
    x[6] <- 2.9 + x[6] * (3.9 - 2.9)
    x[7] <- 5.0 + x[7] * (5.5 - 5.0)
    val <- 0.7854*x[1]*x[2]^2*(3.3333*x[3]^2+14.9334*x[3]-43.0934) - 
      1.508*x[1]*(x[6]^2+x[7]^2) + 7.4777*(x[6]^3+x[7]^3) + 
      0.7854*(x[4]*x[6]^2 + x[5]*x[7]^2)
    return (val)
  },
  g = function(x){
    x[1] <- 2.6 + x[1] * (3.6 - 2.6)
    x[2] <- 0.7 + x[2] * (0.8 - 0.7)
    x[3] <- 17 + x[3] * (28 - 17)
    x[4] <- 7.3 + x[4] * (8.3 - 7.3)
    x[5] <- 7.8 + x[5] * (8.3 - 7.8)
    x[6] <- 2.9 + x[6] * (3.9 - 2.9)
    x[7] <- 5.0 + x[7] * (5.5 - 5.0)
    val <- rep(NA,11)
    val[1] <- 27/(x[1]*x[2]^2*x[3]) - 1
    val[2] <- 397.5/(x[1]*x[2]^2*x[3]^2) - 1
    val[3] <- 1.93*x[4]^3/(x[2]*x[3]*x[6]^4) - 1
    val[4] <- 1.93*x[5]^3/(x[2]*x[3]*x[7]^4) - 1
    val[5] <- ((745*x[4]/(x[2]*x[3]))^2+16.9*1e6)^(0.5)/(110*x[6]^3) - 1
    val[6] <- ((745*x[5]/(x[2]*x[3]))^2+157.5*1e6)^(0.5)/(85*x[7]^3) - 1
    val[7] <- x[2]*x[3]/40 - 1
    val[8] <- 5*x[2]/x[1] - 1
    val[9] <- x[1]/(12*x[2]) - 1
    val[10] <- (1.5*x[6]+1.9)/x[4] - 1
    val[11] <- (1.1*x[7]+1.9)/x[5] - 1
    return (val)
  }
)

problem[[14]] <- list(
  # Liu 2017
  # Three-bar Truss Design
  # range of parameters
  # 0 <= x_i <= 1
  name = "TTD",
  p = 2,
  f = function(x){
    val <- (2*sqrt(2)*x[1] + x[2]) * 100
    return (val)
  },
  g = function(x){
    val <- rep(NA,3)
    val[1] <- ((sqrt(2)*x[1]+x[2])/(sqrt(2)*x[1]^2+2*x[1]*x[2])) * 2 - 2
    val[2] <- (x[2]/(sqrt(2)*x[1]^2+2*x[1]*x[2])) * 2 - 2
    val[3] <- (1/(x[1]+sqrt(2)*x[2])) * 2 - 2
    return (val)
  }
)

problem[[15]] <- list(
  # Pourmohamad 2018
  # Welded Beam Design
  # range of parameters
  # 0.125 <= x_1 <= 10
  # 0.1 <= x_i <= 10 for i = 2-4
  name = "WBD",
  p = 4,
  f = function(x){
    x[1] <- 0.125 + x[1] * (10 - 0.125)
    x[2:4] <- 0.1 + x[2:4] * (10 - 0.1)
    val <- 1.10471*x[1]^2*x[2] + 0.04811*x[3]*x[4]*(14.0 + x[2])
    return (val)
  },
  g = function(x){
    x[1] <- 0.125 + x[1] * (10 - 0.125)
    x[2:4] <- 0.1 + x[2:4] * (10 - 0.1)
    val <- rep(NA,6)
    P <- 6000
    L <- 14
    E <- 30 * 1e6
    G <- 12 * 1e6
    tau.max <- 13600
    sigma.max <- 30000
    delta.max <- 0.25
    M <- P * (L + x[2] / 2)
    R <- sqrt(x[2]^2/4 + (x[1]+x[3])^2/4)
    J <- 2*sqrt(2)*x[1]*x[2]*(x[2]^2/12+(x[1]+x[3])^2/4)
    tau1 <- P / (sqrt(2)*x[1]*x[2])
    tau2 <- M * R / J
    tau <- sqrt(tau1^2 + 2*tau1*tau2*x[2]/(2*R) + tau2^2)
    sigma <- (6*P*L) / (x[4]*x[3]^2)
    delta <- (4*P*L^3) / (E*x[3]^3*x[4])
    Pc <- (4.013*E*sqrt(x[3]^2*x[4]^6/36)) / (L^2) * (1-x[3]/(2*L)*sqrt(E/(4*G)))
    val[1] <- tau - tau.max
    val[2] <- sigma - sigma.max
    val[3] <- x[1] - x[4]
    val[4] <- 0.10471*x[1]^2 + 0.04811*x[3]*x[4]*(14+x[2]) - 5
    val[5] <- delta - delta.max
    val[6] <- P - Pc
    return (val)
  }
)

problem[[16]] <- list(
  # Wang et al.
  # IBD
  # range of parameters
  # 10 <= x_1 <= 80
  # 10 <= x_2 <= 50
  # 0.9 <= x_i <= 5 for i = 3,4
  name = "IBD",
  p = 4,
  f = function(x){
    x[1] <- 10 + x[1] * (80 - 10)
    x[2] <- 10 + x[2] * (50 - 10)
    x[3:4] <- 0.9 + x[3:4] * (5 - 0.9)
    val <- 5000 / (x[3]*(x[1]-2*x[4])^3/12 + x[2]*x[4]^3/6 + 2*x[2]*x[4]*((x[1]-x[4])/2)^2)
    return (val)
  },
  g = function(x){
    x[1] <- 10 + x[1] * (80 - 10)
    x[2] <- 10 + x[2] * (50 - 10)
    x[3:4] <- 0.9 + x[3:4] * (5 - 0.9)
    val <- rep(NA,2)
    val[1] <- 2*x[2]*x[4] + x[3]*(x[1]-2*x[4]) - 300
    val[2] <- 180000*x[1]/(x[3]*(x[1]-2*x[4])^3 + 2*x[2]*x[4]*(4*x[4]^2+3*x[1]*(x[1]-2*x[4]))) +
      15000*x[2]/((x[1]-2*x[4])*x[3]^3+2*x[4]*x[2]^3) - 6
    return (val)
  }
)

problem[[17]] <- list(
  # Dong 2018
  # SCBD
  # range of parameters 
  # 2 <= b_i <= 3.5 for i = 1-5
  # 35 <= h_i <= 60 for i = 1-5
  name = "SCBD",
  p = 10,
  f = function(x){
    l <- 100
    b <- 2 + x[1:5] * (3.5 - 2)
    h <- 35 + x[6:10] * (60 - 35)
    val <- l * sum(b*h)
    return (val)
  },
  g = function(x){
    # https://fr.mathworks.com/help/gads/solving-a-mixed-integer-engineering-design-problem-using-the-genetic-algorithm.html
    l <- 100 # cm, Individual section of beam
    P <- 50000 # N, end load that the beam must support
    E <- 2e7 # N/cm^2, Young's modulus
    b <- 2 + x[1:5] * (3.5 - 2)
    h <- 35 + x[6:10] * (60 - 35)
    I <- b * h^3 / 12
    val <- rep(NA,11)
    # bending stress
    val[1] <- 6*P*l/(b[5]*h[5]^2) - 14000
    val[2] <- 6*P*2*l/(b[4]*h[4]^2) - 14000
    val[3] <- 6*P*3*l/(b[3]*h[3]^2) - 14000
    val[4] <- 6*P*4*l/(b[2]*h[2]^2) - 14000
    val[5] <- 6*P*5*l/(b[1]*h[1]^2) - 14000
    # end deflection
    val[6] <- (P*l^3)/(3*E) * (61/I[1]+37/I[2]+19/I[3]+7/I[4]+1/I[5]) - 2.7
    # aspect ratio
    val[7] <- h[1] / b[1] - 20
    val[8] <- h[2] / b[2] - 20
    val[9] <- h[3] / b[3] - 20
    val[10] <- h[4] / b[4] - 20
    val[11] <- h[5] / b[5] - 20
    return (val)
  }
)

# constraint only problems
problem[[18]] <- list(
  # Tao et al. 2019
  # GG0
  # range of parameters
  # 0 <= x_1 <= 3.7
  # 0 <= x_2 <= 4
  name = "GG0",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x[1] <- x[1] * 3.7
    x[2] <- x[2] * 4
    val <- rep(NA,2)
    val[1] <- x[1]*sin(4*x[1]) + 1.1*x[2]*sin(2*x[2])
    val[2] <- x[2]^2 - 5.1*(x[1]+0.8)^3/(4*pi^2) + 5*(x[1]+0.8)/pi -
      6 + 10*(1-1/(8*pi))*cos(x[1]+0.8)
    return (val)
  }
)

problem[[19]] <- list(
  # Tao et al. 2019
  # GG1
  # range of parameters
  # -2 <= x_1 <= 2
  # -1 <= x_2 <= 1
  name = "GG1",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x[1] <- -2 + x[1] * 4
    x[2] <- -1 + x[2] * 2
    val <- rep(NA, 2)
    val[1] <- (4-2.1*x[1]^2+x[1]^4/3)*x[1]^2 + (-4+4*x[2]^2)*x[2]^2
    val[2] <- x[1]*exp(-x[1]^2-x[2]^2) + 0.1
    return (val)
  }
)

problem[[20]] <- list(
  # Tao et al. 2019
  # GG2
  # range of parameters
  # 0 <= x_i <= 1.5
  name = "GG2",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x <- x * 1.5
    val <- rep(NA,3)
    val[1] <- (x[2] - (5.1/(4*pi^2))*x[1]^2 + (5/pi)*x[1] - 6)^2 +
      10*(1-(1/(8*pi)))*cos(x[1]) - 20
    val[2] <- ((30+5*x[1]*sin(5*x[1])) * (4+exp(-5*x[2])) - 100) / 6 - 2
    val[3] <- (1+(x[1]+x[2]+1)^2*(19-14*x[1]+3*x[1]^2-14*x[2]+6*x[1]*x[2]+3*x[2]^2)) *
      (30+(2*x[1]-3*x[2])^2*(18-32*x[1]+12*x[1]^2+48*x[2]-36*x[1]*x[2]+27*x[2]^2)) - 2000
    return (val)
  }
)

problem[[21]] <- list(
  # Tao et al. 2019
  # GG3 - closely related to G24
  # range of parameters
  # 0 <= x_1 <= 3
  # 0 <= x_2 <= 4
  name = "GG3",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x[1] <- x[1] * 3
    x[2] <- x[2] * 4
    val <- rep(NA,3)
    val[1] <- -2*x[1]^4 + 8*x[1]^3 - 8*x[1]^2 + x[2] - 2
    val[2] <- -4*x[1]^4 + 32*x[1]^3 - 88*x[1]^2 + 96*x[1] + x[2] - 38
    val[3] <- -(x[1]-1)^3 + (x[2]-4)^2 - 4.5
    return (val)
  }
)

problem[[22]] <- list(
  # Tao et al. 2019
  # GG4
  # range of parameters
  # 0 <= x_1 <= 3
  # 0 <= x_2 <= 4
  name = "GG4",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x[1] <- x[1] * 3
    x[2] <- x[2] * 4
    val <- rep(NA,4)
    val[1] <- cos(x[1])^4 + cos(x[2])^4 - 
      2*cos(x[1])^2*cos(x[2])^2/(x[1]^2+2*x[2]^2) - 0.05
    val[2] <- 0.75 - x[1]*x[2]
    val[3] <- (x[1]-10)^3 + (x[2]-10)^3 + 1200
    val[4] <- -x[1] - x[1]*x[2]*cos(1.48477-x[2]) + 0.6
    return (val)
  }
)

problem[[23]] <- list(
  # Motivation
  # range of parameters
  # 0 <= x_i <= 1
  name = "MOT-O",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    val <- rep(NA,3)
    val[1] <- (x[1] - sqrt(50 * (x[2] - 0.52)^2 + 2) + 1)
    val[2] <- (sqrt(120 * (x[2] - 0.48)^2 + 1) - 0.75 - x[1])
    val[3] <- (0.65^2 - x[1]^2 - x[2]^2)
    return (val)
  }
)

problem[[24]] <- list(
  # Motivation - scaling
  # range of parameters
  # 0 <= x_i <= 1
  name = "MOT-S",
  p = 2,
  f = function(x){
    return (NA)
  },
  g = function(x){
    val <- rep(NA,3)
    val[1] <- 1e-3 * (x[1] - sqrt(50 * (x[2] - 0.52)^2 + 2) + 1)
    val[2] <- (sqrt(120 * (x[2] - 0.48)^2 + 1) - 0.75 - x[1])
    val[3] <- 1e3 * (0.65^2 - x[1]^2 - x[2]^2)
    return (val)
  }
)

problem[[25]] <- list(
  # Army
  # range of parameters
  name = "ARMY",
  p = 36,
  f = function(x){
    return (NA)
  },
  g = function(x){
    x.range <- matrix(NA, nrow = 36, ncol = 2)
    x.range[1,] <- c(6.264, 7.656)
    x.range[2,] <- c(16.875, 20.625)
    x.range[3,] <- c(2.475, 3.025)
    x.range[4,] <- c(1.2375, 1.5125)
    x.range[5,] <- c(2.6865, 3.2835)
    x.range[6,] <- c(1.53, 1.87)
    x.range[7,] <- c(17.352, 21.208)
    x.range[8,] <- c(2.7, 3.3)
    x.range[9,] <- c(49.5, 60.5)
    x.range[10,] <- c(1.7226, 2.1054)
    x.range[11,] <- c(18.27, 22.33)
    x.range[12,] <- c(2.6865, 3.2835)
    x.range[13,] <- c(2.61, 3.19)
    x.range[14,] <- c(1.4805, 1.8095)
    x.range[15,] <- c(0.45, 0.55)
    x.range[16,] <- c(2.682, 3.278)
    x.range[17,] <- c(2.61, 3.19)
    x.range[18,] <- c(1.494, 1.826)
    x.range[19,] <- c(0.8103024, 0.9903696)
    x.range[20,] <- c(54, 66)
    x.range[21,] <- c(67.5, 82.5)
    x.range[22,] <- c(58.5, 71.5)
    x.range[23,] <- c(1.755, 2.145)
    x.range[24,] <- c(2.52, 3.08)
    x.range[25,] <- c(1.755, 2.145)
    x.range[26,] <- c(18, 22)
    x.range[27,] <- c(2.25, 2.75)
    x.range[28,] <- c(1.8, 2.2)
    x.range[29,] <- c(2.25, 2.75)
    x.range[30,] <- c(3.339, 4.081)
    x.range[31,] <- c(8.406, 10.274)
    x.range[32,] <- c(0.9, 1.1)
    x.range[33,] <- c(0.000288, 0.000352)
    x.range[34,] <- c(3, 8)
    x.range[35,] <- c(0.00045, 0.00055)
    x.range[36,] <- c(0.9, 1.1)
    for (i in 1:36) {
      x[i] <- x.range[i,1] + x[i] * (x.range[i,2] - x.range[i,1])
    }
    val <- rep(NA,26)
    val[1] <- x[8] - x[1]
    val[2] <- x[1] - x[2]
    val[3] <- x[2] - x[11]
    val[4] <- x[2] + x[4] - x[11]
    val[5] <- x[6] - x[5]
    val[6] <- x[14] - x[6]
    val[7] <- x[11] - x[4] - x[7]
    val[8] <- x[19] + x[28] - x[8]
    val[9] <- x[1] + x[10] - x[2]
    val[10] <- x[14] - x[12]
    val[11] <- x[14] - x[13]
    val[12] <- x[14] - x[5]
    val[13] <- x[14] - x[16]
    val[14] <- x[14] - x[17]
    val[15] <- x[19] - x[8]
    val[16] <- x[20] + x[26] - 90.1
    val[17] <- x[22] - x[21]
    val[18] <- x[23] - x[17]
    val[19] <- x[24] - x[5]
    val[20] <- x[6] - x[24]
    val[21] <- x[27] - x[13]
    val[22] <- x[14] - x[27]
    val[23] <- x[27] - x[12]
    val[24] <- x[8] + x[29] - x[1]
    val[25] <- x[8] + x[30] - x[1]
    val[26] <- x[1] + x[31] - x[2] - x[10]
    return (val)
  }
)
