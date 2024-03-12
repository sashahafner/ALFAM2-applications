library(numDeriv)
source(".ALFAM.r")

##### Define function for calculation of the maximal relative difference
relDiff <- function(x1,x2) {
  u1 <- unlist(x1)
  u2 <- unlist(x2)
  mean <- (abs(u1) + abs(u2))/2
  mean[is.na(mean)] <- 0
  diff <- abs(u1-u2)
  reldiff <- diff/mean
  reldiff[mean<1.0-16] <- mean[mean<1.0-16]
  return(max(reldiff, na.rm=TRUE))
}

##### Define separate functions for calculating numerical derivatives
Ft <- function(t, par, init, eps_ra=1.0e-6) {
  val <- init$F0 * exp(-par$r12*t)
  return(val)
}
St <- function(t, par, init, eps_ra=1.0e-6) {
  if (abs(par$ra) >= eps_ra) {
    val <- (init$S0+init$F0*par$r2/par$ra) * exp(-par$r34*t) -
                   (init$F0*par$r2/par$ra) * exp(-par$r12*t)
  } else {
    val <- (init$S0+init$F0*par$r2*t) * exp(-par$r34*t)
  }
  return(val)
}
Zt <- function(t, par, init, eps_ra=1.0e-6) {
  if (abs(par$ra) >= eps_ra) {
    val <- -(init$S0+init$F0*par$r2/par$ra) * (par$r4/par$r34) * (exp(-par$r34*t) - 1) +
                   (init$F0*par$r2/par$ra) * (par$r4/par$r12) * (exp(-par$r12*t) - 1)
  } else {
    val <- -init$S0 * (par$r4/par$r34) * (exp(-par$r34*t) - 1) -
           init$F0 * (par$r2*par$r4/par$r34^2) * (exp(-par$r34*t) * (par$r34*t + 1) - 1)
  }
  return(init$Z0 + val)
}
Gt <- function(t, par, init, eps_ra=1.0e-6) {
  val <- init$F0 * (par$r1/par$r12) * (1 - exp(-par$r12*t))
  return(init$G0 + val)
}
Tt <- function(t, par, init, eps_ra=1.0e-6) {
  if (abs(par$ra) >= eps_ra) {
    val <- (init$S0+init$F0*par$r2/par$ra) * (par$r3/par$r34) * (1 - exp(-par$r34*t)) -
           (init$F0*par$r2/par$ra) * (par$r3/par$r12) * (1 - exp(-par$r12*t))
  } else {
    val <- -init$S0 * (par$r3/par$r34) * (exp(-par$r34*t) - 1) -
            init$F0 * (par$r2*par$r3/par$r34^2) * (exp(-par$r34*t) * (par$r34*t + 1) - 1)
  }
  return(init$T0 + val)
}


##### Define two sets of parameters
init <- list(F0=75, S0=25, Z0=0, G0=0, T0=0)
par1  <- list(r1=1, r2=2, r3=3,   r4=4)     ##### ra != 0
par2  <- list(r1=1, r2=2, r3=0.5, r4=2.5)   ##### ra = 0
par1$r12 <- par1$r1+par1$r2;  par1$r34 <- par1$r3+par1$r4;  par1$ra <- par1$r12-par1$r34
par2$r12 <- par2$r1+par2$r2;  par2$r34 <- par2$r3+par2$r4;  par2$ra <- par2$r12-par2$r34

##### Show that for emission F0 + S0 - Ft - St - Zt = Gt + Tt.
##### Also compare exact and numerical derivatives
time <- 1:100/25
for (par in list(par1,par2)) {
  fitted <- ALFAM(time,par,init)
  emis <- init$F0 + init$S0 - fitted$Ft - fitted$St - fitted$Zt
  relDiffEmission <- relDiff(emis,fitted$Et)
  #print(fitted)

  ##### Check derivatives
  dFt <- -par$r12 * fitted$Ft
  dSt <- par$r2 * fitted$Ft - par$r34*fitted$St
  dZt <- par$r4 * fitted$St
  dGt <- par$r1 * fitted$Ft
  dTt <- par$r3 * fitted$St
  nFt <- grad(Ft, time, par=par, init=init)
  nSt <- grad(St, time, par=par, init=init)
  nZt <- grad(Zt, time, par=par, init=init)
  nGt <- grad(Gt, time, par=par, init=init)
  nTt <- grad(Tt, time, par=par, init=init)
  relDiffDerivative <- relDiff(c(dTt,dSt,dZt,dGt,dTt), c(nTt,nSt,nZt,nGt,nTt))
  print(data.frame(ra=par$ra, relDiffEmission, relDiffDerivative), row.names=F)
}

##### Show that evaluations for POSITIVE/NEGATIVE ra close to zero are OK
eps_ra <- 1.0e-6
POS1 <- list(r1=1, r2=2 + eps_ra,            r3=1, r4=2)
POS2 <- list(r1=1, r2=2 + eps_ra - eps_ra/100, r3=1, r4=2)
POS1$r12 <- POS1$r1+POS1$r2;  POS1$r34 <- POS1$r3+POS1$r4;  POS1$ra <- POS1$r12-POS1$r34
POS2$r12 <- POS2$r1+POS2$r2;  POS2$r34 <- POS2$r3+POS2$r4;  POS2$ra <- POS2$r12-POS2$r34

NEG1 <- list(r1=1, r2=2, r3=1, r4=2 + eps_ra)
NEG2 <- list(r1=1, r2=2, r3=1, r4=2 + eps_ra - eps_ra/100)
NEG1$r12 <- NEG1$r1+NEG1$r2;  NEG1$r34 <- NEG1$r3+NEG1$r4;  NEG1$ra <- NEG1$r12-NEG1$r34
NEG2$r12 <- NEG2$r1+NEG2$r2;  NEG2$r34 <- NEG2$r3+NEG2$r4;  NEG2$ra <- NEG2$r12-NEG2$r34

for (par in list(list(POS1,POS2), list(NEG1,NEG2))) {
  fit1 <- ALFAM(time,par[[1]],init)
  fit2 <- ALFAM(time,par[[2]],init)
  relDiffFunction <- relDiff(c(fit1$St,fit1$Zt,fit1$Tt), c(fit2$St,fit2$Zt,fit2$Tt))
  print(data.frame(ra1=par[[1]]$ra, ra2=par[[2]]$ra, relDiffFunction), row.names=F)
}

##### Calling the function for successive time intervals
##### Call the function for a time vector
init <- list(F0=75, S0=25, Z0=0, G0=0, T0=0)
par  <- list(r1=0.05, r2=0.1, r3=0.03, r4=0.15)
par$r12 <- par$r1+par$r2;  par$r34 <- par$r3+par$r4;  par$ra <- par$r12-par$r34
time <- c(0, 1, 3, 6, 10, 20)
fit1 <- ALFAM(time, par, init)
print(round(fit1, 2), row.names=F)
##### Call the function for successive time intervals while updating init
interval <- diff(time)
print(interval)
fit2 <- NA * fit1
fit2[1,] <- c(0, init, 0, 0)
row <- 1
for (tt in interval) {
  fit <- ALFAM(tt, par, init)
  row <- row + 1
  fit2[row,] <- fit
  init$F0 <- fit$Ft
  init$S0 <- fit$St
  init$Z0 <- fit$Zt
  init$G0 <- fit$Gt
  init$T0 <- fit$Tt
}
print(round(fit2, 2), row.names=F)
maxDiff <- max(abs(unlist(fit1[,2:6] - unlist(fit2[,2:6]))))
print(maxDiff)
q()
relDiff(fit1,fit2)
q()
