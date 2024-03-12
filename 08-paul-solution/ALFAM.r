##### Function for ALFAM emission model
# INPUT
#   t    : vector of time points
#   par  : list of scalar parameters r1, r2, r3, r4, r12, r34, ra
#   init : list of scalar initial values F0, S0, Z0, G0, T0
#   eps_ra : scalar for branching on ra values close to zero
# OUTPUT dataframe with
#   t    : vector of time points
#   Ft   : amount in the fast pool
#   St   : amount in the slow pool
#   Zt   : amount in the sink pool
#   Gt   : amount emitted from the fast pool including G0
#   Tt   : amount emitted from the slow pool including T0
#   Et   : total amount emitted including G0 and T0 (equals Gt+Zt)
#   Eti  : total amount emitted excluding G0 and T0
# REMARK
#   When the function is called for successive time intervals
#   Eti is the amount emitted in the time interval
ALFAM <- function(t, par, init, eps_ra=1.0e-6) {
  ##### Prepare; par and init should contain scalars
  if (length(unlist(par)) != 7) stop("Argument par should contain 7 scalars.")
  if (length(unlist(init)) != 5) stop("Argument init should contain 5 scalars.")
  exp12 <- exp(-par$r12*t)
  exp34 <- exp(-par$r34*t)
  F0r2  <- init$F0 * par$r2
  ##### Calculate functions
  Ft <- init$F0 * exp12
  Gt <- init$G0 + init$F0*(par$r1/par$r12)*(1 - exp12)
  if (abs(par$ra) >= eps_ra) {
    tmp1  <- F0r2/par$ra
    tmp2  <- init$S0 + tmp1
    St  <- tmp2*exp34 - tmp1*exp12
    tmp <- tmp2*(1 - exp34)/par$r34 - tmp1*(1 - exp12)/par$r12
  } else {
    St  <- (init$S0 + F0r2*t) * exp34
    tmp <- -(init$S0*(exp34-1) + F0r2*(exp34*(par$r34*t+1)-1)/par$r34)/par$r34
  }
  Zt <- init$Z0 + par$r4*tmp
  Tt <- init$T0 + par$r3*tmp
  Et <- Gt + Tt
  return(data.frame(t, Ft, St, Zt, Gt, Tt, Et, Eti=Et - init$G0 - init$T0))
}
