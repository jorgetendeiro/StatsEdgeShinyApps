# I adapted code from Richard for the one-sample t-test.
# The suggestion to adapt the code from the one-sample to the two-sample t test
#   is in Rouder et al. (2009), p. 234, 2nd paragraph.

# Notation:
# n1 = sample size group 1
# n2 = sample size group 2
# x  = binary predictor   (length n1+n2)
# y  = continuous outcome (length n1+n2)

# Priors:
cauchy.prior <- function(delta, location = 0, scale = sqrt(2)/2) 
{
  dcauchy(delta, location = location, scale = scale, log = TRUE) 
}
normal.prior <- function(delta, location = 0, scale  = sqrt(2)/2) 
{
  dnorm  (delta, mean = location, sd = scale, log = TRUE) 
}
tstude.prior <- function(delta, location = 0, scale = sqrt(2)/2, df = 1) 
{
  log(gamma((df+1)/2)) - ((df+1)/2) * log((df+((delta-location)/scale)^2)/df) - 
    log(scale*sqrt(df*pi)*gamma(df/2))
}

# p(D|H1), H1: delta != 0 :
p.y.alt <- function(t.stat, n1, n2, log.prior.dens, type.H1, point.H1, ...) {
  switch(type.H1, 
         "H1.diff0" = {
           normalize <- integrate(function(delta,...) {exp(log.prior.dens(delta, ...))}, 
                                  lower = -Inf, upper = Inf, ...)[[1]]
           py        <- integrate(function(delta,t.stat,N,...) {
             exp(dt(t.stat, n1+n2-2, ncp = delta*sqrt(n1*n2/(n1+n2)), log = TRUE) + 
                   log.prior.dens(delta, ...)) },
             lower = -Inf, upper = Inf, t.stat = t.stat, ...)[[1]]
           py/normalize
         }, 
         "H1.larger0" = {
           normalize <- integrate(function(delta,...) {exp(log.prior.dens(delta, ...))}, 
                                  lower = 0, upper = Inf, ...)[[1]]
           py        <- integrate(function(delta,t.stat,N,...) {
             exp(dt(t.stat, n1+n2-2, ncp = delta*sqrt(n1*n2/(n1+n2)), log = TRUE) + 
                   log.prior.dens(delta, ...)) },
             lower = 0, upper = Inf, t.stat = t.stat, ...)[[1]]
           py/normalize
         }, 
         "H1.smaller0" = {
           normalize <- integrate(function(delta,...) {exp(log.prior.dens(delta, ...))}, 
                                  lower = -Inf, upper = 0, ...)[[1]]
           py        <- integrate(function(delta,t.stat,N,...) {
             exp(dt(t.stat, n1+n2-2, ncp = delta*sqrt(n1*n2/(n1+n2)), log = TRUE) + 
                   log.prior.dens(delta, ...)) },
             lower = -Inf, upper = 0, t.stat = t.stat, ...)[[1]]
           py/normalize
         }, 
         "H1.point" = {
           dt(t.stat, n1+n2-2, ncp = point.H1*sqrt(n1*n2/(n1+n2))) ####
         }
  )
} 



B01 <- function(t.stat, n1, n2, log.prior.dens, type.H1, point.H1, ...) {
  dt(t.stat, n1+n2-2) / p.y.alt(t.stat, n1, n2, log.prior.dens, type.H1, point.H1, ...)
}














