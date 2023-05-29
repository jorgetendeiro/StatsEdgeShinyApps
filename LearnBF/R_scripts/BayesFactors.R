# I adapted code from Richard for the one-sample t-test.
# The suggestion to adapt the code from the one-sample to the two-sample t test
#   is in Rouder et al. (2009), p. 234, 2nd paragraph.

# Notation:
# n1 = sample size group 1
# n2 = sample size group 2
# x  = binary predictor   (length n1+n2)
# y  = continuous outcome (length n1+n2)

p.y.alt <- function(t.stat, n1, n2, log.prior.dens,...) {
  normalize <- integrate(function(delta,...) {exp(log.prior.dens(delta, ...))}, 
                         lower = -Inf, upper = Inf, ...)[[1]]
  py <- integrate(function(delta,t.stat,N,...) {
    exp(dt(t.stat, n1+n2-2, ncp = delta*sqrt(n1*n2/(n1+n2)), log = TRUE) + 
          log.prior.dens(delta, ...)) },
    lower = -Inf, upper = Inf, t.stat = t.stat, ...)[[1]]
  py/normalize
}

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

# t.ts <- t.test(y ~ x, var.equal = TRUE)
# t    <- t.ts$statistic
# df   <- t.ts$parameter

B01 <- function(t.stat, n1, n2, log.prior.dens, ...) {
  dt(t.stat, n1+n2-2) / p.y.alt(t.stat, n1, n2, log.prior.dens, ...)
}



# BF01.c <- suppressWarnings({ 1 / B01(t, n1, n2, cauchy.prior, -Inf, Inf, mu = 0, scale = 0.707)         })
# BF01.n <- suppressWarnings({ 1 / B01(t, n1, n2, normal.prior, -Inf, Inf, mu = 0, sd    = 0.707)         })
# BF01.t <- suppressWarnings({ 1 / B01(t, n1, n2, tstude.prior, -Inf, Inf, mu = 0, scale = 0.707, df = 1) })












