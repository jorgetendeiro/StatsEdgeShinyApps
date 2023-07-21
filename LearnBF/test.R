
mu1 <- 15.292   ;  n1  <- 24
mu2 <- 10.880  ;  n2  <- 25
sd1  <- 6.376; sd2 <- 4.324

g1     <- scale(rnorm(n1)) * sd1 + mu1
g2     <- scale(rnorm(n2)) * sd2 + mu2
res.t  <- t.test(g1, g2, var.equal = TRUE)
sd.p   <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
res.t  <- c(Diff=(mu1-mu2)/sd.p, res.t$statistic, res.t$parameter, p=res.t$p.value) |> round(3)
res.t

prior <- c("cauchy", "normal", "t.student")[1]
res.BF <- suppressWarnings(switch(prior, 
                                  "cauchy"    = B01(res.t["t"], n1, n2, 
                                                    cauchy.prior, location = 0, scale = .707), 
                                  "normal"    = B01(res.t["t"], n1, n2, 
                                                    normal.prior, location = 0, scale = 1), 
                                  "t.student" = B01(res.t["t"], n1, n2, 
                                                    tstude.prior, location = 0, scale = 1, df = 1)))
c(BF01 = res.BF, BF10 = 1/res.BF)



