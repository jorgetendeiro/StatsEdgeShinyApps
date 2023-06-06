mu1 <- -.2   ;  n1  <- 23
mu2 <- .1  ;  n2  <- 35
sd  <- .8

g1     <- scale(rnorm(n1)) * sd + mu1
g2     <- scale(rnorm(n2)) * sd + mu2
res.t  <- t.test(g1, g2, var.equal = TRUE)
res.t  <- c(Diff=(mu1-mu2)/sd, res.t$statistic, res.t$parameter, p=res.t$p.value) |> round(3)
res.t

prior <- c("cauchy", "normal", "t.student")[3]
res.BF <- suppressWarnings(switch(prior, 
                                  "cauchy"    = B01(res.t["t"], n1, n2, 
                                                    cauchy.prior, location = 0, scale = .707), 
                                  "normal"    = B01(res.t["t"], n1, n2, 
                                                    normal.prior, location = 0, scale = 1), 
                                  "t.student" = B01(res.t["t"], n1, n2, 
                                                    tstude.prior, location = 0, scale = 1, df = 1)))
c(BF01 = res.BF, BF10 = 1/res.BF)
