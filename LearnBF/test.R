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


library(kableExtra)
df <- data.frame(
  C1 = 1:2, 
  C2 = c("a", "a"), 
  C3 = 3:4, 
  C4 = 5:6, 
  C5 = c("b", "b"), 
  row.names = c("Row 1", "Row 2")
)

df |> 
  kable(fomat = "latex") |>
  add_header_above(c("", "Group 1" = 2, "", "Group 2" = 2), 
                   extra_css = "border-bottom: 2px solid green;") |>
  kable_styling(full_width = FALSE) |>
  column_spec(3, extra_css = "border-bottom: 2px solid;") |> 
  column_spec(6, extra_css = "border-bottom: 2px solid;") |> 
  collapse_rows(columns = c(3, 6)) |> 
  row_spec(0, extra_css = "border-bottom: 2px solid;") |> 
  row_spec(2, extra_css = "border-top: 1px solid white;") |>
  row_spec(2, extra_css = "border-bottom: 2px solid;") 
