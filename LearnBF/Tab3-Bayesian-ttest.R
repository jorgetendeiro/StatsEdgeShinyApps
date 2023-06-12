
# BAYESIAN t-TEST tab ----

# Bayes factor:
BF <- reactive({
  # Compute BF01: 
  BF.tmp <- switch(input$prior, 
                   "cauchy"    = B01(ttest.res()["t"], input$n1, input$n2, 
                                     cauchy.prior, input$H1hyp, H1pointslide(), location = location.c(), scale = scale.c()), 
                   "normal"    = B01(ttest.res()["t"], input$n1, input$n2, 
                                     normal.prior, input$H1hyp, H1pointslide(), location = location.n(), scale = scale.n()), 
                   "t.student" = B01(ttest.res()["t"], input$n1, input$n2, 
                                     tstude.prior, input$H1hyp, H1pointslide(), location = location.t(), scale = scale.t(), df = df.t()))
  
  # names(BF.tmp) <- "BF01"
  if (input$BF10.01 == "BF10") {
    BF.tmp <- 1 / BF.tmp
    # names(BF.tmp) <- "BF10"
  }
  BF.tmp
})

# Prior odds:
prior.odds <- reactive({
  if (input$BF10.01 == "BF10") {
    input$priorprob1 / input$priorprob0
  } else {
    input$priorprob0 / input$priorprob1
  }
})

# Posterior probabilities:
post.probs <- reactive({
  if (input$BF10.01 == "BF10") {
    c(1, prior.odds() * BF()) / (1 + prior.odds() * BF())
  } else {
    c(prior.odds() * BF(), 1) / (1 + prior.odds() * BF())
  }
})

# Posterior odds:
post.odds <- reactive({
  prior.odds() * BF()
})


output$BF.df1 <- function() {
  if (input$H1hyp != "H1.point") 
  {
    # BF.tmp <- if (substr(input$BF10.01, 3, 4) == "10") paste0("$BF_{10}$") else paste0("$BF_{01}$")
    BF.tmp <- paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$")
    
    tab <- data.frame(
      "$\\delta=0$", 
      "vs", 
      switch(input$H1hyp, 
             "H1.diff0"    = "$\\delta\\not=0$", 
             "H1.larger0"  = "$\\delta>0$", 
             "H1.smaller0" = "$\\delta<0$", 
             "H1.point"    = paste0("$\\delta=", input$H1pointslide, "$")),
      "", 
      switch(input$prior, 
             "cauchy"    = paste0("$\\text{Cauchy}", if (input$H1hyp == 'H1.larger0') '^+' else if (input$H1hyp == 'H1.smaller0') '^-', "$"), 
             "normal"    = "$\\text{Normal}$", 
             "t.student" = "$\\text{$t$-Student}$"),
      switch(input$prior, 
             "cauchy"    = paste0("$", location.c(), "$"), 
             "normal"    = paste0("$", location.n(), "$"), 
             "t.student" = paste0("$", location.t(), "$")), 
      switch(input$prior, 
             "cauchy"    = paste0("$", scale.c(), "$"), 
             "normal"    = paste0("$", scale.n(), "$"), 
             "t.student" = paste0("$", scale.t(), "$")), 
      "", 
      paste0("$", formatC(round(BF(), 3), 3, format = "f"), "$"),
      stringsAsFactors = FALSE, 
      check.names = FALSE, 
      row.names = NULL
    )
    colnames(tab) <- c("$\\mathcal{H}_0$", 
                       " ", 
                       "$\\mathcal{H}_1$", 
                       "$\\hspace{5mm}$", 
                       "$\\text{Distribution}$", 
                       "$\\text{Location}$", 
                       "$\\text{Scale}$", 
                       "$\\hspace{5mm}$", 
                       paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"))
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'c', table.attr='class="myTable"') %>%
      add_header_above(c("$\\text{Hypotheses}$" = 3, "", "$\\text{Prior for }\\delta\\text{ under }\\mathcal{H}_1$" = 3, "", "$\\text{Bayes factor}$" = 1), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE) %>% 
      row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
      row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid;") %>% 
      # row_spec(0, bold = TRUE, background = "#005E3C1A") %>% 
      kable_styling(full_width = FALSE)
  } else 
  {
    tab <- data.frame(
      "$\\delta=0$", 
      "vs", 
      switch(input$H1hyp, 
             "H1.diff0"    = "$\\delta\\not=0$", 
             "H1.larger0"  = "$\\delta>0$", 
             "H1.smaller0" = "$\\delta<0$", 
             "H1.point"    = paste0("$\\delta=", input$H1pointslide, "$")), 
      "", 
      paste0("All probability assigned to ", input$H1pointslide), 
      "", 
      paste0("$", formatC(round(BF(), 3), 3, format = "f"), "$"),
      stringsAsFactors = FALSE, 
      check.names = FALSE
    )
    colnames(tab) <- c("$\\mathcal{H}_0$", 
                       " ", 
                       "$\\mathcal{H}_1$", 
                       "$\\hspace{5mm}$", 
                       "$\\text{Distribution}$", 
                       "$\\hspace{5mm}$", 
                       paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"))
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'c', table.attr='class="myTable"') %>%
      add_header_above(c("$\\text{Hypotheses}$" = 3, "", "$\\text{Prior under }\\mathcal{H}_1$" = 1, "", "$\\text{Bayes factor}$" = 1), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE) %>% 
      row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
      row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid;") %>% 
      # row_spec(0, bold = TRUE, background = "#005E3C1A") %>% 
      kable_styling(full_width = FALSE)
  }
} 

# output$BF.df1 <- renderUI({
#   tab <- switch(input$prior, 
#                 "cauchy"    = data.frame(BF(), "\\text{Cauchy}", location.c(), scale.c()), 
#                 "normal"    = data.frame(BF(), "\\text{Normal}", location.n(), scale.n()), 
#                 "t.student" = data.frame(BF(), "\\text{$t$-Student}", location.t(), scale.t(), df.t()))
#   colnames(tab) <- switch(input$prior, 
#                           "cauchy"    = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Prior}", "\\text{Location}", "\\text{Scale}"), 
#                           "normal"    = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Prior}", "\\text{Location}", "\\text{Scale}"), 
#                           "t.student" = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Prior}", "\\text{Location}", "\\text{Scale}", "\\text{df}"))
#   LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),
#                            digits = c(0, rep(3, ncol(tab)))), 
#                     floating                   = FALSE,
#                     tabular.environment        = "array",
#                     comment                    = FALSE,
#                     print.results              = FALSE,
#                     sanitize.colnames.function = identity,
#                     sanitize.text.function     = identity,
#                     include.rownames           = FALSE, 
#                     add.to.row                 = list(
#                       pos     = as.list(-1),
#                       command = "\\rowcolor{lightgray}"
#                     )
#   )
#   tagList(
#     #withMathJax(),
#     HTML(paste0("$$", LaTeXtab, "$$")),
#     tags$script(HTML(js)),
#     tags$script(
#       async="", 
#       src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
#     )
#   )
# })

output$BF.df2 <- renderUI({
  tab <- data.frame(
    input$priorprob0 / 100,
    input$priorprob1 / 100,
    prior.odds(),
    post.probs()[1],
    post.probs()[2],
    post.odds()
  )
  colnames(tab) <- c("p(\\mathcal{H}_0)","p(\\mathcal{H}_1)",
                     paste0("\\text{Prior odds}=\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), ")}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), ")}"),
                     "p(\\mathcal{H}_0|D)","p(\\mathcal{H}_1|D)",
                     paste0("\\text{Posterior odds}=\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), "|D)}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), "|D)}"))
  # addtorow         <- list()
  # addtorow$pos     <- list(-1)
  # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
  
  LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),
                           digits = c(0, rep(3, ncol(tab)))),
                    floating                   = FALSE,
                    tabular.environment        = "array",
                    comment                    = FALSE,
                    print.results              = FALSE,
                    sanitize.colnames.function = identity,
                    include.rownames           = FALSE,  
                    add.to.row                 = list(
                      pos     = as.list(-1),
                      command = "\\rowcolor{lightgray}"
                    )
  )
  tagList(
    #withMathJax(),
    HTML(paste0("$$", LaTeXtab, "$$")),
    tags$script(HTML(js)),
    tags$script(
      async="", 
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$BFint1 <- renderUI({
  outtext <- paste0(
    HTML("&nbsp;&nbsp;&nbsp;"), em("$BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$: The observed data are $", round(BF(), 2), "$ times more likely in case $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$", " is true than if ", "$\\mathcal{H}_{", substr(input$BF10.01, 4, 4), "}$", " is true."), 
    br(), br(), 
    "The pie chart below gives a visual idea of the relative likelihood of the <font color=\"#DCA559\">data</font> under either hypothesis.", 
    br(), 
    "This should ", strong("not"), " be interpreted as the relative posterior probability of the <font color=\"#DCA559\">hypotheses</font> $\\mathcal{H}_0$ and $\\mathcal{H}_1$!", 
    br(), 
    "See ", actionLink("intro.tab4b", "Keep in mind", style = "font-weight: bold;"), ", section 1."
  )
  
  tagList(
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$BFint2 <- renderUI({
  outtext <- paste0(
    HTML("&nbsp;&nbsp;&nbsp;"), em("$BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$: The prior odds ", " ( $", round(prior.odds(), 3), "$-to-$1$ in favor of $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$", ") are updated by a factor of $BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$", " in favor of $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$."), 
    br(), br(), 
    "Here, the Bayes factor is interpreted as being the factor updating the relative probability between both hypotheses, in light of the observed data.", 
    br(), 
    "This should ", strong("not"), " be interpreted as the relative posterior probability of the <font color=\"#DCA559\">hypotheses</font> $\\mathcal{H}_0$ and $\\mathcal{H}_1$!", 
    br(), 
    "See ", actionLink("intro.tab4c", "Keep in mind", style = "font-weight: bold;"), ", section 1.", 
    br(), br()
  )
  
  tagList(
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
  
})

output$BFplot1 <- renderPlot({
  th.circle    <- seq(0, 2*pi, length.out = 1024)
  x.circle     <- cos(th.circle)
  y.circle     <- sin(th.circle)
  BF.angles    <- c(BF(), 1) / (BF() + 1) * (2*pi)
  lines.angles <- c((pi - BF.angles[1])/2, pi - (pi - BF.angles[1])/2)
  
  par(mar = c(0, 0, 0, 0))
  plot(NULL, xlim = c(-1, 1), ylim = c(-1.2, 1.2), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1)
  polygon(c(0, cos(seq(lines.angles[1], lines.angles[2], length.out = 1024)), 0), 
          c(0, sin(seq(lines.angles[1], lines.angles[2], length.out = 1024)), 0), 
          col = if (input$BF10.01 == "BF10") "#005E3C1A" else "#DCA5591A", border = NA)
  points(cos(seq(lines.angles[1], lines.angles[2], length.out = 1024)), sin(seq(lines.angles[1], lines.angles[2], length.out = 1024)), 
         xlim = c(-1, 1), ylim = c(-1.2, 1.2), type = "l", lwd = 1.5, 
         ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1, 
         col = if (input$BF10.01 == "BF10") "#005E3C" else "#DCA559")
  polygon(c(0, cos(seq(lines.angles[2], 2*pi+lines.angles[1], length.out = 1024)), 0),
          c(0, sin(seq(lines.angles[2], 2*pi+lines.angles[1], length.out = 1024)), 0),
          col = if (input$BF10.01 == "BF10") "#DCA5591A" else "#005E3C1A", border = NA)
  points(cos(seq(lines.angles[2], 2*pi+lines.angles[1], length.out = 1024)), sin(seq(lines.angles[2], 2*pi+lines.angles[1], length.out = 1024)), 
         xlim = c(-1, 1), ylim = c(-1.2, 1.2), type = "l", lwd = 1.5, 
         ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1, 
         col = if (input$BF10.01 == "BF10") "#DCA559" else "#005E3C")
  segments(c(0, 0), c(0, 0), cos(lines.angles), sin(lines.angles), lwd = 1.5, col = if (input$BF10.01 == "BF10") "#005E3C" else "#DCA559")
  if (input$BF10.01 == "BF10") 
  {
    text(0, 1.1,  expression("P( D | " * H[1] * " )"), cex = 1.5, col = "#005E3C", font=2)
    text(0, -1.1, expression("P( D | " * H[0] * " )"), cex = 1.5, col = "#DCA559", font=2)
  } else 
  {
    text(0, 1.1,  expression("P( D | " * H[0] * " )"), cex = 1.5, col = "#DCA559", font=2)
    text(0, -1.1, expression("P( D | " * H[1] * " )"), cex = 1.5, col = "#005E3C", font=2)
  } 
})

output$BFplot2 <- renderPlot({
  layout(matrix(c(0, 1, 1, 1, 2, 2, 3, 3, 3, 0), 1, 10, byrow = TRUE))
  
  # Left plot:
  par(mar = c(3, 7, 2, 0))
  if (input$BF10.01 == "BF10") 
  {
    x.coords <- barplot(c(input$priorprob1/100, input$priorprob0/100), col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2, main = "Prior probabilities", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = c(input$priorprob1/100, input$priorprob0/100), label = c(input$priorprob1/100, input$priorprob0/100), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
  } else 
  {
    x.coords <- barplot(c(input$priorprob0/100, input$priorprob1/100), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2, main = "Prior probability", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = c(input$priorprob0/100, input$priorprob1/100), label = c(input$priorprob0/100, input$priorprob1/100), pos = 3, cex = 2, font=2, col = c("#DCA559", "#005E3C"))
  }
  
  # Middle plot:
  par(mar = c(0, .5, 0, .5))
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
  arrows(0, .5, 1, .5, length = .4, angle = 30, lwd = 5)
  text(.5, .6, "Bayes factor", cex= 2.5)
  
  # Right plot:
  par(mar = c(3, 7, 2, 0))
  if (input$BF10.01 == "BF10") 
  {
    x.coords <- barplot(post.probs()[2:1], col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2, main = "Posterior probabilities", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = post.probs()[2:1], label = round(post.probs()[2:1], 4), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
  } else 
  {
    x.coords <- barplot(post.probs(), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2, main = "Posterior probabilities", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = post.probs(), label = round(post.probs(), 4), pos = 3, cex = 2, font=2, col = c("#DCA559", "#005E3C"))
  }
})

output$BF.formula1 <- renderUI({
  form <- if (input$BF10.01 == "BF10") "$\\boxed{\\Large{BF_{10}=\\frac{\\color{#005E3C}{p(D|\\mathcal{H}_1)}}{\\color{#DCA559}{p(D|\\mathcal{H}_0)}}}}$" else "$\\boxed{\\Large{BF_{01}=\\frac{\\color{#DCA559}{p(D|\\mathcal{H}_0)}}{\\color{#005E3C}{p(D|\\mathcal{H}_1)}}}}$"
  tagList(
    #withMathJax(),
    HTML(form),
    tags$script(HTML(js))
  )
})

output$BF.formula2 <- renderUI({
  form <- if (input$BF10.01 == "BF10") paste0("$\\boxed{\\Large\\frac{\\color{#005E3C}{", input$priorprob1/100, "}}{\\color{#DCA559}{", input$priorprob0/100, "}}\\times BF_{10}=\\frac{\\color{#005E3C}{", round(post.probs()[2], 4), "}}{\\color{#DCA559}{", round(post.probs()[1], 4), "}}}$") else paste0("$\\boxed{\\Large\\frac{\\color{#DCA559}{", input$priorprob0/100, "}}{\\color{#005E3C}{", input$priorprob1/100, "}}\\times BF_{01}=\\frac{\\color{#DCA559}{", round(post.probs()[1], 4), "}}{\\color{#005E3C}{", round(post.probs()[2], 4), "}}}$")
  tagList(
    #withMathJax(),
    HTML(form),
    tags$script(HTML(js))
  )
})
