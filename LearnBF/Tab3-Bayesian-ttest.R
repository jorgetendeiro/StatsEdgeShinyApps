
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
    BF.tmp <- paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$")
    
    if (input$prior != "t.student")
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
        paste0("$", round(cohen.d(), 3), "$"),
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
                         "$\\text{Cohen's d}$", 
                         "$\\hspace{5mm}$",
                         paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"))
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
        paste0("$", df.t(), "$"), 
        "", 
        paste0("$", round(cohen.d(), 3), "$"),
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
                         "$\\text{df}$", 
                         "$\\hspace{5mm}$", 
                         "$\\text{Cohen's d}$", 
                         "$\\hspace{5mm}$", 
                         paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"))
    }
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'c') %>%
      add_header_above(c("$\\text{Hypotheses}$" = 3, "", "$\\text{Prior for }\\delta\\text{ under }\\mathcal{H}_1$" = if (input$prior == "t.student") 4 else 3, "", "$\\text{Obs. ES}$" = 1, "", "$\\text{Bayes factor}$" = 1), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE) %>%
      row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
      row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid; padding: 3px;") %>%
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
      paste0("$", round(cohen.d(), 3), "$"),
      "",
      paste0("$", formatC(round(BF(), 3), 3, format = "f"), "$"),
      stringsAsFactors = FALSE, 
      check.names = FALSE
    )
    
    colnames(tab) <- c(
      "$\\mathcal{H}_0$", 
      " ", 
      "$\\mathcal{H}_1$", 
      "$\\hspace{5mm}$", 
      "$\\text{Distribution}$", 
      "$\\hspace{5mm}$", 
      "$\\text{Cohen's d}$", 
      "$\\hspace{5mm}$", 
      paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"))
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'c') %>%
      add_header_above(c("$\\text{Hypotheses}$" = 3, "", "$\\text{Prior under }\\mathcal{H}_1$" = 1, "", "$\\text{Obs. ES}$" = 1, "", "$\\text{Bayes factor}$" = 1), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE) %>% 
      row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
      row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid; padding: 3px;") %>% 
      # row_spec(0, bold = TRUE, background = "#005E3C1A") %>% 
      kable_styling(full_width = FALSE)
  }
}

output$BF.df2 <- function() {
  tab <- data.frame(
    if (input$BF10.01 == "BF10") c(paste0("$", input$priorprob1 / 100, "$"), paste0("$", input$priorprob0 / 100, "$")) else c(paste0("$", input$priorprob0 / 100, "$"), paste0("$", input$priorprob1 / 100, "$")),
    if (input$BF10.01 == "BF10") rep(paste0("$\\frac{", input$priorprob1 / 100, "}{", input$priorprob0 / 100, "}=", round(prior.odds(), 3), "$"), 2) else rep(paste0("$\\frac{", input$priorprob0 / 100, "}{", input$priorprob1 / 100, "}=", round(prior.odds(), 3), "$"), 2),
    # c("", ""),
    rep(paste0("$", round(BF(), 3), "$"), 2), 
    # c("", ""),
    if (input$BF10.01 == "BF10") c(paste0("$", round(post.probs()[2], 3), "$"), paste0("$", round(post.probs()[1], 3), "$")) else c(paste0("$", round(post.probs()[1], 3), "$"), paste0("$", round(post.probs()[2], 3), "$")),
    if (input$BF10.01 == "BF10") rep(paste0("$\\frac{", round(post.probs()[2], 3), "}{", round(post.probs()[1], 3), "}=", round(post.odds(), 3), "$"), 2) else rep(paste0("$\\frac{", round(post.probs()[1], 3), "}{", round(post.probs()[2], 3), "}=", round(post.odds(), 3), "$"), 2),
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = if (input$BF10.01 == "BF10") c("$\\mathcal{H}_1$", "$\\mathcal{H}_0$") else c("$\\mathcal{H}_0$", "$\\mathcal{H}_1$")
  )
  
  colnames(tab) <- c(
    "$\\text{Probability}$",
    paste0("$\\text{Prior odds}$"),
    # "$\\hspace{5mm}$",
    paste0("$\\hspace{3mm}BF_{", substr(input$BF10.01, 3, 4), "}\\hspace{3mm}$"), 
    # "$\\hspace{5mm}$",
    "$\\text{Probability}$",
    paste0("$\\text{Posterior odds}$"))
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    add_header_above(c("", "$\\text{A priori}$" = 2, "", "$\\text{A posteriori}$" = 2), bold = FALSE, extra_css = "border-bottom: 2px solid;", line = FALSE, escape = TRUE) %>% 
    kable_styling(full_width = FALSE) %>%
    column_spec(3, extra_css = "border-bottom: 2px solid;") %>%  
    column_spec(4, extra_css = "border-bottom: 2px solid;") %>%  
    column_spec(6, extra_css = "border-bottom: 2px solid;") %>%  
    collapse_rows(columns = c(3, 4, 6)) %>%
    row_spec(0, extra_css = "border-bottom: 1px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "padding: 3px;") %>% 
    row_spec(2, extra_css = "border-bottom: 2px solid; padding: 3px; border-top: 1px solid white")
} 

output$BFint1 <- renderUI({
  outtext <- paste0(
    HTML("&nbsp;&nbsp;&nbsp;"), em("$BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$: The observed data are $", round(BF(), 2), "$ times more likely in case $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$", " is true than if ", "$\\mathcal{H}_{", substr(input$BF10.01, 4, 4), "}$", " is true."), 
    br(), br(), 
    "The pie chart below gives a visual idea of the relative likelihood of the <font color=\"#DCA559\">data</font> under either hypothesis.", 
    br(), br(), 
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
    br(), br(), 
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
    text(0, 1.2,  expression("P( D | " * H[1] * " )"), cex = 1.5, col = "#005E3C", font=2)
    text(0, -1.2, expression("P( D | " * H[0] * " )"), cex = 1.5, col = "#DCA559", font=2)
  } else 
  {
    text(0, 1.2,  expression("P( D | " * H[0] * " )"), cex = 1.5, col = "#DCA559", font=2)
    text(0, -1.2, expression("P( D | " * H[1] * " )"), cex = 1.5, col = "#005E3C", font=2)
  } 
})

output$BFplot2 <- renderPlot({
  layout(matrix(c(1, 1, 1, 2, 2, 3, 3, 3), 1, 8, byrow = TRUE))
  
  # Left plot:
  par(mar = c(3, 7, 2, 0))
  if (input$BF10.01 == "BF10") 
  {
    x.coords <- barplot(c(input$priorprob1/100, input$priorprob0/100), col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2, main = "Prior", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = c(input$priorprob1/100, input$priorprob0/100), label = c(input$priorprob1/100, input$priorprob0/100), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
    mtext("Probability", 2, 4.5, cex = 1.5)
  } else 
  {
    x.coords <- barplot(c(input$priorprob0/100, input$priorprob1/100), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2, main = "Prior", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = c(input$priorprob0/100, input$priorprob1/100), label = c(input$priorprob0/100, input$priorprob1/100), pos = 3, cex = 2, font=2, col = c("#DCA559", "#005E3C"))
    mtext("Probability", 2, 4.5, cex = 1.5)
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
    x.coords <- barplot(post.probs()[2:1], col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2, main = "Posterior", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = post.probs()[2:1], label = round(post.probs()[2:1], 4), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
    mtext("Probability", 2, 4.5, cex = 1.5)
  } else 
  {
    x.coords <- barplot(post.probs(), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2, main = "Posterior", cex.main = 2.5, font.main = 1)
    text(x = x.coords, y = post.probs(), label = round(post.probs(), 4), pos = 3, cex = 2, font=2, col = c("#DCA559", "#005E3C"))
    mtext("Probability", 2, 4.5, cex = 1.5)
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

output$BFplot3 <- renderPlot({
  # To determine the x-axis range, I will consider the peak of the prior (location.c()) and of the posterior (~cohen.d()).
  # In particular, I will extend the plot of H1:delta>0 to below 0 and H1:delta<0 to above zero in case cohen.d() is in the 'wrong' side of 0, as _I want to always show cohen.d() with a vertical line.
  # For the spread beyond the range limits of x, I base it on the SD of the prior, since the prior is usually broader than the posterior.
  
  if (input$H1hyp == "H1.diff0")
  {
    x.supp <- switch(input$prior,
                     "cauchy"    = seq(floor  (min(location.c(), cohen.d()) - 2*scale.c()), 
                                       ceiling(max(location.c(), cohen.d()) + 2*scale.c()), length.out = 1024),
                     "normal"    = seq(floor  (min(location.n(), cohen.d()) - 2*scale.n()), 
                                       ceiling(max(location.n(), cohen.d()) + 2*scale.n()), length.out = 1024),
                     "t.student" = seq(floor  (min(location.t(), cohen.d()) - 2*scale.t()), 
                                       ceiling(max(location.t(), cohen.d()) + 2*scale.t()), length.out = 1024))
    y      <- switch(input$prior, 
                     "cauchy"    = dcauchy(x.supp, location.c(), scale.c()), 
                     "normal"    = dnorm  (x.supp, location.n(), scale.n()), 
                     "t.student" = dst    (x.supp, df.t(), location.t(), scale.t()))
  }
  
  if (input$H1hyp == "H1.larger0")
  {
    x.supp <- switch(input$prior,
                     "cauchy"    = seq(floor  (min(location.c(), cohen.d()) - 2*scale.c()), 
                                       ceiling(max(location.c(), cohen.d()) + 2*scale.c()), length.out = 1024),
                     "normal"    = seq(floor  (min(location.n(), cohen.d()) - 2*scale.n()), 
                                       ceiling(max(location.n(), cohen.d()) + 2*scale.n()), length.out = 1024),
                     "t.student" = seq(floor  (min(location.t(), cohen.d()) - 2*scale.t()), 
                                       ceiling(max(location.t(), cohen.d()) + 2*scale.t()), length.out = 1024))
    y      <- switch(input$prior, 
                     "cauchy"    = sapply(x.supp, function(x) if (x <= 0) 0 else dcauchy(x,         location.c(), scale.c()) / pcauchy(0,         location.c(), scale.c(), lower.tail = FALSE)), 
                     "normal"    = sapply(x.supp, function(x) if (x <= 0) 0 else dnorm  (x,         location.n(), scale.n()) / pnorm  (0,         location.n(), scale.n(), lower.tail = FALSE)), 
                     "t.student" = sapply(x.supp, function(x) if (x <= 0) 0 else dst    (x, df.t(), location.t(), scale.t()) / (1 - pst    (0, df.t(), location.t(), scale.t()))))
  }
  
  if (input$H1hyp == "H1.smaller0")
  {
    x.supp <- switch(input$prior,
                     "cauchy"    = seq(floor  (min(location.c(), cohen.d()) - 2*scale.c()), 
                                       ceiling(max(location.c(), cohen.d()) + 2*scale.c()), length.out = 1024),
                     "normal"    = seq(floor  (min(location.n(), cohen.d()) - 2*scale.n()), 
                                       ceiling(max(location.n(), cohen.d()) + 2*scale.n()), length.out = 1024),
                     "t.student" = seq(floor  (min(location.t(), cohen.d()) - 2*scale.t()), 
                                       ceiling(max(location.t(), cohen.d()) + 2*scale.t()), length.out = 1024))
    y      <- switch(input$prior, 
                     "cauchy"    = sapply(x.supp, function(x) if (x >= 0) 0 else dcauchy(x,         location.c(), scale.c()) / pcauchy(0,         location.c(), scale.c(), lower.tail = TRUE)), 
                     "normal"    = sapply(x.supp, function(x) if (x >= 0) 0 else dnorm  (x,         location.n(), scale.n()) / pnorm  (0,         location.n(), scale.n(), lower.tail = TRUE)), 
                     "t.student" = sapply(x.supp, function(x) if (x >= 0) 0 else dst    (x, df.t(), location.t(), scale.t()) / pst    (0, df.t(), location.t(), scale.t())))
  }
  
  if (input$H1hyp != "H1.point")
  {
    y.output <- reactive({
      switch(input$prior,
             "cauchy"    = post.dlt.H1(dlt.supp = x.supp, t.stat = ttest.res()["t"], n1 = input$n1, n2 = input$n2, prior.dens = cauchy.prior, type.H1 = input$H1hyp, point.H1 = 0, location = location.c(), scale = scale.c()),
             "normal"    = post.dlt.H1(x.supp, ttest.res()["t"], input$n1, input$n2, normal.prior, input$H1hyp, 0, location = location.n(), scale = scale.n()),
             "t.student" = post.dlt.H1(x.supp, ttest.res()["t"], input$n1, input$n2, tstude.prior, input$H1hyp, 0, location = location.t(), scale = scale.t(), df = df.t()))
    })
    y.post    <- y.output()[[1]]
    y.95CI.LB <- y.output()[[2]]
    y.95CI.UB <- y.output()[[3]]
    
    par(mar = c(3.5, 5, .5, .5))
    plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, ceiling(1.2*max(y, y.post))), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 1, xaxt = "n", lty = 2, yaxt = "n",
         cex.main = 1.5, font.main = 1, main = "")
    points(x.supp, y.post, col = "#DCA559", type = "l", lwd = 2)
    axis(1, at = min(x.supp):max(x.supp))
    axis(2, at = seq(0, ceiling(1.2*max(y, y.post)), length.out = 5), las = 1)
    mtext("Density", 2, 3, cex = 1.5)
    mtext(expression(delta), 1, 2.5, cex = 1.5)
    abline(v = cohen.d(), lwd = 2, lty = 4, col = "gray")
    text(paste0("d = ", round(cohen.d(), 3)), x = cohen.d(), y = ceiling(1.2*max(y, y.post)), cex = 1.5, font=1, pos = if (input$H1hyp != "H1.smaller0") 4 else 2)
    segments(y.95CI.LB, 1.10*max(y, y.post), y.95CI.UB, 1.10*max(y, y.post), lwd = 2, col = "#DCA559")
    segments(y.95CI.LB, 1.07*max(y, y.post), y.95CI.LB, 1.13*max(y, y.post), lwd = 2, col = "#DCA559")
    segments(y.95CI.UB, 1.07*max(y, y.post), y.95CI.UB, 1.13*max(y, y.post), lwd = 2, col = "#DCA559")
    text(paste0("(", round(y.95CI.LB, 3), ", ", round(y.95CI.UB, 3), ")"), x = mean(c(y.95CI.LB, y.95CI.UB)), y = 1.1*max(y, y.post), cex = 1.5, font=1, pos = 3, offset = 1)
    y.95CI.LB.ind <- which.min(abs(x.supp - y.95CI.LB))
    y.95CI.UB.ind <- which.min(abs(x.supp - y.95CI.UB))
    polygon(c(x.supp[y.95CI.LB.ind:y.95CI.UB.ind], rev(x.supp[y.95CI.LB.ind:y.95CI.UB.ind])), 
            c(y.post[y.95CI.LB.ind:y.95CI.UB.ind], rep(0, length(y.95CI.LB.ind:y.95CI.UB.ind))), col = "#DCA55966", border = NA)
    segments(y.95CI.LB, y.post[y.95CI.LB.ind], y.95CI.LB, 1.07*max(y, y.post), lty = 2, lwd = 1, col = "#DCA55966")
    segments(y.95CI.UB, y.post[y.95CI.UB.ind], y.95CI.UB, 1.07*max(y, y.post), lty = 2, lwd = 1, col = "#DCA55966")
  } else
  {
    x.supp <- seq(H1pointslide() - 3, H1pointslide() + 3, length.out = 1024)
    par(mar = c(3.5, 5, .5, .5))
    plot(NULL, xlim = c(floor(min(x.supp)), ceiling(max(x.supp))), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
         cex.axis = 2, main = "", cex.main = 1.5, font.main = 1)
    axis(1, floor(min(x.supp)):ceiling(max(x.supp)))
    axis(2, c(0, 1), las = 1)
    segments(H1pointslide(), 0, H1pointslide(), 1, lty = 2, col = "gray")
    points(H1pointslide(), 1, pch = 16, cex = 2, col = "#005E3C")
    segments(floor(x.supp), 0, ceiling(x.supp), 0, lty = 1, col = "#005E3C", lwd = 2)
    points(H1pointslide(), 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
    mtext(expression(delta), 1, 2.5, cex = 1.5)
    abline(v = cohen.d(), lwd = 2)
    text(paste0("d = ", round(cohen.d(), 3)), x = cohen.d(), y = 1, cex = 1.5, font=1, pos = if (cohen.d() < H1pointslide()) 2 else 4)
    mtext("Probability", 2, 3, cex = 1.5)
  }
  
})






































