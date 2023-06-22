
# KEEP IN MIND tab ----

# Left panel, H0 depending on the chosen H1, Bayes:
H0hyp.tab4.reactive <- renderText({ switch(input$H1hyptab4, 
                                           "H1.diff0"    = "$\\mathcal{H}_0:\\delta=0$", 
                                           "H1.larger0"  = "$\\mathcal{H}_0:\\delta\\leq 0$", 
                                           "H1.smaller0" = "$\\mathcal{H}_0:\\delta\\geq 0$", 
                                           "H1.point"    = "$\\mathcal{H}_0:\\delta=0$") })
output$H0hyp.tab4 <- renderUI({
  tagList(
    #withMathJax(),
    HTML(H0hyp.tab4.reactive()),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out <- renderUI({
  switch(input$keepinmind, 
         topic1 = {
           outtext <- paste0("$$\\underbrace{\\frac{p(\\mathcal{H}_", substr(rv$BF10.01, 3, 3), ")}{p(\\mathcal{H}_", substr(rv$BF10.01, 4, 4), ")}}_\\text{prior odds}\\times BF_{", substr(rv$BF10.01, 3, 4), "}=\\underbrace{\\frac{p(\\mathcal{H}_", substr(rv$BF10.01, 3, 3), "|D)}{p(\\mathcal{H}_", substr(rv$BF10.01, 4, 4), "|D)}}_\\text{posterior odds}$$ The Bayes factor ",  em("updates"), " the prior odds to the posterior odds, in light of the observed data.", br(), "Only if the prior odds are equal to 1 (i.e., if $p(\\mathcal{H}_0) = p(\\mathcal{H}_1) = .50$) do the Bayes factor and the posterior odds coincide.", br(), br(), "See for yourself!", br(), " Try changing the prior probability of either $\\mathcal{H}_0$ or $\\mathcal{H}_1$ using the sliders on the left side menu and see how the Bayes factor and the posterior odds are affected.")
         }, 
         topic2 = {
           outtext <- paste0("In order to compute the Bayes factor, prior distributions (", em("priors"), " for short) are required for all parameters.", br(), "A prior assigns probability to each possible value of the parameter at hand, ", em("before"), " looking at the data.", br(), "Priors may be chosen with different goals in mind, for example: ", br(), br(), HTML(renderMarkdown(text = "- To reflect current knowledge.\n- To reflect differing scientific perspectives (e.g., skeptical, liberal, or mainstream).\n- To impose constraints (e.g., to preclude negative variances).\n")), "How one should go about priors is not consensual. ", br(), em("Objective"), " Bayesians suggest that relying on ", em("default"), " priors selected on the basis of specific optimal criteria suffices.", br(), "Other, ", em("subjective"), " Bayesians argue that priors should be more carefully selected depending on the problem at hand.",  br(), br(), "We suggest that using default priors is clearly helpful, but one should first explore those priors to make sure that they are minimally well calibrated.", br(),"The true fact is that ", em(" the Bayes factor depends on the priors."), br(), "Thus, it is important to at least: ", br(), br(), HTML(renderMarkdown(text = "- Visualize the priors and judge whether we are comfortable with the prior allocation of probability to the various values of the parameter.\n- Report in full what priors were used while computing the Bayes factor.\n")), br(), "Below you can see the plots of the priors selected for $\\delta$ on the left side menu.", br(), "You can use this information to have a better idea about how whether the prior is working as you intended.", br(), "We highlighted the prior probability of standardized effects sizes of magnitude at most $0.2$, $0.5$, and $0.8$ for reference (in relation to Cohen's commonly used guidelines).", br(), "Try tweaking the prior's distribution, location, or scale and reassess.", br(), br())
         }, 
         topic3 = {
           outtext <- paste0("From $BF_{", substr(rv$BF10.01, 3, 4), "}=", round(BF(), 2), "$ we can conclude that the observed data favor $\\mathcal{H}_", substr(rv$BF10.01, 3, 3), "$ over $\\mathcal{H}_", substr(rv$BF10.01, 4, 4), "$ by a factor of ", round(BF(), 2), "-to-1 in favor of $\\mathcal{H}_", substr(rv$BF10.01, 3, 3), "$. ", br(), "Here the main point is that the evidence provided by the Bayes factor is ", em("relative"), ".", br(), "That is, $\\mathcal{H}_", substr(rv$BF10.01, 3, 3), "$ is being explicitly compared to $\\mathcal{H}_", substr(rv$BF10.01, 4, 4), "$ and that must be acknowledged.", br(), br(), "Hence, we discourage simpliflied summaries of the type:", br(), "'", em("The results provide evidence "), if (BF() > 1) em("in favor of ") else em("against "), "$\\mathcal{H}_", substr(rv$BF10.01, 3, 3), "$' ('against' because $BF_{", substr(rv$BF10.01, 3, 4), "}", if (BF() > 1) "\\geq" else "\\leq", "1$).")
         }, 
         topic4 = {
           outtext <- paste0("We often take a hypothesis such as $\\mathcal{H}_0:\\mu_1=\\mu_2$ to stand for the ", em("absence"), " of an effect (here, a difference between the two population means), and a hypothesis such as $\\mathcal{H}_1:\\mu_1\\not=\\mu_2$ to stand for the ", em("presence"), " of an effect.", br(), "Furthermore, practitioners seem to be often tempted to use the Bayes factor to establish the presence (or lack thereof) of such an effect.", br(), br(), "As it happens, there seems to be a lot of misunderstanding going on here.", br(), "Do notice the following:", br(), br(), HTML(renderMarkdown(text = "1. We should not confuse a _research_ hypothesis with a _statistical_ hypothesis.<br>A _research_ hypothesis is a scientific claim. It reflects a theory that we wish to challenge.<br>A _statistical_ hypothesis, on the other hand, is a precise mathematical statement that should reflect some property of the population, assuming the research hypothesis were in fact correct.<br>As it happens, a theory such as 'an effect is absent' is a _research_ hypothesis, whereas a null hypothesis is only a _statistical_ hypothesis.<br>We cannot really test research hypotheses directly simply because we do not have the ability to fully understand all the intricacies of the real world problem under study. Statistical hypotheses are an easy surrogate for research hypotheses.<br>On its own, this distinction between research and statistical hypotheses should preclude researchers from attempting to use _p_ values or Bayes factors as a tool to _establish_ the presence or absence of an effect. Much more modestly, all we should derive from hypotheses testing is relative evidence between two competing hypotheses.<br><br>  \n2. The Bayes factor is only a number.<br> It would be quite strange to expect that from one sample-based number one could go as far as establishing that a theory essentially holds.\n")), br(), "We strong suggest that special care is taken when choosing the wording used to report findings. For example, it is best to avoid saying something like '(...) from the test we conclude that there is no effect ($BF_{01} = 11.2$)' or even '(...) we found an effect between both groups ($BF_{10}=11.2$)'.")
         }, 
         topic5 = {
           outtext <- paste0("The Bayes factor is ", em("not"), " an effect size measure. This can be easily checked by manipulating some inputs on the left-side menu, as follows:", HTML(renderMarkdown(text = "- Make sure that the two group means are different from each other, even if only by 0.1.\n- Try increasing the sample size of both groups.\n")), "You can compare the value of the Bayes factor to that of Cohen's $d$ (which here is given by $d=\\frac{\\overline{X}_1-\\overline{X}_2}{\\sqrt{(\\hat{\\sigma}_1^2+\\hat{\\sigma}_2^2)/2}}$):")
         }, 
         topic6 = {
           outtext <- paste0("A Bayes factor close to 1 implies that the observed data are about equally likely unde either $\\mathcal{H}_0$ or $\\mathcal{H}_1$.", br(), "In other words, the observed data do not help to distinguish between the predictive ability of the two competing hypotheses.", br(), br(), "In such cases, we should not make the mistake of concluding that there is evidence in favor of the 'no effect' null model. The fallacy would be of reasoning something like this: 'Since the test outcome is inconclusive, then maybe the null hypothesis holds after all'. The common fallacy of drawing support in favor of $\\mathcal{H}_0$ from a nonsignificant frequentist test result is a good analogy here.", br(), br(), "In short: From inconclusive evidence (i.e., Bayes factor of about 1) one should not infer that there is evidence of absence (i.e., $\\mathcal{H}_0$ is more supported than $\\mathcal{H}_1$).")
         }, 
         topic7 = {
           outtext <- paste0("The Bayes factor is just a non-negative real number. How to ", em("interpret"), " this number is not trivial.", br(), "For example, what values of $BF_{10}$ should be considered as weak, moderate, or strong evidence in favor of $\\mathcal{H}_1$ over $\\mathcal{H}_0$?", br(), br(), "Several qualitative classification systems do exist; below you can choose among three popular options:")
         }
  )
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic7.part2 <- renderUI({
  outtext <- paste0("Using descriptive labels as shown above is not without difficulties.", br(), "For example, the strength of evidence expressed in $BF_{10}=5$ could be qualified as either 'substantial', 'positive', or 'moderate', depending on which classification system one considers.", br(), "This is cumbersome since these labels are not necessarily semantically equivalent.", br(), br(), "You can see how the label assigned to the particular Bayes factor that you found may vary between the three classification systems shown above:")
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic7.part3 <- renderUI({
  outtext <- paste0("Furthermore, the discretization shown in the tables above is rather arbitrary. There is nothing really special about the values shown other than some sort of practical convenience.", br(), br(), "Finally, what some consider to be 'substantial' may be deemed uninsteresting or perhaps very relevant by others, depending on the research field or the problem under consideration. For instance, a Bayes factor as little as 4 or 5 may be very relevant in case it pertains to relative evidence is favor of a completely new phenomenon, whereas the same Bayes factor may be considered uninsteresting if it reflects support for a widely established theory.", br(), br(), "In general, we caution against a mechanical use of such labels.", br(), "Instead, we favor explaining the amount of evidence displayed by the Bayes factor in the context of the research being conducted.")
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

kim.out.topic1.df1.reactive <- renderText({
  tab <- data.frame(
    paste0("$", rv$priorprob0/100, "$"), 
    paste0("$", rv$priorprob1/100, "$"), 
    paste0("$\\frac{", if (rv$BF10.01 == "BF10") rv$priorprob1/100 else rv$priorprob0/100, "}{", if (rv$BF10.01 == "BF10") rv$priorprob0/100 else rv$priorprob1/100, "}=", round(prior.odds(), 3), "$"), 
    "", 
    paste0("$", round(BF(), 3), "$"), 
    "", 
    paste0("$", round(post.odds(), 3), "$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL
  )
  
  colnames(tab) <- c(
    "$p(\\mathcal{H}_0)$", 
    "$p(\\mathcal{H}_1)$",
    "$\\text{Prior odds}$",
    " ", 
    paste0("$BF_{", substr(rv$BF10.01, 3, 4), "}$"), 
    " ", 
    "$\\text{Posterior odds}$"
  )
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    add_header_above(c("$\\text{A priori}$" = 3, "", "", "", "$\\text{A posteriori}$" = 1), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE) %>%
    row_spec(0, extra_css = "border-top: 2px solid; border-bottom: 1px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid; padding: 3px;") %>% 
    kable_styling(full_width = FALSE)
})

output$kim.out.topic1.df1 <- renderUI({
  tagList(
    #withMathJax(),
    HTML(kim.out.topic1.df1.reactive()),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic1.part2 <- renderUI({
  outtext <- paste0(HTML("&nbsp;&nbsp;&nbsp;"), em("$BF_{", substr(rv$BF10.01, 3, 4), "}=", round(BF(), 3), "$:", br(), " The ", HTML("<font color=\"#DCA559\"><b>observed data</b></font>"), " are $", round(BF(), 2), "$ times more likely in case $\\mathcal{H}_{", substr(rv$BF10.01, 3, 3), "}$", " is true than if ", "$\\mathcal{H}_{", substr(rv$BF10.01, 4, 4), "}$", " is true."), 
                    br(), br(), 
                    "This is an interpretation about the relative probability of the ", HTML("<font color=\"#DCA559\"><b>data</b></font>"), "!", 
                    br(), br(), br(), 
                    HTML("&nbsp;&nbsp;&nbsp;"), em("Posterior odds = $", round(post.odds(), 3), "$:", br(), " $\\textcolor{#DCA559}{\\mathbf{\\mathcal{H}_{", substr(rv$BF10.01, 3, 3), "}}}$ is $", round(post.odds(), 2), "$ times more likely than $\\textcolor{#DCA559}{\\mathbf{\\mathcal{H}_{", substr(rv$BF10.01, 4, 4), "}}}$, in light of the observed data."), 
                    br(), br(), 
                    "This is an interpretation about the relative probability of the ", HTML("<font color=\"#DCA559\"><b>hypotheses</b></font>"), "!", 
                    br(), br(), 
                    h4("Visual display"),
                    "You can see how both the Bayes factor and the posterior odds relate as we manipulate the prior odds:"
  )
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic1.plot1 <- renderPlot({
  prior.prob.H0.t1 <- seq(.01, .99, by = .01)
  prior.odds.t1    <- if (rv$BF10.01 == "BF10") (1 - prior.prob.H0.t1) / prior.prob.H0.t1 else prior.prob.H0.t1 / (1 - prior.prob.H0.t1)
  post.odds.t1     <- prior.odds.t1 * BF()
  y.lims.log       <- sort( c(floor(log(min(post.odds.t1), 10)), ceiling(log(max(post.odds.t1), 10))) )
  
  par(mar = c(4, 5.5, 2, 1))
  plot(NULL, xlim= c (0, 1), ylim = 10^y.lims.log, log = "y", las = 1, bty = "n", yaxs = "i", xaxs = "i", 
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  abline(v = .5, lty = 3, col = "gray")
  abline(h = BF(), lwd = 2, col = "#DCA559")
  points(prior.prob.H0.t1[1:(100*input$priorH0.kim.t1)], 
         post.odds.t1[1:(100*input$priorH0.kim.t1)], type = "l", col = "#005E3C", lwd = 2)
  axis(1, at = seq(0, 1, by = .25), las = 1)
  axis(2, at = 10^(y.lims.log[1]:y.lims.log[2]), las = 1, labels = prettyNum(10^(y.lims.log[1]:y.lims.log[2]), scientific = FALSE, digits = 16))
  mtext(expression("Prior probability of " * H[0]), 1, 2.5)
  if (rv$BF10.01 == "BF10") mtext(expression("Posterior odds"["10"]*" (log scale)"), 2, 3) else mtext(expression("Posterior odds"["01"]*" (log scale)"), 2, 3)
  legend.text <- if (rv$BF10.01 == "BF10") bquote("BF"["10"] * " = " * .(round(BF(), 3))) else bquote("BF"["01"] * " = " * .(round(BF(), 3)))
  legend("bottomright", legend = legend.text, lwd = 2, col = "#DCA559", 
         inset=c(0,1), xpd = TRUE, horiz = TRUE, bty = "n", seg.len = 4)
})

output$kim.out.topic2.plot1 <- renderPlot({
  layout(matrix(c(1, 2), 1, 2, byrow = TRUE))
  
  # Left plot:
  if (rv$H1hyptab4 %in% c("H1.diff0", "H1.point"))
  {
    par(mar = c(4, 4, 1.5, .5))
    plot(NULL, xlim = c(-2, 2), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
         cex.axis = 2, main = expression("Prior for " * delta * "  under " * H[0]), cex.main = 1.2, font.main = 1)
    axis(1, c(-2, 0, 2), c("", "0", ""))
    axis(2, c(0, 1), las = 1)
    segments(0, 0, 0, 1, lty = 2, col = "gray")
    points(0, 1, pch = 16, cex = 2, col = "#005E3C")
    segments(-2, 0, 2, 0, lty = 1, col = "#005E3C", lwd = 2)
    points(0, 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    mtext("Probability", 2, 2, cex = 1.2)
  } else if (rv$H1hyptab4 == "H1.larger0")
  {
    x.abs  <- max(abs(c(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()))))
    x.supp <- seq(-x.abs, 0, length.out = 1024)
    y.area <- integrate(function(delta) prior(delta), lower = -Inf, upper = 0)[[1]]
    y      <- prior(x.supp) / y.area
    y.max  <- max(prior(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(-x.abs, 0), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(rv$priortab4,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[0] * " (Cauchy-)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[0] * " (Normal-)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[0] * " (" * italic(t) * "-Student-)")))
    axis(1, at = c(min(x.supp):0, c(-.8, -.5, -.2)), labels = FALSE)
    text(c(min(x.supp):0, c(-.8, -.5, -.2)), par("usr")[3]-.04*y.max, srt = 60, adj = 1, xpd = TRUE, cex = 1, 
         labels = c(min(x.supp):0, c(-.8, -.5, -.2)))
    # axis(4, las = 1)
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    # mtext("Density", 4, 3, cex = 1.5)
    # polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
    # 1SD area:
    x.supp.1SD <- seq(-.2, 0, length.out = 1024)
    y.1SD      <- prior(x.supp.1SD) / y.area
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA559CC", border = NA)
    # 2SD area:
    x.supp.2SD <- seq(-.5, -.2, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD) / y.area
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    # 3SD area:
    x.supp.3SD <- seq(-.8, -.5, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD) / y.area
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # Arrows:
    den <- integrate(function(x) prior(x), -Inf, 0)[[1]]
    pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta)/den, lower = c(-.2, -.5, -.8)[n], upper = 0)[[1]])
    arrows(-.2, .8*y.max, 0, .8*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.2, 0, -.2, .8*y.max, col = "#DCA559CC", lwd = 2)
    segments(0, 0, 0, .8*y.max, col = "#DCA559CC", lwd = 2)
    arrows(-.5, .5*y.max, 0, .5*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.5, 0, -.5, .5*y.max, col = "#DCA55980", lwd = 2)
    segments(0, 0, 0, .5*y.max, col = "#DCA55980", lwd = 2)
    arrows(-.8, .2*y.max, 0, .2*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.8, 0, -.8, .2*y.max, col = "#DCA55940", lwd = 2)
    segments(0, 0, 0, .2*y.max, col = "#DCA55940", lwd = 2)
    # Text:
    text(x = -.2/2, y = .8*y.max, paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = -.5/2, y = .5*y.max, paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = -.8/2, y = .2*y.max, paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topleft", legend = c("\u20130.2 < " ~ delta ~ " < 0", "\u20130.5 < " ~ delta ~ " < 0", "\u20130.8 < " ~ delta ~ " < 0"), 
           fill = c("#DCA559CC", "#DCA55980", "#DCA55940"), border = c("#DCA559CC", "#DCA55980", "#DCA55940"), bty = "n")
  } else 
  {
    x.abs  <- max(abs(c(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()))))
    x.supp <- seq(0, x.abs, length.out = 1024)
    y.area <- integrate(function(delta) prior(delta), lower = 0, upper = Inf)[[1]]
    y      <- prior(x.supp) / y.area
    y.max  <- max(prior(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(0, x.abs), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(rv$priortab4,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[0] * " (Cauchy+)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[0] * " (Normal+)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[0] * " (" * italic(t) * "-Student+)")))
    axis(1, at = c(0:max(x.supp), c(.2, .5, .8)), labels = FALSE)
    text(c(0:max(x.supp), c(.2, .5, .8)), par("usr")[3]-.04*y.max, srt = 60, adj = 1, xpd = TRUE, cex = 1, 
         labels = c(0:max(x.supp), c(.2, .5, .8)))
    # axis(4, las = 1)
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    # mtext("Density", 4, 3, cex = 1.5)
    # 1SD area:
    x.supp.1SD <- seq(0, .2, length.out = 1024)
    y.1SD      <- prior(x.supp.1SD) / y.area
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA559CC", border = NA)
    # 2SD area:
    x.supp.2SD <- seq(.2, .5, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD) / y.area
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    # 3SD area:
    x.supp.3SD <- seq(.5, .8, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD) / y.area
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # Arrows:
    den <- integrate(function(x) prior(x), 0, Inf)[[1]]
    pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta)/den, lower = 0, upper = c(.2, .5, .8)[n])[[1]])
    arrows(0, .8*y.max, .2, .8*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.2, 0, .2, .8*y.max, col = "#DCA559CC", lwd = 2)
    segments(0, 0, 0, .8*y.max, col = "#DCA559CC", lwd = 2)
    arrows(0, .5*y.max, .5, .5*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.5, 0, .5, .5*y.max, col = "#DCA55980", lwd = 2)
    segments(0, 0, 0, .5*y.max, col = "#DCA55980", lwd = 2)
    arrows(0, .2*y.max, .8, .2*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.8, 0, .8, .2*y.max, col = "#DCA55940", lwd = 2)
    segments(0, 0, 0, .2*y.max, col = "#DCA55940", lwd = 2)
    # Text:
    text(x = .2/2, y = .8*y.max, paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = .5/2, y = .5*y.max, paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = .8/2, y = .2*y.max, paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topleft", legend = c("0 < " ~ delta ~ " < 0.2", "0 < " ~ delta ~ " < 0.5", "0 < " ~ delta ~ " < 0.8"), 
           fill = c("#DCA559CC", "#DCA55980", "#DCA55940"), border = c("#DCA559CC", "#DCA55980", "#DCA55940"), bty = "n")
  }
  
  # Right plot:
  if (rv$H1hyptab4 == "H1.point")
  {
    par(mar = c(4, .5, 1.5, 4.5))
    plot(NULL, xlim = c(-2, 2), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
         cex.axis = 2, main = expression("Prior for " * delta * "  under " * H[1]), cex.main = 1.2, font.main = 1)
    axis(1, c(-2, input$H1pointslide, 2), c("", input$H1pointslide, ""))
    axis(4, c(0, 1), las = 1)
    segments(input$H1pointslide, 0, input$H1pointslide, 1, lty = 2, col = "gray")
    points(input$H1pointslide, 1, pch = 16, cex = 2, col = "#005E3C")
    segments(-2, 0, 2, 0, lty = 1, col = "#005E3C", lwd = 2)
    points(input$H1pointslide, 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    mtext("Probability", 4, 2, cex = 1.2)
  } else if (rv$H1hyptab4 == "H1.diff0")
  {
    x.supp <- seq(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()), length.out = 1024)
    y      <- prior(x.supp)
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, 1.2*max(y)), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(rv$priortab4,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student)")))
    axis(1, at = c(setdiff(min(x.supp):max(x.supp), 0), c(-.8, -.5, -.2, .2, .5, .8)), labels = FALSE)
    text(c(setdiff(min(x.supp):max(x.supp), 0), c(-.8, -.5, -.2, .2, .5, .8)), par("usr")[3]-.02, srt = 60, adj = 1, xpd = TRUE, cex = 1, 
         labels = c(setdiff(min(x.supp):max(x.supp), 0), c(-.8, -.5, -.2, .2, .5, .8)))
    # axis(4, las = 1)
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    # mtext("Density", 4, 3, cex = 1.5)
    # 1SD area:
    x.supp.1SD <- seq(-.2, .2, length.out = 1024)
    y.1SD      <- prior(x.supp.1SD)
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA559CC", border = NA)
    # 2SD area:
    x.supp.2SD <- seq(-.5, -.2, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD)
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    x.supp.2SD <- seq(.2, .5, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD)
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    # 3SD area:
    x.supp.3SD <- seq(-.8, -.5, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD)
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    x.supp.3SD <- seq(.5, .8, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD)
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # Arrows:
    pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta), lower = c(-.2, -.5, -.8)[n], upper = c(.2, .5, .8)[n])[[1]])
    arrows(-.2, .8*max(y), .2, .8*max(y), code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.2, 0, -.2, .8*max(y), col = "#DCA559CC", lwd = 2)
    segments(.2, 0, .2, .8*max(y), col = "#DCA559CC", lwd = 2)
    arrows(-.5, .5*max(y), .5, .5*max(y), code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.5, 0, -.5, .5*max(y), col = "#DCA55980", lwd = 2)
    segments(.5, 0, .5, .5*max(y), col = "#DCA55980", lwd = 2)
    arrows(-.8, .2*max(y), .8, .2*max(y), code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.8, 0, -.8, .2*max(y), col = "#DCA55940", lwd = 2)
    segments(.8, 0, .8, .2*max(y), col = "#DCA55940", lwd = 2)
    # Text:
    text(x = 0, y = .8*max(y), paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = 0, y = .5*max(y), paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = 0, y = .2*max(y), paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topright", legend = c(expression("\u20130.2 < " ~ delta ~ " < 0.2"), "\u20130.5 < " ~ delta ~ " < 0.5", "\u20130.8 < " ~ delta ~ " < 0.8"), 
           fill = c("#DCA559CC", "#DCA55980", "#DCA55940"), border = c("#DCA559CC", "#DCA55980", "#DCA55940"), bty = "n")
  } else if (rv$H1hyptab4 == "H1.smaller0")
  {
    x.abs  <- max(abs(c(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()))))
    x.supp <- seq(-x.abs, 0, length.out = 1024)
    y.area <- integrate(function(delta) prior(delta), lower = -Inf, upper = 0)[[1]]
    y      <- prior(x.supp) / y.area
    y.max  <- max(prior(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(-x.abs, 0), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(rv$priortab4,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy-)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal-)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student-)")))
    axis(1, at = c(min(x.supp):0, c(-.8, -.5, -.2)), labels = FALSE)
    text(c(min(x.supp):0, c(-.8, -.5, -.2)), par("usr")[3]-.04*y.max, srt = 60, adj = 1, xpd = TRUE, cex = 1, 
         labels = c(min(x.supp):0, c(-.8, -.5, -.2)))
    # axis(4, las = 1)
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    # mtext("Density", 4, 3, cex = 1.5)
    # polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
    # 1SD area:
    x.supp.1SD <- seq(-.2, 0, length.out = 1024)
    y.1SD      <- prior(x.supp.1SD) / y.area
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA559CC", border = NA)
    # 2SD area:
    x.supp.2SD <- seq(-.5, -.2, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD) / y.area
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    # 3SD area:
    x.supp.3SD <- seq(-.8, -.5, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD) / y.area
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # Arrows:
    den <- integrate(function(x) prior(x), -Inf, 0)[[1]]
    pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta)/den, lower = c(-.2, -.5, -.8)[n], upper = 0)[[1]])
    arrows(-.2, .8*y.max, 0, .8*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.2, 0, -.2, .8*y.max, col = "#DCA559CC", lwd = 2)
    segments(0, 0, 0, .8*y.max, col = "#DCA559CC", lwd = 2)
    arrows(-.5, .5*y.max, 0, .5*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.5, 0, -.5, .5*y.max, col = "#DCA55980", lwd = 2)
    segments(0, 0, 0, .5*y.max, col = "#DCA55980", lwd = 2)
    arrows(-.8, .2*y.max, 0, .2*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(-.8, 0, -.8, .2*y.max, col = "#DCA55940", lwd = 2)
    segments(0, 0, 0, .2*y.max, col = "#DCA55940", lwd = 2)
    # Text:
    text(x = -.2/2, y = .8*y.max, paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = -.5/2, y = .5*y.max, paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = -.8/2, y = .2*y.max, paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topright", legend = c("\u20130.2 < " ~ delta ~ " < 0", "\u20130.5 < " ~ delta ~ " < 0", "\u20130.8 < " ~ delta ~ " < 0"), 
           fill = c("#DCA559CC", "#DCA55980", "#DCA55940"), border = c("#DCA559CC", "#DCA55980", "#DCA55940"), bty = "n")
  } else 
  {
    x.abs  <- max(abs(c(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()))))
    x.supp <- seq(0, x.abs, length.out = 1024)
    y.area <- integrate(function(delta) prior(delta), lower = 0, upper = Inf)[[1]]
    y      <- prior(x.supp) / y.area
    y.max  <- max(prior(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(0, x.abs), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(rv$priortab4,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy+)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal+)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student+)")))
    axis(1, at = c(0:max(x.supp), c(.2, .5, .8)), labels = FALSE)
    text(c(0:max(x.supp), c(.2, .5, .8)), par("usr")[3]-.04*y.max, srt = 60, adj = 1, xpd = TRUE, cex = 1, 
         labels = c(0:max(x.supp), c(.2, .5, .8)))
    # axis(4, las = 1)
    mtext(expression("Standardized effect size ( " * delta * " )"), 1, 3, cex = 1.2)
    # mtext("Density", 4, 3, cex = 1.5)
    # 1SD area:
    x.supp.1SD <- seq(0, .2, length.out = 1024)
    y.1SD      <- prior(x.supp.1SD) / y.area
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA559CC", border = NA)
    # 2SD area:
    x.supp.2SD <- seq(.2, .5, length.out = 1024)
    y.2SD      <- prior(x.supp.2SD) / y.area
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55980", border = NA)
    # 3SD area:
    x.supp.3SD <- seq(.5, .8, length.out = 1024)
    y.3SD      <- prior(x.supp.3SD) / y.area
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # Arrows:
    den <- integrate(function(x) prior(x), 0, Inf)[[1]]
    pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta)/den, lower = 0, upper = c(.2, .5, .8)[n])[[1]])
    arrows(0, .8*y.max, .2, .8*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.2, 0, .2, .8*y.max, col = "#DCA559CC", lwd = 2)
    segments(0, 0, 0, .8*y.max, col = "#DCA559CC", lwd = 2)
    arrows(0, .5*y.max, .5, .5*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.5, 0, .5, .5*y.max, col = "#DCA55980", lwd = 2)
    segments(0, 0, 0, .5*y.max, col = "#DCA55980", lwd = 2)
    arrows(0, .2*y.max, .8, .2*y.max, code = 3,
           length = .05, angle = 30, lwd = 1, col = "#005E3C")
    segments(.8, 0, .8, .2*y.max, col = "#DCA55940", lwd = 2)
    segments(0, 0, 0, .2*y.max, col = "#DCA55940", lwd = 2)
    # Text:
    text(x = .2/2, y = .8*y.max, paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = .5/2, y = .5*y.max, paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = .8/2, y = .2*y.max, paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topright", legend = c("0 < " ~ delta ~ " < 0.2", "0 < " ~ delta ~ " < 0.5", "0 < " ~ delta ~ " < 0.8"), 
           fill = c("#DCA559CC", "#DCA55980", "#DCA55940"), border = c("#DCA559CC", "#DCA55980", "#DCA55940"), bty = "n")
  }
  
})

kim.out.topic2.df1.reactive <- renderText({
  distH0 <- switch(rv$prior,
                   "cauchy"    = paste0("$\\text{Cauchy}", if (rv$H1hyp == 'H1.larger0') '^-' else if (rv$H1hyp == 'H1.smaller0') '^+', "\\text{ (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                   "normal"    = paste0("$\\text{Normal}", if (rv$H1hyp == 'H1.larger0') '^-' else if (rv$H1hyp == 'H1.smaller0') '^+', "\\text{  (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                   "t.student" = paste0("$t\\text{-Student}", if (rv$H1hyp == 'H1.larger0') '^-' else if (rv$H1hyp == 'H1.smaller0') '^+', "\\text{  (location = }", location(), "\\text{, scale = }", scale(), "\\text{, df = }", df(), "\\text{)}$"))
  if (rv$H1hyp %in% c("H1.diff0", "H1.point")) distH0 <- paste("$\\text{All probability assigned to }\\delta = 0$")
  # 
  distH1 <- switch(rv$prior,
                   "cauchy"    = paste0("$\\text{Cauchy}", if (rv$H1hyp == 'H1.larger0') '^+' else if (rv$H1hyp == 'H1.smaller0') '^-', "\\text{ (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                   "normal"    = paste0("$\\text{Normal}", if (rv$H1hyp == 'H1.larger0') '^+' else if (rv$H1hyp == 'H1.smaller0') '^-', "\\text{  (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                   "t.student" = paste0("$t\\text{-Student}", if (rv$H1hyp == 'H1.larger0') '^+' else if (rv$H1hyp == 'H1.smaller0') '^-', "\\text{  (location = }", location(), "\\text{, scale = }", scale(), "\\text{, df = }", df(), "\\text{)}$"))
  if (rv$H1hyp == "H1.point") distH1 <- paste0("$\\text{All probability assigned to }\\delta = ", rv$H1pointslide, "$")
  
  tab <- data.frame(
    c(
      "$\\textbf{Hypotheses}$",
      "$\\textbf{Prior for }\\delta\\textbf{ under }\\mathcal{H}_0\\hspace{10mm}$", 
      "$\\textbf{Prior for }\\delta\\textbf{ under }\\mathcal{H}_1$"
    ), 
    c(
      paste0(switch(rv$H1hyp,
                    "H1.diff0"    = "$\\mathcal{H}_0: \\delta=0\\quad\\text{vs}\\quad",
                    "H1.larger0"  = "$\\mathcal{H}_0: \\delta\\leq 0\\quad\\text{vs}\\quad",
                    "H1.smaller0" = "$\\mathcal{H}_0: \\delta\\geq 0\\quad\\text{vs}\\quad",
                    "H1.point"    = paste0("$\\mathcal{H}_0: \\delta=0\\quad\\text{vs}\\quad")), 
             switch(rv$H1hyp,
                    "H1.diff0"    = "\\mathcal{H}_1: \\delta\\not=0$",
                    "H1.larger0"  = "\\mathcal{H}_1: \\delta>0$",
                    "H1.smaller0" = "\\mathcal{H}_1: \\delta<0$",
                    "H1.point"    = paste0("\\mathcal{H}_1: \\delta=", rv$H1pointslide, "$"))
      ), 
      distH0, 
      distH1
    ), 
    stringsAsFactors = FALSE, 
    check.names = FALSE)
  if (rv$H1hyp == "H1.diff0") 
  {
    tab <- rbind(tab, 
                 c("", paste0("&#8226; $p(-0.2<\\delta<0.2)=", round(integrate(function(x) prior(x), -.2, .2)[[1]], 3), "$")), 
                 c("", paste0("&#8226; $p(-0.5<\\delta<0.5)=", round(integrate(function(x) prior(x), -.5, .5)[[1]], 3), "$")), 
                 c("", paste0("&#8226; $p(-0.8<\\delta<0.8)=", round(integrate(function(x) prior(x), -.8, .8)[[1]], 3), "$"))
    )
    colnames(tab) <- NULL
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'l') %>%
      row_spec(1, extra_css = "border-top: 2px solid; padding: 3px;") %>%
      row_spec(2, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(3, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(5, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(6, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") %>%
      kable_styling(full_width = FALSE)
  } else if (rv$H1hyp == "H1.point") 
  {
    colnames(tab) <- NULL
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'l') %>%
      row_spec(1, extra_css = "border-top: 2px solid; padding: 3px;") %>%
      row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(3, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") %>%
      kable_styling(full_width = FALSE)
  } else if (rv$H1hyp == "H1.larger0") 
  {
    den.H0 <- integrate(function(x) prior(x), -Inf, 0)[[1]]
    den.H1 <- integrate(function(x) prior(x),  0, Inf)[[1]]
    tab1   <- data.frame(c("", "", "", "", "", ""), 
                         c(paste0("&#8226; $p(-0.2<\\delta<0)=", round(integrate(function(x) prior(x), -.2, 0)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(-0.5<\\delta<0)=", round(integrate(function(x) prior(x), -.5, 0)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(-0.8<\\delta<0)=", round(integrate(function(x) prior(x), -.8, 0)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.2)=", round(integrate(function(x) prior(x), 0, .2)[[1]]/den.H1, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.5)=", round(integrate(function(x) prior(x), 0, .5)[[1]]/den.H1, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.8)=", round(integrate(function(x) prior(x), 0, .8)[[1]]/den.H1, 3), "$")),
                         stringsAsFactors = FALSE, 
                         check.names = FALSE
    )
    colnames(tab1)  <- colnames(tab)
    tab   <- rbind(tab[1:2,], tab1[1:3, ], tab[3, ], tab1[4:6, ])
    colnames(tab) <- NULL
    rownames(tab) <- NULL
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'l') %>%
      row_spec(1, extra_css = "border-top: 2px solid; padding: 3px;") %>%
      row_spec(2, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(5, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(6, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(7, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(8, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(9, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") %>%
      kable_styling(full_width = FALSE)
  } else
  {
    den.H0 <- integrate(function(x) prior(x), 0, Inf)[[1]]
    den.H1 <- integrate(function(x) prior(x), -Inf, 0)[[1]]
    tab1   <- data.frame(c("", "", "", "", "", ""), 
                         c(paste0("&#8226; $p(-0.2<\\delta<0)=", round(integrate(function(x) prior(x), 0, .2)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(-0.5<\\delta<0)=", round(integrate(function(x) prior(x), 0, .5)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(-0.8<\\delta<0)=", round(integrate(function(x) prior(x), 0, .8)[[1]]/den.H0, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.2)=", round(integrate(function(x) prior(x), -.2, 0)[[1]]/den.H1, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.5)=", round(integrate(function(x) prior(x), -.5, 0)[[1]]/den.H1, 3), "$"),
                           paste0("&#8226; $p(0<\\delta<0.8)=", round(integrate(function(x) prior(x), -.8, 0)[[1]]/den.H1, 3), "$")),
                         stringsAsFactors = FALSE, 
                         check.names = FALSE
    )
    colnames(tab1)  <- colnames(tab)
    tab   <- rbind(tab[1:2,], tab1[1:3, ], tab[3, ], tab1[4:6, ])
    colnames(tab) <- NULL
    rownames(tab) <- NULL
    
    tab %>%
      knitr::kable("html", escape = FALSE, align = 'l') %>%
      row_spec(1, extra_css = "border-top: 2px solid; padding: 3px;") %>%
      row_spec(2, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(5, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(6, extra_css = "border-top: 1px solid black; padding: 3px;") %>%
      row_spec(7, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(8, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
      row_spec(9, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") %>%
      kable_styling(full_width = FALSE)
  }
})
output$kim.out.topic2.df1 <- renderUI({
  tagList(
    #withMathJax(),
    HTML(kim.out.topic2.df1.reactive()),
    br(), 
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic5.part2 <- renderUI({
  outtext <- paste0("Do observe that the Bayes factor keeps changing as the sample sizes change. In fact, since the two group means are different, $BF_{10}$ is expected to increase with no bound as the sample sizes increase.", br(), "Cohen's $d$, however, is constant since it does not depend on the groups sample sizes.", br(), br(), "We can also see this in motion.", br(), "Below are the plots of both $BF_{10}$ and Cohen's $d$ as functions of the sample size, for one particular configuration (Group 1: mean = 0.1, SD = 1; Group 2: mean = 0, SD = 1; standard normal prior under $\\mathcal{H}_0$; equal sample size for both groups).", br(), "Try varying the sample size and see how both the Bayes factor and the effect size vary:", br())
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

output$kim.out.topic5.part3 <- renderUI({
  outtext <- paste0(br(), "The main message here is twofold:", HTML(renderMarkdown(text = "- Do not interpret the magnitude of the Bayes factor as the magnitude of the effect size (here, the standardized difference between the group means).\n - When reporting the results, always include some effect size measure together with the test's result. These two pieces of information complement than replace each other.\n")))
  tagList(
    #withMathJax(),
    HTML(outtext),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

kim.out.topic5.df1.reactive <- renderText({
  tab <- data.frame(
    paste0("$", round(BF(), 3), "$"), 
    paste0("$", round(cohen.d(), 3), "$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL)
  
  colnames(tab) <- c(
    paste0("$BF_{", substr(rv$BF10.01, 3, 4), "}$"), 
    paste0("$\\text{Cohen's }d$"))
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    row_spec(0, extra_css = "border-top: 2px solid; border-bottom: 1px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid; padding: 3px;") %>%
    kable_styling(full_width = FALSE)
})
output$kim.out.topic5.df1 <- renderUI({
  tagList(
    HTML(kim.out.topic5.df1.reactive()),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

# Compute things to render both plots in topic 5:
{# Common sample size:
  N.supp  <- seq(50, 5000, by = 50)
  t.vals  <- sapply(N.supp, function(n) t.test.summ(.1, 0, 1, 1, n, n)["t"])
  BF.vals <- sapply(1:length(N.supp), function(i) 1/B01(t.vals[i], N.supp[i], N.supp[i], normal.prior, "H1.diff0", 0, scale = 1))
}

output$kim.out.topic5.plot1 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon/50)], BF.vals[1:(input$Ncommon/50)], type = "l", log = "y", las = 1, bty = "n", yaxs = "i", xaxs = "i", 
       xlim = c(50, 5000), ylim = c(0.1, 10000), col = "#005E3C", lwd = 2, 
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = 10^(-1:4), labels = c("0.1", "1", "10", "100", "1000", "10000"), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression("BF"[10]*" (log scale)"), 2, 3)
})

# Cohen's d is always 0.1:
output$kim.out.topic5.plot2 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon/50)], rep(.1, input$Ncommon/50), type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i", 
       xlim = c(50, 5000), ylim = c(0, .3), col = "#005E3C", lwd = 2, 
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = seq(0, .3, by = .1), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression("Cohen's d"), 2, 3)
})

kim.out.topic7.df1 <- renderText({
  tab <- data.frame(
    c("$1 - 3.2$", "$3.2 - 10$", "$10 - 31.6$", "$31.6 - 100$", "$> 100$"), 
    c("$\\text{Not worth more than a bare mention}$", "$\\text{Substantial}$", "$\\text{Strong}$", "$\\text{Very strong}$", "$\\text{Decisive}$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL)
  
  colnames(tab) <- c("$BF_{10}$", 
                     "$\\text{Strength of evidence against }\\mathcal{H}_0$")
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    add_header_above(c("$\\text{Jeffreys (1961).}$" = 2), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE, align = 'l') %>%
    kable_styling(full_width = FALSE) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "padding: 3px;") %>% 
    row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(5, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") 
})

kim.out.topic7.df2 <- renderText({
  tab <- data.frame(
    c("$1 - 3$", "$3 - 20$", "$20 - 150$", "$> 150$"), 
    c("$\\text{Not worth more than a bare mention}$", "$\\text{Positive}$", "$\\text{Strong}$", "$\\text{Very strong}$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL)
  
  colnames(tab) <- c("$BF_{10}$", 
                     "$\\text{Strength of evidence against }\\mathcal{H}_0$")
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    add_header_above(c("$\\text{Kass and Raftery (1995).}$" = 2), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE, align = 'l') %>%
    kable_styling(full_width = FALSE) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "padding: 3px;") %>% 
    row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(4, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") 
})

kim.out.topic7.df3 <- renderText({
  tab <- data.frame(
    c("$1 - 3$", "$3 - 10$", "$10 - 30$", "$30 - 100$", "$> 100$"), 
    c("$\\text{Anecdotal}$", "$\\text{Moderate}$", "$\\text{Strong}$", "$\\text{Very strong}$", "$\\text{Extreme}$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL)
  
  colnames(tab) <- c("$BF_{10}$", 
                     "$\\text{Strength of evidence against }\\mathcal{H}_0$")
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    add_header_above(c("$\\text{Lee and Wagenmakers (2014).}$" = 2), bold = FALSE, extra_css = "border-bottom: 1px solid black;", line = FALSE, escape = TRUE, align = 'l') %>% 
    kable_styling(full_width = FALSE) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "padding: 3px;") %>% 
    row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(5, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") 
})

output$kim.out.topic7.dfchosen <- renderUI({
  LaTeXtab <- switch(input$BFClassTbl,
                     "Jeffreys (1961)"            = kim.out.topic7.df1(),
                     "Kass and Raftery (1995)"    = kim.out.topic7.df2(),
                     "Lee and Wagenmakers (2014)" = kim.out.topic7.df3())
  
  tagList(
    #withMathJax(),
    HTML(LaTeXtab),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})


kim.out.topic7.df4.reactive <- renderText({
  BF.tmp   <- if (BF() > 1) BF() else 1/BF()
  BF.labs1 <- c("$\\text{Not worth more than a bare mention}$", "$\\text{Substantial}$", "$\\text{Strong}$", "$\\text{Very strong}$", "$\\text{Decisive}$")
  BF.lab1  <- if (BF.tmp <= 3.2) BF.labs1[1] else if (BF.tmp <= 10) BF.labs1[2] else if (BF.tmp <= 31.6) BF.labs1[3] else if (BF.tmp <= 100) BF.labs1[4] else  BF.labs1[5]
  BF.labs2 <- c("$\\text{Not worth more than a bare mention}$", "$\\text{Positive}$", "$\\text{Strong}$", "$\\text{Very strong}$")
  BF.lab2  <- if (BF.tmp <= 3) BF.labs2[1] else if (BF.tmp <= 20) BF.labs2[2] else if (BF.tmp <= 150) BF.labs2[3] else BF.labs2[4]
  BF.labs3 <- c("$\\text{Anecdotal}$", "$\\text{Moderate}$", "$\\text{Strong}$", "$\\text{Very strong}$", "$\\text{Extreme}$")
  BF.lab3  <- if (BF.tmp <= 3) BF.labs3[1] else if (BF.tmp <= 10) BF.labs3[2] else if (BF.tmp <= 30) BF.labs3[3] else if (BF.tmp <= 100) BF.labs3[4] else  BF.labs3[5]
  
  tab      <- data.frame(
    c("$\\text{Jeffreys (1961)}$", "$\\text{Kass and Raftery (1955)}$", "$\\text{Lee and Wagenmakers (2014)}$"), 
    c(BF.lab1, BF.lab2, BF.lab3)
  )
  
  colnames(tab) <- c("$\\text{Classification}$", paste0("$BF_{", substr(rv$BF10.01, 3, 4), "} = ", round(BF(), 3), "$"))
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    row_spec(0, extra_css = "border-top: 2px solid; border-bottom: 1px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "border-top: 1px solid; padding: 3px;") %>% 
    row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>% 
    row_spec(3, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;") %>% 
    kable_styling(full_width = FALSE)
})
output$kim.out.topic7.df4 <- renderUI({
  tagList(
    HTML(kim.out.topic7.df4.reactive()),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})
