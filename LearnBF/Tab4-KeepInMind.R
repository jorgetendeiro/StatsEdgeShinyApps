
# KEEP IN MIND tab ----

output$kim.out <- renderUI({
  switch(input$keepinmind, 
         topic1 = {
           outtext <- paste0("$$\\underbrace{\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), ")}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), ")}}_\\text{prior odds}\\times BF_{", substr(input$BF10.01, 3, 4), "}=\\underbrace{\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), "|D)}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), "|D)}}_\\text{posterior odds}$$ The Bayes factor ",  em("updates"), " the prior odds to the posterior odds, in light of the observed data.", br(), "Only if the prior odds are equal to 1 (i.e., if $p(\\mathcal{H}_0) = p(\\mathcal{H}_1) = .50$) do the Bayes factor and the posterior odds coincide.", br(), br(), "See for yourself!", br(), " Try changing the prior probability of either $\\mathcal{H}_0$ or $\\mathcal{H}_1$ using the sliders on the left side menu and see how the Bayes factor and the posterior odds are affected.")
         }, 
         topic2 = {
           outtext <- paste0("In order to compute the Bayes factor, prior distributions (", em("priors"), " for short) are required for all parameters. A prior assigns probability to each possible value of the parameter at hand, ", em("before"), " looking at the data. Priors may be chosen with different goals in mind, for example: ", HTML(renderMarkdown(text = "- To reflect current knowledge.\n- To reflect differing scientific perspectives (e.g., skeptical, liberal, or mainstream).\n- To impose constraints (e.g., to preclude negative variances).\n")), "How one should go about priors is not consensual. ", em("Objective"), " Bayesians suggest that relying on ", em("default"), " priors selected on the basis of specific optimal criteria suffices. Other, ", em("subjective"), " Bayesians argue that priors should be more carefully selected depending on the problem at hand.",  br(), br(), "We suggest that using default priors is clearly helpful, but one should first explore those priors to make sure that they are minimally well calibrated. The true fact is that ", em(" The Bayes factor depends on the priors"), ". Thus, it is important to at least: ", HTML(renderMarkdown(text = "- Visualize the priors and judge whether we are comfortable with the prior allocation of probability to the various values of the parameter.\n- Report in full what priors were used while computing the Bayes factor.\n")), br(), "Below you can see the plot of the prior selected on the left side menu, together with some descriptives. You can use this information to have a better idea about how whether the prior is working as you intended. If not, try tweaking the prior's distribution, location, or scale and reassess.", br(), br())
         }, 
         topic3 = {
           outtext <- paste0("From $BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$ we can conclude that the observed data favor $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$ over $\\mathcal{H}_", substr(input$BF10.01, 4, 4), "$ by a factor of ", round(BF(), 2), "-to-1 in favor of $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$. ", br(), "Here the main point is that the evidence provided by the Bayes factor is ", em("relative"), ".", br(), "That is, $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$ is being explicitly compared to $\\mathcal{H}_", substr(input$BF10.01, 4, 4), "$ and that must be acknowledged.", br(), br(), "Hence, we discourage simpliflied summaries of the type:", br(), "'", em("The results provide evidence "), if (BF() > 1) em("in favor of ") else em("against "), "$\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$' ('against' because $BF_{", substr(input$BF10.01, 3, 4), "}", if (BF() > 1) "\\geq" else "\\leq", "1$).")
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
    paste0("$", input$priorprob0/100, "$"), 
    paste0("$", input$priorprob1/100, "$"), 
    paste0("$\\frac{", if (input$BF10.01 == "BF10") input$priorprob1/100 else input$priorprob0/100, "}{", if (input$BF10.01 == "BF10") input$priorprob0/100 else input$priorprob1/100, "}=", round(prior.odds(), 3), "$"), 
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
    paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"), 
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

output$kim.out.topic2.plot1 <- renderPlot({
  x.supp <- seq(floor(location() - 3.5*scale()), ceiling(location() + 3.5*scale()), length.out = 1024)
  y      <- prior(x.supp)
  
  par(mar = c(4.5, 5, 1.5, .5))
  plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, 1.2*max(y)), ylab = "Density", xlab = "Standardized mean difference", bty = "n",
       las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxs = "i", 
       main = switch(input$prior,
                     "cauchy"    = paste0("Cauchy (location = ", location(), ", scale = ", scale(), ")"),
                     "normal"    = paste0("Normal (mean = ", location(), ", SD = ", scale(), ")"),
                     "t.student" = paste0("Student t (location = ", location(), ", scale = ", scale(), ", df = ", df(), ")")))
  axis(1, at = min(x.supp):max(x.supp))
  # 1SD area:
  x.supp.1SD <- seq(location() - 1*scale(), location() + 1*scale(), length.out = 1024)
  y.1SD      <- prior(x.supp.1SD)
  polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA55966", border = NA)
  # 2SD area:
  x.supp.2SD <- seq(location() - 2*scale(), location() - 1*scale(), length.out = 1024)
  y.2SD      <- prior(x.supp.2SD)
  polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55940", border = NA)
  x.supp.2SD <- seq(location() + 1*scale(), location() + 2*scale(), length.out = 1024)
  y.2SD      <- prior(x.supp.2SD)
  polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55940", border = NA)
  # 3SD area:
  x.supp.3SD <- seq(location() - 3*scale(), location() - 2*scale(), length.out = 1024)
  y.3SD      <- prior(x.supp.3SD)
  polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
  x.supp.3SD <- seq(location() + 2*scale(), location() + 3*scale(), length.out = 1024)
  y.3SD      <- prior(x.supp.3SD)
  polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55940", border = NA)
  # Arrows:
  pct.use <- sapply(1:3, function(n) integrate(function(delta) prior(delta), lower = location() - n * scale(), upper = location() + n * scale())[[1]])
  arrows(location() - 1*scale(), .8*max(y), location() + 1*scale(), .8*max(y), code = 3,
         length = .15, angle = 30, lwd = 1, col = "#005E3C")
  segments(location() - 1*scale(), 0, location() - 1*scale(), .8*max(y), col = "#DCA55966", lwd = 2)
  segments(location() + 1*scale(), 0, location() + 1*scale(), .8*max(y), col = "#DCA55966", lwd = 2)
  arrows(location() - 2*scale(), .5*max(y), location() + 2*scale(), .5*max(y), code = 3,
         length = .15, angle = 30, lwd = 1, col = "#005E3C")
  segments(location() - 2*scale(), 0, location() - 2*scale(), .5*max(y), col = "#DCA55940", lwd = 2)
  segments(location() + 2*scale(), 0, location() + 2*scale(), .5*max(y), col = "#DCA55940", lwd = 2)
  arrows(location() - 3*scale(), .2*max(y), location() + 3*scale(), .2*max(y), code = 3,
         length = .15, angle = 30, lwd = 1, col = "#005E3C")
  segments(location() - 3*scale(), 0, location() - 3*scale(), .2*max(y), col = "#DCA55926", lwd = 2)
  segments(location() + 3*scale(), 0, location() + 3*scale(), .2*max(y), col = "#DCA55926", lwd = 2)
  # Text:
  text(x = location(), y = .8*max(y), paste0(round(100*pct.use[1], 1), "%"), pos = 3)
  text(x = location(), y = .5*max(y), paste0(round(100*pct.use[2], 1), "%"), pos = 3)
  text(x = location(), y = .2*max(y), paste0(round(100*pct.use[3], 1), "%"), pos = 3)
  # Legend:
  legend("topright", legend = c("location \u00B1 1\u00D7 scale", "location \u00B1 2\u00D7 scale", "location \u00B1 3\u00D7 scale"), 
         fill = c("#DCA55966", "#DCA55940", "#DCA55926"), bty = "n")
})

kim.out.topic2.df1.reactive <- renderText({
  dist <- switch(input$prior,
                 "cauchy"    = paste0("$\\text{Cauchy (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                 "normal"    = paste0("$\\text{Normal (location = }", location(), "\\text{, scale = }", scale(), "\\text{)}$"),
                 "t.student" = paste0("$t\\text{-Student (location = }", location(), "\\text{, scale = }", scale(), "\\text{, df = }", df(), "\\text{)}$"))
  if (input$H1hyp == "H1.point") dist <- paste0("$\\text{Spike (all probability on }\\delta=", input$H1pointslide, ")$")
  
  tab <- data.frame(
    dist,
    paste0("$", round(BF(), 3), "$"), 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL
  )
  
  colnames(tab) <- c(
    "$\\text{Prior distribution}$", 
    paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$")
  )
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>%
    row_spec(0, extra_css = "border-top: 2px solid; border-bottom: 1px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "border-bottom: 2px solid; border-top: 1px solid; padding: 3px;") %>%
    kable_styling(full_width = FALSE)
})
output$kim.out.topic2.df1 <- renderUI({
  tagList(
    #withMathJax(),
    HTML(kim.out.topic2.df1.reactive()),
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
    paste0("$BF_{", substr(input$BF10.01, 3, 4), "}$"), 
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
  
  colnames(tab) <- c("$\\text{Classification}$", paste0("$BF_{", substr(input$BF10.01, 3, 4), "} = ", round(BF(), 3), "$"))
  
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
