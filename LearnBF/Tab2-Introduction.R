
# INTRODUCTION tab ----

# Run t-test:
ttest.res <- reactive({
  t.test.summ(input$mean1, input$mean2, input$sd1, input$sd2, input$n1, input$n2)
})

# t-test result:
output$ttest <- function() {
  tab <- data.frame(
    paste0("$", round((input$mean1 - input$mean2) / sqrt((((input$n1-1)*(input$sd1^2)+(input$n2-1)*(input$sd2^2)))/(input$n1+input$n2-2)), 3), "$"), 
    paste0("$", round(ttest.res()["t"], 3), "$"), 
    paste0("$", round(ttest.res()["df"], 3), "$"), 
    if (round(ttest.res()["p"], 3) >= .001) paste0("$", round(ttest.res()["p"], 3), "$") else "<.001", 
    if (ttest.res()["p"] <= input$alpha) "$\\text{Reject }\\mathcal{H}_0$" else "$\\text{Fail to reject }\\mathcal{H}_0$", 
    stringsAsFactors = FALSE, 
    check.names = FALSE, 
    row.names = NULL
  )
  
  colnames(tab) <- c("$\\text{Cohen's }d$", "$t$", "$\\text{df}$", "$p\\text{-value}$", "$\\text{Test decision}$")
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'c') %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid; border-top: 2px solid;", background = "#005E3C1A") %>%
    row_spec(1, extra_css = "border-bottom: 2px solid; padding: 3px;") %>% 
    # row_spec(0, bold = TRUE, background = "#005E3C1A") %>% 
    kable_styling(full_width = FALSE)
  
  
  
  
  
  # LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1), 
  #                          digits = c(0, 3, 3, if (ttest.res()["df"] - round(ttest.res()["df"]) < 1e-12) 0 else 3, 3, 3)
  # ), 
  # floating                   = FALSE,
  # tabular.environment        = "array",
  # comment                    = FALSE,
  # print.results              = FALSE,
  # sanitize.colnames.function = identity,
  # sanitize.text.function     = identity,
  # include.rownames           = FALSE,
  # add.to.row                 = list(
  #   pos     = as.list(c(-1)),
  #   command = "\\rowcolor{lightgray}"
  # )
  # )
  # 
  # tagList(
  #   #withMathJax(),
  #   HTML(paste0("$$", LaTeXtab, "$$")),
  #   tags$script(HTML(js)),
  #   tags$script(
  #     async="", 
  #     src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
  #   )
  # )
  
}

output$ttest.crit <- renderPlot({
  xlimupp <- max(5, ceiling(abs(ttest.res()["t"]))+2)
  par(mar = c(3, 1, 2, 1))
  curve(dt(x, ttest.res()["df"]), from = -xlimupp, to = xlimupp, n = 1024, 
        ylim = c(0, 1.1 * dt(0, ttest.res()["df"])), 
        bty = "n", yaxt = "n", ylab = "", lwd = 2, xaxt = "n", xlab = "", yaxs = "i", 
        main = paste0("Sampling distribution assuming H0 is true = Student's t (", round(ttest.res()["df"], 3), ")"), 
        col = "#005E3C")
  axis(1, seq(-xlimupp, xlimupp, 1))
  mtext("Test statistic", 1, 2)
  x.supp.crit <- seq(qt(1-input$alpha/2, ttest.res()["df"]), xlimupp, length.out = 101)
  polygon(x = c(x.supp.crit, rev(x.supp.crit)),
          y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
          col = "#FF000005", border = NA)
  polygon(x = c(-x.supp.crit, rev(-x.supp.crit)),
          y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
          col = "#FF000005", border = NA)
  polygon(x = c(x.supp.crit, rev(x.supp.crit)), 
          y = c(dt(x.supp.crit, ttest.res()["df"]), rep(0, 101)), 
          col = "#FF00004D", border = NA)
  polygon(x = c(-x.supp.crit, rev(-x.supp.crit)), 
          y = c(dt(-x.supp.crit, ttest.res()["df"]), rep(0, 101)), 
          col = "#FF00004D", border = NA)
  x.supp <- seq(abs(ttest.res()["t"]), xlimupp, length.out = 101)
  polygon(x = c(x.supp, rev(x.supp)), 
          y = c(dt(x.supp, ttest.res()["df"]), rep(0, 101)), 
          col = "#DCA55966", border = NA, density = 10, lwd = 4)
  polygon(x = c(-x.supp, rev(-x.supp)), 
          y = c(dt(-x.supp, ttest.res()["df"]), rep(0, 101)), 
          col = "#DCA55966", border = NA, density = 10, lwd = 4)
  abline(v = ttest.res()["t"], lwd = 2, col = "#DCA559")
  text(ttest.res()["t"], 1.05*dt(0, ttest.res()["df"]), paste0("t = ", round(ttest.res()["t"], 3)), cex = 1.4, pos = if (ttest.res()["t"] < 0) 2 else 4)
  text(if (ttest.res()["t"] < 0) xlimupp else -xlimupp, 1.05*dt(0, ttest.res()["df"]), paste0("Critical region (probability = ", input$alpha, ")"), 
       pos = if (ttest.res()["t"] < 0) 2 else 4, cex = 1.4, col = "#F00000")
})

output$introduction1 <- renderUI({
  outtext <- paste0(
    "Null hypothesis significance testing (NHST in short) is one of the most popular tools currently in use in science for performing statistical inference (ref).", 
    br(), 
    "Concepts such as the null hypothesis ($\\mathcal{H}_0$) and the alternative hypothesis ($\\mathcal{H}_1$), type I and II error rates, significance level, the $p$-value, 'reject' or 'fail to reject' $\\mathcal{H}_0$, all became common buzz words in science.", 
    br(), 
    "We use them all the time in our research and we pretty much feel obliged to include them in our scientific reports.", 
    br(), br(), 
    "Being this as it may be, it has been widely established that NHST and the $p$-value are poorly understood by practitioners (refs).", 
    br(), 
    "This has led to various developments to try to mitigate the problem. The ", em("Bayes factor"), " can be considered one such development.", 
    br(), br(), 
    h4("Null hypothesis test used"), 
    "We focus on one particular hypothesis testing scenario: The ", 
    em("two-sided independent samples $t$-test with equal variances assumed"), 
    ".", 
    br(), 
    "The idea is to use a very simple and familiar testing setting, so that we can allocate most of our attention on the Bayes factor itself.", 
    br(), 
    "All general guidelines discussed for this test extend to about all other Bayes factors currently available.", 
    br(), br(), 
    "Thus, suppose we have two groups, say, A and B.", 
    br(), 
    "It is assumed that the population associated to each group is normally distributed, with mean $\\mu_A$ (group A) and $\\mu_B$ (group B), and that the population standard deviation is the same for both groups (say, $\\sigma$).", 
    br(), 
    "The hypotheses being tested are given by $$\\mathcal{H}_0: \\mu_A = \\mu_B \\qquad\\text{ versus }\\qquad \\mathcal{H}_1: \\mu_A \\not= \\mu_B,$$ or, equivalently, $$\\mathcal{H}_0: \\underset{\\mu_D}{\\underbrace{\\mu_A-\\mu_B}}=0 \\quad \\text{ versus } \\quad \\mathcal{H}_1: \\underset{\\mu_D}{\\underbrace{\\mu_A-\\mu_B}}\\not=0.$$ For the Bayesian independent samples $t$-test used in this app, the parameter being tested is actually a ", em("standardized effect size"), " defined as $\\delta=\\frac{\\mu_D}{\\sigma}$ (Rouder et al., 2009).", 
    br(), 
    "The hypotheses being tested then become: $$\\mathcal{H}_0: \\delta=0 \\quad \\text{ versus } \\quad \\mathcal{H}_1: \\delta\\not=0.$$", 
    br(), br()
  )
  
  tagList(h3(strong("Background")), 
          br(), 
          HTML(outtext), 
          tags$script(HTML(js)), 
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction2a <- renderUI({
  outtext <- paste0(
    h4("An example"), 
    "Let's start by using what we know, which is the classical independent samples $t$-test.", 
    br(), 
    "But we'll need some data!", 
    br(), br(), 
    "Go to the left-side menu, section ", strong("Descriptives"), ".", 
    br(), 
    "Choose the mean and sample size for each group, and the common standard deviation.", 
    br(), br(), 
    "Below is the outcome from running a frequentist two-sided independent samples $t$-test.", 
    br(), 
    "You can also manipulate the test's significance level and see how that affects the test's decision."
  )
  
  tagList(h3(strong("Null hypothesis significance testing (NHST)")), 
          br(),  
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)), 
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction2b <- renderUI({
  outtext <- paste0(
    h4("Interpretation"), 
    "The green curve in the plot above shows the ", em("sampling distribution"), " of the test statistic $t$. This is the distribution of all possible values of $t$ under repeated sampling, assuming that $\\mathcal{H}_0$ were indeed true.", 
    br(), 
    "The ", em("critical region"), " (red color) consists of all the values of $t$ that would lead to rejecting $\\mathcal{H}_0$ at $\\alpha=", round(100*(input$alpha), 3), "\\%$ significance level. The probability of $t$ belonging to the critical region, under repeated sampling under $\\mathcal{H}_0$, is precisely equal to $\\alpha$.", 
    br(), 
    "Finally, the $p$-", em("value"), " (brown color) is the probability of observing a value of $t$ at least as extreme as the one we did observe, assuming $\\mathcal{H}_0$ is true. Since we are running a two-tailed test, both the lower and the upper tail of the sampling distribution are considered.", 
    br(), br(), 
    "In significance testing, we decide to reject $\\mathcal{H}_0$ when the $p$-value is smaller than $\\alpha$ (the result is 'statistically significant') and we fail to reject $\\mathcal{H}_0$ otherwise (the result is not statistically significant).", 
    br(), 
    "In this case, the test result is ",
    if (ttest.res()["p"] > input$alpha) "not ",
    "statistically significant at $", round(100*(input$alpha), 3), "\\%$ significance level ($t = ",
    round(ttest.res()["t"], 3),
    "$, df $ = ",
    if (ttest.res()["df"] - round(ttest.res()["df"]) < 1e-12) round(ttest.res()["df"], 0) else round(ttest.res()["df"], 3), "$, $p ",
    if (round(ttest.res()["p"], 3) <= .001) "< .001" else paste0("=", round(ttest.res()["p"], 3)),
    "$).", 
    br(), br(), 
    em("Conclusion: "), 
    br(), 
    "We ",
    if (round(ttest.res()["p"], 3) > input$alpha) "fail to ",
    "reject the null hypothesis that the population group means are equal to each other.",
    br(), br(), 
    h4("Misconceptions"), 
    "There are a lot of misconceptions related to NHST in general and to the $p$-value in particular.", 
    br(), br(), 
    "Just to mention a few examples (for an extended list see Goodman, 2008; Greenland et al., 2016), do observe that the following interpretations of the $p$-value are ", em("all incorrect:"), 
    br(), br(), 
    HTML(renderMarkdown(text = "1. The probability of \\$\\mathcal{H}_0\\$ being true is equal to \\$p\\$.\n 1. The probability of \\$\\mathcal{H}_1\\$ being true is equal to \\$(1-p)\\$.\n 1. A non-significant test result implies that \\$\\mathcal{H}_0\\$ is true.\n 1. A significant test result implies that \\$\\mathcal{H}_1\\$ is false.\n 1. A non-significant test result implies that the effect size is small.\n 1. A significant test result implies that the effect size is large.\n 1. The probability that a significant test result is a false positive is equal to \\$\\alpha\\$.")), 
    "The main conclusion here is that NHST and its $p$-value are rather elusive and hard to understand concepts.", 
    br(), 
    "This is really problematic since most science relies on hypothesis testing.", 
    br(), 
    "And this has motivated statisticians to search for patches and valid alternatives to the $p$-value.", 
    br(), br(), 
    "One such alternative is, precisely, the ", em("Bayes factor"), ".", br(), br(), 
    h4("References"), 
    "Goodman, S. (2008). A Dirty Dozen: Twelve P-Value Misconceptions. ", em("Seminars in Hematology"), ", ", em("45"), " (3), 135–140. ", a("https://doi.org/10.1053/j.seminhematol.2008.04.003", href="https://doi.org/10.1053/j.seminhematol.2008.04.003", target="_blank"), 
    br(), 
    "Greenland, S., Senn, S. J., Rothman, K. J., Carlin, J. B., Poole, C., Goodman, S. N., & Altman, D. G. (2016). Statistical tests, P values, confidence intervals, and power: A guide to misinterpretations. ", em("European Journal of Epidemiology"), ", ", em("31"), " (4), 337–350. ", a("https://doi.org/10.1007/s10654-016-0149-3", href="https://doi.org/10.1007/s10654-016-0149-3", target="_blank")
  )
  
  tagList(br(),
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction3 <- renderUI({
  outtext <- paste0(
    "Let's denote the observed data by $D$.", 
    br(), br(), 
    "While the $p$-value is defined as 
      $$p\\text{-value} = p(D\\text{ or more extreme data}|\\mathcal{H}_0),$$ 
      the Bayes factor is defined as follows: 
      $$BF_{01} = \\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}.$$ 
      Three immediately apparent differences between the $p$-value and the Bayes factor are clear:", 
    br(), br(), 
    HTML(renderMarkdown(text = "- While the \\$p\\$-value only entertains what happens in case \\$\\mathcal{H}_0\\$ were true, the Bayes factor entertains both scenarios of either \\$\\mathcal{H}_0\\$ or \\$\\mathcal{H}_1\\$ being true.\n - While the \\$p\\$-value involves 'imaginary data' (data _more extreme than the observed data_), the Bayes factor only relies on the observed data.\n - While the \\$p\\$-value is a real number between 0 and 1, the Bayes factor can be any non-negative real number.")), 
    "The Bayes factor offers a means of comparing the ", em("predictive ability"), " of both hypotheses (better: of both statistical models represented by the hypotheses).", 
    br(), 
    "$BF_{01}$ indicates how many times are the observed data more likely under $\\mathcal{H}_0$ in comparison to $\\mathcal{H}_1$.", 
    br(), 
    "$BF_{10} = \\frac{p(D|\\mathcal{H}_1)}{p(D|\\mathcal{H}_0)}$, on the other hand, is equal to $\\frac{1}{BF_{01}}$ and it indicates how many times are the observed data more likely under $\\mathcal{H}_1$ in comparison to $\\mathcal{H}_0$.", 
    br(), br(), 
    h4("Example"), 
    "Suppose we had found that $BF_{10} = 5.2$. Then we may say this:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"The observed data are 5.2 times more likely in case $\\mathcal{H}_1$ were true than if $\\mathcal{H}_0$ were true.\""), 
    br(), br(), 
    "Had we found that $BF_{10} = 0.08$ we could then conclude the following:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"The observed data are $\\frac{1}{0.08}=12.5$ times more likely in case $\\mathcal{H}_0$ were true than if $\\mathcal{H}_1$ were true.\""), 
    br(), br(), 
    "To be thorough, we must supply more information than what we show here.", 
    br(), 
    "This is related to what is actually required to compute a Bayes factor.", 
    br(), 
    "We further explain this when we discuss ", em("prior distributions"), " (", strong("Introduction"), ", section 5)."
  )
  
  tagList(h3(strong("The Bayes factor")), 
          br(), 
          h4("Definition 1: As a ratio of marginal likelihoods"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction4a <- renderUI({
  outtext <- paste0(
    "It can be shown (see the Box below for details) that the following formula holds:", 
    br(), 
    "$$\\underset{\\text{prior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)}}} \\times \\textcolor{#DCA559}{\\underset{BF_{01}}{\\underbrace{\\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}}}} = \\underset{\\text{posterior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)}}}.$$
      This formula recovers expressing the Bayes factor as a ratio of two marginal likelihoods.", 
    br(), 
    "But now the Bayes factor can also be interpreted as ", em("the factor updating the prior odds into the posterior odds."), 
    br(), br(), 
    h4("Prior odds"), 
    "The prior odds reflect the relative initial belief on either hypothesis.", 
    br(), 
    "For example, equal initial belief implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = 1$.", 
    br(), br(), 
    "Or, strong initial belief in $\\mathcal{H}_0$, say 80% for $\\mathcal{H}_0$ and 20% for $\\mathcal{H}_1$, implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{.8}{.2} = 4$.", 
    br(), 
    "In this case, the odds are 4-to-1 in favor of $\\mathcal{H}_0$, before considering the data.", 
    br(), br(), 
    h4("Posterior odds"), 
    "Likewise, the posterior odds reflect the relative belief on either hypothesis, ", em("after"), " looking at the data.", 
    br(), br(), 
    "So, if the results indicate that the posterior probability of $\\mathcal{H}_0$ is equal to 40% (and therefore the posterior probability of $\\mathcal{H}_1$ is equal to 60%), then the posterior odds equal $\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{.4}{.6} = \\frac{2}{3}$.", 
    br(), 
    "In this case, the odds are 2-to-3 in favor of $\\mathcal{H}_1$, after considering the data.", 
    br(), br(), 
    h4("Example"), 
    "The Bayes factor indicates how a rational agent (i.e., one who adheres to basic axioms of probability) updates his or her relative belief on each hypothesis in light of the observed data.", 
    br(), br(), 
    "Suppose we had found that $BF_{01} = 4$. Then we may say this:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"In light of the observed data, one must revise his or her initial relative belief by a factor of 4-to-1 in favor of $\\mathcal{H}_0$.\""), 
    br(), br(), 
    "Observe that this interpretation holds ", em("irrespective of the prior odds"), ":", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- Given equal prior odds (i.e., \\$\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = 1\\$), then the posterior odds are equal to \\$1\\times 4=4\\$<br> (so, \\$p(\\mathcal{H}_0|D)=.8\\$ and \\$p(\\mathcal{H_1}|D)=.2\\$).\n - If the prior odds are equal to \\$\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{3}{2}\\$, then the posterior odds are equal to \\$\\frac{3}{2}\\times 4=6\\$<br> (so, \\$p(\\mathcal{H}_0|D)=\\frac{6}{6+1}=.86\\$ and \\$p(\\mathcal{H_1}|D)=\\frac{1}{6+1}=.14\\$).\n - ...")))
  )
  
  tagList(h3(strong("The Bayes factor")), 
          br(), 
          h4("Definition 2: As an updating factor"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction4b <- renderUI({
  outtext <- paste0(
    "The Bayes factor can be derived from the ", tagList("", a("Bayes theorem", href="https://en.wikipedia.org/wiki/Bayes%27_theorem", target="_blank")), ", $$\\overset{\\text{posterior}}{\\overbrace{p(\\mathcal{H}_i|D)}} = \\frac{\\overset{\\text{prior}}{\\overbrace{p(\\mathcal{H}_i)}}\\ \\overset{\\text{marginal likelihood}}{\\overbrace{p(D|\\mathcal{H}_i)}}}{\\underset{\\text{evidence}}{\\underbrace{p(D)}}}, \\text{ for } i=0, 1.$$ Before proceeding, we need to assume that $\\mathcal{H}_0$ and $\\mathcal{H}_1$ are the only hypotheses of interest.", 
    br(), 
    "In this sense, we act as if $\\mathcal{H}_0$ and $\\mathcal{H}_1$ reflect the only two models that could have generated the observed data.", 
    br(), br(), 
    "Let's decode what we can see in Bayes theorem:", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- \\$p(\\mathcal{H}_0)\\$ and \\$p(\\mathcal{H}_1)\\$ are the so-called _prior probabilities_ of \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$, respectively.<br> These probabilities reflect our initial belief on either hypothesis, before we consider the observed data.<br>Because \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$ are the only hypotheses of interest, these prior probabilities are complementary, that is, they sum to 1: \\$p(\\mathcal{H}_0)+p(\\mathcal{H}_1)=1\\$.<br><br>\n - \\$p(D|\\mathcal{H}_0)\\$ and \\$p(D|\\mathcal{H}_1)\\$ are the _marginal likelihoods_ of the data under either hypothesis.<br> These values reflect the probability of the observed data under either hypothesis.<br><br>\n - \\$p(D)\\$ is known as the _evidence_. It represents the probability of the observed data across both hypotheses.<br> It is just a normalizing constant.<br><br>\n - Finally, \\$p(\\mathcal{H}_0|D)\\$ and \\$p(\\mathcal{H}_1|D)\\$ are the so-called _posterior probabilities_ of \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$, respectively.<br> These probabilities reflect our belief on either hypothesis, _after_ we consider the observed data.<br> Also these probabilities sum to 1: \\$p(\\mathcal{H}_0|D)+p(\\mathcal{H}_1|D)=1\\$."))), 
    br(), 
    "Let's derive the Bayes factor from the Bayes theorem.", 
    br(), 
    "Dividing the equations member by member when $i=0$ and $i=1$ we have that 
      $$\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) \\bigg/ p(D)}{p(\\mathcal{H}_1)p(D|\\mathcal{H}_1) \\bigg/ p(D)}.$$ Do note that $p(D)$ is the same for both hypotheses (namely, $p(D)=p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) + p(\\mathcal{H}_1)p(D|\\mathcal{H}_1)$; this is due to the ", tagList("", a("law of total probability", href="https://en.wikipedia.org/wiki/Law_of_total_probability", target="_blank")), "). We can therefore drop the two $p(D)$ terms in the equation above. Simplifying and rearranging the remaining terms finally leads to 
      $$\\underset{\\text{prior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)}}} \\times \\textcolor{#DCA559}{\\underset{BF_{01}}{\\underbrace{\\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}}}} = \\underset{\\text{posterior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)}}}.$$"
  )
  
  tagList(HTML(outtext), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5a <- renderUI({
  outtext <- paste0(
    "The formula to compute either probability of the Bayes factor, $p(D|\\mathcal{H}_i)$ for $i=0, 1$, is not simple (see Box below for the technical details).", 
    br(), 
    "It involves two steps:", 
    br(), br(), 
    HTML(renderMarkdown(text = "1. We need to choose a <font color=\"#DCA559\"><b>prior distribution</b></font> for each parameter (here we have two parameters: The standardized effect size \\$\\delta\\$ and the common groups standard deviation \\$\\sigma\\$).<br> A prior distribution allocates probability to each possible parameter value, irrespective of what the observed data are.<br> This may be done taking various things into account, for example: Current knowledge, differing scientific perspectives (e.g., skeptical or liberal), or known parameter constraints (e.g., avoid negative variance values 0).<br><br>\n 2. For each hypothesis, we need to compute a weighted sum of the probability of the observed data at each combination of parameter values.<br> The weights are determined by the prior distributions.")), 
    "The quantity $p(D|\\mathcal{H}_i)$ is known as the ", em("marginal likelihood"), " of the observed data under $\\mathcal{H}_i$.", 
    br(), 
    "'Marginal' means 'across all possible parameter values', with weights given by the prior distributions.", 
    br(), br(), 
    "At this point what is important to notice is that choosing prior distributions is not only important; it is crucial.", 
    br(), 
    "Prior distributions are used directly in the computation of the Bayes factor.", 
    br(), 
    "Different prior distributions ultimately instantiate different hypotheses, and this will lead to different Bayes factor values.", 
    br(), br(), 
    "For this reason, it does not suffice to simply say that something like \"$\\mathcal{H}_1$ is the hypothesis that $\\delta\\not=0$\".", 
    br(), 
    "We must further specify, and report, how likely we think each value under $\\mathcal{H}_1$ may be.",
    br(), 
    "In other words, we ", em("must choose"), " prior distributions.",
    br(), br(), 
    h4("Default priors"), 
    "Choosing a prior distribution for each parameter is not trivial. As explained above, we may take different things into account when choosing a prior",
    br(), br(), 
    "To make things easier, most software packages suggest seemingly sensible <font color=\"#DCA559\"><b>default priors</b></font>. Such priors are chosen by taking some idealized properties into consideration (often mathematical reasons). Whether such priors match the requirements of the researcher for each performed test is of course unknown. Only the researcher may be able to answer to such a question.", 
    br(), br(), 
    "We strongly advice that researchers consider the priors they are using. At a bare minimum, vizualizing the priors can be extremely insightful. We will do so below.",
    br(), 
    "Let's discuss the options available for both parameters (standardized effect size $\\delta$ and the standard deviation $\\sigma$).", 
    br(), br(),
    h4(em("Prior for $\\delta$")), 
    "Since $\\mathcal{H}_0: \\delta=0$, there is really only one possible prior for $\\delta$ &#8212; The distribution assigning all probability to the $\\delta=0$ value.", 
    br(), br(), 
    "Choosing a prior for $\\delta$ under $\\mathcal{H}_1:\\delta\\not=0$ is a notoriously more difficult task. There is an infinity of possible distributions that we could choose from.", 
    br(), br(), 
    "On the left-hand side menu under ", strong("Bayesian test"), " we offer three possible priors for $\\delta$, based on the Cauchy, normal, and $t$-Student distribution families.", 
    br(), 
    "Try changing the distribution and their parameters and see how that affects the probability of each value of $\\delta$."
  )
  
  tagList(h3(strong("Priors")), 
          br(), 
          h4("Marginal likelihoods"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5b <- renderUI({
  outtext <- paste0(
    h4(em("Prior for $\\sigma$")), 
    "Parameter $\\sigma$ is common to both hypotheses $\\mathcal{H}_0$ and $\\mathcal{H}_1$"
  )
  
  tagList(HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5c <- renderUI({
  outtext <- paste0(
    "The <font color=\"#DCA559\"><b>marginal likelihood</b></font> for $\\mathcal{H}_i$ is given by $$p(D|\\mathcal{H}_i) = \\int_{\\Theta_i}\\underbrace{p(D|\\theta_i,\\mathcal{H}_i)}_{\\text{likelihood}}\\,\\underbrace{p(\\theta_i|\\mathcal{H}_i)}_{\\text{prior}}d\\theta_i.$$ The first term is the ", em("likelihood function"), " (for the $t$-test, the normal distribution), evaluated for the observed data $D$.", 
    br(), 
    "The second term is the joint ", em("prior distribution"), " for all parameters from the likelihood function (for the Bayesian $t$-test, $\\delta$ and $\\sigma$). ", 
    br(), 
    "The parameters of the likelihood function are jointly denoted by vector $\\theta_i$. Thus, for this $t$-test in particular, $\\theta_i=(\\delta, \\sigma)$.", 
    br(), 
    "And finally, all possible values for the parameters (here, all possible parameter combinations $(\\delta, \\sigma)$) are denoted by $\\Theta_i$.",
    br(), br(), 
    "In simple terms, $p(D|\\mathcal{H}_i)$ is related to the probability of the observed data, taking into account all the uncertainty in the parameter values as dictated by the chosen prior distributions.", 
    br(), br(), 
    "$p(D|\\mathcal{H}_i)$ is actually a <font color=\"#DCA559\"><b>weighted average</b></font> of $p(D|\\theta_i,\\mathcal{H}_i)$, which is related to the probability of the observed data at each combination of parameter values, $\\theta_i$. The weights are determined by $p(\\theta_i|\\mathcal{H}_i)$, the prior distribution for the parameters."
  )
  
  tagList(HTML(outtext), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction6 <- renderUI({
  outtext <- paste0(
    "NHBT and its Bayes factor is not just a simple replacement to NHST and its $p$-value. The Bayes factor and the $p$-value are different statistical quantities, with different properties and allowing for different types of conclusions. For users of the $p$-value who are now trying to transition towards the Bayes factor, this cannot be stressed enough: ", em("The Bayes factor must be learned and appreciated by its own merits and pitfals."), 
    br(), br(), 
    "Here we highlight one clear difference between the $p$-value and the Bayes factor:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("Bayes factors allow providing relative support in favor of $\\mathcal{H}_0$, whereas $p$-values do not."), 
    br(), br(), 
    "Let's assume that $\\mathcal{H}_0$ is ", em("true"), ".", 
    br(), br(), 
    "It is ", tagList("", a("well known", href="https://statproofbook.github.io/P/pval-h0.html", target="_blank")), " that, in this case, the $p$-value is uniformly distributed in the $(0,1)$ interval, across repeated sampling. In other words, the p-value is equally likely to assume any value between 0 and 1. At significance level $\\alpha$, it is therefore expected that the test will turn out non-significant (i.e., $p>\\alpha$) with probability $(1-\\alpha)$, irrespective of the sample size.", 
    br(), 
    "And in case the observed means are ", em("exactly"), " equal to each other, then the $p$-value will be exactly equal to 1, thus the test outcome will always be non-significant.", 
    br(), 
    "Unfortunately, we can not derive any conclusion from a non-significant test result. By definition, the $p$-value is a probability that is computed assuming that $\\mathcal{H}_0$ is true. We cannot both ", em("assume"), " that $\\mathcal{H}_0$ is true and then, conditional on this assumption, derive ", em("support"), " in favor of $\\mathcal{H}_0$!", 
    br(), br(), 
    "The Bayes factor is different from the $p$-value in this regard. ", em("For a given alternative hypothesis $\\mathcal{H}_1$"), ", large values of $BF_{01}$ do provide relative evidence favoring $\\mathcal{H}_0$ over ", em("the particular alternative hypothesis $\\mathcal{H}_1$"), " that was used in the testing procedure.", 
    br(), br(), 
    "The app below gives an idea about how the Bayes factor varies, in an idealized setting. The two group means are fixed and ", em("exactly"), " equal to each other at each sample size value (rather arbitrarily we used a common mean of 0 for both groups). Both group standard deviations are equal to 1, and we use a standard normal prior distribution of the effect size $\\delta$. You can see how the $p$-value and the Bayes factor behave as the sample size increases."
  )
  
  tagList(h3(strong("Supporting $\\mathcal{H}_0$")), 
          br(), 
          HTML(outtext), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

# Compute things to render the plot in intro.topic 6:
{# Common sample size:
  N.supp.it6  <- seq(50, 5000, by = 50)
  t.vals.it6  <- sapply(N.supp.it6, function(n) t.test.summ(0, 0, 1, 1, n, n)["t"])
  p.vals.it6  <- sapply(N.supp.it6, function(n) t.test.summ(0, 0, 1, 1, n, n)["p"])
  BF.vals.it6 <- sapply(1:length(N.supp.it6), function(i) B01(t.vals.it6[i], N.supp.it6[i], N.supp.it6[i], normal.prior, "H1.diff0", 0, scale = 1))
}

output$intro.topic6.plot1 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon.BF.p/50)], BF.vals.it6[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i",
       xlim = c(50, 5000), ylim = c(0, 50),
       col = "#005E3C", lwd = 2,
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = seq(0, 50, 10), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression("BF"["01"] * " (log scale)"), 2, 3)
})

output$intro.topic6.plot2 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon.BF.p/50)], p.vals.it6[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i",
       xlim = c(50, 5000), ylim = c(0, 1.01), col = "#005E3C", lwd = 2,
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = seq(0, 1, by = .2), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression(italic("p") * "-value"), 2, 3)
})

output$intro.topic5.plot1 <- renderPlot({
  layout(matrix(c(1, 2), 1, 2, byrow = TRUE))
  
  # Left plot:
  par(mar = c(2, 4, 1.5, .5))
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
       cex.axis = 2, main = expression("Prior for " * delta * "  under " * H[0]), cex.main = 1.5, font.main = 1)
  axis(1, c(0, .5, 1), c("", "0", ""))
  axis(2, c(0, 1), las = 1)
  segments(.5, 0, .5, 1, lty = 2, col = "gray")
  points(.5, 1, pch = 16, cex = 2, col = "#005E3C")
  segments(0, 0, 1, 0, lty = 1, col = "#005E3C", lwd = 2)
  points(.5, 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
  mtext(expression(delta), 1, 1, at = .95, cex = 1.5)
  mtext("Probability", 2, 2, cex = 1.5)
  
  # Right plot:
  x.supp <- switch(input$prior,
                   "cauchy"    = seq(floor(location.c() - 3.5*scale.c()), ceiling(location.c() + 3.5*scale.c()), length.out = 1024),
                   "normal"    = seq(floor(location.n() - 3.5*scale.n()), ceiling(location.n() + 3.5*scale.n()), length.out = 1024),
                   "t.student" = seq(floor(location.t() - 3.5*scale.t()), ceiling(location.t() + 3.5*scale.t()), length.out = 1024))
  y      <- switch(input$prior, 
                   "cauchy"    = dcauchy(x.supp, location.c(), scale.c()), 
                   "normal"    = dnorm  (x.supp, location.n(), scale.n()), 
                   "t.student" = dst    (x.supp, df.t(), location.t(), scale.t()))
  
  par(mar = c(2, 4, 1.5, .5))
  plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, 1.2*max(y)), ylab = "", xlab = "", bty = "n",
       las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", 
       cex.main = 1.5, font.main = 1, 
       main = switch(input$prior,
                     "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy)"),
                     "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal)"),
                     "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student)")))
  axis(1, at = min(x.supp):max(x.supp))
  mtext("Density", 2, 3, cex = 1.5)
  polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  
})





























