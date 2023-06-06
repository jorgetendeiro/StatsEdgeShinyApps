# Load Shiny ----
library(shiny)
library(shinyWidgets)
library(dplyr)
library(broom)
library(kableExtra)
library(BayesFactor)
library(xtable)
library(DT)
library(markdown)
library(stevemisc)

# Function computing the t-test from summaries ----
t.test.summ <- function(m1, m2, sd1, sd2, n1, n2)
{
  group1 <- scale(1:n1) * sd1 + m1
  group2 <- scale(1:n2) * sd2 + m2
  t.test(x = group1, y = group2)
}
# Load the functions allowing to compute the Bayes factors:
source("R_scripts/BayesFactors.R")

# To edit the LaTeX tables:
js <- "
window.MathJax = {
  loader: {
    load: ['[tex]/colortbl']
  },
  tex: {
    inlineMath: [['$', '$']], 
    displayMath: [['$$', '$$']],
    packages: {'[+]': ['colortbl']}
  }
};
"

# User interface ----
ui <- fluidPage(
  includeCSS("www/mystyle.css"),
  #!#!#!#!
  tags$head(
    tags$script(async = "", 
                src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
    tags$script( HTML(js) )
  ), 
  #!#!#!#!
  
  # Type LaTeX code, including inline $expressions$:
  # withMathJax(),
  # tags$script(
  #   type = "text/x-mathjax-config", 
  #   "MathJax.Hub.Config({
  #             tex2jax: {inlineMath: [['$','$'], ['$$','$$']]}
  #           });"
  # ),
  
  # Style tabs:
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: #C0C0C0; color:white; width: 10VW;}
                  .tabbable > .nav > li[class=active]    > a {background-color: #005E3C; color:white; width: 10VW; font-weight: bold}
                  ")),
  
  titlePanel("Learning about the Bayes factor!"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3(strong("Descriptives")), 
      fluidRow(
        column(width = 6, align = "left", 
               h4("Group A"), 
               
               tags$div(class = "inline", 
                        numericInput(inputId = "mean1",
                                     label   = "Mean:",
                                     min     = -10,
                                     max     = 10,
                                     value   = 0, 
                                     step    = 0.1, 
                                     width = "50%"), 
                        numericInput(inputId = "sd1",
                                     label   = "SD:",
                                     min     = 0,
                                     max     = NA,
                                     value   = 1, 
                                     step    = 0.1, 
                                     width = "50%"), 
                        numericInput(inputId = "n1",
                                     label   = "N:",
                                     min     = 5,
                                     max     = 50,
                                     value   = 30, 
                                     width = "50%")
               )
        ), 
        column(width = 6, align = "left", 
               h4("Group B"), 
               tags$div(class = "inline", 
                        numericInput(inputId = "mean2",
                                     label   = "Mean:",
                                     min     = -10,
                                     max     = 10,
                                     value   = .2, 
                                     step    = 0.1, 
                                     width   = "50%"), 
                        numericInput(inputId = "sd2",
                                     label   = "SD:",
                                     min     = 0,
                                     max     = NA,
                                     value   = 1, 
                                     step    = 0.1, 
                                     width   = "50%"), 
                        numericInput(inputId = "n2",
                                     label   = "N:",
                                     min     = 5,
                                     max     = 50,
                                     value   = 30, 
                                     width   = "50%")
               )
        )
      ), 
      hr(style = "border-top: 2px solid #005E3C;"), 
      h3(strong("Bayesian test")), 
      fluidRow(
        column(width = 6, 
               setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
               sliderInput(inputId = "priorprob0", 
                           label   = "Prior probability H0:", 
                           min     = 0, 
                           max     = 100, 
                           value   = 50, 
                           post    = "%", 
                           ticks   = FALSE, 
                           step    = 5)), 
        column(width = 6, 
               sliderInput(inputId = "priorprob1", 
                           label   = "Prior probability H1:", 
                           min     = 0, 
                           max     = 100, 
                           value   = 50, 
                           post    = "%", 
                           ticks   = FALSE, 
                           step    = 5))
      ), 
      br(), 
      fluidRow(
        column(width = 4, 
               prettyRadioButtons(inputId  = "prior",
                                  label    = "Prior:", 
                                  choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "t-Student" = "t.student"), 
                                  selected = "cauchy", 
                                  inline   = FALSE, 
                                  width    = "100%", 
                                  status   = "success", 
                                  shape    = "round", 
                                  fill     = TRUE
               )
        ), 
        column(width = 8, 
               fluidRow(
                 column(width = 4, uiOutput("prior.param1")), 
                 column(width = 4, uiOutput("prior.param2")), 
                 column(width = 4, uiOutput("prior.param3"))
               )
        )
      ), 
      br(),
      prettyRadioButtons(inputId  = "BF10.01",
                         label    = "Bayes Factor:", 
                         choices  = list("BF10" = "BF10", 
                                         "BF01" = "BF01"), 
                         selected = "BF10", 
                         inline   = TRUE, 
                         width    = "100%", 
                         status   = "success", 
                         shape    = "round", 
                         fill     = TRUE), 
      hr(style = "border-top: 2px solid #005E3C;"), 
      h3(strong("Frequentist test")), 
      fluidRow(
        column(width = 12, 
               tags$div(class = "inline", 
                        numericInput(inputId = "alpha",
                                     label   = "Sig. level:",
                                     min     = 0,
                                     max     = 1, 
                                     step    = .01, 
                                     value   = .05, 
                                     width   = "100%"))
        )
      ), 
      width = 4
    ), 
    
    mainPanel( 
      
      tabsetPanel(
        type = "pills",
        selected = "Introduction", ##### "Instructions", ##### "Keep in mind", ##### "Bayesian t-test",
        tabPanel("Instructions", 
                 uiOutput("instructions")
        ),
        tabPanel("Introduction", 
                 tags$style(
                   ".btn-mycolor        {background-color: #005E3C1A}",
                   ".btn-mycolor.active {background-color: #DCA55966}", 
                   ".btn                {text-align: left}"
                 ), 
                 radioGroupButtons(
                   inputId = "intro",
                   label = "",
                   choices = c("1. Background"                                  = "intro.topic1", 
                               "2. Null hypothesis significance testing (NHST)" = "intro.topic2", 
                               "3. The Bayes factor \u2013 Definition 1"        = "intro.topic3", 
                               "4. The Bayes factor \u2013 Definition 2"        = "intro.topic4", 
                               "5. Supporting $\\mathcal{H}_0$"                 = "intro.topic5"
                   ),
                   direction = "vertical", 
                   status = "mycolor", 
                   checkIcon = list(
                     yes = icon("ok", lib = "glyphicon")
                     
                   )
                 ), 
                 br(), 
                 conditionalPanel("input.intro == 'intro.topic1'", uiOutput("introduction1")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("introduction2a")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("ttest")), 
                 conditionalPanel("input.intro == 'intro.topic2'", plotOutput("ttest.crit")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("introduction2b")), 
                 conditionalPanel("input.intro == 'intro.topic3'", uiOutput("introduction3")), 
                 conditionalPanel("input.intro == 'intro.topic4'", uiOutput("introduction4")), 
                 conditionalPanel("input.intro == 'intro.topic5'", uiOutput("introduction5")), 
                 conditionalPanel("input.intro == 'intro.topic5'", fluidRow(
                   align = "center", 
                   column(3),
                   column(6, align = "center", sliderInput("Ncommon.BF.p", "Sample size:", min = 50, max = 5000, value = 50, step = 50, 
                                                           animate = animationOptions( interval = 100 ), width = '100%')), 
                   column(3)
                 )), 
                 conditionalPanel("input.intro == 'intro.topic5'", fluidRow(
                   column(1),
                   column(5, align = 'center', plotOutput("intro.topic5.plot1")), 
                   column(5, align = 'center', plotOutput("intro.topic5.plot2")), 
                   column(1)))
        ), 
        tabPanel("Bayesian t-test", 
                 br(), 
                 h5(strong(textOutput("Intro.BF.df1"))), 
                 uiOutput("BF.df1"), 
                 br(), 
                 h5(strong("Prior and posterior model probabilities")),
                 uiOutput("BF.df2"), 
                 br(), 
                 h5(strong("Interpretation 1")), 
                 withMathJax(uiOutput("BFint1")), 
                 fluidRow(align = 'center', 
                          column(3), 
                          column(3, align = 'center', plotOutput("BFplot1")), 
                          column(3, align = 'center', tags$style(HTML('
                                        .verticalcenter {
                                        display: table-cell;
                                        height: 400px;
                                        vertical-align: middle;
                                        }')), 
                                 tags$div(class = "verticalcenter", uiOutput("BF.formula1"))), 
                          column(3)),
                 h5(strong("Interpretation 2")), 
                 withMathJax(uiOutput("BFint2")), 
                 br(), 
                 fluidRow(uiOutput("BF.formula2"), align = "center"), 
                 plotOutput("BFplot2")
        ),
        # tabPanel("Classic t-test", 
        #          br(), 
        #          # uiOutput("ttest"), 
        #          br(), 
        #          h5(strong("Interpretation")), 
        #          withMathJax(uiOutput("freqint1"))
        # ),
        tabPanel("Keep in mind",
                 br(), 
                 h5("Below is a list of important things to keep in mind when interpreting the Bayes factor."),
                 br(), 
                 tags$style(
                   ".btn-mycolor        {background-color: #005E3C1A}",
                   ".btn-mycolor.active {background-color: #DCA55966}", 
                   ".btn                {text-align: left}"
                 ), 
                 radioGroupButtons(
                   inputId = "keepinmind",
                   label = "Keep in mind that...",
                   choices = c("1. Bayes factors are not posterior odds!"                                = "topic1", 
                               "2. Priors matter!"                                                       = "topic2", 
                               "3. Bayes factors provide relative assessments!"                          = "topic3", 
                               "4. Bayes factors cannot establish the presence or absence of an effect!" = "topic4", 
                               "5. Bayes factors are not effect sizes!"                                  = "topic5", 
                               "6. Inconclusive evidence is not evidence of absence!"                    = "topic6", 
                               "7. Using description labels is not problem-free!"                        = "topic7"
                   ),
                   direction = "vertical", 
                   status = "mycolor", 
                   checkIcon = list(
                     yes = icon("ok", lib = "glyphicon")
                     
                   )
                 ), 
                 br(), 
                 h3(strong("Explanation")), 
                 uiOutput("kim.out"), 
                 conditionalPanel("input.keepinmind == 'topic1'", uiOutput("kim.out.topic1.df1")), 
                 conditionalPanel("input.keepinmind == 'topic2'", plotOutput("kim.out.topic2.plot1")), 
                 conditionalPanel("input.keepinmind == 'topic2'", uiOutput("kim.out.topic2.df1")), 
                 conditionalPanel("input.keepinmind == 'topic5'", uiOutput("kim.out.topic5.df1")), 
                 conditionalPanel("input.keepinmind == 'topic5'", uiOutput("kim.out.topic5.part2")), 
                 conditionalPanel("input.keepinmind == 'topic5'", fluidRow(
                   align = "center", 
                   column(3),
                   column(6, align = "center", sliderInput("Ncommon", "Sample size:", min = 50, max = 5000, value = 50, step = 50, 
                                                           animate = animationOptions( interval = 100 ), width = '100%')), 
                   column(3)
                 )), 
                 conditionalPanel("input.keepinmind == 'topic5'", fluidRow(
                   column(1),
                   column(5, align = 'center', plotOutput("kim.out.topic5.plot1")), 
                   column(5, align = 'center', plotOutput("kim.out.topic5.plot2")), 
                   column(1))), 
                 conditionalPanel("input.keepinmind == 'topic5'", uiOutput("kim.out.topic5.part3")), 
                 conditionalPanel("input.keepinmind == 'topic7'", fluidRow(column(4, uiOutput("kim.out.topic7.df1")),
                                                                           column(4, uiOutput("kim.out.topic7.df2")),
                                                                           column(4, uiOutput("kim.out.topic7.df3")))),
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.part2")), 
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.df4")),
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.part3"))
                 
                 
        ), 
        tabPanel("Let's practice!", 
                 br(), 
                 # tags$style(
                 #   ".irs--shiny .irs-bar {border-top: 1px solid #428bca; border-bottom: 1px solid #428bca; background: #428bca;}"
                 # ), 
                 # setSliderColor("#DCA559", 1),
                 fluidRow( htmlOutput("practice") ), 
                 "abc")
      ),
      width = 8
    )
    
  )
)

# Server function ----
server <- function(input, output, session) {
  
  # INSTRUCTIONS tab:
  output$instructions <- renderUI({
    outtext1 <- paste0("Welcome to the '", em("Learning about the Bayes factor!"), "' app!", 
                       br(), br(), 
                       "This app was created as a support medium for those interested in learning about the Bayes factor. ")
    outtext2 <- paste0("Practitioners interested in null hypotheses Bayesiantesting")
    
    tagList(br(), 
            HTML(outtext1), 
            br(), br(), 
            h5(strong("Intended users")), 
            br()
    )
  })
  
  # INTRODUCTION tab:
  output$introduction1 <- renderUI({
    outtext <- paste0(
      "Significance testing is arguably the most popular tool currently in use in science for performing statistical inference (ref). Concepts such as the null hypothesis ($\\mathcal{H}_0$) and alternative hypothesis ($\\mathcal{H}_1$), type I and II error rates, the $p$-value, 'reject' or 'fail to reject' $\\mathcal{H}_0$, became common buzz words in science. We use them all the time in our research and we pretty much feel obliged to include them in our scientific reports.", 
      br(), br(), 
      "Being this as it may be, it has been widely established that NHST is poorly understood by practitioners (refs).", 
      br(), 
      "At least to some extent, it has been speculated that one of the main reasons for this sad state of affairs is the fact that NHST, and its $p$-value, does not do what we wish it could do.", 
      br(), 
      "This has led to various developments to try to mitigate the problem. The ", em("Bayes factor"), " can be considered one such development.", 
      br(), br(), 
      "This app aims to offer a practical tutorial over the inner workings of the Bayes factor.", 
      br(), 
      "The app is particularly aimed at all those who are new to Bayesian hypothesis testing and the Bayes factor.", 
      br(), 
      "We tried to keep the technical level of the presentation to a minimum. Our main goal is to understand how to use and interpret the Bayes factor, and not how to compute it (those interested may refer to refs).", 
      br(), br(), 
      "We focus on one particular hypothesis testing scenario: The independent samples $t$-test with equal variances assumed.", 
      br(), 
      "The idea is to use a very simple and familiar testing setting, so that we can allocate most of our attention on the Bayes factor itself.", 
      br(), 
      "All general guidelines discussed for this test extend to about all other Bayes factors currently available.", 
      br(), br(), 
      "Thus, suppose we have two groups, say, A and B. It is assumed that the population associated to each group is normally distributed with means $\\mu_A$ and $\\mu_B$, and that the population standard deviation is the same for both groups (say, $\\sigma$). The hypotheses being tested are given by $$\\mathcal{H}_0: \\mu_A = \\mu_B \\qquad\\text{ versus }\\qquad \\mathcal{H}_1: \\mu_A \\not= \\mu_B,$$ where $\\mu_A$ and $\\mu_B$ are the population means for group A and B, respectively. Equivalently, defining parameter $\\mu_D$ as $(\\mu_A-\\mu_B)$, we can re-express $\\mathcal{H}_0$ and $\\mathcal{H}_1$ as follows: $$\\mathcal{H}_0: \\mu_D=0 \\quad \\text{ versus } \\quad \\mathcal{H}_0: \\mu_D\\not=0.$$ For the Bayesian independent samples $t$-test used in this app (Rouder et al., 2009), the parameter being tested is actually the standardized effect size (i.e., standardized mean difference) defined as $\\delta=\\frac{\\mu_D}{\\sigma}$. The hypotheses being tested then become: $$\\mathcal{H}_0: \\delta=0 \\quad \\text{ versus } \\quad \\mathcal{H}_0: \\delta\\not=0.$$", 
      br(), br(), 
      "Below, we start by quickly revisiting how the independent samples $t$-test is typically performed in classical statistics.", 
      br(), 
      "We then proceed towards using Bayesian statistics and the Bayes factor.", 
      br(), 
      "Feel free to manipulate the inputs on the left-side menu. The results will update accordingly."
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
      "Running a classic NHST independent samples $t$-test leads to the following output (Group A: mean $=", round(input$mean1, 3), "$, SD $=", round(input$sd1, 3), "$, $N=", input$n1, "$; Group B: mean $=", round(input$mean2, 3), "$, SD $=", round(input$sd2, 3), "$, $N=", input$n2, "$):"
    )
    
    tagList(h3(strong("Null hypothesis significance testing (NHST)")), 
            br(),  
            HTML(outtext), 
            tags$script(HTML(js)), 
            tags$script(
              async="",
              src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
            )
    )
  })
  
  output$introduction2b <- renderUI({
    outtext <- paste0(
      "The green curve in the plot above shows the ", em("sampling distribution"), " of the test statistic $t$. This is the distribution of $t$ under repeated sampling, assuming that $\\mathcal{H}_0$ were indeed true.", 
      br(), 
      "The ", em("critical region"), " (red color) consists of all the values of $t$ that would lead to rejecting $\\mathcal{H}_0$ at $", round(100*(input$alpha), 3), "\\%$ significance level. The probability of $t$ belonging to the critical region is precisely equal to $\\alpha$.", 
      br(), 
      "Finally, the $p$-", em("value"), " (brown color) is the probability of observing a value of $t$ at least as extreme as the one we did observe.", 
      br(), br(), 
      "In significance testing, we decide to reject $\\mathcal{H}_0$ when the $p$-value is smaller than $\\alpha$ (the result is 'statistically significant') and we fail to reject $\\mathcal{H}_0$ otherwise (the result is not statistically significant).", 
      br(), 
      "In this case, the test result is ",
      if (ttest.res()$p.value > input$alpha) "not ",
      "statistically significant at $", round(100*(input$alpha), 3), "\\%$ significance level ($t = ",
      round(ttest.res()$statistic, 3),
      "$, df $ = ",
      if (ttest.res()$parameter - round(ttest.res()$parameter) < 1e-12) round(ttest.res()$parameter, 0) else round(ttest.res()$parameter, 3), "$, $p ",
      if (round(ttest.res()$p.value, 3) <= .001) "< .001" else paste0("=", round(ttest.res()$p.value, 3)),
      "$).", 
      br(), 
      "We ",
      if (round(ttest.res()$p.value, 3) > input$alpha) "fail to ",
      "reject the null hypothesis that the population group means are equal to each other.",
      br(), br(),
      "There are a lot of misconceptions related to NHST in general and to the $p$-value in particular (refs). Just to mention a few examples, do observe that all the following interpretations of the $p$-value are ", em("incorrect:"), HTML(renderMarkdown(text = "- The probability of \\$\\mathcal{H}_0\\$ is equal to \\$p\\$.\n- The probability of \\$\\mathcal{H}_1\\$ is equal to \\$(1-p)\\$.\n- A nonsignificant difference (i.e., \\$p>\\alpha\\$) implies that there is no group mean difference.\n- If the result is significant (i.e., \\$p<\\alpha\\$), then probability of a Type I error is equal to \\$\\alpha\\$.")), 
      "Also because of all these misconceptions, valid alternatives to the $p$-value have been entertained. One such alternative is, precisely, the ", em("Bayes factor"), "."
    )
    
    tagList(br(),
            HTML(outtext), 
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
      Two immediately apparent differences between the $p$-value and the Bayes factor are clear:", 
      HTML(renderMarkdown(text = "- While the \\$p\\$-value only entertains what happens in case \\$\\mathcal{H}_0\\$ were true, the Bayes factor entertains both \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$.\n - While the \\$p\\$-value involves 'imaginary data' (data _more extreme than the observed data_), the Bayes factor only relies on the observed data.")), 
      "The Bayes factor offers a means of comparing the ", em("predictive ability"), " of both hypotheses.", 
      br(), 
      "$BF_{01}$ indicates how many times are the observed data more likely under $\\mathcal{H}_0$ in comparison to $\\mathcal{H}_1$.", 
      br(), 
      "$BF_{10} = \\frac{p(D|\\mathcal{H}_1)}{p(D|\\mathcal{H}_0)}$, on the other hand, is equal to $\\frac{1}{BF_{01}}$ and it indicates how many times are the observed data more likely under $\\mathcal{H}_1$ in comparison to $\\mathcal{H}_0$.", 
      br(), br(), 
      "The formula to compute either probability of the Bayes factor is not simple. It involves two steps:", 
      HTML(renderMarkdown(text = "1. We need to choose a _prior distribution_ for each parameter (here we have two parameters: The standardized effect size \\$\\delta\\$ and the common groups standard deviation \\$\\sigma\\$). A prior distribution allocates probability to each possible parameter value, irrespective of what the observed data are. This may be done taking various things into account, for example:\n 2. We need to compute a weighted sum of the probability of the observed data at each combination of parameter values. The weights for each parameter are determined by the prior distributions.")), 
      "The quantity $p(D|\\mathcal{H}_i)$, for $i=0, 1$, is known as the ", em("marginal likelihood"), " of the observed data under $\\mathcal{H}_i$.", 
      br(), 
      "'Marginal' means 'across all possible parameter values', with weights given by the prior distributions."
    )
    
    tagList(h3(strong("The Bayes factor")), 
            br(), 
            h4(strong("Definition 1: As a ratio of marginal likelihoods")), 
            br(), 
            HTML(outtext), 
            tags$script(HTML(js)),
            tags$script(
              async="",
              src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
            )
    )
  })
  
  output$introduction4 <- renderUI({
    outtext <- paste0(
      "The Bayes factor can be derived from the ", tagList("", a("Bayes theorem", href="https://en.wikipedia.org/wiki/Bayes%27_theorem", target="_blank")), ", $$\\overset{\\text{posterior}}{\\overbrace{p(\\mathcal{H}_i|D)}} = \\frac{\\overset{\\text{prior}}{\\overbrace{p(\\mathcal{H}_i)}}\\ \\overset{\\text{likelihood}}{\\overbrace{p(D|\\mathcal{H}_i)}}}{\\underset{\\text{marginal likelihood}}{\\underbrace{p(D)}}}, \\text{ for } i=0, 1.$$ To proceed, we need to assume that $\\mathcal{H}_0$ and $\\mathcal{H}_1$ are the only hypotheses of interest. In this sense, we act as if $\\mathcal{H}_0$ and $\\mathcal{H}_1$ reflect the only two models that could have generated the observed data. A consequence is that the probabilities of both hypotheses are complementary, that is, they sum to 1. This is true both ", em("a priori"), " (i.e., before observing the data: $p(\\mathcal{H}_0)+p(\\mathcal{H}_1)=1$) and ", em("a posteriori"), " (i.e., after observing the data: $p(\\mathcal{H}_0|D)+p(\\mathcal{H}_1|D)=1$).", 
      br(), br(), 
      "Dividing the equations member by member when $i=0$ and $i=1$ we have that 
      $$\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) \\bigg/ p(D)}{p(\\mathcal{H}_1)p(D|\\mathcal{H}_1) \\bigg/ p(D)}.$$ Do note that, due to the fact that we are conditional on $\\mathcal{H}_0$ and $\\mathcal{H}_1$ only, the marginal likelihood, $p(D)$, is the same for both hypotheses (namely, $p(D)=p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) + p(\\mathcal{H}_1)p(D|\\mathcal{H}_1)$; this is due to the ", tagList("", a("law of total probability", href="https://en.wikipedia.org/wiki/Law_of_total_probability", target="_blank")), "). We can therefore drop the two $p(D)$ terms in the equation above. Simplifying and rearranging the remaining terms leads to 
      $$\\underset{\\text{prior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)}}} \\times \\textcolor{#DCA559}{\\underset{BF_{01}}{\\underbrace{\\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}}}} = \\underset{\\text{posterior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)}}}.$$
      We just recovered Definition 1: The Bayes factor as a ratio of two marginal likelihoods.", 
      br(), 
      "But now the Bayes factor can also be interpreted as ", em("the factor updating the prior odds into the posterior odds."), 
      br(), br(), 
      "The prior odds reflect the relative initial belief on either hypothesis.", 
      br(), 
      "For example, equal initial belief implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = 1$.", 
      br(), 
      "Or, strong initial belief in $\\mathcal{H}_0$, say 80% for $\\mathcal{H}_0$ and 20% for $\\mathcal{H}_1$, implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{.8}{.2} = 4$. In this case, the odds are 4-to-1 in favor of $\\mathcal{H}_0$, before considering the data.", 
      br(), br(), 
      "Likewise, the posterior odds reflect the relative belief on either hypothesis, ", em("after"), " looking at the data.", 
      br(), 
      "So, if the results indicate that the posterior probability of $\\mathcal{H}_0$ is equal to 40% (and therefore the posterior probability of $\\mathcal{H}_1$ is equal to 60%), then the posterior odds equal $\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{.4}{.6} = \\frac{2}{3}$. In this case, the odds are 2-to-3 in favor of $\\mathcal{H}_1$, after considering the data.", 
      br(), br(), 
      "The Bayes factor indicates how a rational agent (i.e., one who adheres to basic axioms of probability) updates her relative belief on each hypothesis in light of the observed data. For example, $BF_{01}=5$ means that, ", em("irrespective of the prior odds"), ", one must revise his or her initial relative belief by a factor of 5-to-1 in favor of $\\mathcal{H}_0$."
    
    )
    
    tagList(h3(strong("The Bayes factor")), 
            br(), 
            h4(strong("Definition 2: As an updating factor")), 
            br(), 
            HTML(outtext), 
            tags$script(HTML(js)),
            tags$script(
              async="",
              src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
            )
    )
  })
  
  output$introduction5 <- renderUI({
    outtext <- paste0(
      "NHBT and its Bayes factor is not just a simple replacement to NHST and its $p$-value. The Bayes factor and the $p$-value are different statistical quantities, with different properties and allowing for different types of conclusions. For users of the $p$-value who are now trying to transition towards the Bayes factor, this cannot be stressed enough: ", em("The Bayes factor must be learned and appreciated by its own merits and pitfals."), 
      br(), br(), 
      "Here we highlight one clear difference between the $p$-value and the Bayes factor:", 
      br(), 
      HTML("&nbsp;&nbsp;&nbsp;"), em("Bayes factors allow providing relative support in favor of \\$\\mathcal{H}_0\\$, whereas \\$p\\$-values do not."), 
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
  
  # Compute things to render the plot in intro.topic 5:
  {# Common sample size:
    N.supp <- seq(50, 5000, by = 50)
    
    # BF and p-value per sample size:
    BF.val.it5 <- rep(NA, length(N.supp))
    p.val.it5  <- rep(NA, length(N.supp))
    for (i in 1:length(N.supp))
    {
      group1     <- scale(rnorm(N.supp[i]))
      group2     <- scale(rnorm(N.supp[i]))
      # 
      ttest.tmp     <- t.test(group1, group2)
      t.tmp         <- ttest.tmp$statistic
      BF.val.it5[i] <- suppressWarnings({ B01(t.tmp, N.supp[i], N.supp[i], normal.prior, scale = 1) })
      p.val.it5[i]  <- ttest.tmp$p.value
    }
  }
  
  output$intro.topic5.plot1 <- renderPlot({
    par(mar = c(4, 5.5, .5, 1))
    plot(N.supp[1:(input$Ncommon.BF.p/50)], BF.val.it5[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i", 
         xlim = c(50, 5000), ylim = c(0, 50), col = "#005E3C", lwd = 2,
         xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    axis(1, at = seq(0, 5000, by = 1000), las = 1)
    axis(2, at = seq(0, 50, 10), las = 1)
    mtext("Sample size per group", 1, 2.5)
    mtext(expression("BF"["01"] * " (log scale)"), 2, 3)
  })
  
  output$intro.topic5.plot2 <- renderPlot({
    par(mar = c(4, 5.5, .5, 1))
    plot(N.supp[1:(input$Ncommon.BF.p/50)], p.val.it5[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i",
         xlim = c(50, 5000), ylim = c(0, 1.01), col = "#005E3C", lwd = 2,
         xlab = "", ylab = "", yaxt = "n", xaxt = "n")
    axis(1, at = seq(0, 5000, by = 1000), las = 1)
    axis(2, at = seq(0, 1, by = .2), las = 1)
    mtext("Sample size per group", 1, 2.5)
    mtext(expression(italic("p") * "-value"), 2, 3)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # LET'S PRACTICE tab:
  output$practice <- renderUI({
    tags$iframe(
      src="https://statsedge.org/StatsEdgeShinyApps/learnBF_tutorial/", width="100%", height="100%", frameBorder=0, style="height: 100vh;", scrolling = 'yes'
    )
  })
  
  
  
  # Run t-test:
  ttest.res <- reactive({
    t.test.summ(input$mean1, input$mean2, input$sd1, input$sd2, input$n1, input$n2)
  })
  
  # t-test result:
  output$ttest <- renderUI({
    tab <- data.frame(
      input$mean1 - input$mean2, 
      ttest.res()$statistic, 
      ttest.res()$parameter, 
      if (round(ttest.res()$p.value, 3) >= .001) ttest.res()$p.value else "<.001"
    ) 
    colnames(tab) <- c("\\text{Mean difference}", "\\text{$t$}", "\\text{df}", "\\text{$p$-value}")
    
    LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1), 
                             digits = c(0, 3, 3, if (ttest.res()$parameter - round(ttest.res()$parameter) < 1e-12) 0 else 3, 3)), 
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE,
                      add.to.row                 = list(
                        pos     = as.list(c(-1)),
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
  
  output$ttest.crit <- renderPlot({
    xlimupp <- max(5, ceiling(abs(ttest.res()$statistic))+2)
    par(mar = c(3, 1, 2, 1))
    curve(dt(x, ttest.res()$parameter), from = -xlimupp, to = xlimupp, n = 1024, 
          ylim = c(0, 1.1 * dt(0, ttest.res()$parameter)), 
          bty = "n", yaxt = "n", ylab = "", lwd = 2, xaxt = "n", xlab = "", yaxs = "i", 
          main = paste0("Sampling distribution assuming H0 is true = Student's t (", round(ttest.res()$parameter, 3), ")"), 
          col = "#005E3C")
    axis(1, seq(-xlimupp, xlimupp, 1))
    mtext("Test statistic", 1, 2)
    x.supp.crit <- seq(qt(1-input$alpha/2, ttest.res()$parameter), xlimupp, length.out = 101)
    polygon(x = c(x.supp.crit, rev(x.supp.crit)),
            y = c(rep(1.*dt(0, ttest.res()$parameter), 101), rep(0, 101)),
            col = "#FF000005", border = NA)
    polygon(x = c(-x.supp.crit, rev(-x.supp.crit)),
            y = c(rep(1.*dt(0, ttest.res()$parameter), 101), rep(0, 101)),
            col = "#FF000005", border = NA)
    polygon(x = c(x.supp.crit, rev(x.supp.crit)), 
            y = c(dt(x.supp.crit, ttest.res()$parameter), rep(0, 101)), 
            col = "#FF00004D", border = NA)
    polygon(x = c(-x.supp.crit, rev(-x.supp.crit)), 
            y = c(dt(-x.supp.crit, ttest.res()$parameter), rep(0, 101)), 
            col = "#FF00004D", border = NA)
    x.supp <- seq(abs(ttest.res()$statistic), xlimupp, length.out = 101)
    polygon(x = c(x.supp, rev(x.supp)), 
            y = c(dt(x.supp, ttest.res()$parameter), rep(0, 101)), 
            col = "#DCA55966", border = NA, density = 10, lwd = 4)
    polygon(x = c(-x.supp, rev(-x.supp)), 
            y = c(dt(-x.supp, ttest.res()$parameter), rep(0, 101)), 
            col = "#DCA55966", border = NA, density = 10, lwd = 4)
    abline(v = ttest.res()$statistic, lwd = 2, col = "#DCA559")
    text(ttest.res()$statistic, 1.05*dt(0, ttest.res()$parameter), paste0("t = ", round(ttest.res()$statistic, 3)), cex = 1.4, pos = if (ttest.res()$statistic < 0) 2 else 4)
    text(if (ttest.res()$statistic < 0) xlimupp else -xlimupp, 1.05*dt(0, ttest.res()$parameter), paste0("Critical region (probability = ", input$alpha, ")"), 
         pos = if (ttest.res()$statistic < 0) 2 else 4, cex = 1.4, col = "#F00000")
  })
  
  
  
  
  
  # Bayes factor:
  BF <- reactive({
    # Compute BF01: 
    BF.tmp <- switch(input$prior, 
                     "cauchy"    = B01(ttest.res()$statistic, input$n1, input$n2, 
                                       cauchy.prior, location = location.c(), scale = scale.c()), 
                     "normal"    = B01(ttest.res()$statistic, input$n1, input$n2, 
                                       normal.prior, location = location.n(), scale = scale.n()), 
                     "t.student" = B01(ttest.res()$statistic, input$n1, input$n2, 
                                       tstude.prior, location = location.t(), scale = scale.t(), df = df.t()))
    
    # names(BF.tmp) <- "BF01"
    if (input$BF10.01 == "BF10") {
      BF.tmp <- 1 / BF.tmp
      # names(BF.tmp) <- "BF10"
    }
    BF.tmp
  })
  
  prior.odds <- reactive({
    if (input$BF10.01 == "BF10") {
      input$priorprob1 / input$priorprob0
    } else {
      input$priorprob0 / input$priorprob1
    }
  })
  post.probs <- reactive({
    if (input$BF10.01 == "BF10") {
      c(1, prior.odds() * BF()) / (1 + prior.odds() * BF())
    } else {
      c(prior.odds() * BF(), 1) / (1 + prior.odds() * BF())
    }
  })
  post.odds <- reactive({
    prior.odds() * BF()
  })
  
  
  output$Intro.BF.df1 <- renderText({
    paste0("Bayes factor and information on the ", switch(input$prior, "cauchy"="Cauchy", "normal"="Normal", "t.student"="t-Student"), " prior (location, scale", if (input$prior == "t.student") ", df):" else ")"
    )
  })
  
  output$BF.df1 <- renderUI({
    # tab <- data.frame(
    #   BF()
    # )
    tab <- switch(input$prior, 
                  "cauchy"    = data.frame(BF(), location.c(), scale.c()), 
                  "normal"    = data.frame(BF(), location.n(), scale.n()), 
                  "t.student" = data.frame(BF(), location.t(), scale.t(), df.t()))
    colnames(tab) <- switch(input$prior, 
                            "cauchy"    = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Location}", "\\text{Scale}"), 
                            "normal"    = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Location}", "\\text{Scale}"), 
                            "t.student" = c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), "\\text{Location}", "\\text{Scale}", "\\text{df}"))
    LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),), 
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      include.rownames           = FALSE, 
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = "\\rowcolor{lightgray}" # \\rowcolor{#005E3C1A}
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
    
    LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      include.rownames           = FALSE,  
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = "\\rowcolor{lightgray}" # \\rowcolor{#005E3C1A}
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
    withMathJax(
      paste0("$BF_{", substr(input$BF10.01, 3, 4), "}=$",
             " $", round(BF(), 2), "$: The observed data are $",
             round(BF(), 2),
             " $ times more likely in case $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$", 
             " is true than if ",
             " $\\mathcal{H}_{", substr(input$BF10.01, 4, 4), "}$", 
             " is true."
      )
    )
  })
  output$BFint2 <- renderUI({
    withMathJax(
      paste0("$BF_{", substr(input$BF10.01, 3, 4), "}=$",
             " $", round(BF(), 2), "$: The prior odds ",
             " (", round(prior.odds(), 3), "-to-1 in favor of $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$", 
             ") are updated by a factor of $BF_{", substr(input$BF10.01, 3, 4), "}=$",
             " $", round(BF(), 2), "$", 
             " in favor of $\\mathcal{H}_{", substr(input$BF10.01, 3, 3), "}$."
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
    layout(matrix(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3), 1, 11, byrow = TRUE))
    
    # Left plot:
    par(mar = c(3, 7, .5, 0))
    if (input$BF10.01 == "BF10") 
    {
      x.coords <- barplot(c(input$priorprob1/100, input$priorprob0/100), col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2)
      mtext("Prior probability", 2, 4.5, cex = 1.5)
      text(x = x.coords, y = c(input$priorprob1/100, input$priorprob0/100), label = c(input$priorprob1/100, input$priorprob0/100), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
    } else 
    {
      x.coords <- barplot(c(input$priorprob0/100, input$priorprob1/100), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2)
      mtext("Prior probability", 2, 4.5, cex = 1.5)
      text(x = x.coords, y = c(input$priorprob0/100, input$priorprob1/100), label = c(input$priorprob0/100, input$priorprob1/100), pos = 3, cex = 2, font=2, col = c("#DCA559", "#005E3C"))
    }
    
    # Middle plot:
    par(mar = c(0, .5, 0, .5))
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
    arrows(0, .5, 1, .5, length = .4, angle = 30, lwd = 5)
    
    # Right plot:
    par(mar = c(3, 7, .5, 0))
    if (input$BF10.01 == "BF10") 
    {
      x.coords <- barplot(post.probs()[2:1], col = c("#005E3C1A", "#DCA5591A"), border = c("#005E3C", "#DCA559"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[1]), expression(H[0])), ylab = "", cex.axis = 2, cex.names = 2)
      mtext("Posterior probability", 2, 4.5, cex = 1.5)
      text(x = x.coords, y = post.probs()[2:1], label = round(post.probs()[2:1], 4), pos = 3, cex = 2, font=2, col = c("#005E3C", "#DCA559"))
    } else 
    {
      x.coords <- barplot(post.probs(), col = c("#DCA5591A", "#005E3C1A"), border = c("#DCA559", "#005E3C"), las = 1, ylim = c(0, 1.05), names.arg = c(expression(H[0]), expression(H[1])), ylab = "", cex.axis = 2, cex.names = 2)
      mtext("Posterior probability", 2, 4.5, cex = 1.5)
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
  
  
  output$kim.out <- renderUI({
    switch(input$keepinmind, 
           topic1 = {
             outtext <- paste0("$$\\Large\\underbrace{\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), ")}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), ")}}_\\text{prior odds}\\times BF_{", substr(input$BF10.01, 3, 4), "}=\\underbrace{\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), "|D)}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), "|D)}}_\\text{posterior odds}$$ The Bayes factor ",  em("updates"), " the prior odds to the posterior odds, in light of the observed data.", br(), "Only if the prior odds are equal to 1 (i.e., if $p(\\mathcal{H}_0) = p(\\mathcal{H}_1) = .50$) do the Bayes factor and the posterior odds coincide.", br(), br(), "See for yourself!", br(), " Try changing the prior probability of either $\\mathcal{H}_0$ or $\\mathcal{H}_1$ using the sliders on the left side menu and see how the Bayes factor and the posterior odds are affected.") 
           }, 
           topic2 = {
             outtext <- paste0("In order to compute the Bayes factor, prior distributions (", em("priors"), " for short) are required for all parameters. A prior assigns probability to each possible value of the parameter at hand, ", em("before"), " looking at the data. Priors may be chosen with different goals in mind, for example: ", HTML(renderMarkdown(text = "- To reflect current knowledge.\n- To reflect differing scientific perspectives (e.g., skeptical, liberal, or mainstream).\n- To impose constraints (e.g., to preclude negative variances).\n")), "How one should go about priors is not consensual. ", em("Objective"), " Bayesians suggest that relying on ", em("default"), " priors selected on the basis of specific optimal criteria suffices. Other, ", em("subjective"), " Bayesians argue that priors should be more carefully selected depending on the problem at hand.",  br(), br(), "We suggest that using default priors is clearly helpful, but one should first explore those priors to make sure that they are minimally well calibrated. The true fact is that ", em(" The Bayes factor depends on the priors"), ". Thus, it is important to at least: ", HTML(renderMarkdown(text = "- Visualize the priors and judge whether we are comfortable with the prior allocation of probability to the various values of the parameter.\n- Report in full what priors were used while computing the Bayes factor.\n")), br(), "Below you can see the plot of the prior selected on the left side menu, together with some descriptives. You can use this information to have a better idea about how whether the prior is working as you intended. If not, try tweaking the prior's distribution, location, or scale and reassess.", br(), br())
           }, 
           topic3 = {
             outtext <- paste0("From $BF_{", substr(input$BF10.01, 3, 4), "}=", round(BF(), 2), "$ we can conclude that the observed data favor $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$ over $\\mathcal{H}_", substr(input$BF10.01, 4, 4), "$ by a factor of ", round(BF(), 2), "-to-1 in favor of $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$. ", br(), "Here the main point is that the evidence provided by the Bayes factor is ", em("relative"), ".", br(), "That is, $\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$ is being explicitly compared to $\\mathcal{H}_", substr(input$BF10.01, 4, 4), "$ and that must be acknowledged.", br(), br(), "Hence, we discourage simpliflied summaries of the type:", br(), "'", em("The results provide evidence "), if (BF() > 1) em("in favor of ") else em("against "), "$\\mathcal{H}_", substr(input$BF10.01, 3, 3), "$' ('against' because $BF_{", substr(input$BF10.01, 3, 4), "}", if (BF() > 1) "\\geq" else "\\leq", "1$).")
           }, 
           topic4 = {
             outtext <- paste0("We often take a hypothesis such as $\\mathcal{H}_0:\\mu_1=\\mu_2$ to stand for the ", em("absence"), " of an effect (here, a difference between the two population means), and a hypothesis such as $\\mathcal{H}_1:\\mu_1\\not=\\mu_2$ to stand for the ", em("presence"), " of an effect.", br(), "Furthermore, practitioners seem to be often tempted to use the Bayes factor to establish the presence (or lack thereof) of such an effect.", br(), br(), "As it happens, there seems to be a lot of misunderstanding going on here.", br(), "Do notice the following:", br(), br(), HTML(renderMarkdown(text = "1. We should not confuse a _research_ hypothesis with a _statistical_ hypothesis.<br>A _research_ hypothesis is a scientific claim. It reflects a theory that we wish to put challenge.<br>A _statistical_ hypothesis, on the other hand, is a precise mathematical statement that should reflect some property of the population, assuming the research hypothesis were in fact correct.<br>As it happens, a theory such as 'an effect is absent' is a _research_ hypothesis, whereas a null hypothesis is only a _statistical_ hypothesis.<br>We cannot really test research hypotheses directly simply because we do not have the ability to fully understand all the intricacies of the real world problem under study. Statistical hypotheses are an easy surrogate for research hypotheses.<br>On its own, this distinction between research and statistical hypotheses should preclude researchers from attempting to use _p_ values or Bayes factors as a tool to _establish_ the presence or absence of an effect. Much more modestly, all we should derive from hypotheses testing is relative evidence between two competing hypotheses.<br><br>  \n2. The Bayes factor is only a number.<br> It would be quite strange to expect that from one sample-based number one could go as far as establishing that a theory essentially holds.\n")), br(), "We strong suggest that special care is taken when choosing the wording used to report findings. For example, it is best to avoid saying something like '(...) from the test we conclude that there is no effect ($BF_{01} = 11.2$)' or even '(...) we found an effect between both groups ($BF_{10}=11.2$)'.")
           }, 
           topic5 = {
             outtext <- paste0("The Bayes factor is ", em("not"), " an effect size measure. This can be easily checked by manipulating some inputs on the left-side menu, as follows:", HTML(renderMarkdown(text = "- Make sure that the two group means are different from each other, even if only by 0.1.\n- Try increasing the sample size of both groups.\n")), "You can compare the value of the Bayes factor to that of Cohen's $d$ (which here is given by $d=\\frac{\\overline{X}_1-\\overline{X}_2}{\\sqrt{(\\hat{\\sigma}_1^2+\\hat{\\sigma}_2^2)/2}}$):")
           }, 
           topic6 = {
             outtext <- paste0("A Bayes factor close to 1 implies that the observed data are about equally likely unde either $\\mathcal{H}_0$ or $\\mathcal{H}_1$.", br(), "In other words, the observed data do not help to distinguish between the predictive ability of the two competing hypotheses.", br(), br(), "In such cases, we should not make the mistake of concluding that there is evidence in favor of the 'no effect' null model. The fallacy would be of reasoning something like this: 'Since the test outcome is inconclusive, then maybe the null hypothesis holds after all'. The common fallacy of drawing support in favor of $\\mathcal{H}_0$ from a nonsignificant frequentist test result is a good analogy here.", br(), br(), "In short: From inconclusive evidence (i.e., Bayes factor of about 1) one should not infer that there is evidence of absence (i.e., $\\mathcal{H}_0$ is more supported than $\\mathcal{H}_1$).")
           }, 
           topic7 = {
             outtext <- paste0("The Bayes factor is just a non-negative real number. How to ", em("interpret"), " this number is not trivial.", br(), "For example, what values of $BF_{10}$ should be considered as weak, moderate, or strong evidence in favor of $\\mathcal{H}_1$ over $\\mathcal{H}_0$?", br(), br(), "Several qualitative classification systems do exist; below are three commonly used grading systems.")
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
  
  output$kim.out.topic1.df1 <- renderUI({
    tab <- data.frame(
      prior.odds(), 
      BF(), 
      post.odds()
    )
    colnames(tab) <- c(paste0("\\text{Prior odds}=\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), ")}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), ")}"),
                       paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), 
                       paste0("\\text{Posterior odds}=\\frac{p(\\mathcal{H}_", substr(input$BF10.01, 3, 3), "|D)}{p(\\mathcal{H}_", substr(input$BF10.01, 4, 4), "|D)}")
    )
    # addtorow         <- list()
    # addtorow$pos     <- list(-1)
    # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
    
    LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),digits=c(0, if (input$priorprob0 == 50) 0 else 3, 3, 3)),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      include.rownames           = FALSE,  
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = "\\rowcolor{lightgray}" # \\rowcolor{#005E3C1A}
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
  
  output$kim.out.topic2.plot1 <- renderPlot({
    x.supp <- switch(input$prior,
                     "cauchy"    = seq(floor(location.c() - 3.5*scale.c()), ceiling(location.c() + 3.5*scale.c()), length.out = 1024),
                     "normal"    = seq(floor(location.n() - 3.5*scale.n()), ceiling(location.n() + 3.5*scale.n()), length.out = 1024),
                     "t.student" = seq(floor(location.t() - 3.5*scale.t()), ceiling(location.t() + 3.5*scale.t()), length.out = 1024))
    y      <- switch(input$prior, 
                     "cauchy"    = dcauchy(x.supp, location.c(), scale.c()), 
                     "normal"    = dnorm  (x.supp, location.n(), scale.n()), 
                     "t.student" = dst    (x.supp, df.t(), location.t(), scale.t()))
    
    par(mar = c(4.5, 5, 1.5, .5))
    plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, 1.2*max(y)), ylab = "Density", xlab = "Standardized mean difference", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxs = "i", 
         main = switch(input$prior,
                       "cauchy"    = paste0("Cauchy (location = ", location.c(), ", scale = ", scale.c(), ")"),
                       "normal"    = paste0("Normal (mean = ", location.n(), ", SD = ", scale.n(), ")"),
                       "t.student" = paste0("Student t (location = ", location.t(), ", scale = ", scale.t(), ", df = ", df.t(), ")")))
    axis(1, at = min(x.supp):max(x.supp))
    # 1SD area:
    x.supp.1SD <- switch(input$prior,
                         "cauchy"    = seq(location.c() - 1*scale.c(), location.c() + 1*scale.c(), length.out = 1024),
                         "normal"    = seq(location.n() - 1*scale.n(), location.n() + 1*scale.n(), length.out = 1024),
                         "t.student" = seq(location.t() - 1*scale.t(), location.t() + 1*scale.t(), length.out = 1024))
    y.1SD      <- switch(input$prior, 
                         "cauchy"    = dcauchy(x.supp.1SD, location.c(), scale.c()), 
                         "normal"    = dnorm  (x.supp.1SD, location.n(), scale.n()), 
                         "t.student" = dst    (x.supp.1SD, df.t(), location.t(), scale.t()))
    polygon(c(x.supp.1SD, rev(x.supp.1SD)), c(y.1SD, rep(0, 1024)), col = "#DCA55966", border = NA)
    # 2SD area:
    x.supp.2SD <- switch(input$prior,
                         "cauchy"    = seq(location.c() - 2*scale.c(), location.c() - 1*scale.c(), length.out = 1024),
                         "normal"    = seq(location.n() - 2*scale.n(), location.n() - 1*scale.n(), length.out = 1024),
                         "t.student" = seq(location.t() - 2*scale.t(), location.t() - 1*scale.t(), length.out = 1024))
    y.2SD      <- switch(input$prior, 
                         "cauchy"    = dcauchy(x.supp.2SD, location.c(), scale.c()), 
                         "normal"    = dnorm  (x.supp.2SD, location.n(), scale.n()), 
                         "t.student" = dst    (x.supp.2SD, df.t(), location.t(), scale.t()))
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    x.supp.2SD <- switch(input$prior,
                         "cauchy"    = seq(location.c() + 1*scale.c(), location.c() + 2*scale.c(), length.out = 1024),
                         "normal"    = seq(location.n() + 1*scale.n(), location.n() + 2*scale.n(), length.out = 1024),
                         "t.student" = seq(location.t() + 1*scale.t(), location.t() + 2*scale.t(), length.out = 1024))
    y.2SD      <- switch(input$prior, 
                         "cauchy"    = dcauchy(x.supp.2SD, location.c(), scale.c()), 
                         "normal"    = dnorm  (x.supp.2SD, location.n(), scale.n()), 
                         "t.student" = dst    (x.supp.2SD, df.t(), location.t(), scale.t()))
    polygon(c(x.supp.2SD, rev(x.supp.2SD)), c(y.2SD, rep(0, 1024)), col = "#DCA55940", border = NA)
    # 3SD area:
    x.supp.3SD <- switch(input$prior,
                         "cauchy"    = seq(location.c() - 3*scale.c(), location.c() - 2*scale.c(), length.out = 1024),
                         "normal"    = seq(location.n() - 3*scale.n(), location.n() - 2*scale.n(), length.out = 1024),
                         "t.student" = seq(location.t() - 3*scale.t(), location.t() - 2*scale.t(), length.out = 1024))
    y.3SD      <- switch(input$prior, 
                         "cauchy"    = dcauchy(x.supp.3SD, location.c(), scale.c()), 
                         "normal"    = dnorm  (x.supp.3SD, location.n(), scale.n()), 
                         "t.student" = dst    (x.supp.3SD, df.t(), location.t(), scale.t()))
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55926", border = NA)
    x.supp.3SD <- switch(input$prior,
                         "cauchy"    = seq(location.c() + 2*scale.c(), location.c() + 3*scale.c(), length.out = 1024),
                         "normal"    = seq(location.n() + 2*scale.n(), location.n() + 3*scale.n(), length.out = 1024),
                         "t.student" = seq(location.t() + 2*scale.t(), location.t() + 3*scale.t(), length.out = 1024))
    y.3SD      <- switch(input$prior, 
                         "cauchy"    = dcauchy(x.supp.3SD, location.c(), scale.c()), 
                         "normal"    = dnorm  (x.supp.3SD, location.n(), scale.n()), 
                         "t.student" = dst    (x.supp.3SD, df.t(), location.t(), scale.t()))
    polygon(c(x.supp.3SD, rev(x.supp.3SD)), c(y.3SD, rep(0, 1024)), col = "#DCA55926", border = NA)
    # Arrows:
    loc.use <- switch(input$prior,
                      "cauchy"    = location.c(),
                      "normal"    = location.n(),
                      "t.student" = location.t())
    scl.use <- switch(input$prior,
                      "cauchy"    = scale.c(),
                      "normal"    = scale.n(),
                      "t.student" = scale.t())
    pct.use <- switch(input$prior,
                      "cauchy"    = pcauchy(location.c() + (1:3) * scale.c(), location.c(), scale.c())         - pcauchy(location.c() - (1:3) * scale.c(), location.c(), scale.c()),
                      "normal"    = pnorm  (location.n() + (1:3) * scale.n(), location.n(), scale.n())         - pnorm  (location.n() - (1:3) * scale.n(), location.n(), scale.n()),
                      "t.student" = pst    (location.t() + (1:3) * scale.t(), df.t(), location.t(), scale.t()) - pst    (location.t() - (1:3) * scale.t(), df.t(), location.t(), scale.t()))
    arrows(loc.use - 1*scl.use, .8*max(y), loc.use + 1*scl.use, .8*max(y), code = 3,
           length = .15, angle = 30, lwd = 1, col = "#005E3C")
    segments(loc.use - 1*scl.use, 0, loc.use - 1*scl.use, .8*max(y), col = "#DCA55966", lwd = 2)
    segments(loc.use + 1*scl.use, 0, loc.use + 1*scl.use, .8*max(y), col = "#DCA55966", lwd = 2)
    arrows(loc.use - 2*scl.use, .5*max(y), loc.use + 2*scl.use, .5*max(y), code = 3,
           length = .15, angle = 30, lwd = 1, col = "#005E3C")
    segments(loc.use - 2*scl.use, 0, loc.use - 2*scl.use, .5*max(y), col = "#DCA55940", lwd = 2)
    segments(loc.use + 2*scl.use, 0, loc.use + 2*scl.use, .5*max(y), col = "#DCA55940", lwd = 2)
    arrows(loc.use - 3*scl.use, .2*max(y), loc.use + 3*scl.use, .2*max(y), code = 3,
           length = .15, angle = 30, lwd = 1, col = "#005E3C")
    segments(loc.use - 3*scl.use, 0, loc.use - 3*scl.use, .2*max(y), col = "#DCA55926", lwd = 2)
    segments(loc.use + 3*scl.use, 0, loc.use + 3*scl.use, .2*max(y), col = "#DCA55926", lwd = 2)
    # Text:
    text(x = loc.use, y = .8*max(y), paste0(round(100*pct.use[1], 1), "%"), pos = 3)
    text(x = loc.use, y = .5*max(y), paste0(round(100*pct.use[2], 1), "%"), pos = 3)
    text(x = loc.use, y = .2*max(y), paste0(round(100*pct.use[3], 1), "%"), pos = 3)
    # Legend:
    legend("topright", legend = c("location \u00B1 1\u00D7 scale", "location \u00B1 2\u00D7 scale", "location \u00B1 3\u00D7 scale"), 
           fill = c("#DCA55966", "#DCA55940", "#DCA55926"), bty = "n")
  })
  
  output$kim.out.topic2.df1 <- renderUI({
    dist <- switch(input$prior, 
                   "cauchy"    = paste0("\\text{Cauchy(location = }", location.c(), "\\text{, scale = }", scale.c(), "\\text{)}"), 
                   "normal"    = paste0("\\text{Normal(location = }", location.n(), "\\text{, scale = }", scale.n(), "\\text{)}"), 
                   "t.student" = paste0("\\text{t-Student(location = }", location.t(), "\\text{, scale = }", scale.t(), "\\text{, df = }", df.t(), "\\text{)}"))
    
    
    tab <- data.frame(dist, BF())
    colnames(tab) <- c(paste0("\\text{Prior}"), 
                       paste0("BF_{", substr(input$BF10.01, 3, 4), "}")
    )
    # addtorow         <- list()
    # addtorow$pos     <- list(-1)
    # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
    
    LaTeXtab <- print(xtable(tab, align = c("l", "l", "c"), digits=c(0, 0, 3), display = c("s", "s", "f")),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE,
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = "\\rowcolor{lightgray}" # \\rowcolor{#005E3C1A}
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
  
  output$kim.out.topic5.df1 <- renderUI({
    cohen.d <- round((input$mean1 - input$mean2) / sqrt((input$sd1 + input$sd2)/2), 2)
    tab <- data.frame(
      BF(), 
      cohen.d
    )
    colnames(tab) <- c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), 
                       paste0("\\text{Cohen's $d$}")
    )
    # addtorow         <- list()
    # addtorow$pos     <- list(-1)
    # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
    
    LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1),digits=c(0, 3, 2)),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      include.rownames           = FALSE,  
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = "\\rowcolor{lightgray}" # \\rowcolor{#005E3C1A}
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
  
  # Compute things to render both plots in topic 5:
  {# Common sample size:
    N.supp <- seq(50, 5000, by = 50)
    
    # BF per sample size:
    t.test.summ <- function(m1=.1, m2=0, sd1=1, sd2=1, n1, n2)
    {
      group1 <- scale(1:n1) * sd1 + m1
      group2 <- scale(1:n2) * sd2 + m2
      t.test(x = group1, y = group2)
    }
    BF.val <- rep(NA, length(N.supp))
    for (i in 1:length(N.supp))
    {
      t.tmp     <- t.test.summ(n1 = N.supp[i], n2 = N.supp[i])$statistic
      BF.tmp    <- suppressWarnings({ B01(t.tmp, N.supp[i], N.supp[i], normal.prior, scale = 1) })
      BF.val[i] <- 1 / BF.tmp
    }
  }
  
  output$kim.out.topic5.plot1 <- renderPlot({
    par(mar = c(4, 5.5, .5, 1))
    plot(N.supp[1:(input$Ncommon/50)], BF.val[1:(input$Ncommon/50)], type = "l", log = "y", las = 1, bty = "n", yaxs = "i", xaxs = "i", 
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$kim.out.topic7.df1 <- renderUI({
    tab <- data.frame(
      c("1 - 3.2", "3.2 - 10", "10 - 31.6", "31.6 - 100", "> 100"), 
      c("\\text{Not worth more than a bare mention}", "\\text{Substantial}", "\\text{Strong}", "\\text{Very strong}", "\\text{Decisive}")
    )
    colnames(tab) <- c(paste0("BF_{10}"), 
                       paste0("\\text{Strength of evidence against }\\mathcal{H}_0")
    )
    addtorow         <- list()
    addtorow$pos     <- as.list(c(-1, -1))
    addtorow$command <- as.vector(c("& \\text{Jeffreys (1961).}\\\\", "\\rowcolor{lightgray}"), mode = "character")
    
    LaTeXtab <- print(xtable(tab, align = c("c", "c", "l")),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE, 
                      add.to.row                 = addtorow
    )
    tagList(
      #withMathJax(),
      HTML(paste0("$$", LaTeXtab, "$$"))#,
      # tags$script(HTML(js)),
      # tags$script(
      #   async="", 
      #   src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
      # )
    )
  })
  
  output$kim.out.topic7.df2 <- renderUI({
    tab <- data.frame(
      c("1 - 3", "3 - 20", "20 - 150", "> 150"), 
      c("\\text{Not worth more than a bare mention}", "\\text{Positive}", "\\text{Strong}", "\\text{Very strong}")
    )
    colnames(tab) <- c(paste0("BF_{10}"), 
                       paste0("\\text{Strength of evidence against }\\mathcal{H}_0")
    )
    addtorow         <- list()
    addtorow$pos     <- as.list(c(-1, -1))
    addtorow$command <- as.vector(c("& \\text{Kass and Raftery (1995).}\\\\", "\\rowcolor{lightgray}"), mode = "character")
    
    LaTeXtab <- print(xtable(tab, align = c("c", "c", "l")),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE, 
                      add.to.row                 = addtorow
    )
    tagList(
      #withMathJax(),
      HTML(paste0("$$", LaTeXtab, "$$"))#,
      # tags$script(HTML(js)),
      # tags$script(
      #   async="", 
      #   src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
      # )
    )
  })
  
  output$kim.out.topic7.df3 <- renderUI({
    tab <- data.frame(
      c("1 - 3", "3 - 10", "10 - 30", "30-100", "> 100"), 
      c("\\text{Anecdotal}", "\\text{Moderate}", "\\text{Strong}", "\\text{Very strong}", "\\text{Extreme}")
    )
    colnames(tab) <- c(paste0("BF_{10}"), 
                       paste0("\\text{Strength of evidence against }\\mathcal{H}_0")
    )
    addtorow         <- list()
    addtorow$pos     <- as.list(c(-1, -1))
    addtorow$command <- as.vector(c("& \\text{Lee and Wagenmakers (2014).}\\\\", "\\rowcolor{lightgray}"), mode = "character")
    
    LaTeXtab <- print(xtable(tab, align = c("c", "c", "l")),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE, 
                      add.to.row                 = addtorow
    )
    tagList(
      #withMathJax(),
      HTML(paste0("$$", LaTeXtab, "$$")),
      # tags$script(HTML(js)),
      # tags$script(
      #   async="", 
      #   src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
      # )
    )
  })
  
  output$kim.out.topic7.df4 <- renderUI({
    BF.tmp   <- if (BF() > 1) BF() else 1/BF()
    BF.labs1 <- c("\\text{Not worth more than a bare mention}", "\\text{Substantial}", "\\text{Strong}", "\\text{Very strong}", "\\text{Decisive}")
    BF.lab1  <- if (BF.tmp <= 3.2) BF.labs1[1] else if (BF.tmp <= 10) BF.labs1[2] else if (BF.tmp <= 31.6) BF.labs1[3] else if (BF.tmp <= 100) BF.labs1[4] else  BF.labs1[5]
    BF.labs2 <- c("\\text{Not worth more than a bare mention}", "\\text{Positive}", "\\text{Strong}", "\\text{Very strong}")
    BF.lab2  <- if (BF.tmp <= 3) BF.labs2[1] else if (BF.tmp <= 20) BF.labs2[2] else if (BF.tmp <= 150) BF.labs2[3] else BF.labs2[4]
    BF.labs3 <- c("\\text{Anecdotal}", "\\text{Moderate}", "\\text{Strong}", "\\text{Very strong}", "\\text{Extreme}")
    BF.lab3  <- if (BF.tmp <= 3) BF.labs3[1] else if (BF.tmp <= 10) BF.labs3[2] else if (BF.tmp <= 30) BF.labs3[3] else if (BF.tmp <= 100) BF.labs3[4] else  BF.labs3[5]
    # tab <- data.frame(BF(), BF.lab1, BF.lab2, BF.lab3)
    # colnames(tab) <- c(paste0("BF_{", substr(input$BF10.01, 3, 4), "}"), 
    #                    c("\\text{Jeffreys (1961)}", "\\text{Kass and Raftery (1955)}", "\\text{Lee and Wagenmakers (2014)}")
    # )
    # # addtorow         <- list()
    # # addtorow$pos     <- list(-1)
    # # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
    # 
    # LaTeXtab <- print(xtable(tab, align = rep("c", ncol(tab)+1), digits=c(0, 3, 0, 0, 0)),
    #                   floating                   = FALSE,
    #                   tabular.environment        = "array",
    #                   comment                    = FALSE,
    #                   print.results              = TRUE,
    #                   sanitize.colnames.function = identity,
    #                   sanitize.text.function     = identity,
    #                   include.rownames           = FALSE,  
    #                   add.to.row                 = list(
    #                     pos     = as.list(-1),
    #                     command = as.vector(c("\\rowcolor{lightgray}"), mode = "character") # \\rowcolor{#005E3C1A}
    #                   )
    # )
    tab           <- data.frame(c("\\text{Jeffreys (1961)}", "\\text{Kass and Raftery (1955)}", "\\text{Lee and Wagenmakers (2014)}"), 
                                c(BF.lab1, BF.lab2, BF.lab3)
    )
    colnames(tab) <- c("\\text{Classification}", paste0("BF_{", substr(input$BF10.01, 3, 4), "} = ", round(BF(), 3)))
    # addtorow         <- list()
    # addtorow$pos     <- list(-1)
    # addtorow$command <- "\\multicolumn\\{6\\}\\{l\\}\\{abc\\}\\\\"
    
    LaTeXtab <- print(xtable(tab, align = c("l", "l", "l")),
                      floating                   = FALSE,
                      tabular.environment        = "array",
                      comment                    = FALSE,
                      print.results              = FALSE,
                      sanitize.colnames.function = identity,
                      sanitize.text.function     = identity,
                      include.rownames           = FALSE,  
                      add.to.row                 = list(
                        pos     = as.list(-1),
                        command = as.vector(c("\\rowcolor{lightgray}"), mode = "character") # \\rowcolor{#005E3C1A}
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Change prior prob of H1 when that of H0 changes:
  observeEvent(input$priorprob0,  {
    updateSliderInput(session = session, 
                      inputId = "priorprob1", 
                      value   = 100 - input$priorprob0)
  })
  # Change prior prob of H0 when that of H1 changes:
  observeEvent(input$priorprob1,  {
    updateSliderInput(session = session, 
                      inputId = "priorprob0", 
                      value   = 100 - input$priorprob1)
  })
  
  # Decide with parameter input to show depending on the chosen prior:
  output$prior.param1 <- renderUI({
    if (input$prior == 'cauchy') {
      numericInput(inputId  = "location.c", 
                   label    = "Location:", 
                   min      = -5,
                   max      = 5,
                   value    = 0, 
                   step     = 0.1, 
                   width    = "100%")
    } else if (input$prior == 'normal') {
      numericInput(inputId = "location.n",
                   label   = "Location:",
                   min     = -5,
                   max     = 5,
                   value   = 0, 
                   step    = 0.1, 
                   width   = "100%")
    } else {
      numericInput(inputId = "location.t",
                   label   = "Location:",
                   min     = -5,
                   max     = 5,
                   value   = 0, 
                   step    = 0.1, 
                   width   = "100%")
    }
  })
  output$prior.param2 <- renderUI({
    if (input$prior == 'cauchy') {
      numericInput(inputId  = "scale.c", 
                   label    = "Scale:", 
                   min     = 0,
                   max     = 5,
                   value   = 0.707, 
                   step    = 0.1, 
                   width   = "100%")
    } else if (input$prior == 'normal') {
      numericInput(inputId  = "scale.n", 
                   label    = "Scale:", 
                   min     = 0,
                   max     = 5,
                   value   = 1, 
                   step    = 0.1, 
                   width   = "100%")
    } else {
      numericInput(inputId  = "scale.t", 
                   label    = "Scale:", 
                   min      = 0,
                   max      = 5,
                   value    = 1, 
                   step     = 0.1, 
                   width    = "100%")
    }
  })
  output$prior.param3 <- renderUI({
    if (input$prior == 't.student') {
      numericInput(inputId = "df.t",
                   label   = "df:",
                   min     = 1,
                   max     = 50,
                   value   = 1, 
                   width   = "100%")
    }
  })
  
  # To force these inputs to be available:
  location.c <- reactive({ if (input$prior == 'cauchy') req(input$location.c) })
  scale.c    <- reactive({ if (input$prior == 'cauchy') req(input$scale.c) })
  location.n <- reactive({ if (input$prior == 'normal') req(input$location.n) })
  scale.n    <- reactive({ if (input$prior == 'normal') req(input$scale.n) })
  location.t <- reactive({ if (input$prior == 't.student') req(input$location.t) })
  scale.t    <- reactive({ if (input$prior == 't.student') req(input$scale.t) })
  df.t       <- reactive({ if (input$prior == 't.student') req(input$df.t) })
  
  
  
}

# Call the app ----
shinyApp(ui = ui, server = server)






