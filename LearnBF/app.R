# Load Shiny ----
library(shiny)
library(shinyWidgets)
# library(dplyr)
# library(broom)
library(kableExtra)
library(BayesFactor)
library(xtable)
# library(DT)
library(markdown)
# library(stevemisc)
# library(katex)
# library(BH)
library(Rcpp)
library(fontawesome)
# install devtools in server
# install kableExtra (now 1.3.4) from Github: devtools::install_github("haozhu233/kableExtra")

# Function computing the t-test from summaries ----
t.test.summ <- function(m1, m2, sd1, sd2, n1, n2, alternative = "H1.diff0")
{
  group1 <- scale(1:n1) * sd1 + m1
  group2 <- scale(1:n2) * sd2 + m2
  # t.test(x = group1, y = group2, var.equal = TRUE)
  sp     <- sqrt((((n1-1)*(sd1^2)+(n2-1)*(sd2^2)))/(n1+n2-2))
  t      <- (m1 - m2) / (sp * sqrt(1/n1 + 1/n2))
  df     <- n1 + n2 - 2
  p      <- switch(alternative,
                   "H1.diff0"    = 2 * pt(abs(t), df, lower.tail = FALSE),
                   "H1.smaller0"  = pt(t, df, lower.tail = TRUE),
                   "H1.larger0" = pt(t, df, lower.tail = FALSE)
  )
  c(t = t, df = df, p = p)
}

# Load the functions allowing to compute the Bayes factors:
source("R_scripts/BayesFactors.R")

# Load BOOST's non-central t (better precision and less warnings than R's):
# https://stackoverflow.com/questions/39183938/rs-t-distribution-says-full-precision-may-not-have-been-achieved
# sourceCpp("www/boost_noncentralt.cpp", cacheDir = "cache_rcpp/")
sourceCpp("www/boost_noncentralt.cpp")

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
  # tags$head(
  #   tags$script(async = "",
  #               src   = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
  #   tags$script( HTML(js) )
  # ), 
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
                  "), 
             HTML(".not_bold label {font-weight:normal;}")),
  
  titlePanel(""),
  #tagList(tags$head(tags$style(type = 'text/css','')),
  tags$style(type = 'text/css', HTML(
    ".container-fluid > .nav > li >
                        a[data-value='The Bayes Factor'] {font-size: 20px}"
  )), 
  navbarPage(
    title = "", #h4(HTML("The Bayes factor!")),
    id = "maintabs", 
    # type = "pills",
    selected = "The Bayes Factor", ##### "Keep in mind", ##### "Bayesian t-test", ##### "Introduction", 
    position = "fixed-top", 
    tabPanel("The Bayes Factor", 
             icon = icon("house-chimney"), 
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
                           "5. Priors"                                      = "intro.topic5", 
                           "6. Supporting $\\mathcal{H}_0$"                 = "intro.topic6"
               ),
               direction = "vertical", 
               status = "mycolor", 
               checkIcon = list(
                 yes = icon("ok", lib = "glyphicon")
                 
               )
             ), 
             br(), 
             conditionalPanel("input.intro == 'intro.topic1'", 
                              uiOutput("introduction1a"), 
                              plotOutput("intro.topic1.plot1"), 
                              uiOutput("introduction1b")), 
             conditionalPanel("input.intro == 'intro.topic2'",
                              h3(strong("Null hypothesis significance testing (NHST)")), 
                              sidebarLayout(
                                sidebarPanel(
                                  # style = "position:fixed;width:30%;",
                                  h3(strong("Descriptives")),
                                  h4("Group A"),
                                  fluidRow(
                                    column(4, align = "left",
                                           div(numericInput(inputId = "mean1.tab2",
                                                            label   = em("Mean:"),
                                                            min     = -16,      # -10
                                                            max     = 16,       # 10
                                                            value   = 15.292,   # 0
                                                            step    = 0.1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "sd1.tab2",
                                                            label   = em("SD:"),
                                                            min     = 0,
                                                            max     = NA,
                                                            value   = 6.376,    # 1
                                                            step    = .1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "n1.tab2",
                                                            label   = em("$N$:"),
                                                            min     = 5,
                                                            max     = 50,
                                                            value   = 24,    # 30
                                                            width = "100%"), class = "not_bold")
                                    )
                                  ),
                                  h4("Group B"),
                                  fluidRow(
                                    column(4, align = "left",
                                           div(numericInput(inputId = "mean2.tab2",
                                                            label   = em("Mean:"),
                                                            min     = -16,       # -10
                                                            max     = 16,        # 10
                                                            value   = 10.88,     # 0.2
                                                            step    = 0.1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "sd2.tab2",
                                                            label   = em("SD:"),
                                                            min     = 0,
                                                            max     = NA,
                                                            value   = 4.324,    # 1
                                                            step    = .1,
                                                            width = "100%"), class = "not_bold")
                                    ),
                                    column(4, align = "left",
                                           div(numericInput(inputId = "n2.tab2",
                                                            label   = em("$N$:"),
                                                            min     = 5,
                                                            max     = 50,
                                                            value   = 25,    # 30
                                                            width = "100%"), class = "not_bold")
                                    )
                                  ), 
                                  hr(style = "border-top: 2px solid #005E3C;"),
                                  h3(strong("Classical test")),
                                  em("Null and alternative hypotheses:"),
                                  fluidRow(
                                    column(width = 4, uiOutput("H1hyp.cls")),
                                    column(width = 8,
                                           div(prettyRadioButtons(inputId  = "H1hyp.cls",
                                                                  label    = em("$\\mathcal{H}_1:\\hspace{1mm}$"),
                                                                  choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                                  "$\\delta>0$" = "H1.larger0",
                                                                                  "$\\delta<0$" = "H1.smaller0"),
                                                                  selected = "H1.diff0",
                                                                  inline   = TRUE,
                                                                  width    = "100%",
                                                                  status   = "success",
                                                                  shape    = "round",
                                                                  fill     = TRUE
                                           ), class = "not_bold")
                                    )
                                  )
                                ), 
                                
                                mainPanel(
                                  uiOutput("introduction2a"), 
                                  br(), 
                                  fluidRow(
                                    align = "left", 
                                    column(4, align = "left", 
                                           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), 
                                           div(sliderInput("alpha", em("Sig. level:"), 
                                                           min   = .001, 
                                                           max   = 0.2, 
                                                           value = .05, 
                                                           step  = .01, 
                                                           ticks = FALSE, 
                                                           width = '100%'), class = "not_bold")), 
                                    column(8)
                                  ), 
                                  plotOutput("ttest.crit"), 
                                  br(), br(), 
                                  # tableOutput("ttest"), 
                                  # br(), 
                                  tableOutput("ttestB"),  
                                  uiOutput("introduction2b")
                                )
                              ), 
             ), 
             conditionalPanel("input.intro == 'intro.topic3'", uiOutput("introduction3")), 
             conditionalPanel("input.intro == 'intro.topic4'", uiOutput("introduction4a")), 
             conditionalPanel("input.intro == 'intro.topic4'", h4(em("Box: How to derive the Bayes factor.")), div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                                                                                                   uiOutput("introduction4b"))), 
             conditionalPanel("input.intro == 'intro.topic4'", br(), br()), 
             conditionalPanel("input.intro == 'intro.topic5'", 
                              uiOutput("introduction5a"), 
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h3(strong("Bayesian test")),
                                  em("Null and alternative hypotheses:"),
                                  fluidRow(
                                    column(width = 4, uiOutput("H1hyp.bys")),
                                    column(width = 8,
                                           div(prettyRadioButtons(inputId  = "H1hyp.bys",
                                                                  label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                                  choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                                  "$\\delta>0$" = "H1.larger0",
                                                                                  "$\\delta<0$" = "H1.smaller0",
                                                                                  "$\\delta=\\delta_1$" = "H1.point"),
                                                                  selected = "H1.diff0",
                                                                  inline   = TRUE,
                                                                  width    = "100%",
                                                                  status   = "success",
                                                                  shape    = "round",
                                                                  fill     = TRUE
                                           ), class = "not_bold"),
                                           conditionalPanel("input.H1hyp == 'H1.point'",
                                                            div(sliderInput(inputId = "H1pointslide.bys", 
                                                                            label   = NULL, 
                                                                            min     = -2, 
                                                                            max     = 2, 
                                                                            value   = .2, 
                                                                            ticks   = FALSE, 
                                                                            step    = .1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ),
                                  br(),
                                  conditionalPanel("input.H1hyp != 'H1.point'", 
                                                   div(prettyRadioButtons(inputId  = "prior.bys",
                                                                          label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                                          choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                                          selected = "cauchy",
                                                                          inline   = TRUE,
                                                                          width    = "100%",
                                                                          status   = "success",
                                                                          shape    = "round",
                                                                          fill     = TRUE
                                                   ), class = "not_bold"), 
                                                   fluidRow(
                                                     column(width = 4,
                                                            conditionalPanel("input.prior == 'cauchy'",
                                                                             div(sliderInput(inputId = "location.c.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.prior == 'normal'",
                                                                             div(sliderInput(inputId = "location.n.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.prior == 't.student'",
                                                                             div(sliderInput(inputId = "location.t.bys",
                                                                                             label   = em("Location:"),
                                                                                             min     = -3,
                                                                                             max     = 3,
                                                                                             value   = 0,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold"))
                                                     ),
                                                     column(width = 4,
                                                            # uiOutput("prior.param2")
                                                            conditionalPanel("input.prior == 'cauchy'",
                                                                             div(sliderInput(inputId = "scale.c.bys",
                                                                                             label   = em("Scale:"),
                                                                                             min     = .1,
                                                                                             max     = 2,
                                                                                             value   = .707,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.prior == 'normal'",
                                                                             div(sliderInput(inputId = "scale.n.bys",
                                                                                             label   = em("Scale:"),
                                                                                             min     = .1,
                                                                                             max     = 2,
                                                                                             value   = 1,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold")),
                                                            conditionalPanel("input.prior == 't.student'",
                                                                             div(sliderInput(inputId = "scale.t.bys",
                                                                                             label   = em("Scale:"),
                                                                                             min     = .1,
                                                                                             max     = 2,
                                                                                             value   = 1,
                                                                                             ticks   = FALSE,
                                                                                             step    = 0.1,
                                                                                             width   = "100%"), class = "not_bold"))
                                                     ),
                                                     column(width = 4,
                                                            # uiOutput("prior.param3")
                                                            conditionalPanel("input.prior == 't.student'",
                                                                             div(sliderInput(inputId = "df.t.bys",
                                                                                             label   = em("df:"),
                                                                                             min     = 1,
                                                                                             max     = 100,
                                                                                             value   = 1,
                                                                                             ticks   = FALSE,
                                                                                             step    = 1,
                                                                                             width   = "100%"), class = "not_bold"))
                                                     )
                                                   ), 
                                                   br(),
                                  ), 
                                  width = 4
                                ),
                                
                                mainPanel(
                                  plotOutput("intro.topic5.plot1"), 
                                  width=8
                                )
                              ), 
                              
                              br(), br(),
                              uiOutput("introduction5b"), 
                              h4(em("Box: The marginal likelihood.")), 
                              div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                  uiOutput("introduction5c")), 
                              br(), br()
             ),
             conditionalPanel("input.intro == 'intro.topic6'", uiOutput("introduction6")
             ), 
             conditionalPanel("input.intro == 'intro.topic6'", fluidRow(
               align = "center", 
               column(3),
               column(6, align = "center", sliderInput("Ncommon.BF.p", "Sample size:", min = 50, max = 5000, value = 50, step = 50, 
                                                       animate = animationOptions( interval = 100 ), width = '100%', ticks   = FALSE)), 
               column(3)
             )), 
             conditionalPanel("input.intro == 'intro.topic6'", fluidRow(
               column(1),
               column(5, align = 'center', plotOutput("intro.topic6.plot1")), 
               column(5, align = 'center', plotOutput("intro.topic6.plot2")), 
               column(1)))
    ), 
    tabPanel("Bayesian t-test", 
             br(), 
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed;width:30%;",
                 h3(strong("Descriptives")),
                 h4("Group A"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean1",
                                           label   = em("Mean:"),
                                           min     = -16,      # -10
                                           max     = 16,       # 10
                                           value   = 15.292,   # 0
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd1",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 6.376,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n1",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 24,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 h4("Group B"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean2",
                                           label   = em("Mean:"),
                                           min     = -16,       # -10
                                           max     = 16,        # 10
                                           value   = 10.88,     # 0.2
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd2",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 4.324,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n2",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 25,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Bayesian test")),
                 em("Null and alternative hypotheses:"),
                 fluidRow(
                   column(width = 4, "$\\mathcal{H}_0:\\delta=0$"),
                   column(width = 8,
                          div(prettyRadioButtons(inputId  = "H1hyp",
                                                 label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                 choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                 "$\\delta>0$" = "H1.larger0",
                                                                 "$\\delta<0$" = "H1.smaller0",
                                                                 "$\\delta=\\delta_1$" = "H1.point"),
                                                 selected = "H1.diff0",
                                                 inline   = TRUE,
                                                 width    = "100%",
                                                 status   = "success",
                                                 shape    = "round",
                                                 fill     = TRUE
                          ), class = "not_bold"),
                          # uiOutput("H1point")
                          conditionalPanel("input.H1hyp == 'H1.point'",
                                           div(sliderInput(inputId = "H1pointslide", 
                                                           label   = NULL, 
                                                           min     = -2, 
                                                           max     = 2, 
                                                           value   = .2, 
                                                           ticks   = FALSE, 
                                                           step    = .1, 
                                                           width   = "100%"), class = "not_bold"))
                   )
                 ),
                 br(),
                 conditionalPanel("input.H1hyp != 'H1.point'", 
                                  div(prettyRadioButtons(inputId  = "prior",
                                                         label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                         choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                         selected = "cauchy",
                                                         inline   = TRUE,
                                                         width    = "100%",
                                                         status   = "success",
                                                         shape    = "round",
                                                         fill     = TRUE
                                  ), class = "not_bold"), 
                                  fluidRow(
                                    column(width = 4,
                                           # uiOutput("prior.param1")
                                           conditionalPanel("input.prior == 'cauchy'",
                                                            div(sliderInput(inputId = "location.c",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 'normal'",
                                                            div(sliderInput(inputId = "location.n",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderInput(inputId = "location.t",
                                                                            label   = em("Location:"),
                                                                            min     = -3,
                                                                            max     = 3,
                                                                            value   = 0,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4,
                                           # uiOutput("prior.param2")
                                           conditionalPanel("input.prior == 'cauchy'",
                                                            div(sliderInput(inputId = "scale.c",
                                                                            label   = em("Scale:"),
                                                                            min     = .1,
                                                                            max     = 2,
                                                                            value   = .707,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 'normal'",
                                                            div(sliderInput(inputId = "scale.n",
                                                                            label   = em("Scale:"),
                                                                            min     = .1,
                                                                            max     = 2,
                                                                            value   = 1,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold")),
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderInput(inputId = "scale.t",
                                                                            label   = em("Scale:"),
                                                                            min     = .1,
                                                                            max     = 2,
                                                                            value   = 1,
                                                                            ticks   = FALSE,
                                                                            step    = 0.1,
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4,
                                           # uiOutput("prior.param3")
                                           conditionalPanel("input.prior == 't.student'",
                                                            div(sliderInput(inputId = "df.t",
                                                                            label   = em("df:"),
                                                                            min     = 1,
                                                                            max     = 100,
                                                                            value   = 1,
                                                                            ticks   = FALSE,
                                                                            step    = 1,
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ), 
                                  br(),
                 ),
                 div(prettyRadioButtons(inputId  = "BF10.01",
                                        label    = em("Bayes Factor:"),
                                        choices  = list("$BF_{10}$" = "BF10",
                                                        "$BF_{01}$" = "BF01"),
                                        selected = "BF10",
                                        inline   = TRUE,
                                        width    = "100%",
                                        status   = "success",
                                        shape    = "round",
                                        fill     = TRUE), class = "not_bold"),
                 br(),
                 em("Prior probability:"),
                 fluidRow(
                   column(width = 6,
                          setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
                          div(sliderInput(inputId = "priorprob0",
                                          label   = em("$\\mathcal{H}_0$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold")),
                   column(width = 6,
                          div(sliderInput(inputId = "priorprob1",
                                          label   = em("$\\mathcal{H}_1$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold"))
                 ),
                 br(),
                 width = 4
               ),
               
               mainPanel(
                 h4("Summary"),
                 br(), 
                 tableOutput("BF.df1B"),
                 # br(),
                 # tableOutput("BF.df1"),
                 br(), br(),
                 # h4("Bayes factor and the prior distribution for $\\delta$ under $\\mathcal{H}_1$"),
                 # uiOutput("BF.df1"),
                 # br(), br(),
                 h4("Prior and posterior model probabilities"),
                 tableOutput("BF.df2"),
                 br(), br(),
                 h4("Posterior distribution of $\\delta$ under $\\mathcal{H}_1$"),
                 plotOutput("BFplot3"),
                 br(), br(),
                 h4("Bayes factor ", HTML("&#8212;"), " Interpretation 1"),
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
                 br(), br(),
                 h4("Bayes factor ", HTML("&#8212;"), " Interpretation 2"),
                 withMathJax(uiOutput("BFint2")),
                 br(),
                 fluidRow(uiOutput("BF.formula2"), align = "center"),
                 br(), br(),
                 plotOutput("BFplot2"),
                 br(), br(), width=8
               )
             )
             
    ),
    tabPanel("Keep in mind",
             br(), 
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed;width:30%;", 
                 h3(strong("Descriptives")),
                 h4("Group A"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean1.tab4",
                                           label   = em("Mean:"),
                                           min     = -16,      # -10
                                           max     = 16,       # 10
                                           value   = 15.292,   # 0
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd1.tab4",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 6.376,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n1.tab4",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 24,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 h4("Group B"),
                 fluidRow(
                   column(4, align = "left",
                          div(numericInput(inputId = "mean2.tab4",
                                           label   = em("Mean:"),
                                           min     = -16,       # -10
                                           max     = 16,        # 10
                                           value   = 10.88,     # 0.2
                                           step    = 0.1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "sd2.tab4",
                                           label   = em("SD:"),
                                           min     = 0,
                                           max     = NA,
                                           value   = 4.324,    # 1
                                           step    = .1,
                                           width = "100%"), class = "not_bold")
                   ),
                   column(4, align = "left",
                          div(numericInput(inputId = "n2.tab4",
                                           label   = em("$N$:"),
                                           min     = 5,
                                           max     = 50,
                                           value   = 25,    # 30
                                           width = "100%"), class = "not_bold")
                   )
                 ),
                 hr(style = "border-top: 2px solid #005E3C;"),
                 h3(strong("Bayesian test")),
                 em("Null and alternative hypotheses:"),
                 fluidRow(
                   column(width = 4, "$\\mathcal{H}_0:\\delta=0$"),
                   column(width = 8,
                          div(prettyRadioButtons(inputId  = "H1hyp.tab4",
                                                 label    = em("$\\mathcal{H}_1$:$\\hspace{1mm}$"),
                                                 choices  = list("$\\delta\\not=0$" = "H1.diff0",
                                                                 "$\\delta>0$" = "H1.larger0",
                                                                 "$\\delta<0$" = "H1.smaller0",
                                                                 "$\\delta=\\delta_1$" = "H1.point"),
                                                 selected = "H1.diff0",
                                                 inline   = TRUE,
                                                 width    = "100%",
                                                 status   = "success",
                                                 shape    = "round",
                                                 fill     = TRUE
                          ), class = "not_bold"),
                          # uiOutput("H1point.tab4")
                          conditionalPanel("input.H1hyp == 'H1.point'",
                                           div(sliderInput(inputId = "H1pointslide.tab4", 
                                                           label   = NULL, 
                                                           min     = -2, 
                                                           max     = 2, 
                                                           value   = .2, 
                                                           ticks   = FALSE, 
                                                           step    = .1, 
                                                           width   = "100%"), class = "not_bold"))
                   )
                 ),
                 br(),
                 conditionalPanel("input.H1hyp != 'H1.point'", 
                                  div(prettyRadioButtons(inputId  = "prior.tab4",
                                                         label    = em("Prior for $\\delta$ under $\\mathcal{H}_1$:"),
                                                         choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "$t$-Student" = "t.student"),
                                                         selected = "cauchy",
                                                         inline   = TRUE,
                                                         width    = "100%",
                                                         status   = "success",
                                                         shape    = "round",
                                                         fill     = TRUE
                                  ), class = "not_bold"),
                                  fluidRow(
                                    column(width = 4, 
                                           # uiOutput("prior.param1.tab4")
                                           conditionalPanel("input.prior == 'cauchy'", 
                                                            div(sliderInput(inputId = "location.c.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.prior == 'normal'", 
                                                            div(sliderInput(inputId = "location.n.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.prior == 't.student'", 
                                                            div(sliderInput(inputId = "location.t.tab4", 
                                                                            label   = em("Location:"), 
                                                                            min     = -3, 
                                                                            max     = 3, 
                                                                            value   = 0, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4, 
                                           # uiOutput("prior.param2.tab4")
                                           conditionalPanel("input.prior == 'cauchy'", 
                                                            div(sliderInput(inputId = "scale.c.tab4", 
                                                                            label   = em("Scale:"), 
                                                                            min     = .1, 
                                                                            max     = 2, 
                                                                            value   = .707, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.prior == 'normal'", 
                                                            div(sliderInput(inputId = "scale.n.tab4", 
                                                                            label   = em("Scale:"), 
                                                                            min     = .1, 
                                                                            max     = 2, 
                                                                            value   = 1, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold")), 
                                           conditionalPanel("input.prior == 't.student'", 
                                                            div(sliderInput(inputId = "scale.t.tab4", 
                                                                            label   = em("Scale:"), 
                                                                            min     = .1, 
                                                                            max     = 2, 
                                                                            value   = 1, 
                                                                            ticks   = FALSE, 
                                                                            step    = 0.1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    ),
                                    column(width = 4, 
                                           # uiOutput("prior.param3.tab4")
                                           conditionalPanel("input.prior == 't.student'", 
                                                            div(sliderInput(inputId = "df.t.tab4", 
                                                                            label   = em("df:"), 
                                                                            min     = 1, 
                                                                            max     = 100, 
                                                                            value   = 1, 
                                                                            ticks   = FALSE, 
                                                                            step    = 1, 
                                                                            width   = "100%"), class = "not_bold"))
                                    )
                                  ),
                                  br()
                 ),
                 div(prettyRadioButtons(inputId  = "BF10.01.tab4",
                                        label    = em("Bayes Factor:"),
                                        choices  = list("$BF_{10}$" = "BF10",
                                                        "$BF_{01}$" = "BF01"),
                                        selected = "BF10",
                                        inline   = TRUE,
                                        width    = "100%",
                                        status   = "success",
                                        shape    = "round",
                                        fill     = TRUE), class = "not_bold"),
                 br(),
                 em("Prior probability:"),
                 fluidRow(
                   column(width = 6,
                          setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
                          div(sliderInput(inputId = "priorprob0.tab4",
                                          label   = em("$\\mathcal{H}_0$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold")),
                   column(width = 6,
                          div(sliderInput(inputId = "priorprob1.tab4",
                                          label   = em("$\\mathcal{H}_1$:"),
                                          min     = 0,
                                          max     = 100,
                                          value   = 50,
                                          post    = "%",
                                          ticks   = FALSE,
                                          step    = 5), class = "not_bold"))
                 ),
                 br(),
                 width = 4
               ),
               
               mainPanel(
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
                 # br(), 
                 conditionalPanel("input.keepinmind == 'topic1'", br(), htmlOutput("kim.out.topic1.df1")), 
                 conditionalPanel("input.keepinmind == 'topic2'", br(), plotOutput("kim.out.topic2.plot1")), 
                 br(), br(), 
                 conditionalPanel("input.keepinmind == 'topic2'", htmlOutput("kim.out.topic2.df1")),
                 conditionalPanel("input.keepinmind == 'topic5'", htmlOutput("kim.out.topic5.df1")), 
                 conditionalPanel("input.keepinmind == 'topic5'", uiOutput("kim.out.topic5.part2"), br()), 
                 conditionalPanel("input.keepinmind == 'topic5'", fluidRow(
                   align = "center", 
                   column(3),
                   column(6, align = "center", sliderInput("Ncommon", "Sample size:", min = 50, max = 5000, value = 50, step = 50, 
                                                           animate = animationOptions( interval = 100 ), width = '100%', ticks   = FALSE, )), 
                   column(3)
                 )), 
                 conditionalPanel("input.keepinmind == 'topic5'", fluidRow(
                   column(1),
                   column(5, align = 'center', plotOutput("kim.out.topic5.plot1")), 
                   column(5, align = 'center', plotOutput("kim.out.topic5.plot2")), 
                   column(1))), 
                 conditionalPanel("input.keepinmind == 'topic5'", uiOutput("kim.out.topic5.part3")), 
                 # conditionalPanel("input.keepinmind == 'topic7'", fluidRow(column(4, htmlOutput("kim.out.topic7.df1")),
                 #                                                           column(4, htmlOutput("kim.out.topic7.df2")),
                 #                                                           column(4, htmlOutput("kim.out.topic7.df3")))),
                 conditionalPanel("input.keepinmind == 'topic7'",
                                  fluidRow(
                                    column(4, selectInput("BFClassTbl", "Choose:",
                                                          choices = c("Jeffreys (1961)",
                                                                      "Kass and Raftery (1995)",
                                                                      "Lee and Wagenmakers (2014)"),
                                                          selected = "Kass and Raftery (1995)", width = '100%')
                                    ),
                                    column(8, htmlOutput("kim.out.topic7.dfchosen"))
                                  )
                 ),
                 
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.part2"), br()), 
                 conditionalPanel("input.keepinmind == 'topic7'", htmlOutput("kim.out.topic7.df4")),
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.part3"), br()), 
                 width = 8
               ))), 
    tabPanel("Let's practice!", 
             br(), 
             fluidRow( htmlOutput("practice") ), 
             "abc")
  )#)
  
)

# Server function ----
server <- function(input, output, session) {
  
  # Tab 1 - Instructions:
  source("Tab1-Instructions.R", local = TRUE)
  
  # Tab 2 - Introduction:
  source("Tab2-Introduction.R", local = TRUE)
  
  # Tab 3 - Bayesian t-test:
  source("Tab3-Bayesian-ttest.R", local = TRUE)
  
  # Tab 4 - Keep in mind:
  source("Tab4-KeepInMind.R", local = TRUE)
  
  # Tab 5 - Let's practice:
  output$practice <- renderUI({
    tags$iframe(
      src="https://statsedge.org/StatsEdgeShinyApps/learnBF_tutorial/", 
      width="100%", height="100%", frameBorder=0, style="height: 100vh;", scrolling = 'yes'
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
  # Change prior prob of H1 when that of H0 changes (tab4):
  observeEvent(input$priorprob0.tab4,  {
    updateSliderInput(session = session, 
                      inputId = "priorprob1.tab4", 
                      value   = 100 - input$priorprob0.tab4)
  })
  # Change prior prob of H1 when that of H0 changes (tab4):
  observeEvent(input$priorprob1.tab4,  {
    updateSliderInput(session = session, 
                      inputId = "priorprob0.tab4", 
                      value   = 100 - input$priorprob1.tab4)
  })
  
  # To force these inputs to be available:
  location   <- reactive({ switch(input$prior, 
                                  'cauchy'    = req(input$location.c), 
                                  'normal'    = req(input$location.n), 
                                  't.student' = req(input$location.t)) })
  scale      <- reactive({ switch(input$prior, 
                                  'cauchy'    = req(input$scale.c), 
                                  'normal'    = req(input$scale.n), 
                                  't.student' = req(input$scale.t)) })
  df         <- reactive({ switch(input$prior, 
                                  'cauchy'    = NULL, 
                                  'normal'    = NULL, 
                                  't.student' = req(input$df.t)) })
  
  # Hyperlinks to tabs:
  observeEvent(input$intro.tab2a, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab2b, {
    updateTabsetPanel(session, "maintabs", "Introduction")
  })
  observeEvent(input$intro.tab3a, {
    updateTabsetPanel(session, "maintabs", "Bayesian t-test")
  })
  observeEvent(input$intro.tab4a, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab4b, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab4c, {
    updateTabsetPanel(session, "maintabs", "Keep in mind")
  })
  observeEvent(input$intro.tab5a, {
    updateTabsetPanel(session, "maintabs", "Let's practice!")
  })
  
  # Cohen's d:
  cohen.d <- reactive({ (input$mean1 - input$mean2) / sqrt(((input$n1-1) * (input$sd1^2) + (input$n2-1) * (input$sd2^2)) / (input$n1 + input$n2 - 2)) })
  
  # Sync the side panel of tabs 3 and 4 (and 2 for descriptives only):
  observe({
    req(input$mean1)
    updateNumericInput(session, 'mean1.tab4',        value = input$mean1)
    updateNumericInput(session, 'sd1.tab4',          value = input$sd1)
    updateNumericInput(session, 'n1.tab4',           value = input$n1)
    updateNumericInput(session, 'mean2.tab4',        value = input$mean2)
    updateNumericInput(session, 'sd2.tab4',          value = input$sd2)
    updateNumericInput(session, 'n2.tab4',           value = input$n2)
    updateNumericInput(session, 'H1hyp.tab4',        value = input$H1hyp)
    updateNumericInput(session, 'H1pointslide.tab4', value = input$H1pointslide)
    updateNumericInput(session, 'prior.tab4',        value = input$prior)
    updateNumericInput(session, 'location.c.tab4',   value = input$location.c)
    updateNumericInput(session, 'location.n.tab4',   value = input$location.n)
    updateNumericInput(session, 'location.t.tab4',   value = input$location.t)
    updateNumericInput(session, 'scale.c.tab4',      value = input$scale.c)
    updateNumericInput(session, 'scale.n.tab4',      value = input$scale.n)
    updateNumericInput(session, 'scale.t.tab4',      value = input$scale.t)
    updateNumericInput(session, 'df.t.tab4',         value = input$df.t)
    updateNumericInput(session, 'BF10.01.tab4',      value = input$BF10.01)
    updateNumericInput(session, 'priorprob0.tab4',   value = input$priorprob0)
    # updateNumericInput(session, 'priorprob1.tab4',   value = input$priorprob1) # avoid infinite loop
    # Intro, classical test:
    updateNumericInput(session, 'mean1.tab2',        value = input$mean1)
    updateNumericInput(session, 'sd1.tab2',          value = input$sd1)
    updateNumericInput(session, 'n1.tab2',           value = input$n1)
    updateNumericInput(session, 'mean2.tab2',        value = input$mean2)
    updateNumericInput(session, 'sd2.tab2',          value = input$sd2)
    updateNumericInput(session, 'n2.tab2',           value = input$n2)
    # Intro, Bayes test:
    updateNumericInput(session, 'H1hyp.bys',        value = input$H1hyp)
    updateNumericInput(session, 'H1pointslide.bys', value = input$H1pointslide)
    updateNumericInput(session, 'prior.bys',        value = input$prior)
    updateNumericInput(session, 'location.c.bys',   value = input$location.c)
    updateNumericInput(session, 'location.n.bys',   value = input$location.n)
    updateNumericInput(session, 'location.t.bys',   value = input$location.t)
    updateNumericInput(session, 'scale.c.bys',      value = input$scale.c)
    updateNumericInput(session, 'scale.n.bys',      value = input$scale.n)
    updateNumericInput(session, 'scale.t.bys',      value = input$scale.t)
    updateNumericInput(session, 'df.t.bys',         value = input$df.t)
  })
  observe({
    req(input$mean1.tab4)
    updateNumericInput(session, 'mean1',        value = input$mean1.tab4)
    updateNumericInput(session, 'sd1',          value = input$sd1.tab4)
    updateNumericInput(session, 'n1',           value = input$n1.tab4)
    updateNumericInput(session, 'mean2',        value = input$mean2.tab4)
    updateNumericInput(session, 'sd2',          value = input$sd2.tab4)
    updateNumericInput(session, 'n2',           value = input$n2.tab4)
    updateNumericInput(session, 'H1hyp',        value = input$H1hyp.tab4)
    updateNumericInput(session, 'H1pointslide', value = input$H1pointslide.tab4)
    updateNumericInput(session, 'prior',        value = input$prior.tab4)
    updateNumericInput(session, 'location.c',   value = input$location.c.tab4)
    updateNumericInput(session, 'location.n',   value = input$location.n.tab4)
    updateNumericInput(session, 'location.t',   value = input$location.t.tab4)
    updateNumericInput(session, 'scale.c',      value = input$scale.c.tab4)
    updateNumericInput(session, 'scale.n',      value = input$scale.n.tab4)
    updateNumericInput(session, 'scale.t',      value = input$scale.t.tab4)
    updateNumericInput(session, 'df.t',         value = input$df.t.tab4)
    updateNumericInput(session, 'BF10.01',      value = input$BF10.01.tab4)
    updateNumericInput(session, 'priorprob0',   value = input$priorprob0.tab4)
  })
  observe({
    req(input$mean1.tab2)
    updateNumericInput(session, 'mean1',        value = input$mean1.tab2)
    updateNumericInput(session, 'sd1',          value = input$sd1.tab2)
    updateNumericInput(session, 'n1',           value = input$n1.tab2)
    updateNumericInput(session, 'mean2',        value = input$mean2.tab2)
    updateNumericInput(session, 'sd2',          value = input$sd2.tab2)
    updateNumericInput(session, 'n2',           value = input$n2.tab2)
  })
  observe({
    req(input$H1hyp.bys)
    updateNumericInput(session, 'H1hyp',        value = input$H1hyp.bys)
    updateNumericInput(session, 'H1pointslide', value = input$H1pointslide.bys)
    updateNumericInput(session, 'prior',        value = input$prior.bys)
    updateNumericInput(session, 'location.c',   value = input$location.c.bys)
    updateNumericInput(session, 'location.n',   value = input$location.n.bys)
    updateNumericInput(session, 'location.t',   value = input$location.t.bys)
    updateNumericInput(session, 'scale.c',      value = input$scale.c.bys)
    updateNumericInput(session, 'scale.n',      value = input$scale.n.bys)
    updateNumericInput(session, 'scale.t',      value = input$scale.t.bys)
    updateNumericInput(session, 'df.t',         value = input$df.t.bys)
  })
  
}

# Call the app ----
shinyApp(ui = ui, server = server)






