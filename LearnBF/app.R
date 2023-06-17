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
# install devtools in server
# install kableExtra (now 1.3.4) from Github: devtools::install_github("haozhu233/kableExtra")

# Function computing the t-test from summaries ----
t.test.summ <- function(m1, m2, sd1, sd2, n1, n2)
{
  group1 <- scale(1:n1) * sd1 + m1
  group2 <- scale(1:n2) * sd2 + m2
  # t.test(x = group1, y = group2, var.equal = TRUE)
  sp     <- sqrt((((n1-1)*(sd1^2)+(n2-1)*(sd2^2)))/(n1+n2-2))
  t      <- (m1 - m2) / (sp * sqrt(1/n1 + 1/n2))
  p      <- 2 * pt(abs(t), n1 + n2 - 2, lower.tail = FALSE)
  df     <- n1 + n2 - 2
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
  
  titlePanel("Learning about the Bayes factor!"),
  
  sidebarLayout(
    
    sidebarPanel(
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
               uiOutput("H1point"))
      ), 
      br(), 
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
        column(width = 4, uiOutput("prior.param1")), 
        column(width = 4, uiOutput("prior.param2")), 
        column(width = 4, uiOutput("prior.param3"))
      ), 
      br(),
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
      
      tabsetPanel(
        id = "maintabs", 
        type = "pills",
        selected = "Instructions", ##### "Bayesian t-test", ##### "Keep in mind", ##### "Introduction", 
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
                 conditionalPanel("input.intro == 'intro.topic1'", uiOutput("introduction1")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("introduction2a")), 
                 conditionalPanel("input.intro == 'intro.topic2'", fluidRow(
                   align = "left", 
                   column(4, align = "left", 
                          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), 
                          sliderInput("alpha", "Sig. level:", min = .001, max = 0.2, value = .05, step = .01, width = '100%')), 
                   column(8)
                 )), 
                 conditionalPanel("input.intro == 'intro.topic2'", plotOutput("ttest.crit")), 
                 conditionalPanel("input.intro == 'intro.topic2'", br(), br(), tableOutput("ttest")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("introduction2b")), 
                 conditionalPanel("input.intro == 'intro.topic3'", uiOutput("introduction3")), 
                 conditionalPanel("input.intro == 'intro.topic4'", uiOutput("introduction4a")), 
                 conditionalPanel("input.intro == 'intro.topic4'", h4(em("Box: How to derive the Bayes factor.")), div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                                                                                                       uiOutput("introduction4b"))), 
                 conditionalPanel("input.intro == 'intro.topic4'", br(), br()), 
                 conditionalPanel("input.intro == 'intro.topic5'", uiOutput("introduction5a")), 
                 conditionalPanel("input.intro == 'intro.topic5'", plotOutput("intro.topic5.plot1")), 
                 conditionalPanel("input.intro == 'intro.topic5'", br(), br()),
                 conditionalPanel("input.intro == 'intro.topic5'", uiOutput("introduction5b")), 
                 conditionalPanel("input.intro == 'intro.topic5'", h4(em("Box: The marginal likelihood.")), div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                                                                                                uiOutput("introduction5c"))), 
                 conditionalPanel("input.intro == 'intro.topic5'", br(), br()),
                 conditionalPanel("input.intro == 'intro.topic6'", uiOutput("introduction6")), 
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
                 br(), br()
        ),
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
                 conditionalPanel("input.keepinmind == 'topic7'", uiOutput("kim.out.topic7.part3"), br())
                 
                 
        ), 
        tabPanel("Let's practice!", 
                 br(), 
                 fluidRow( htmlOutput("practice") ), 
                 "abc")
      ),
      width = 8
    )
    
  )
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
  
  # Decide which parameter input to show depending on the chosen prior:
  output$prior.param1 <- renderUI({
    if (input$prior == 'cauchy') {
      div(sliderInput(inputId = "location.c", 
                      label   = em("Location:"), 
                      min     = -3, 
                      max     = 3, 
                      value   = 0, 
                      ticks   = FALSE, 
                      step    = 0.1, 
                      width   = "100%"), class = "not_bold")
    } else if (input$prior == 'normal') {
      div(sliderInput(inputId = "location.n", 
                      label   = em("Location:"), 
                      min     = -3, 
                      max     = 3, 
                      value   = 0, 
                      ticks   = FALSE, 
                      step    = 0.1, 
                      width   = "100%"), class = "not_bold")
    } else {
      div(sliderInput(inputId = "location.t", 
                      label   = em("Location:"), 
                      min     = -3, 
                      max     = 3, 
                      value   = 0, 
                      ticks   = FALSE, 
                      step    = 0.1, 
                      width   = "100%"), class = "not_bold")
    }
  })
  output$prior.param2 <- renderUI({
    if (input$prior == 'cauchy') {
      div(sliderInput(inputId = "scale.c", 
                      label   = em("Scale:"), 
                      min     = 0.1, 
                      max     = 2, 
                      value   = 0.707, 
                      ticks   = FALSE, 
                      step    = 0.001, 
                      round   = -3, 
                      width   = "100%"), class = "not_bold")
    } else if (input$prior == 'normal') {
      div(sliderInput(inputId = "scale.n", 
                      label   = em("Scale:"), 
                      min     = 0.1, 
                      max     = 2, 
                      value   = 1, 
                      ticks   = FALSE, 
                      step    = 0.001, 
                      round   = -3, 
                      width   = "100%"), class = "not_bold")
    } else {
      div(sliderInput(inputId = "scale.t", 
                      label   = em("Scale:"), 
                      min     = 0.1, 
                      max     = 2, 
                      value   = 1, 
                      ticks   = FALSE, 
                      step    = 0.001, 
                      round   = -3, 
                      width   = "100%"), class = "not_bold")
    }
  })
  output$prior.param3 <- renderUI({
    if (input$prior == 't.student') {
      div(sliderInput(inputId = "df.t", 
                      label   = em("df:"), 
                      min     = 1, 
                      max     = 100, 
                      value   = 1, 
                      ticks   = FALSE, 
                      step    = 1, 
                      width   = "100%"), class = "not_bold")
    } else NULL
  })
  
  # To force these inputs to be available:
  location.c <- reactive({ if (input$prior == 'cauchy') req(input$location.c) })
  scale.c    <- reactive({ if (input$prior == 'cauchy') req(input$scale.c) })
  location.n <- reactive({ if (input$prior == 'normal') req(input$location.n) })
  scale.n    <- reactive({ if (input$prior == 'normal') req(input$scale.n) })
  location.t <- reactive({ if (input$prior == 't.student') req(input$location.t) })
  scale.t    <- reactive({ if (input$prior == 't.student') req(input$scale.t) })
  df.t       <- reactive({ if (input$prior == 't.student') req(input$df.t) })
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
  
  # Slide bar for point H1:
  output$H1point <- renderUI({
    if (input$H1hyp == 'H1.point') {
      div(sliderInput(inputId = "H1pointslide", 
                      label   = NULL, 
                      min     = -2, 
                      max     = 2, 
                      value   = .2, 
                      ticks   = FALSE, 
                      step    = .1, 
                      width   = "100%"), class = "not_bold")
    }
  })
  # To force this input to be available:
  H1pointslide <- reactive({ if (input$H1hyp == 'H1.point') req(input$H1pointslide) })
  
  # Cohen's d:
  cohen.d <- reactive({ (input$mean1 - input$mean2) / sqrt(((input$n1-1) * (input$sd1^2) + (input$n2-1) * (input$sd2^2)) / (input$n1 + input$n2 - 2)) })
  
}

# Call the app ----
shinyApp(ui = ui, server = server)






