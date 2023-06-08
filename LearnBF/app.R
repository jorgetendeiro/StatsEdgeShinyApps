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
t.test.summ <- function(m1, m2, sd, n1, n2)
{
  group1 <- scale(1:n1) * sd + m1
  group2 <- scale(1:n2) * sd + m2
  # t.test(x = group1, y = group2, var.equal = TRUE)
  t      <- (m1 - m2) / (sd * sqrt(1/n1 + 1/n2))
  p      <- 2 * pt(abs(t), n1 + n2 - 2, lower.tail = FALSE)
  df     <- n1 + n2 - 2
  c(t = t, df = df, p = p)
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
                  "), 
             HTML(".not_bold label {font-weight:normal;}")),
  
  titlePanel("Learning about the Bayes factor!"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3(strong("Descriptives")), 
      h4("Group A"), 
      fluidRow(
        column(6, align = "left", 
               div(numericInput(inputId = "mean1",
                                label   = em("Mean:"),
                                min     = -10,
                                max     = 10,
                                value   = 0, 
                                step    = 0.1, 
                                width = "100%"), class = "not_bold")
        ), 
        column(6, align = "left", 
               div(numericInput(inputId = "n1",
                                label   = em("Sample size:"),
                                min     = 5,
                                max     = 50,
                                value   = 30, 
                                width = "100%"), class = "not_bold")
        )
      ), 
      h4("Group B"), 
      fluidRow(
        column(6, align = "left", 
               div(numericInput(inputId = "mean2",
                                label   = em("Mean:"),
                                min     = -10,
                                max     = 10,
                                value   = 0.2, 
                                step    = 0.1, 
                                width = "100%"), class = "not_bold")
        ), 
        column(6, align = "left", 
               div(numericInput(inputId = "n2",
                                label   = em("Sample size:"),
                                min     = 5,
                                max     = 50,
                                value   = 30, 
                                width = "100%"), class = "not_bold")
        )
      ), 
      fluidRow(
        column(3), 
        column(6, align = "left", 
               div(numericInput(inputId = "sdcommon",
                                label   = em("Standard deviation:"),
                                min     = 0,
                                max     = NA,
                                value   = 1, 
                                step    = 0.1, 
                                width = "100%"), class = "not_bold")
        ), 
        column(3)
      ), 
      hr(style = "border-top: 2px solid #005E3C;"), 
      h3(strong("Bayesian test")), 
      fluidRow(
        column(width = 6, 
               setSliderColor(c("#DCA559", "#DCA559"), c(1, 2)),
               div(sliderInput(inputId = "priorprob0", 
                               label   = em("Prior probability of $\\mathcal{H}_0$:"), 
                               min     = 0, 
                               max     = 100, 
                               value   = 50, 
                               post    = "%", 
                               ticks   = FALSE, 
                               step    = 5), class = "not_bold")), 
        column(width = 6, 
               div(sliderInput(inputId = "priorprob1", 
                               label   = em("Prior probability of $\\mathcal{H}_1$:"), 
                               min     = 0, 
                               max     = 100, 
                               value   = 50, 
                               post    = "%", 
                               ticks   = FALSE, 
                               step    = 5), class = "not_bold"))
      ), 
      br(), 
      div(prettyRadioButtons(inputId  = "prior",
                             label    = em("Prior:"), 
                             choices  = list("Cauchy" = "cauchy", "Normal" = "normal", "t-Student" = "t.student"), 
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
                             choices  = list("BF10" = "BF10", 
                                             "BF01" = "BF01"), 
                             selected = "BF10", 
                             inline   = TRUE, 
                             width    = "100%", 
                             status   = "success", 
                             shape    = "round", 
                             fill     = TRUE), class = "not_bold"), 
      width = 4
    ), 
    
    mainPanel( 
      
      tabsetPanel(
        id = "maintabs", 
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
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("ttest")), 
                 conditionalPanel("input.intro == 'intro.topic2'", uiOutput("introduction2b")), 
                 conditionalPanel("input.intro == 'intro.topic3'", uiOutput("introduction3")), 
                 conditionalPanel("input.intro == 'intro.topic4'", uiOutput("introduction4a")), 
                 conditionalPanel("input.intro == 'intro.topic4'", h4(em("Box: How to derive the Bayes factor.")), div(style = "border-style: solid; border-color: #DCA559; padding: 10px; color: gray;",
                                                                       uiOutput("introduction4b"))), 
                 conditionalPanel("input.intro == 'intro.topic4'", br(), br()), 
                 conditionalPanel("input.intro == 'intro.topic5'", uiOutput("introduction5")), 
                 conditionalPanel("input.intro == 'intro.topic6'", uiOutput("introduction6")), 
                 conditionalPanel("input.intro == 'intro.topic6'", fluidRow(
                   align = "center", 
                   column(3),
                   column(6, align = "center", sliderInput("Ncommon.BF.p", "Sample size:", min = 50, max = 5000, value = 50, step = 50, 
                                                           animate = animationOptions( interval = 100 ), width = '100%')), 
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
      div(numericInput(inputId  = "location.c", 
                       label    = em("Location:"), 
                       min      = -5,
                       max      = 5,
                       value    = 0, 
                       step     = 0.1, 
                       width    = "100%"), class = "not_bold")
    } else if (input$prior == 'normal') {
      div(numericInput(inputId = "location.n",
                       label   = em("Location:"),
                       min     = -5,
                       max     = 5,
                       value   = 0, 
                       step    = 0.1, 
                       width   = "100%"), class = "not_bold")
    } else {
      div(numericInput(inputId = "location.t",
                       label   = em("Location:"),
                       min     = -5,
                       max     = 5,
                       value   = 0, 
                       step    = 0.1, 
                       width   = "100%"), class = "not_bold")
    }
  })
  output$prior.param2 <- renderUI({
    if (input$prior == 'cauchy') {
      div(numericInput(inputId  = "scale.c", 
                       label    = em("Scale:"), 
                       min     = 0,
                       max     = 5,
                       value   = 0.707, 
                       step    = 0.1, 
                       width   = "100%"), class = "not_bold")
    } else if (input$prior == 'normal') {
      div(numericInput(inputId  = "scale.n", 
                       label    = em("Scale:"), 
                       min     = 0,
                       max     = 5,
                       value   = 1, 
                       step    = 0.1, 
                       width   = "100%"), class = "not_bold")
    } else {
      div(numericInput(inputId  = "scale.t", 
                       label    = em("Scale:"), 
                       min      = 0,
                       max      = 5,
                       value    = 1, 
                       step     = 0.1, 
                       width    = "100%"), class = "not_bold")
    }
  })
  output$prior.param3 <- renderUI({
    if (input$prior == 't.student') {
      div(numericInput(inputId = "df.t",
                       label   = em("df:"),
                       min     = 1,
                       max     = 50,
                       value   = 1, 
                       width   = "100%"), class = "not_bold")
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
  observeEvent(input$intro.tab5a, {
    updateTabsetPanel(session, "maintabs", "Let's practice!")
  })
  
}

# Call the app ----
shinyApp(ui = ui, server = server)






