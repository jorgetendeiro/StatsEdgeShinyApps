
# INSTRUCTIONS tab ----

output$instructions <- renderUI({
  outtext <- paste0(
    "Welcome to the <font color=\"#DCA559\"><b>Learning about the Bayes factor!</b></font>", 
    br(), br(),
    h4("Goal"), 
    "This app offers a practical tutorial of the Bayes factor in the context of ", em("null hypothesis Bayesian testing"), " (NHBT in short).", 
    br(), 
    "Think of the Bayes factor as the Bayesian counterpart to the $p$-value. There are not equivalent concepts, however, so it is important to distinguish both tools. This is one of our goals.", 
    br(), br(), 
    "We focus more on ", em("understanding"), " and ", em("correctly"), " interpreting the Bayes factor than on computational details.", 
    br(), 
    "We do provide some formulas in the ", actionLink("intro.tab2a", "Introduction", style = "font-weight: bold;"), ", but we keep things conceptual as much as possible.", 
    br(), br(),
    h4("Intended audience"), 
    "The app is particularly aimed at those who are new to Bayesian hypothesis testing and to the Bayes factor and need to ", em("apply"), " it in their research.", 
    br(), 
    "This may include academics at any stage of their careers.", 
    br(), 
    "Also journal editors and reviewers may benefit from the insights that we offer, in order to improve the quality of the published literature with respect to Bayesian hypotheses testing.", 
    br(), br(), 
    h4("How to use this app"), 
    "This app may be used for self-learning, or as a support medium in a workshop.", 
    br(), 
    "There are a lot of important concepts and nuances to learn and discuss. We therefore suggest that it may be best to allocate a minimum of 2 hours to work through all the material in detail.", 
    br(), br(), 
    "We suggest the following approach:", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- Proceed to the ", actionLink("intro.tab2b", "Introduction", style = "font-weight: bold;"), " tab.<br> We start by running the very familiar two-sided independent samples \\$t\\$-test. We do so in order to start from familiar ground, and also to start highlighting the similarities and differences between NHST and the \\$p\\$-value and their Bayesian counterparts (NHBT and the Bayes factor).<br> We then introduce the Bayes factor. The Bayes factor may be thought of in two complementary ways. We discuss each in turn.<br> We end the introduction by underlining some clear differences between the Bayes factor and the \\$p\\$-value. This is important, because the Bayes factor is not simply the 'Bayesian \\$p\\$-value'.<br><br>\n- Proceed to the ", actionLink("intro.tab3a", "Bayesian \\$t\\$-test", style = "font-weight: bold;"), " tab.<br> Feel free to manipulate the data (left-hand side menu, **Descriptives**) and the various test options (left-hand side menu, **Bayesian test**), and see how that affects the various results displayed.<br><br>\n- Proceed to the ", actionLink("intro.tab4a", "Keep in mind", style = "font-weight: bold;"), " tab.<br> Here we highlight important ideas, especially pitfalls that one should avoid (ref).<br> We also provide some good examples that may be used as a template.<br><br>\n- Proceed to the ", actionLink("intro.tab5a", "Let's practice!", style = "font-weight: bold;"), " tab.<br> Test your knowledge by answering to a few questions.<br> Feedback is provided to each question, including links to the app's sections where the point at hand was discussed, or extra suggested literature.") ))
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
