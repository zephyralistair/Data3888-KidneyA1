#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(glue)
library(tidyverse)
library(shinydashboard)

load("shiny.rdata")
#final_model <- lr_fitall

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction"),
            menuItem("Preidction", tabName = "preidction")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "introduction",
                    h2("Introduction"),
                    p("This is a risk calculator about kidney transplantation, which can predict the success of kidney transplantation by inputting age, gender and other variables. Its main targeted users are doctors, which can help doctors accurately predict the results of kidney transplantation."),
                    br(),
                    br(),
                    h2("Gene Summarize"),
                    p("The tissue-specific pattern of mRNA expression can indicate important clues about gene function. High-density oligonucleotide arrays offer the opportunity to examine patterns of gene expression on a genome scale. Toward this end, we have designed custom arrays that interrogate the expression of the vast majority of protein-encoding human and mouse genes and have used them to profile a panel of 79 human and 61 mouse tissues. The resulting data set provides the expression patterns for thousands of predicted genes, as well as known and poorly characterized genes, from mice and humans. We have explored this data set for global trends in gene expression, evaluated commonly used lines of evidence in gene prediction methodologies, and investigated patterns indicative of chromosomal organization of transcription. We describe hundreds of regions of correlated transcription and show that some are subject to both tissue and parental allele-specific expression, suggesting a link between spatial expression and imprinting."),
                    br(),
                    h4("NM_005526"),
                    p("Gene Symbol: HSF1"),
                    p(strong("Heat shock factor 1 (HSF1)"), "is a protein that in humans is encoded by the HSF1 gene. HSF1 is highly conserved in eukaryotes and is the primary mediator of transcriptional responses to proteotoxic stress with important roles in non-stress regulation such as development and metabolism."),
                    br(),
                    h4("NM_014654"),
                    p("Gene Symbol: SDC3"),
                    p(strong("Syndecan-3 (SDC3)"), " is a protein that in humans is encoded by the SDC3 gene."),
                    br(),
                    h4("NM_012474"),
                    p("Gene Symbol: UCK2"),
                    p(strong("Uridine-cytidine kinase 2 (UCK2)"),"is an enzyme that in humans is encoded by the UCK2 gene."),
                    p("The protein encoded by this gene catalyzes the phosphorylation of uridine and cytidine to uridine monophosphate (UMP) and cytidine monophosphate (CMP), respectively. This is the first step in the production of the pyrimidine nucleoside triphosphates required for RNA and DNA synthesis. In addition, an allele of this gene may play a role in mediating nonhumoral immunity to Hemophilus influenzae type B."),
                    br(),
                    br(),
                    br(),
                    h2("CV accuracy"),
                    p("The overall cv accuracy is 0.76."),
                    p("The cv accuracy of GLM(generalized linear model) model is a little over 0.825."),
                    p("The cv accuracy of RF(random forest) model is a little over 0.84.")
                    
            ),
            
            # Second tab content
            tabItem(tabName = "preidction",
                    fluidRow(
                      box("If the patient is non-binary", br(), "There is no option here",
                          selectInput('sex', 'Select Sex', unique(combined_df_unscaled$sex))),
                      box(numericInput('age', 'Select Age', '0', '100','1')),
                      box(numericInput('NM_000544', 'Select NM_000544', '0', '100','1')),
                      box(numericInput('NM_004803', 'Select NM_004803', '0', '100','1'))
                    ),
                    mainPanel(
                      textOutput('outcome')
                    )
            )
        )
    )
)


server <- function(input, output) {
  output$outcome <- renderText({
    combined_df_unscaled$outcome %>%
      filter(
        sex == input$sex,
        age == input$age,
        NM_000544 == input$NM_000544,
        NM_004803 == input$NM_004803
      )
    print(paste("Your outcome is", outcome))
  })
}


shinyApp(ui, server)






