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
library(shinyvalidate)
library(dashboardthemes)
library(naivebayes)
load("shiny.rdata")
#final_model <- final_model

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green", 
    dashboardHeader(title = "kidney transplantation"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction"),
            menuItem("Gene Summarize", tabName = "gene_summarize"),
            menuItem("Prediction", tabName = "prediction")
        )
    ),
    dashboardBody(
     shinyDashboardThemes(
      theme = "grey_light"
    ),
        tabItems(
            # First tab content
            tabItem(tabName = "introduction",
                    h2("Introduction"),
                    p("This is a risk calculator for kidney transplantation that predicts the success of a kidney transplant by entering age, gender and other variables. The main target users are doctors, who can fill in patient information and data to help them accurately predict the outcome of their kidney transplant."),
                    br(),
                    br(),
                    
                    h2("Notes"),
                    h4("Age: "),
                    p("The age range is 18-68 years old, as the age range of our training set is only 18-68 years old, we will collect more data in the future to expand the age range so that we can get more accurate prediction results for people beyond this age range."),
                    h4("Sex: "),
                    p("The data in our training set is only binary gender, so unfortunately we are unable to provide a non-binary gender option here and will need to collect further data to expand our options."),
                    h4("Gene:"),
                    p("The default value is the average value of the genes in the database.If the doctor does not know the patient's NM_000544 and NM_004803 gene data or is unable to fill in these two options, the average of these two genes in the database will be taken automatically instead."),
           
            ),
            
            # Seconed tab content
            tabItem(tabName = "gene_summarize",
                    h2("Gene Summarize"),
                    p("The tissue-specific pattern of mRNA expression can indicate important clues about gene function. High-density oligonucleotide arrays offer the opportunity to examine patterns of gene expression on a genome scale. Toward this end, we have designed custom arrays that interrogate the expression of the vast majority of protein-encoding human and mouse genes and have used them to profile a panel of 79 human and 61 mouse tissues. The resulting data set provides the expression patterns for thousands of predicted genes, as well as known and poorly characterized genes, from mice and humans. We have explored this data set for global trends in gene expression, evaluated commonly used lines of evidence in gene prediction methodologies, and investigated patterns indicative of chromosomal organization of transcription. We describe hundreds of regions of correlated transcription and show that some are subject to both tissue and parental allele-specific expression, suggesting a link between spatial expression and imprinting."),
                    br(),
                    h4("NM_000544"),
                    p("Gene Symbol: TAP2"),
                    p(strong("TAP2"), " is a gene in humans that encodes the protein ",strong("Antigen peptide transporter 2.")),
                    br(),
                    h4("NM_004803"),
                    p("Gene Symbol: SLC22A14"),
                    p(strong("Solute carrier family 22 member 14"), " is a protein that in humans is encoded by the SLC22A14 gene."),
                   
                    
            ),
            
            
            # Third tab content
            tabItem(tabName = "prediction",
                    fluidRow(
                      column(width = 12,
                             box(
                               title = "Inputs", status = "danger", width = NULL,
                               radioButtons("sex", "Select Sex:",
                                            c("Male", "Female")),
                              numericInput("age", "Age:",
                                           min = 18,
                                           max = 68,
                                           value = 18
                                 ),
                              numericInput("NM_000544",
                                           "NM_000544 (optional):",
                                           min = 3,
                                           max = 5,
                                           value = 4.087559
                                 ),
                              numericInput("NM_004803",
                                           "NM_004803 (optional):",
                                           min = 4,
                                           max = 6,
                                           value = 4.827927
                               ),
                               )
                             ) 
                      ),
                      
                      column(width = 12,
                             box(
                               title = "Prediction", status = "warning", width = NULL,
                               htmlOutput("modelPrediction"),
                               br(),
                               h5("CV accuracy: "),
                               p("The overall cv accuracy is 0.76."),
                               p("The cv accuracy of GLM(generalized linear model) model is a little over 0.825."),
                               p("The cv accuracy of RF(random forest) model is a little over 0.84.")
                             )
                          )
            )
        )
    )
)


server <- function(input, output) {
  # ensures age is between 18 and 68
  ivAge <- InputValidator$new()
  ivAge$add_rule("age", sv_optional())
  ivAge$add_rule("age", sv_between(18, 68))
  ivAge$enable()
  
  # ensures NM_000544 is between 3 and 5
  ivNM_000544 <- InputValidator$new()
  ivNM_000544$add_rule("NM_000544", sv_between(3, 5))
  ivNM_000544$enable()
  
  # ensures NM_004803 is between 4 and 6
  ivNM_004803 <- InputValidator$new()
  ivNM_004803$add_rule("NM_004803", sv_between(4, 6))
  ivNM_004803$enable()
  


  output$modelPrediction <- shiny::renderUI({
    if (!ivNM_000544$is_valid() || !ivNM_004803$is_valid()) {
      output = sprintf("Predicted the success of a kidney transplant composition cannot be computed since one or more inputs are invalid.")
      return(output)
    }
   
    input_data <- data.frame(
      age = input$age,
      sex = input$sex,
      NM_000544 = input$NM_000544,
      NM_004803 = input$NM_004803
    )

    # calculation

    predicted_outcome <- predict(final_model, input_data)

    
    HTML(sprintf("The predicted kidney transplantation result for given inputs is <strong>%s%</strong>.", 
                 predicted_outcome))
  })
}


shinyApp(ui, server)






