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
library(DT)
library(data.table)
load("shiny.rdata")
#final_model <- final_model

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green", 
    dashboardHeader(title = "Kidney Transplant Outcome Predictor"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction"),
            menuItem("Gene Summary", tabName = "gene_summarize"),
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
                    p("This Shiny app is designed to help doctors to predict the outcome of a patient's kidney transplant, given age, gender and 2 gene expression values."),
                    br(),
                    br(),
                    
                    h3("Variable Summary"),
                    p("Note that given the limitations of our model's training set, it can only make accurate predictions for humans of the following types."),
                    h4("Age: "),
                    p("18 - 68 years old, as the age range of our training set is 18 - 68."),
                    h4("Gender: "),
                    p("Please input your biological gender, i.e. Male or Female. Unfortuanlly we are not able to provide an option for non-binary genders, as the data in our training set contains only people of binary genders."),
                    h4("Gene expression values:"),
                    p("Expression values from 3 to 6. Note that your input must be Log2 transformed. If the doctor is unable to determine the patient's TAP2 and SLC22A14 gene expression values, the default values can be used as inputs."),
                    p("Further research could be performed to improve the diversity of our model."),
                    br(),
                    br(),
                    h3("Data Provenance"),
                    navbarPage('GEO', 
                               tabPanel("Source Table",
                                        fluidPage(
                                          fluidRow(dataTableOutput("dt_table")))),
                               
                    ),
               
                    
                    
            ),
            
            # Seconed tab content
            tabItem(tabName = "gene_summarize",
                    h2("Gene Summary"),
                    p("In RNAseq data analysis, the expression value of a gene represents how much it is being expressed in a cell. In our model, 2 genes are selected as variables:"),
                    br(),
                    h3("TAP2"),
                    p(strong("Antigen peptide transporter 2."), " The protein encoded by this gene is involved in antigen presentation. This protein forms a heterodimer with ABCB2 in order to transport peptides from the cytoplasm to the endoplasmic reticulum. Mutations in this gene may be associated with ankylosing spondylitis, insulin-dependent diabetes mellitus, and celiac disease."),
                    h3("SLC22A14"),
                    p(strong("Solute carrier family 22 member 14."), " The encoded protein is a transmembrane protein which is thought to transport small molecules and since this protein is conserved among several species, it is suggested to have a fundamental role in mammalian systems."),
                    br(),
                    navbarPage('Gene', 
                               tabPanel("Information Table",
                                        fluidPage(
                                          fluidRow(dataTableOutput("dt_table1")))),
                    ),
                    br(),
                    br(),
                    p("Gene information is retrived from GeneCards."),
                    
            ),
            
            
            # Third tab content
            tabItem(tabName = "prediction",
                    fluidRow(
                      column(width = 12,
                             box(
                               title = "Inputs", status = "danger", width = NULL,
                               radioButtons("gender", "Gender:",
                                            c("Male", "Female")),
                              numericInput("age", "Age:",
                                           min = 18,
                                           max = 68,
                                           value = 18
                                 ),
                              numericInput("NM_000544",
                                           "Log2 TAP2 expression values:",
                                           min = 3,
                                           max = 6,
                                           value = 4.087559
                                 ),
                              numericInput("NM_004803",
                                           "Log2 SLC22A14 expression values:",
                                           min = 3,
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
                               h4("How reliable is this result?"),
                               p("Our model has an average accuracy of around 89% in a repeated cross validation evaluation."),
                               p("The model is also tested on 2 independent datasets, giving accuracies of 67% and 65%."),
                             )
                          )
            )
        )
    )
)


server <- function(input, output) {
  df_table <- reactive({
    data.table(
      Datasets = c(
        paste0("<a href='https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE51675'>", "GEO51675", "</a>"),
        paste0("<a href='https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE1563'>", "GEO1563", "</a>"),
        paste0("<a href='https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE46474'>", "GEO46474", "</a>")),
      Site = c('https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE51675', 'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE1563', 'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE46474')
    )
    
  })
  output$dt_table <- renderDataTable(
    df_table(), escape = FALSE, options = list(pageLength = 3))
  
  
  df_table1 <- reactive({
    data.table(
      Genes = c(
        paste0("<a href='http://biogps.org/#goto=genereport&id=6891'>", "TAP2", "</a>"),
        paste0("<a href='http://biogps.org/#goto=genereport&id=9389'>", "SLC22A14", "</a>")),
      Site = c('http://biogps.org/#goto=genereport&id=6891', 'http://biogps.org/#goto=genereport&id=9389')
    )
    
  })
  output$dt_table1 <- renderDataTable(
    df_table1(), escape = FALSE, options = list(pageLength = 2))
  
  
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
      output = sprintf("The outcome of your patient's a kidney transplant cannot be predicted since one or more inputs are invalid.")
      return(output)
    }
    
    mean_age = mean(combined_df_unscaled$age)
    sd_age = sd(combined_df_unscaled$age)
    
    mean_NM_000544 = mean(combined_df_unscaled$NM_000544)
    sd_NM_000544 = sd(combined_df_unscaled$NM_000544)
      
    mean_NM_004803 = mean(combined_df_unscaled$NM_004803)
    sd_NM_004803 = sd(combined_df_unscaled$NM_004803)
   
    input_data <- data.frame(
      age = (input$age - mean_age) / sd_age,
      gender = input$gender,
      NM_000544 = (input$NM_000544 - mean_NM_000544) / sd_NM_000544,
      NM_004803 = (input$NM_004803 - mean_NM_004803) / sd_NM_004803
    )

    # calculation

    predicted_outcome <- predict(final_model, input_data)

    
    HTML(sprintf("Your patient's predicted kidney transplant outcome for the given inputs is <strong>%s</strong>.", 
                 predicted_outcome))
  })
}


shinyApp(ui, server)






