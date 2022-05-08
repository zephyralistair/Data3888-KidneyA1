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
library(shinyvalidate)
library(tidyverse)
library(ggtext)
library(kableExtra)
library(shinydashboard)
library(dashboardthemes)

load("all_in_one.RData")
final_model <- stable_lm_1

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Body Fat Percentage Calculator", titleWidth = 350),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    fluidRow(
      box(
        title = "Welcome!",
        width = 12, status = "primary",
        htmlOutput("intro")
      )
    ),
    
    fluidRow(
      column(width = 4,
             box(
               title = "Inputs", status = "danger", width = NULL,
               radioButtons("sys", "System of Measurement:",
                            c("Metric" = "metric",
                              "Imperial" = "imperial")),
               conditionalPanel(
                 condition = "input.sys == 'metric'",
                 numericInput("abdomenMetric",
                              "Abdomen Circumference (cm):",
                              min = 70,
                              max = 150,
                              value = 90
                 ),
                 numericInput("weightMetric",
                              "Weight (kg):",
                              min = 53,
                              max = 165,
                              value = 80
                 )
               ),
               conditionalPanel(
                 condition = "input.sys == 'imperial'",
                 numericInput("abdomenImperial",
                              "Abdomen Circumference (in):",
                              min = 28,
                              max = 59,
                              value = 35.4
                 ),
                 numericInput("weightImperial",
                              "Weight (lb):",
                              min = 117,
                              max = 363,
                              value = 176.3
                 )
               ),
               numericInput("age",
                            "Age (optional):",
                            min = 22,
                            max = 81,
                            value = NULL
               )
             ) 
      ),
      
      column(width = 8,
             box(
               title = "Prediction", status = "warning", width = NULL,
               htmlOutput("modelPrediction"),
               plotOutput("plot", height = 100),
               htmlOutput("colourCode"),
               htmlOutput("category"),
               htmlOutput("reference")
             )
      )
    )

  )
)


#     # newborn has a abdomen circ of 20-30 cm
#     if(input < 0 || input > 200){
#         "Please Input Appropriate Abdomen Circumference!"
#     }else{
#         NULL
#     }
# }

# valid_weight <- function(input){
#     # newborn has a weight of 2-5 kg
#     # world's heaviest man 595 kg?
#     if(input < 0 || input > 700){
#         "Please Input Appropriate Weight!"
#     }else{
#         NULL
#     }
# }

# valid_age <- function(input){
#   if(input < 0 || input > 150){
#     "Please Input Appropriate Age!"
#   }else{
#     NULL
#   }
# }

# Define server logic required to output the model prediction
server <- function(input, output) {
  
    # ensures age is between 22 and 81
    ivAge <- InputValidator$new()
    ivAge$add_rule("age", sv_optional())
    ivAge$add_rule("age", sv_between(22, 81))
    ivAge$enable()
    
    # ensures weightMetric is between 53 and 165
    ivWeightMetric <- InputValidator$new()
    ivWeightMetric$add_rule("weightMetric", sv_between(53, 165))
    ivWeightMetric$enable()
    
    # ensures abdomenMetric is between 70 and 150
    ivAbdomenMetric <- InputValidator$new()
    ivAbdomenMetric$add_rule("abdomenMetric", sv_between(70, 150))
    ivAbdomenMetric$enable()
    
    # ensures weightImperial is between 117 and 363
    ivWeightImperial <- InputValidator$new()
    ivWeightImperial$add_rule("weightImperial", sv_between(117, 363))
    ivWeightImperial$enable()
    
    # ensures abdomenMetric is between 28 and 59
    ivAbdomenImperial <- InputValidator$new()
    ivAbdomenImperial$add_rule("abdomenImperial", sv_between(28, 59))
    ivAbdomenImperial$enable()
    
    output$intro <- shiny::renderUI({
        
      HTML("This app is designed to help you calculate your body fat percentage, given your weight and abdominal circumference.<br/><br/>
          You can also input your age if you'd like to see your body fat percentage category. Otherwise, you can just leave it blank.<br/><br/>
          Note that currently our model can only make accurate predictions for <strong>male adults</strong> aged between <strong>22-81 years old</strong>, with weight between <strong>53-165 kg (117-363 lb)</strong> and abdominal circumference between <strong>70-150 cm (28-59 in)</strong>.<br/><br/>
          If by any chance you input values within these limits, but our model still cannot accurately predict your body fat percentage (i.e. predicted body fat percentage is negative), the app will inform you of this.")
    })

    output$modelPrediction <- shiny::renderUI({
        # data validation
        # validate(
        #   need(input$abdomen, 'Please Input Your Abdomen Circumference!'),
        #   need(input$weight, 'Please Input Your Weight!'),
        #   valid_abdomen(input$abdomen),
        #   valid_weight(input$weight)
        # )
      
        if ((input$sys == "metric") && (!ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
          output = sprintf("Predicted body fat composition cannot be computed since one or more inputs are invalid.")
          return(output)
        }
      
        if ((input$sys == "imperial") && (!ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
          output = sprintf("Predicted body fat composition cannot be computed since one or more inputs are invalid.")
          return(output)
        }
      
        input_data <- data.frame()
        
        if (input$sys == "metric") {
          input_data <- data.frame(abdomen = input$abdomenMetric, weight_kg = input$weightMetric)
        } else {
          abdomenMetric = input$abdomenImperial * 2.54
          weightMetric = input$weightImperial * 0.45
          input_data <- data.frame(abdomen = abdomenMetric, weight_kg = weightMetric)
        }
        
        # calculation
        predicted_body_fat <- predict(final_model, input_data)
        
        if (predicted_body_fat < 0) {
          output = sprintf("Sorry, our model cannot accurately predict your body fat percentage (predicted body fat percentage is negative).")
          return(output)
        }
        
        confidence = predict(final_model, input_data, interval = "confidence", 0.95)$fit
        prediction = predict(final_model, input_data, interval = "predict", 0.95)$fit

        # information display
        HTML(sprintf("The predicted body fat composition for given inputs is <strong>%.2f%%</strong>.<br/><br/>
                     95%% confidence interval: %.2f%% - %.2f%%<br/>
                     95%% prediction interval: %.2f%% - %.2f%%<br/><br/>",
                     predicted_body_fat,
                     confidence[2],
                     confidence[3],
                     prediction[2],
                     prediction[3]
                     ))
    })
    
    output$category <- shiny::renderUI({
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      input_data <- data.frame()
      
      if (input$sys == "metric") {
        input_data <- data.frame(abdomen = input$abdomenMetric, weight_kg = input$weightMetric)
      } else {
        abdomenMetric = input$abdomenImperial * 2.54
        weightMetric = input$weightImperial * 0.45
        input_data <- data.frame(abdomen = abdomenMetric, weight_kg = weightMetric)
      }
      
      predicted_body_fat <- predict(final_model, input_data)
      
      if (predicted_body_fat < 0) {
        return(NULL)
      }
      
      pct_bf_boundaries = c(rep(0, 4))
      
      # https://zfcphp.arizona.edu/your-health/basics
      if (input$age <= 40) {
        pct_bf_boundaries = c(8, 19, 25, 100)
      } else if (input$age <= 60) {
        pct_bf_boundaries = c(11, 22, 27, 100)
      } else {
        pct_bf_boundaries = c(13, 25, 30, 100)
      }
      
      category = ""
      
      if (predicted_body_fat <= pct_bf_boundaries[1]) {
        category = "Your body fat percentage category is:<br/><br/><strong>Below healthy range</strong>"
      } else if (predicted_body_fat <= pct_bf_boundaries[2]) {
        category = "Your body fat percentage category is:<br/><br/><strong>Healthy weight</strong>"
      } else if (predicted_body_fat <= pct_bf_boundaries[3]) {
        category = "Your body fat percentage category is:<br/><br/><strong>Overweight</strong>"
      } else {
        category = "Your body fat percentage category is:<br/><br/><strong>Obese</strong>"
      }
      
      return(HTML(category))
    })
    
    output$plot <- shiny::renderPlot({
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      input_data <- data.frame()
      
      if (input$sys == "metric") {
        input_data <- data.frame(abdomen = input$abdomenMetric, weight_kg = input$weightMetric)
      } else {
        abdomenMetric = input$abdomenImperial * 2.54
        weightMetric = input$weightImperial * 0.45
        input_data <- data.frame(abdomen = abdomenMetric, weight_kg = weightMetric)
      }
      
      predicted_body_fat <- predict(final_model, input_data)
      
      if (predicted_body_fat < 0) {
        return(NULL)
      }
      
      pct_bf_boundaries = c(rep(0, 4))
      
      # https://zfcphp.arizona.edu/your-health/basics
      if (input$age <= 40) {
        pct_bf_boundaries = c(8, 19, 25, 100)
      } else if (input$age <= 60) {
        pct_bf_boundaries = c(11, 22, 27, 100)
      } else {
        pct_bf_boundaries = c(13, 25, 30, 100)
      }
      
      pct_bf_df = tibble(
        name = "Body Fat %",
        value = predicted_body_fat
      )
      
      pct_bf_plot <- pct_bf_df %>%
        ggplot() +
        geom_col(
          aes(x = 1, y = pct_bf_boundaries[4]),
          fill = "#ff6961",
          alpha = 0.9,
          width = 0.2
        ) +
        geom_col(
          aes(x = 1, y = pct_bf_boundaries[3]),
          fill = "#ffb54c",
          alpha = 0.9,
          width = 0.2
        ) +
        geom_col(
          aes(x = 1, y = pct_bf_boundaries[2]),
          fill = "#7abd7e",
          alpha = 0.9,
          width = 0.2
        ) +
        geom_col(
          aes(x = 1, y = pct_bf_boundaries[1]),
          fill = "#8dd8e6",
          alpha = 0.9,
          width = 0.2
        ) +
        geom_errorbar(
          aes(x = 1, ymin = predicted_body_fat, ymax = predicted_body_fat),
          color = "#000000",
          width = 0.2,
          size = 1
        ) +
        theme_minimal() +
        labs(
          x = "",
          y = ""
        ) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          axis.text = element_blank(),
          legend.position = "none"
        ) +
        scale_y_continuous(breaks = c()) +
        coord_flip() +
        geom_text(aes(label = sprintf("%.2f%%", predicted_body_fat), x = 0.85, y = predicted_body_fat, size = 12), vjust = 0)
     
      return(pct_bf_plot)
    })
    
    output$colourCode <- shiny::renderText({
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      input_data <- data.frame()
      
      if (input$sys == "metric") {
        input_data <- data.frame(abdomen = input$abdomenMetric, weight_kg = input$weightMetric)
      } else {
        abdomenMetric = input$abdomenImperial * 2.54
        weightMetric = input$weightImperial * 0.45
        input_data <- data.frame(abdomen = abdomenMetric, weight_kg = weightMetric)
      }
      
      predicted_body_fat <- predict(final_model, input_data)
      
      if (predicted_body_fat < 0) {
        return(NULL)
      }
      
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      df = data.frame(
        below = c("Below healthy range"),
        healthy = c("Healthy weight"),
        over = c("Overweight"),
        obese = c("Obese")
      )
      
      kbl(df, col = NULL) %>%
        kable_styling(full_width = F) %>%
        column_spec(1, bold = T, background = "#8dd8e6") %>%
        column_spec(2, bold = T, background = "#7abd7e") %>%
        column_spec(3, bold = T, background = "#ffb54c") %>%
        column_spec(4, bold = T, background = "#ff6961")
    })
    
    output$reference <- shiny::renderUI({
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      input_data <- data.frame()
      
      if (input$sys == "metric") {
        input_data <- data.frame(abdomen = input$abdomenMetric, weight_kg = input$weightMetric)
      } else {
        abdomenMetric = input$abdomenImperial * 2.54
        weightMetric = input$weightImperial * 0.45
        input_data <- data.frame(abdomen = abdomenMetric, weight_kg = weightMetric)
      }
      
      predicted_body_fat <- predict(final_model, input_data)
      
      if (predicted_body_fat < 0) {
        return(NULL)
      }
      
      if ((input$sys == "metric") && (!input_provided(input$age) || !ivWeightMetric$is_valid() || !ivAbdomenMetric$is_valid())) {
        return(NULL)
      }
      
      if ((input$sys == "imperial") && (!input_provided(input$age) || !ivWeightImperial$is_valid() || !ivAbdomenImperial$is_valid())) {
        return(NULL)
      }
      
      HTML("<br/><span style='color: gray; font-size: 0.8em'>Body fat percentage categories obtained from  <a href=\"https://zfcphp.arizona.edu/your-health/basics\">The University of Arizona's Zuckerman Family Center for Prevention & Health Promotion</a></span>")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)



# validate(
#   valid_age(input$age)
# )
# use a validate function for validation
# valid_age is a user defined function that:
#   check the input and returns a message of TRUE and FALSE
# you can create your own function
# you may also use need()