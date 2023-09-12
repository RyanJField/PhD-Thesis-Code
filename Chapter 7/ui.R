library(shiny)
library(rio)
library(JMbayes2)
library(tidyverse)

baseline <- import("../../r-scripts-ryan/Final-Analysis/ALL-NT-PROBNP/Data/baseline.Rds")
ntprobnp <- import("../../r-scripts-ryan/Final-Analysis/ALL-NT-PROBNP/Data/ntprobnp.Rds")

mostfrequentFactor <- function(column){
  mff <- data.frame(table(column))
  mff[which.max(mff$Freq),]$column
}

linebreaks <- function(n){HTML(strrep(br(), n))}

fluidPage(
  tags$head(
    # load custom stylesheet
    includeCSS("styles.css"),
  ),
  tags$div(
    titlePanel("DynamicHF: Dynamic Predictions for Survival"),
    class = "main-heading"
  ),
  
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Baseline Measurements",
                         fluidRow(
                           column(3, 
                                  numericInput("age", h4("Age"), value = median(baseline$age))),
                           column(3,
                                  radioButtons("sex",
                                               h4("Sex"),
                                               choices=list("Male" = "MALE", "Female" = "FEMALE"), selected = mostfrequentFactor(baseline$sex))),
                           column(3,
                                  radioButtons("region",
                                               h4("Region"),
                                               choices=list("North American" = "North American",
                                                            "Latin America" = "Latin America",
                                                            "Western Europe" = "Western Europe",
                                                            "Central Europe" = "Central Europe",
                                                            "Pacific Asia/Pacific and Other" = "Pacific Asia/Pacific and Other"),
                                               selected = mostfrequentFactor(baseline$region))),
                           column(3,
                                  radioButtons("NYHA_class",
                                               h4("NYHA Class"),
                                               choices=list("Class I" = "CLASS_1",
                                                            "Class II" = "CLASS_2",
                                                            "Class III" = "CLASS_3",
                                                            "Class IV" = "CLASS_4"),
                                               selected = mostfrequentFactor(baseline$NYHA_class))),
                           
                         ),
                         fluidRow(
                           column(3, 
                                  numericInput("bmi", h4("BMI"), value = median(baseline$bmi))),
                           column(3, 
                                  numericInput("ejf", h4("EJF"), value = median(baseline$ejf))),
                           column(3, 
                                  numericInput("sbp", h4("SBP"), value = median(baseline$sbp))),
                           column(3, 
                                  numericInput("heart_rate", h4("Heart Rate"), value = median(baseline$heart_rate))),
                         ),
                         fluidRow(
                           column(3, 
                                  numericInput("egfr", h4("eGFR"), value = median(baseline$eGFR))),
                           column(3,
                                  radioButtons("drug",
                                               h4("Treatment with Sacubitril / Valsartan"),
                                               choices=list("No" = "Enalapril", "Yes" = "LCZ"), selected = "LCZ")),
                           column(3,
                                  radioButtons("atrial_fibrillation",
                                               h4("Atrial Fibrillation"),
                                               choices=list("Yes" = "TRUE",
                                                            "No" = "FALSE"),
                                               selected = mostfrequentFactor(baseline$atrial_fibrillation))),
                           column(3,
                                  radioButtons("diabetes",
                                               h4("Diabetes Mellitus"),
                                               choices=list("Yes" = "TRUE",
                                                            "No" = "FALSE"),
                                               selected = mostfrequentFactor(baseline$diabetes))),
                           
                           # column(3,
                           #        radioButtons("hypertension",
                           #                     h4("Hypertension"),
                           #                     choices=list("Yes" = "TRUE",
                           #                                  "No" = "FALSE"),
                           #                     selected = mostfrequentFactor(baseline$hypertension))),
                         ),
                         fluidRow(
                           column(3,
                                  radioButtons("ischemic_hf",
                                               h4("Ischemic HF"),
                                               choices=list("Yes" = "YES",
                                                            "No" = "NO"),
                                               selected = mostfrequentFactor(baseline$ischemic_hf))),
                           column(3,
                                  radioButtons("prior_hf",
                                               h4("Prior Hospitalisation for Heart Failure"),
                                               choices=list("Yes" = "TRUE",
                                                            "No" = "FALSE"),
                                               selected = mostfrequentFactor(baseline$prior_hf))),
                           column(3,
                                  radioButtons("prior_mi",
                                               h4("Prior Myocardial Infarction"),
                                               choices=list("Yes" = "TRUE",
                                                            "No" = "FALSE"),
                                               selected = mostfrequentFactor(baseline$prior_mi))),
                           column(3,
                                  radioButtons("prior_stroke",
                                               h4("Prior Stroke"),
                                               choices=list("Yes" = "TRUE",
                                                            "No" = "FALSE"),
                                               selected = mostfrequentFactor(baseline$prior_stroke))),
                           
                         ),
                         fluidRow(
                           
                         )),
                tabPanel("Repeat Measurements",
                         fluidRow(
                           column(4,
                                  numericInput("month_0", h3("Month (from Baseline)"), value = 0)),
                           column(4, 
                                  numericInput("NTProBNP_0", h3("NT-ProBNP"), value = median(ntprobnp$ntprobnp_value))),column(4, 
                                  sliderInput("noInputs", h3("Add More Observations"),
                                              min = min(0), max = max(10), value = 0))),
                         uiOutput("observations")
                         
                         
                ),
                tabPanel("Debug",
                         uiOutput("debug_table"))
                ), style = "width:100%"),
  
  
  fluidRow(
    column(9,
      plotOutput(outputId = "output1")),
    column(3,
           linebreaks(3),
           p("Survival Probability at:"),
           uiOutput("survTime"),
           uiOutput("survivalValue")
    )),
  
  
)