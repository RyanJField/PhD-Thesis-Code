library(shiny)
library(rio)
library(JMbayes2)
library(tidyverse)

library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

jm1 <- import('jointFit.comp.4.Rds')
baseline <- import("baseline.Rds")
ntprobnp <- import("ntprobnp.Rds")

function(input, output) {

  newData <- reactive({
    tryCatch({
      reps <- 1 + input$noInputs
      months <- input$month_0
      NTProBNP <- input$NTProBNP_0
      eGFR <- input$eGFR_0
      
      
      if(!(input$noInputs == 0))
      {
        for(i in 1:input$noInputs)
        {
          months <- c(months, input[[paste("month_", i, sep="")]])
          NTProBNP <- c(NTProBNP, input[[paste("NTProBNP_", i, sep="")]])
          #eGFR <- c(eGFR, input[[paste("eGFR_", i, sep="")]])
        }
      }
      
      data.frame(
        "sid1a" = rep(1, reps),
        "sample_month" = months,
        "logNTProBNP" = round(log(NTProBNP),2),
        "eGFR" = rep(input$egfr, reps),
        "sex" = rep(factor(input$sex, levels=levels(baseline$sex)), reps),
        "trt_drug" = rep(factor(input$drug, levels=levels(baseline$trt_drug)), reps),
        "region" = rep(factor(input$region, levels=levels(baseline$region)), reps),
        "age" = rep(input$age, reps),
        "bmi" = rep(input$bmi, reps),
        "ejf" = rep(input$ejf, reps),
        "NYHA_class" = rep(factor(input$NYHA_class, levels=levels(baseline$NYHA_class)), reps),
        "atrial_fibrillation" = rep(factor(input$atrial_fibrillation, levels=levels(baseline$atrial_fibrillation)), reps),
        "diabetes" = rep(factor(input$diabetes, levels=levels(baseline$diabetes)), reps),
        "prior_hf" = rep(factor(input$prior_hf, levels=levels(baseline$prior_hf)), reps),
        "prior_mi" = rep(factor(input$prior_mi, levels=levels(baseline$prior_mi)), reps),
        "prior_stroke" = rep(factor(input$prior_stroke, levels=levels(baseline$prior_stroke)), reps),
        "ischemic_hf" = rep(factor(input$ischemic_hf, levels=levels(baseline$ischemic_hf)), reps),
        "sbp" = rep(input$sbp, reps),
        "heart_rate" = rep(input$heart_rate, reps)
      )
    }, 
    error = function(e) {
      print(e$message)
    })
    
  })
  observe({
    output$debug_table <- renderUI(
      tagList(
        DT::renderDataTable(newData(),
                            filter='top',
                            options = list(scrollX = T, pageLength = 10, info = FALSE,
                                           lengthMenu = list(c(10, -1), c("10", "All")))
        )
      )
    )
  }) %>% bindEvent(newData())
    

  
  observeEvent(input$noInputs, {
    output$observations = renderUI({
      input_list <- list()
      if(input$noInputs == 0)
      {}
      else{
        for(i in 1:input$noInputs)
        {
          x <- length(input_list) + 1
          input_list[[x]] <- fluidRow(
            column(4, numericInput(paste("month_", i, sep=""), h3("Month"), (i)*3)),
            column(4, 
                   numericInput(paste("NTProBNP_", i, sep=""), h3("NT-ProBNP"), value = median(ntprobnp$ntprobnp_value)))
          )
        }
      }
      return(input_list)
    })
  })
  
  s_pred <- reactive({
    pred <- predict(jm1, newdata = newData(),
            process = "event", times = seq(0, 48, length.out = 51),
            return_newdata = T)
    pred
  })
  
  l_pred <- reactive({
    pred <- predict(jm1, newdata = newData(),
            times = seq(0, 48, length.out = 51),
            return_newdata = T)
    pred
  })
  
  output$survTime = renderUI({
      survTimes <- as.list(s_pred()$sample_month)
      names(survTimes) <- round(s_pred()$sample_month,0)

      return(selectInput("survTime",
                         "Survival Time (Months)",
                         survTimes))
    })
  
  selectedSurvTime <- reactive({as.numeric(input$survTime)})
  
  output$survivalValue <- renderUI({
    stimes <- as.data.frame(s_pred()[,c('sample_month', 'pred_CIF', "low_CIF", "upp_CIF")])
    s_fun <- function(x) 1 - x
    stimes[,2:4] <- s_fun(stimes[,2:4])
    stimes <- round(stimes,3)
    sval <- stimes[stimes$sample_month == round(selectedSurvTime(), 3),]
    sout <- paste0(sval$pred_CIF,  " ( ", sval$low_CIF, " - ",  sval$upp_CIF, ")")
    return(sout)
  })
  
  output$output1 <- renderPlot({
    plot(l_pred(), s_pred(),
         fun_long = exp,
         fun_event = function (x) 1 - x,
         ylab_event = "Survival Probability",
         ylab_long = "NT-ProBNP",
         CI_long = T,
         CI_event = T,
         ylim_long_outcome_range = F
    )
  })
  
  
  
}