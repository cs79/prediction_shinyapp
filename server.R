source("predict.R")
column_classes = c("integer", "character", "integer", "integer", "character", "character")
dict = read.csv("lookup_no_hapaxes.csv", colClasses = column_classes)

shinyServer(function(input, output) {

    # get predictions based on input -- need to ensure that predict.R will return "" if input is empty

    observeEvent(input$predictNow, {
        output$pred0 <- renderText({ predict_v2(input$textToPredict, dict = dict)[1] })
    })

    # alternative which tries to predict "live" but I think will die with a real prediction fn running
    #output$pred0 <- renderText({ predict(input$textToPredict)[1] })

    # to generate buttons, but this is more of a pain in the ass than seems worthwhile in shiny:
    #output$pred0 <- renderText({ predict(input$textToPredict)[1] })
    #output$pred1 <- renderUI({ actionButton("appendPred1", label=predictions[1]) })
    #output$pred2 <- renderUI({ actionButton("appendPred2", label=predictions[2]) })
    #output$pred3 <- renderUI({ actionButton("appendPred3", label=predictions[3]) })


})