shinyUI(fluidPage(

    title = "hi.",

    hr(),

    fluidRow(
        column(2),
        column(3,
                textInput("textToPredict", label = h1("Type something:"), value = ""),
                actionButton("predictNow", "Now press here :)"),
                br()
               ),
        column(5,
               h1("Here's a best guess at your next word:"),
               textOutput( "pred0" )#,
               #uiOutput( "pred2" ),
               #uiOutput( "pred3" )
        )
    ),
    fluidRow(
            column(2),
            column(8,
           h2("Some things you can try:"),
           br(),
           tags$ul(
                   tags$li("Gain an intuitive understanding of Markov Chains by trying the actual predictions"),
                   tags$li("Complain about how bad the model is and how you built a much better one"),
                   tags$li("Third comedy option")
           )
        )
    )
))