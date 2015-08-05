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
    )
))