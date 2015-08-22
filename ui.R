shinyUI(fluidPage(

    title = "hi.",

    hr(),
    fluidRow(
        column(2),
        column(8,
               h1("predict-o-matic", style= "color: #22FFFF; background-color: #342F2E; text-align: center; padding: 20px 0px 20px 0px; border-radius: 3px; font-size: 5em; font-family: sans-serif; font-style: italic; font-weight: bold")
               )
    ),
    fluidRow(
        column(2),
        column(4,
                textInput("textToPredict", label = h1("start typing:", style = "font-weight: bold; padding: 10px 0px 10px 0px"), value = ""),
                actionButton("predictNow", "Press here if nothing happens :)", style="font-size: 1.2em"),
                br()
               ),
        column(4,
               h1("presto predict-o:", style = "font-weight: bold; padding: 10px 0px 10px 0px"),
               h3(textOutput( "pred0" ))#,
               #uiOutput( "pred2" ),
               #uiOutput( "pred3" )
        ), style = "padding-bottom: 25px"
    ),
    fluidRow(
            column(2),
            column(4,
           h1("more stuff you can try:", style = "font-weight: bold; padding: 10px 0px 5px 0px"),
           br(),
           tags$ul(
                   tags$li("Gain an intuitive understanding of ", tags$a(href="https://en.wikipedia.org/wiki/Markov_chain", "Markov Chains", target="_blank"), " by trying the suggested predictions", style="font-size: 1.2em"),
                   br(),
                   tags$li("Complain about how bad the model is and how you built a much better one", style="font-size: 1.2em"),
                   br(),
                   tags$li("Third comedy option", style="font-size: 1.2em")
           )
        ),
        column(4,
               h1("runners up:", style = "font-weight: bold; padding: 10px 0px 5px 0px"),
               plotOutput( "plot" ))
    ),
    fluidRow(
        column(2),
        column(8, style = "border: 1px solid #6e6e6e")
    ),
    fluidRow(
        column(2),
        column(8,
           tags$span("created using python and R shiny by ", tags$a(href="https://github.com/cs79", "cs79", target="_blank"))
        )
    )
))
