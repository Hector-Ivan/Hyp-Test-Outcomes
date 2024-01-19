## User Interface for Power Calculator

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Hypothesis Test OUTCOMES",
                style = "font-family: Lucida Handwriting; color: darkblue",
                align = "center")),

  fluidRow(
    # TOP ROW; main inputs, the plot
    
    column(2,
           h4("Null Hypothesis"),
           numericInput("Alpha",
                        "Input Alpha",
                        min =.01 ,
                        max = .50,
                        value = .05,
                        step = .005) ,
           
           radioButtons("tailtype","Choose Tail Type", 
                        choices = list("Two-Tailed" = 0, "Left-Tailed" = 1,
                                       "Right-tailed" = 2), selected = 0),
           
           h4("Choose:"),
           
           numericInput("x_bar","Effect Size",
                        min = -10 ,
                        max = 10,
                        value = 1.5,
                        step = .1),
           
           numericInput("se","Standard Error",
                        min = .1,
                        max = 9,
                        value = 1,
                        step = .1)
           ),
    
    column(10, plotOutput("powPlot"))
  
  ),
  
  fluidRow(
    # 3RD FROM BOTTOM ROW: Fail to reject, reject
    
    column(3, h3("")),
    column(3,h3("")),
    column(3, h4("Fail to Reject",HTML("H<sub>0</sub>")), style = "text-align: center; 
           border: 5px solid lightgrey;
           font-family: Lucida Handwriting"),
    
    column(3, h4("Reject",HTML("H<sub>0</sub>")), style = "text-align: center;
           border: 5px solid lightgrey;
           font-family: Lucida Handwriting"),
    
    style = "padding-right: 40px"
  ),
  
  fluidRow(
    # SECOND FROM BOTTOM ROW: The Null Hyp, 1 - a, a
    
    column(3, h2("What's the Truth?", 
                 style="font-family: Lucida Handwriting; color: darkblue")

    ),
    column(3,
           
           htmlOutput("theNul"),
          
           style = "text-align: center; 
           height: 110px; border: 5px solid lightblue"),
    
    column(3,h5("Correct Decision",style="margin-bottom:0; color:green"),
           
           htmlOutput("ncd"),
           htmlOutput("ncdnum"),
           
           style = "text-align: center;
           height: 110px; border: 5px solid black"),
    
    column(3,h5("Incorrect Decision",style="margin-bottom:0; color:red"),
           htmlOutput("nid"),
           htmlOutput("nidnum"),
           
           style = "text-align: center;
           height: 110px; border: 5px solid red"),
  
    style = "padding-right: 40px"
  ),
  
  fluidRow(
    #BOTTOM ROW includes: alternative, beta, power

    column(3,
           radioButtons("truth","",
                        choices = list("The Null" = 0, "The Alternative" = 1)
                        , selected = 0)
           ),
    column(3,
          htmlOutput("theAlt"),
           style = "text-align: center;
           height: 110px; border: 5px solid lightblue"),
    column(3, h5("Incorrect Decision", style="margin-bottom: 0; color:gold"), 
           htmlOutput("bet"), 
           htmlOutput("btnumcol"),
           style = "text-align: center;
           height: 110px; border: 5px solid gold"),
    column(3, h5("Correct Decision", style="margin-bottom: 0; color:green"), 
           htmlOutput("powcol"),
           htmlOutput("pwnumcol"),
           style = "text-align: center;
           height: 110px; border: 5px solid #1E90FFFF"),
    style = "padding-bottom: 25px; padding-right: 40px"
  )
  
))
