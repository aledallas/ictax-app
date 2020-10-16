# Theme: SIMPLEX

library(shiny)
library(DT)
library(shinythemes)
library(shinyjs)
# library(shinyFeedback)



# Define UI for application
fluidPage(
    
    # useShinyFeedback(), # include shinyFeedback
    
    # shinythemes::themeSelector(),
    theme = shinytheme("simplex"),
    
    # Application title
    # titlePanel("ICTax Calculator"),
    
    # Horizontal line ----
    # hr(),
    
    fluidRow(
        column(12,
               style = "height:5px; background-color:#029ACF; width:100%;"
        )
    ),
    
    fluidRow(
        # style = "background-color: #D3D3D3; padding: 2rem 0rem",
        
        column(1),
        column(10,
               div(style = "height:100px;"),
               div(
                   h1(strong("Swiss", style = "color:#d9230f;"),
                      strong("Tax Calculator"),
                      tags$small(class="text-muted", "for your Funds on"),
                      tags$small(class="text-muted", strong("DEGIRO", style = "color:#029ACF;")),
                      div("BETA", class = "badge")
                   )
               ),
               br(),
               p(class = "lead", 
                 "Import your DEGIRO transactions and calculate tax and dividends of your funds for your Swiss tax declaration."
                 ),
               p("The calculation is based on financial instruments information provided by the Swiss tax authority in the ICTax portal."),
               br()
        ),
        column(1)
    ),
    
    hr(),
    
    # div(class = "alert alert-dismissible alert-info",
    #     HTML("&dArr;"),
    #     "START",
    #     HTML("&dArr;")),
    
    # Import section 
    fluidRow(
        
        column(1),
        column(3,
               
        ),
        column(8)
    ),
    
    fluidRow(
        
        # style = "background-color: #eeeeee; padding: 2rem 0rem",
        
        column(1),
        column(3,
               # class="jumbotron",
               tags$style(".progress-bar{background-color:#029ACF;}"),
               # tags$label(class="progress-bar bg-info",
               # tags$head(tags$style(".progress-bar{bg-info;}")),
               
               fileInput("file1", label = h5("Upload Degiro transactions (csv)"),
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
               )
        ),
        column(3,
               offset = 1,
               radioButtons("radio", label = h5("Choose Year"),
                            choices = list("2019" = 1), 
                            selected = 1)
        ),
        column(5)
    ),
    
    # Horizontal line ----
    hr(),
    
    
    fluidRow(
        # style = "background-color: #FCFCFC; padding: 2rem 0rem",
        column(1),
        column(5,
               # h3("Consolidated View"),
               h3(textOutput("tbl_agg_header")),
               DTOutput('tbl_agg')
               # style = "font-size: 12pt"
        ),
        column(6)
    ),
    
    fluidRow(
        
        style = "height:50px;"
    ),
    
    fluidRow(
        column(1),
        column(10,
               h3(textOutput("tbl_det_header")),
               DTOutput('tbl_det'),
               br(),
               textOutput("info")
               # br(),
               # br(),
               # textOutput("disclaimer")
               # style = "font-size: 12pt"
        ),
        column(1)
    ),
    
    
    fluidRow(
        style = "height:100px;"
    ),
    
    fluidRow(
        style = "background-color:#eeeeee; text-align:center; padding:2rem; height:120px;",
        column(4),
        column(4,
               "The Swiss Tax Calculator is a project of",
               tags$a("@aledallas", href="https://github.com/aledallas"),
               "to facilitate the tax declaration.
               No responsibility is taken on the accuracy of the information provided.",
               p("If you encounter any bug, please report it",
                 tags$a("here.",  href="https://github.com/aledallas/ictax-app/issues"), 
                 "or",
                 tags$a("reach out", href="mailto:aledallas@hotmail.it")
               ),
        ),
        column(4)
    )
    
)