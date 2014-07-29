# TODO: Add comment
# 
# Author: kirk
###############################################################################
library(shiny)
library(shinyData)
source("chooser.R")
source("functions.R")

# write uiOutput for Modifying parameter value. 
UI_G1 <- function(group=NULL){
 tmp <- lapply(1:12,function(ct) {eval(parse(text=paste("uiOutput('para_",group,".",ct,"')",sep="",collapse="")))})
 tmp
}

# function to generate dynamic ui for Type2
#UI_G2 <- function(){
# tmp <- lapply(1:12,function(ct) {eval(parse(text=paste("uiOutput('paraT2.",ct,"')",sep="",collapse="")))})
# tmp
#}

customTextInput<-function (inputId, label, value="",...) {
 tagList(tags$label(label, `for` = inputId), tags$input(id = inputId,
     type="text",
     value=value,...))
}

shinyUI(navbarPage("Dynamic Network Visualization", 
    tabPanel("ndtv app",
      tabsetPanel(
        tabPanel('Data',
          fluidRow(
            column(5,
              wellPanel(
                h4('Choose a dataset'),
                selectInput('dataset',
                  label = 'Sample datasets',
                  c(Choose = '', 'stergm.sim.2'),
                  selectize = FALSE),
                br(),
                actionButton('load_ndtv', 'Load Data')
              )),
            column(5,wellPanel(
                verbatimTextOutput("nwdndtv")
              )
            )
          )
        ),
        tabPanel('Computation Parameter',
          fluidRow(
            column(6,
              wellPanel(
                chooserInput("mychooser_slice.par", "Available arguments", "Selected 
                    metrics",slice.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Parameter Value"),
              column(4,
                UI_G1("slice.par")),
              column(1),
              column(4,
                verbatimTextOutput("exp_slice.par"))#
            ),
            column(6,
              wellPanel(
                chooserInput("mychooser_ca", "Available arguments", "Selected 
                    metrics",ca.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Parameter Value"),
              column(4,
                UI_G1("ca")),
              column(1),
              column(4,
                verbatimTextOutput("exp_ca"))
            )
          ),
          fluidRow(
            column(6, 
              actionButton('compute_ndtv', 'Compute'),
              br(),      
              br(),
              verbatimTextOutput('ca_ndtv')))
        ),
        
        tabPanel('Rendering Parameter',
          fluidRow(
            column(6,
              wellPanel(
                chooserInput("mychooser_render.par", "Available arguments", "Selected 
                    metrics",render.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              )
              ,
              h4("Parameter Value"),
              column(4,
                UI_G1("render.par")),
              column(1),
              column(4,
                verbatimTextOutput("exp_render.par"))
            ),
            column(6,
              wellPanel(
                chooserInput("mychooser_ra", "Available arguments", "Selected 
                    metrics",ra.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Parameter Value"),
              column(4,
                UI_G1("ra")),
              column(1),
              column(4,
                verbatimTextOutput("exp_ra")))
          ),
          fluidRow(
            column(6,
              actionButton('render_ndtv', 'Render') ,
              br(),
              br(),
              verbatimTextOutput('ra_ndtv')
            ))
        ),
        
        tabPanel('Play Moive',
          fluidRow(
            column(6,                
              wellPanel(
                br(),
                tags$head(tags$script(src="moivebutton.js")),
                uiOutput("movie1"),
                actionButton('save_ndtv', 'Play')
              
              )),  
            column(6,
              wellPanel(
                chooserInput("mychooser_sa", "Available arguments", "Selected 
                    metrics",sa.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Parameter Value"),
              column(4,
                UI_G1("sa")),
              column(1),
              column(4,
                verbatimTextOutput("exp_sa"))
            ),
            downloadButton('downloadData', 'Download')),
            fluidRow(
              column(6,
                verbatimTextOutput('sa_ndtv')
                )			
        )))
    )
#      ,
#      wellPanel(   
#        h4('Diagnose Message'),
#        h4('Console Message'),
#        verbatimTextOutput('ca_ndtv'),
#        verbatimTextOutput('ra_ndtv'),
#        verbatimTextOutput('sa_ndtv')
  
#    )
  ))




