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

customTextInput<-function (inputId, label, value="",...) {
 tagList(tags$label(label, `for` = inputId), tags$input(id = inputId,
     type="text",
     value=value,...))
}

shinyUI(navbarPage("Dynamic Network Visualization", 
    tabPanel("network app",
      fluidRow(
        column(6, 
          tabsetPanel(
            tabPanel('Data',
              fluidRow(
                column(12,
                  wellPanel(
                    h5('Choose Dataset'),
                    selectInput('dataset',
                      label = 'Sample dataset',
                      c(Choose = '', 'ecoli1', 'ecoli2', 'faux.mesa.high',
                        'fauxhigh', 'flobusiness','flomarriage',
                        'kapferer','kapferer2','samplike'),
                      selectize = FALSE),
                    br(),
                    actionButton('goButton', 'Load Data')))
              )),
            tabPanel('Generic',
              fluidRow(
                column(12,
                  wellPanel(
                    chooserInput("mychooser_generic", "Available Arguments", "Selected 
                        Arguments",generic.arg.vec(), c(), size = 10, multiple = TRUE
                    )),
                  h5("Argument Values"),
                  column(5,
                    UI_G1("generic")),
                  column(1),
                  column(5,
                    verbatimTextOutput("exp_generic"))# written in function.R
                )
              )),
            tabPanel('Layout',
              fluidRow(
                column(12,
                  wellPanel(
                    chooserInput("mychooser_layout", "Available Arguments", "Selected 
                        Arguments",layout.arg.vec(), c(), size = 10, multiple = TRUE
                    )),
                  h5("Argument Values"),
                  column(5,
                    UI_G1("layout")),
                  column(1),
                  column(5,
                    verbatimTextOutput("exp_layout"))# written in function.R
                )
              )),									
            tabPanel('Vertex',
              fluidRow(
                column(12,
                  wellPanel(
                    chooserInput("mychooser_vertex", "Available Arguments", "Selected 
                        Arguments",vertex.arg.vec(), c(), size = 10, multiple = TRUE
                    )),
                  h5("Argument Values"),
                  column(5,
                    UI_G1("vertex")),
                  column(1),
                  column(5,
                    verbatimTextOutput("exp_vertex"))# written in function.R
                )
              )),
            tabPanel('Edge',
              fluidRow(
                column(12,
                  wellPanel(
                    chooserInput("mychooser_edge", "Available Arguments", "Selected 
                        Arguments",edge.arg.vec(), c(), size = 10, multiple = TRUE
                    )),
                  h5("Argument Values"),
                  column(5,
                    UI_G1("edge")),
                  column(1),
                  column(5,
                    verbatimTextOutput("exp_edge"))# written in function.R
                ))
            ))),
        column(6,
          downloadLink('downloadData', 'Download'),
          h4('Network Plot'),
          plotOutput('nwplot',height="600px"),
          h4('Console Message'),
          verbatimTextOutput('console'),	
          h4('Network Summary'),
          verbatimTextOutput("nwOut")
#          h4('Diagnose Message'),
#          verbatimTextOutput('diag')
        )
      ),
      
      fluidRow(
        column(1, img(src = 'csdelogo_crop.png', height = 50, width = 50)),
        column(2, h6('Center for Studies in Demography and Ecology'))
      )
    ),
    
    ########Jul 29, 2014################
    ########Jul 29, 2014################
    
    tabPanel("ndtv app",
      tabsetPanel(
        tabPanel('Data',
          fluidRow(
            column(5,
              wellPanel(
                h4('Choose a dataset'),
                selectInput('dataset_ndtv',
                  label = 'Sample dataset',
                  c(Choose = '', 'stergm.sim.1','stergm.sim.2'),
                  selectize = FALSE),
                br(),
                actionButton('load_ndtv', 'Load Data')
#    ,
#                plotOutput('nwdSummary')
              )),
            column(5,wellPanel(
                h5("Summary of dynamic network"),
                verbatimTextOutput("nwdndtv")
              )
            )
          )
        ),
        tabPanel('Computation Arguments',
          fluidRow(
            column(6,
              wellPanel(
                h4("Select Timing Info (slice.par):"),
                chooserInput("mychooser_slice.par", "Available arguments", "Selected 
                    metrics",slice.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
              column(4,
                UI_G1("slice.par")),
              column(1),
              column(4,
                verbatimTextOutput("exp_slice.par"))#
            ),
            column(6,
              wellPanel(
                h4("Select Computation Info (compute.animation):"),
                chooserInput("mychooser_ca", "Available arguments", "Selected 
                    metrics",ca.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
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
              h5("Compute Animation"),
              verbatimTextOutput('ca_ndtv')))
        ),
        
        tabPanel('Rendering Arguments',
          fluidRow(
            column(6,
              wellPanel(
                h4("Select Display Info (render.par):"),
                chooserInput("mychooser_render.par", "Available arguments", "Selected 
                    metrics",render.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              )
              ,
              h4("Arguments Values"),
              column(4,
                UI_G1("render.par")),
              column(1),
              column(4,
                verbatimTextOutput("exp_render.par"))
            ),
            column(6,
              wellPanel(
                h4("Select Rendering Info (render.animation):"),
                chooserInput("mychooser_ra", "Available arguments", "Selected 
                    metrics",ra.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
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
              h5("Render Animation"),
              verbatimTextOutput('ra_ndtv')
            ))
        ),
        
        tabPanel('Play Movie',
          fluidRow(
            column(6,                
#              wellPanel(
              br(),
              tags$head(tags$script(src="moivebutton.js")),
              uiOutput("movie1"),
              actionButton('save_ndtv', 'Play')
            ),  
            column(6,
              wellPanel(
                h4("Select Saving Info (saveVideo):"),
                chooserInput("mychooser_sa", "Available arguments", "Selected 
                    metrics",sa.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              downloadButton('downloadData_ndtv', 'Download'),
              h4("Arguments Values"),
              column(4,
                UI_G1("sa")),
              column(1),
              column(4,
                verbatimTextOutput("exp_sa"))
            )),
          fluidRow(
            column(6,
              h5("Play Animation"),
              verbatimTextOutput('sa_ndtv')
            )			
          )))
    )
  ))




