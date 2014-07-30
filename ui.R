# TODO: Add comment
# 
# Author: kirk
###############################################################################
library(shiny)
library(shinyData)
library(shinyIncubator)
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

shinyUI(
  navbarPage(
    title="Dynamic Network Visualization", 
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
                    actionButton('load', 'Load Data'),
                    br(),
                    br(),
                    fluidRow(column(12,
                        verbatimTextOutput('datahelp')))
                  ))
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
            ),
            
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
              ))
          )),
        column(6,
          downloadLink('downloadData', 'Download Plot'), 
          h4('Console Message'),
          verbatimTextOutput('console'),	
          h4('Network Plot'),
          plotOutput('nwplot',height="500px"),
          h4('Network Summary'),
          verbatimTextOutput("nwOut"),
          h4('Diagnostic Message'),
          textInput("console_msg", "Console input", value = ""),
          verbatimTextOutput('diag')
        )
      )
    ),
    
    ########Jul 29, 2014################
    ########Jul 29, 2014################
    
    progressInit(),
    
    
    tabPanel("ndtv app",
      tabsetPanel(
        tabPanel('Data',
          fluidRow(
            column(6,
              wellPanel(
                h4('Choose a dataset'),
                selectInput('dataset_ndtv',
                  label = 'Sample dataset',
                  c(Choose = '', 'stergm.sim.1','short.stergm.sim'),
                  selectize = FALSE),
                br(),
                actionButton('load_ndtv', 'Load Data'),
                br(),
                br(),
                fluidRow(column(12,
                    verbatimTextOutput('datahelp_ndtv')))
              )),
            column(6,wellPanel(
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
                h4("Select Timing Arguments (slice.par):"),
                chooserInput("mychooser_slice.par", "Available arguments", "Selected 
                    metrics",slice.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
              column(4,
                UI_G1("slice.par")),
              column(1),
              column(4
#                verbatimTextOutput("exp_slice.par")
              )#
            ),
            column(6,
              wellPanel(
                h4("Select Computation Arguments (compute.animation):"),
                chooserInput("mychooser_ca", "Available arguments", "Selected 
                    metrics",ca.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
              column(4,
                UI_G1("ca")),
              column(1),
              column(4
#                verbatimTextOutput("exp_ca")
              )
            )
          ),
          fluidRow(
            column(6,
              actionButton('compute_ndtv', 'Compute'),
              br(),
              br(),
              h5("Compute Animation"),
              verbatimTextOutput('ca_ndtv')
            ),
            column(6,
              verbatimTextOutput('computehelp')))
        ),
        
        tabPanel('Rendering Arguments',
          fluidRow(
            column(6,
              wellPanel(
                h4("Select Display Arguments (render.par):"),
                chooserInput("mychooser_render.par", "Available arguments", "Selected 
                    metrics",render.par.arg.vec(), c(), size = 5, multiple = TRUE
                )
              )
              ,
              h4("Arguments Values"),
              column(4,
                UI_G1("render.par")),
              column(1),
              column(4
#                verbatimTextOutput("exp_render.par")
              )
            ),
            column(6,
              wellPanel(
                h4("Select Rendering Arguments (render.animation):"),
                chooserInput("mychooser_ra", "Available arguments", "Selected 
                    metrics",ra.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
              h4("Arguments Values"),
              column(4,
                UI_G1("ra")),
              column(1),
              column(4
#                verbatimTextOutput("exp_ra")
              ))
          ),
          fluidRow(
            column(6,
              actionButton('render_ndtv', 'Render') ,
              br(),
              br(),
              h5("Render Animation"),
              verbatimTextOutput('ra_ndtv')
            ),
            column(6,
              verbatimTextOutput("renderhelp")))
        ),
        
        tabPanel('Play Movie',
          fluidRow(
            column(6,                
              br(),
              tags$head(tags$script(src="moivebutton.js")),
              tags$head(tags$script(src="js/jquery.scianimator.min.js")),
              tags$head(tags$script(type="text/javascript",src="http://yandex.st/highlightjs/7.3/highlight.min.js")),
              tags$head(tags$script(type="text/javascript",src="http://yandex.st/highlightjs/7.3/languages/r.min.js")),
#              tags$body(div(class="scianimator",id="Rplot",style="display: inline-block;"),div(class="scianimator",style="width: 480px; text-align: left"),tags$script(src="js/Rplot.js"))      ,
              ##                
              actionButton('save_ndtv', 'Play Movie'),
              actionButton('save_ndtv_html','Generate HTML'),
              uiOutput("movie1")
#        ,
#              uiOutput("movie2")
            ),  
            column(6,
              wellPanel(
                h4("Select Saving Arguments (saveVideo):"),
                chooserInput("mychooser_sa", "Available arguments", "Selected 
                    metrics",sa.arg.vec(), c(), size = 5, multiple = TRUE
                ),
                
                h4("Select Saving Arguments (saveHTML):"),
                chooserInput("mychooser_sh", "Available arguments", "Selected 
                    metrics",sh.arg.vec(), c(), size = 5, multiple = TRUE
                )
              ),
#              downloadButton('downloadData_ndtv', 'Download'),
              h4("Arguments Values"),
              column(4,
                UI_G1("sa"),
                UI_G1("sh")),
              column(1),
              column(4
#                verbatimTextOutput("exp_sa"),
#                verbatimTextOutput("exp_sh")
              )
            )),
          fluidRow(
            column(6,
              h5("Play Animation"),
              verbatimTextOutput('sa_ndtv'),
              verbatimTextOutput('sh_ndtv')
            )			
          ))
        
        ,
#          
#          tabPanel('HTML',
#                tags$body(div(class="scianimator",id="Rplot",style="display: inline-block;"),div(class="scianimator",style="width: 480px; text-align: left"),tags$script(src="js/Rplot.js"))),
        ##             
        fluidRow(
          column(6,
            h4('Diagnostic Message'),
            textInput("console_msg_ndtv", "Console input", value = ""),
            verbatimTextOutput('diag_ndtv')))
      )
    )
  ,header=img(src = 'statnetLogo.png', height = 1),
    foot= 
  fluidRow(
    column(1, img(src = 'csdelogo_crop.png', height = 50, width = 50)),
    column(2, h6('Center for Studies in Demography and Ecology'))
  )))




