shinyUI(fluidPage(
  titlePanel('File download'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("movie", "gif")),
      radioButtons("filetype", "File type:",
                   choices = c("movie", "gif")),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
))