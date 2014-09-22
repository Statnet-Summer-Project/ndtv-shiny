# ui.R

shinyUI(fluidPage(
  titlePanel("STATNET"),
  sidebarLayout(
    sidebarPanel(
      h2("Tongfang Sun"),
      p("These is the UI is for the 'ndtv' social network model"),
      code('library("ndtv")'),
      br(),
      br(),
      br(),
      img(src = "links.jpg", height = 72, width = 72),
      "shiny is a product of ", 
      span("RStudio", style = "color:blue")
    ),
    mainPanel(
      h1("GIF for ndtv simple example"),
      div(class = "busy",
          p("Calculation in progress.."),
          img(src="temp1.gif")
      )
      #tags$video(src = "video1.mp4", type = "video/mp4", autoplay = NA, controls = NA)
    )
  )
))