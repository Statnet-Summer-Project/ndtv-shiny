h3("Tongfang Sun"),
p("The user interface is for the social network model (network dynamic temporal visualization)"),
br(),
br(),
img(src = "links.jpg", height = 72, width = 72),
p("Here is the simple test for"),
code('saveMovie()'), 
code('saveVideo()'),
),
mainPanel(
  div(class = "busy",
      p("Calculation in progress.."),
      img(src="temp1.gif")
  )
  #tags$video(src = "video1.mp4", type = "video/mp4", autoplay = NA, controls = NA)
  
)