# TODO: Add comment
# 
# Author: kirk
###############################################################################

textfnserver <- function(envir){
 eval(parse(text='output$servertest <- renderUI({
    h3("fdsaaafasdffds")
   })'),envir=envir)
}

renderParaInputUI <- function(){
metric.list_generic <- reactive({
   if(input$goButton==0)return()
   if(length(input$mychooser_generic$right)>=1)
    input$mychooser_generic$right
   else return()
  })

ct=1:12
eval(parse(text=paste0("output$para_generic.",ct," <- renderUI({
        if(length(input$mychooser_generic$right)>= ",ct," ){
        count <- ",ct,"
        inputId = eval(parse(text=paste0('\"para_generic.',count,'\"')))
        label= eval(parse(text=paste0('paste0(metric.list_generic()[',count,'],\":\")')))
        value= eval(parse(text=paste0('paste0(metric.list_generic()[',count,'],\"=\",generic.arg.vec.val()[metric.list_generic()[[',count,']]],collapse=\"\n\")')))
        if(nchar(value)>=2 & length(value)>0) # colum sign 
        inputTextarea(inputId,label,value,nrow=5,ncol=10)
        else return()
        }
        else return()
        })")))
}
