# TODO: Add comment
# Jul 28, 2014
# Author: Kirk Li
# Email: kirli@stat.washington.edu
###############################################################################


# TODO: Add comment
# 
# Author: kirk
###############################################################################
library(shiny)
library(network)
library(ergm)
library(tergm)
library(ndtv)
library(shinyIncubator)
library(networkDynamic)
library(shinyData)
source("functions.R")

shinyServer(
  function(input, output,session){
   
   data(ecoli)
   data(florentine)
   data(fauxhigh)
   data(faux.mesa.high)
   data(kapferer)
   data(sampson)
   data(para)
   data(plotnetworkExp)
   
   
   nw <- reactive({
      if(input$load==0)return()
      input$load
      isolate(eval(parse(text = input$dataset)))
     })
   #number of nodes in nw
   nodes <- reactive({
      if(input$load==0)return()
      input$load
      isolate(nw()$gal$n)}) 
   #get coordinates to plot network with
   coords <- reactive({
      if(input$load==0)return()
      input$load
      isolate(plot.network(eval(parse(text = input$dataset))))})
   
   #list of vertex attributes in nw
   attr <- reactive({
      if(input$load==0)return()
      input$load
      attr <- c()
      if(input$dataset != ''){      
       isolate(  attr<-list.vertex.attributes(nw()))
      }
      attr
     }) 
   
   #numeric attributes only (for size menu)
   numattr <- reactive({
      if(input$load==0)return()
      input$load   
      numattr <- c()
      if(input$dataset != ''){  
       for(i in 1:length(attr())){
        if(is.numeric(isolate(get.vertex.attribute(nw(),attr()[i])))){
         numattr <- append(numattr,attr()[i])
        } 
       }} 
      numattr})
   
   
   metric.list_generic <- reactive({
      if(input$load==0)return()
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
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   metric.list_layout <- reactive({
      if(input$load==0)return()
      if(length(input$mychooser_layout$right)>=1)
       input$mychooser_layout$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_layout.",ct," <- renderUI({
           if(length(input$mychooser_layout$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_layout.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_layout()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_layout()[',count,'],\"=\",layout.arg.vec.val()[metric.list_layout()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   metric.list_vertex <- reactive({
      if(input$load==0)return()
      if(length(input$mychooser_vertex$right)>=1)
       input$mychooser_vertex$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_vertex.",ct," <- renderUI({
           if(length(input$mychooser_vertex$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_vertex.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_vertex()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_vertex()[',count,'],\"=\",vertex.arg.vec.val()[metric.list_vertex()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   metric.list_edge <- reactive({
      if(input$load==0)return()
      if(length(input$mychooser_edge$right)>=1)
       input$mychooser_edge$right
      else return()
     })
   
   ct=1:12
   eval(parse(text=paste0("output$para_edge.",ct," <- renderUI({
           if(length(input$mychooser_edge$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_edge.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_edge()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_edge()[',count,'],\"=\",edge.arg.vec.val()[metric.list_edge()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_generic.",ct,"<- reactive({if(length(input$mychooser_generic$right)>=",ct,"){
           if(length(input$para_generic.",ct,")>0){
           l1 <- input$para_generic.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_layout.",ct,"<- reactive({if(length(input$mychooser_layout$right)>=",ct,"){
           if(length(input$para_layout.",ct,")>0){
           l1 <- input$para_layout.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_vertex.",ct,"<- reactive({if(length(input$mychooser_vertex$right)>=",ct,"){
           if(length(input$para_vertex.",ct,")>0){
           l1 <- input$para_vertex.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_edge.",ct,"<- reactive({if(length(input$mychooser_edge$right)>=",ct,"){
           if(length(input$para_edge.",ct,")>0){
           l1 <- input$para_edge.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ########Jul 17, 2014######## read-in argument input ########
   argFun <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_generic.",1:12,"()",collapse=","),",",paste0("metric.list.m_layout.",1:12,"()",collapse=","),",",paste0("metric.list.m_vertex.",1:12,"()",collapse=","),",",paste0("metric.list.m_edge.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })
   
   expFun_generic <- reactive({
      tmp <- c(input$mychooser_generic$right)
      tmp2 <- unlist(lapply(l12.list,function(x)x[1]))
      tmp3 <- unlist(lapply(l12.list,function(x)paste(x[1],x[2],sep=":")))
      cat(paste(tmp3[match(tmp ,tmp2)],collapse="\n\n"))
     })
   
   expFun_layout <- reactive({
      tmp <- c(input$mychooser_layout$right)
      tmp2 <- unlist(lapply(l12.list,function(x)x[1]))
      tmp3 <- unlist(lapply(l12.list,function(x)paste(x[1],x[2],sep=":")))
      cat(paste(tmp3[match(tmp,tmp2)],collapse="\n\n"))
     })
   
   
   expFun_vertex<- reactive({
      tmp <- c(input$mychooser_vertex$right)
      tmp2 <- unlist(lapply(l12.list,function(x)x[1]))
      tmp3 <- unlist(lapply(l12.list,function(x)paste(x[1],x[2],sep=":")))
      cat(paste(tmp3[match(tmp,tmp2)],collapse="\n\n"))
     })
   
   
   expFun_edge <- reactive({
      tmp <- c(input$mychooser_edge$right)
      tmp2 <- unlist(lapply(l12.list,function(x)x[1]))
      tmp3 <- unlist(lapply(l12.list,function(x)paste(x[1],x[2],sep=":")))
      cat(paste(tmp3[match(tmp,tmp2)],collapse="\n\n"))
     })
   
   
   output$exp_generic <- renderPrint({
      expFun_generic()
     })
   output$exp_layout <- renderPrint({
      expFun_layout()
     })
   output$exp_vertex <- renderPrint({
      expFun_vertex()
     })
   output$exp_edge <- renderPrint({
      expFun_edge()
     })
   
   #################################Console message#################################
   output$console <- renderPrint({
      if(input$load==0) return(cat("Select Dataset\n"))
      input$load
      cat("Help message:\n")
      nw <- isolate({nw()})
      plotlist <- argFun()
      tmp <- if(length(plotlist)){","}
      if("coord=NULL" %in% plotlist)
       
       tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
           )),error=function(e)cat("Input format is invalid"))
      
      else
       
       tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw,coord=coords()",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
           )),error=function(e)cat("Input format is invalid"))
     })
   
   
   printPlot <- reactive({  	
      input$load
      nw <- isolate({nw()})
      plotlist <- argFun()
      #only add , if there is argument after nw
      tmp <- if(length(plotlist)){","}
      # add error handler
      
  if("coord=NULL" %in% plotlist)
   
   tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
       )),error=function(e)cat("Input format is invalid"))
  
  else
  
  tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw,coord=coords()",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
   ########Jul 18, 2014######## generate plot ########
   output$nwplot <- renderPlot({
      printPlot()
     })
   #summary of network attributes
   
   output$nwOut <- renderPrint({
      if(input$load==0)return(cat("Select dataset"))
      isolate({nw()})
     })
   
   output$downloadData<- downloadHandler(
     filename = function() {
      paste('networkplot')
     },
     content = function(file) { 
      jpeg(file,quality=100)
      input$load
      nw <- isolate({nw()})
      plotlist <- argFun()
      #only add , if there is argument after nw
      tmp <- if(length(plotlist)){","}
      # add error handler
      tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw,coord=coords()",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      dev.off()
     },contentType="image/png")
   
   
   output$datahelp <- renderPrint({
      if(input$load==0)return(cat("select dataset"))
      input$load
      
      tryCatch(eval(parse(text=paste0('tools:::Rd2txt(utils:::.getHelpFile(help(data(',input$dataset,'))),out="www/datahelp.txt")'))),error=function(e)cat("no help file found"))
      value <- readLines("www/datahelp.txt")
#      value(sapply(value,nchar)==0
   			value <- gsub("_","",value)
      value <- paste(value,collapse="\n")
      cat(value)
     })
   
   #################################
   #################################
   #####ndtv########
   #################################
   #################################
   
   
   data(stergm.sim.1)
   data(short.stergm.sim)
   
   nwd.reac <- reactive({
      if(input$load_ndtv==0)return()
      input$load_ndtv
      isolate(eval(parse(text = input$dataset_ndtv)))
     })
   
   
   output$datahelp_ndtv <- renderPrint({
      if(input$load_ndtv==0)return(cat("select dataset"))
      input$load_ndtv
      tryCatch(eval(parse(text=paste0('tools:::Rd2txt(utils:::.getHelpFile(help(data(',input$dataset_ndtv,'))),out="www/datahelp_ndtv.txt")'))),error=function(e)cat("no help file found"))
      value <- readLines("www/datahelp_ndtv.txt")
#      value(sapply(value,nchar)==0
      value <- gsub("_","",value)
      value <- paste(value,collapse="\n")
      cat(value)
     })
   
   output$nwdndtv <- renderPrint({
      if(input$load_ndtv==0)return(cat("Select dataset"))
      isolate({nwd.reac()})
     })
   
   
#   output$nwdSummary <- renderPlot({
#      if(input$load_ndtv==0)return()
#      filmstrip(isolate({nwd.reac()}))
#     })
   
   output$diag <- renderPrint({
      text <- input$console_msg
      if(!nchar(text))
       text <- "cat('wait for input')"
      
      eval(parse(text=paste(text)))
     })
   
   output$diag_ndtv <- renderPrint({
      
      
      text <- input$console_msg_ndtv
      if(!nchar(text))
       text <- "cat('wait for input')"
      
      eval(parse(text=paste(text)))
      
     })
   
   ########Jul 27, 2014################
   metric.list_slice.par <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_slice.par$right)>=1)
       input$mychooser_slice.par$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_slice.par.",ct," <- renderUI({
           if(length(input$mychooser_slice.par$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_slice.par.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_slice.par()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_slice.par()[',count,'],\"=\",guessSlicePar(nwd.reac())[metric.list_slice.par()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   metric.list_ca <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_ca$right)>=1)
       input$mychooser_ca$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_ca.",ct," <- renderUI({
           if(length(input$mychooser_ca$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_ca.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_ca()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_ca()[',count,'],\"=\",ca.arg.vec.val()[metric.list_ca()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   metric.list_render.par <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_render.par$right)>=1)
       input$mychooser_render.par$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_render.par.",ct," <- renderUI({
           if(length(input$mychooser_render.par$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_render.par.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_render.par()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_render.par()[',count,'],\"=\",render.par.arg.vec.val()[metric.list_render.par()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   metric.list_ra <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_ra$right)>=1)
       input$mychooser_ra$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_ra.",ct," <- renderUI({
           if(length(input$mychooser_ra$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_ra.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_ra()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_ra()[',count,'],\"=\",ra.arg.vec.val()[metric.list_ra()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   
   metric.list_sa <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_sa$right)>=1)
       input$mychooser_sa$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_sa.",ct," <- renderUI({
           if(length(input$mychooser_sa$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_sa.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_sa()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_sa()[',count,'],\"=\",sa.arg.vec.val()[metric.list_sa()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   
   metric.list_sh <- reactive({
      if(input$load_ndtv==0)return()
      if(length(input$mychooser_sh$right)>=1)
       input$mychooser_sh$right
      else return()
     })
   
   
   ct=1:12
   eval(parse(text=paste0("output$para_sh.",ct," <- renderUI({
           if(length(input$mychooser_sh$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_sh.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_sh()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_sh()[',count,'],\"=\",sh.arg.vec.val()[metric.list_sh()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=2,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   
   
   
   
   
   
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_slice.par.",ct,"<- reactive({if(length(input$mychooser_slice.par$right)>=",ct,"){
           if(length(input$para_slice.par.",ct,")>0){
           l1 <- input$para_slice.par.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_ca.",ct,"<- reactive({if(length(input$mychooser_ca$right)>=",ct,"){
           if(length(input$para_ca.",ct,")>0){
           l1 <- input$para_ca.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_render.par.",ct,"<- reactive({if(length(input$mychooser_render.par$right)>=",ct,"){
           if(length(input$para_render.par.",ct,")>0){
           l1 <- input$para_render.par.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_ra.",ct,"<- reactive({if(length(input$mychooser_ra$right)>=",ct,"){
           if(length(input$para_ra.",ct,")>0){
           l1 <- input$para_ra.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_sa.",ct,"<- reactive({if(length(input$mychooser_sa$right)>=",ct,"){
           if(length(input$para_sa.",ct,")>0){
           l1 <- input$para_sa.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   ct=1:12
   eval(parse(text=paste0("metric.list.m_sh.",ct,"<- reactive({if(length(input$mychooser_sh$right)>=",ct,"){
           if(length(input$para_sh.",ct,")>0){
           l1 <- input$para_sh.",ct,"
           unlist(l1) }
           else return()
           } 
           })")))
   
   
   
   argFun.slice.par <- reactive({
      if(input$compute_ndtv==0)return()
      input$compute_ndtv
      string.use <- paste0("c(",paste0("metric.list.m_slice.par.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })   
   
   argFun.ca <- reactive({
      if(input$compute_ndtv==0)return()
      input$compute_ndtv
      string.use <- paste0("c(",paste0("metric.list.m_ca.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   argFun.render.par <- reactive({
      if(input$render_ndtv==0)return()
      input$render_ndtv
      string.use <- paste0("c(",paste0("metric.list.m_render.par.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   argFun.ra <- reactive({
      if(input$render_ndtv==0)return()
      input$render_ndtv
      string.use <- paste0("c(",paste0("metric.list.m_ra.",1:12,"()",collapse=","),")") 
      #TODO:other plot.network.parameter
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   
   argFun.sa <- reactive({
      if(input$save_ndtv==0)return()
      input$save_ndtv
      string.use <- paste0("c(",paste0("metric.list.m_sa.",1:12,"()",collapse=","),")") 
      #TODO:other plot.network.parameter
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   argFun.sh <- reactive({
      if(input$save_ndtv_html==0)return()
      input$save_ndtv_html
      string.use <- paste0("c(",paste0("metric.list.m_sh.",1:12,"()",collapse=","),")") 
      #TODO:other plot.network.parameter
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   
   
   slice.par<-reactive({
      if(input$compute_ndtv==0)return()
      input$compute_ndtv
      tmp <- argFun.slice.par()
      l1 <- guessSlicePar(nwd.reac())
      if(!is.null(tmp)){
       tmp2 <- strsplit(tmp,"=")
       for(i in 1:length(tmp2))
        eval(parse(text=paste0("l1$",tmp2[[i]][1],"<-",tmp2[[i]][2])))}
      
      l1
     })
   
   ca.fn <- reactive({
      if(input$compute_ndtv==0)return()
      input$compute_ndtv

      calist <- argFun.ca()
      tmp <- if(length(calist)){","}   
      net <- isolate({nwd.reac()})
      tryCatch(eval(parse(text=paste("tryCatch(expr=compute.animation(net,slice.par=slice.par()",tmp,paste(calist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))

     })
   
   
   output$ca_ndtv <- renderPrint({
      if(input$compute_ndtv==0)return(cat("Click button to compute"))
      input$compute_ndtv
      calist <- argFun.ca()
      tmp <- if(length(calist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=compute.animation(isolate({nwd.reac()}),slice.par=slice.par()",tmp,paste(calist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
   
   
   
   render.par<-reactive({
      if(input$render_ndtv==0)return()
      input$render_ndtv
      tmp <- argFun.render.par()
      l1 <- render.par.arg.vec.val()
      if(!is.null(tmp)){
       tmp2 <- strsplit(tmp,"=")
       for(i in 1:length(tmp2))
        eval(parse(text=paste0("l1$",tmp2[[i]][1],"<-",tmp2[[i]][2])))}
      l1
     })
   
   ra.fn <- reactive({
      if(input$render_ndtv==0)return()
      input$render_ndtv
      ralist <- argFun.ra()
      tmp <- if(length(ralist)){","}   
      res <- isolate({ca.fn()})
      tryCatch(eval(parse(text=paste("tryCatch(expr=render.animation(res,render.par=render.par()",tmp,paste(ralist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      
     })
   
   
   output$ra_ndtv <- renderPrint({
      if(input$render_ndtv==0)return(cat("Click button to render"))
      input$render_ndtv
      
            
      withProgress(session, {
         setProgress(message = "Rendering, please wait",
           detail = "This may take a few moments...")
      
   
   
      ralist <- isolate({argFun.ra()})
      tmp <- if(length(ralist)){","}   
      res <- isolate({ca.fn()})
      tryCatch(eval(parse(text=paste("tryCatch(expr=render.animation(res,render.par=isolate({render.par()})",tmp,paste(ralist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      
      
      setProgress(message = "It's done")
      
     })
     })
   
   video.name <- reactive({
      input$save_ndtv
      tmp <- isolate({argFun.sa()})
      l1 <- list(video.name="animation.mp4")
      if(!is.null(tmp)){
       tmp2 <- strsplit(tmp,"=")
       for(i in 1:length(tmp2))
        eval(parse(text=paste0("l1$",tmp2[[i]][1],"<-",tmp2[[i]][2])))}
      l1$video.name
     })
   
   
   sa.fn <- reactive({
      if(input$save_ndtv==0)return()
      input$save_ndtv
      salist <- isolate({argFun.sa()})
      salist <- c(salist,"autobrowse=FALSE","autoplay=FALSE")
      tmp <- if(length(salist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveVideo(isolate({ra.fn()})",tmp,paste(salist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      try(file.remove("www/animation.mp4"))
      file.copy(from=video.name(),to="www/animation.mp4")
      file.remove(isolate({video.name()}))
     })
   
   
   
   sh.fn <- reactive({
      if(input$save_ndtv_html==0)return()
      input$save_ndtv_html
      
      shlist <- isolate({argFun.sh()})
      shlist <- c(shlist,"autobrowse=FALSE","autoplay=FALSE")
      tmp <- if(length(shlist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveHTML(isolate({ra.fn()})",tmp,paste(shlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      
      try(unlink("www/images",force=T,recursive=T))
      try(file.remove("www/index.html"))
      
      try(file.copy(from="index.html",to="www/index.html",overwrite=TRUE))
      dir.create("www/images")
      try(lapply(list.files("images"),function(x)file.copy(paste0("images/",x),to=paste0("www/images/",x),overwrite=TRUE)))
      
      dir.create("www/js")
      try(lapply(list.files("js"),function(x)file.copy(paste0("js/",x),to=paste0("www/js/",x),overwrite=TRUE)))
      
      try(unlink("./images",force=T,recursive=T))
      try(unlink("./css",force=T,recursive=T))
      try(unlink("./js",force=T,recursive=T))
      try(file.remove("index.html"))
    
     })
   
   
   output$sa_ndtv <- renderPrint({
      if(input$save_ndtv==0)return(cat("Click Movie button to play"))
      input$save_ndtv
      
      withProgress(session, {
         setProgress(message = "Generating, please wait",
           detail = "This may take a few moments...")
      salist <- isolate({argFun.sa()})
      salist <- c(salist,"autobrowse=FALSE")
      tmp <- if(length(salist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveVideo(isolate({ra.fn()})",tmp,paste(salist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      
      setProgress(message = "It's done")
     })
     })
   
   
   output$sh_ndtv <- renderPrint({
      if(input$save_ndtv_html==0)return(cat("Click HTML button to play"))
      input$save_ndtv_html
      shlist <- isolate({argFun.sh()})
      shlist <- c(shlist,"autobrowse=TRUE","autoplay=FALSE")
      tmp <- if(length(shlist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveHTML(isolate({ra.fn()})",tmp,paste(shlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
#   
   
   output$movie1 <- renderUI({
      if(input$save_ndtv==0)return()
      input$save_ndtv
      sa.fn()
      tags$video(src = "animation.mp4", type = "video/mp4", autoplay = FALSE, controls = TRUE,width="550",height="550")
     })
   
   output$movie2 <- renderUI({
      if(input$save_ndtv_html==0)return()
      input$save_ndtv_html
      sh.fn()
      
  })
   
   
   output$downloadData_ndtv<- downloadHandler(
     filename = function() {
      paste('ndtv.mp4')
     },
     content = function(file) { 
       sa.fn()
     })
   
   
   
   output$computehelp <- renderPrint({
      if(input$load_ndtv==0)return(cat("select dataset"))
      input$load_ndtv
      
      tryCatch(eval(parse(text=paste0('tools:::Rd2txt(utils:::.getHelpFile(help(compute.animation)),out="www/computehelp.txt")'))),error=function(e)cat("no help file found"))
      value <- readLines("www/computehelp.txt")
#      value(sapply(value,nchar)==0
      value <- gsub("_","",value)
      value <- paste(value,collapse="\n")
      cat(value)
     })
   
   
   output$renderhelp <- renderPrint({
      if(input$load_ndtv==0)return(cat("select dataset"))
      input$load_ndtv
      tryCatch(eval(parse(text=paste0('tools:::Rd2txt(utils:::.getHelpFile(help(render.animation)),out="www/renderhelp.txt")'))),error=function(e)cat("no help file found"))
      value <- readLines("www/renderhelp.txt")
#      value(sapply(value,nchar)==0
      value <- gsub("_","",value)
      value <- paste(value,collapse="\n")
      cat(value)
     })
   
  })













