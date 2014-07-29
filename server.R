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
library(networkDynamic)
library(shinyData)
source("functions.R")

shinyServer(
  function(input, output){
   
   #################################
   #################################
   #####ndtv########
   #################################
   #################################
   
   data(stergm.sim.2)
   
   nwd.reac <- reactive({
      if(input$load_ndtv==0)return()
      input$load_ndtv
      isolate(eval(parse(text = input$dataset)))
     })
   
   
   output$nwdndtv <- renderPrint({
      nwd.reac()
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
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
   
   argFun.slice.par <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_slice.par.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })   
   
   argFun.ca <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_ca.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   argFun.render.par <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_render.par.",1:12,"()",collapse=","),")") 
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   argFun.ra <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_ra.",1:12,"()",collapse=","),")") 
      #TODO:other plot.network.parameter
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })  
   
   
   argFun.sa <- reactive({
      string.use <- paste0("c(",paste0("metric.list.m_sa.",1:12,"()",collapse=","),")") 
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
      if(input$compute_ndtv==0)return()
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
      if(input$render_ndtv==0)return()
      input$render_ndtv
      ralist <- isolate({argFun.ra()})
      tmp <- if(length(ralist)){","}   
      res <- isolate({ca.fn()})
      tryCatch(eval(parse(text=paste("tryCatch(expr=render.animation(res,render.par=isolate({render.par()})",tmp,paste(ralist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
   
   
   
   
   video.name <- reactive({
      input$save_ndtv
      tmp <- argFun.sa()
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
      salist <- argFun.sa()
      tmp <- if(length(salist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveVideo(isolate({ra.fn()})",tmp,paste(salist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
      try(file.remove("www/animation.mp4"))
      file.copy(from=video.name(),to="www/animation.mp4")
#      file.remove(isolate({video.name()}))
     })
   
   
   output$sa_ndtv <- renderPrint({
      if(input$save_ndtv==0)return()
      input$save_ndtv
      salist <- argFun.sa()
      tmp <- if(length(salist)){","}   
      tryCatch(eval(parse(text=paste("tryCatch(expr=saveVideo(isolate({ra.fn()})",tmp,paste(salist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })

   
   output$movie1 <- renderUI({
      if(input$save_ndtv==0)return()
      input$save_ndtv
      sa.fn()
      tags$video(src = "animation.mp4", type = "video/mp4", autoplay = FALSE, controls = TRUE,width="550",height="550")
     })
   
   
#   output$downloadData<- downloadHandler(
#     filename = function() {
#      paste('ndtv')
#     },
#     content = function(file) { 
   ##    sa.fn()
#     })
   
   
  })













