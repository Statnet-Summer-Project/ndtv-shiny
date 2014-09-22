# TODO: Add comment
# 
# Author: kirk
###############################################################################
library(shiny)
library(statnet)
source("functions.R")
source("server.functions.R")
source("ui.functions.R")

shinyServer(
  function(input, output){
   
   #load datasets
   data(ecoli)
   data(florentine)
   data(fauxhigh)
   data(faux.mesa.high)
   data(kapferer)
   data(sampson)
   load("para.RData")
   load("plotnetworkExp.RData")
   nw.reac <- reactive({
      if(input$goButton==0)return()
      input$goButton
      isolate(eval(parse(text = input$dataset)))
     })
   #number of nodes in nw
   nodes <- reactive({
      if(input$goButton==0)return()
      input$goButton
      isolate(nw.reac()$gal$n)}) 
   #get coordinates to plot network with
   coords <- reactive({
      if(input$goButton==0)return()
      input$goButton
      isolate(plot.network(eval(parse(text = input$dataset))))})
   
   #list of vertex attributes in nw
   attr <- reactive({
      if(input$goButton==0)return()
      input$goButton
      attr <- c()
      if(input$dataset != ''){      
       isolate(  attr<-list.vertex.attributes(nw.reac()))
      }
      attr
     }) 
   
   #numeric attributes only (for size menu)
   numattr <- reactive({
      if(input$goButton==0)return()
      input$goButton   
      numattr <- c()
      if(input$dataset != ''){  
       for(i in 1:length(attr())){
        if(is.numeric(isolate(get.vertex.attribute(nw.reac(),attr()[i])))){
         numattr <- append(numattr,attr()[i])
        } 
       }} 
      numattr})
   
   
#      output$dynamiccolor <- renderUI({
#            selectInput('colorby',
#                label = 'Color nodes according to:',
#                c('None' = 2, attr()),
#                selectize = FALSE)
#          })
#      
#      output$dynamicsize <- renderUI({
#            selectInput('sizeby',
#                label = 'Size nodes according to:',
#                c('None' = 1, numattr()),
#                selectize = FALSE)
#          })
   
#   pf <- reactive({parent.frame()}) 
   
#   
#   
#   textfnserver(sys.parents())
#   
   
   
   
   ########Jul 17, 2014########G-para TYPE1########
   
#    eval(call("renderParaInputUI"))
   ##			
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
           else return()
           }
           else return()
           })")))
   
   ########Jul 22, 2014################
   metric.list_layout <- reactive({
      if(input$goButton==0)return()
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
           else return()
           }
           else return()
           })")))
   
   ########Jul 22, 2014################
   metric.list_vertex <- reactive({
      if(input$goButton==0)return()
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
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
   metric.list_edge <- reactive({
      if(input$goButton==0)return()
      if(length(input$mychooser_edge$right)>=1)
       input$mychooser_edge$right
      else return()
     })
   
   ########Jul 22, 2014################
   ct=1:12
   eval(parse(text=paste0("output$para_edge.",ct," <- renderUI({
           if(length(input$mychooser_edge$right)>= ",ct," ){
           count <- ",ct,"
           inputId = eval(parse(text=paste0('\"para_edge.',count,'\"')))
           label= eval(parse(text=paste0('paste0(metric.list_edge()[',count,'],\":\")')))
           value= eval(parse(text=paste0('paste0(metric.list_edge()[',count,'],\"=\",edge.arg.vec.val()[metric.list_edge()[[',count,']]],collapse=\"\n\")')))
           if(nchar(value)>=2 & length(value)>0) # colum sign 
           inputTextarea(inputId,label,value,nrow=1,ncol=5)
           else return()
           }
           else return()
           })")))
   
   
#  metric.list <- reactive({
#              if(input$goButton==0)return()
#              if(length(input$mychooser$right)>=1)
#                  input$mychooser$right
#              else return()
#          })
#  
   #########Jul 22, 2014################
#      ct=1:50
#      eval(parse(text=paste0("output$para.",ct," <- renderUI({
#                      if(length(input$mychooser$right)>= ",ct," ){
#                      count <- ",ct,"
#                      inputId = eval(parse(text=paste0('\"para.',count,'\"')))
#                      label= eval(parse(text=paste0('paste0(metric.list()[',count,'],\":\")')))
#                      value= eval(parse(text=paste0('paste0(metric.list()[',count,'],\"=\",network.arg.vec.val()[metric.list()[[',count,']]],collapse=\"\n\")')))
#                      if(nchar(value)>=2 & length(value)>0) # colum sign 
#                      inputTextarea(inputId,label,value,nrow=1,ncol=5)
#                      else return()
#                      }
#                      else return()
#                      })")))
#      
#      
#      eval(parse(text=paste0("metric.list.m.",ct,"<- reactive({if(length(input$mychooser$right)>=",ct,"){
#                      if(length(input$para.",ct,")>0){
#                      l1 <- input$para.",ct,"
#                      unlist(l1) }
#                      else return()
#                      } 
#                      })")))
#      
#      
   
   
#      ########Jul 17, 2014########G-para TYPE 2########
#      output$ui <- renderUI({
#            if (is.null(input$net.ele))
#              return()
#          })
#      
#      argSelect <- reactive({
#            tmp <- network.arg.vec() 
#            if(is.null(input$net.ele) & is.null(input$net.eff)) return() else
#            if(is.null(input$net.ele) & !is.null(input$net.eff)) arg.names.vec <- tmp[grep(input$net.eff,tmp)] else
#            if(!is.null(input$net.ele) & is.null(input$net.eff)) arg.names.vec <- tmp[grep(input$net.ele,tmp)] else
#            if(!is.null(input$net.ele) & !is.null(input$net.eff))
#              arg.names.vec <- intersect(tmp[grep(input$net.ele,tmp)],tmp[grep(input$net.eff,tmp)]) 
#            arg.names.vec
#          })
#      
#      
#      ct=1:50
#      eval(parse(text=paste0("output$paraT2.",ct," <- renderUI({
#                      if(length(argSelect())>= ",ct," ){
#                      count <- ",ct,"
#                      inputId = eval(parse(text=paste0('\"paraT2.',count,'\"')))
#                      label= eval(parse(text=paste0('paste0(argSelect()[',count,'],\":\")')))
#                      value= eval(parse(text=paste0('paste0(argSelect()[',count,'],\"=\",network.arg.vec.val()[argSelect()[[',count,']]],collapse=\"\n\")')))
#                      if(nchar(value)>=2 & length(value)>0) # colum sign 
#                      inputTextarea(inputId,label,value,nrow=1,ncol=5)
#                      else return()
#                      }
#                      else return()
#                      })")))
#      
   
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
   
   
#   eval(parse(text=paste0("metric.list.m.2.",ct,"<- reactive({if(length(argSelect())>=",ct,"){
#           if(length(input$paraT2.",ct,")>0){
#           l1 <- input$paraT2.",ct,"
#           unlist(l1) }
#           else return()
#           } 
#           })")))
   
   
   ########Jul 17, 2014######## read-in argument input ########
   argFun <- reactive({
#      if(input$Type1 >input$Type2) # click Type 1
      string.use <- paste0("c(",paste0("metric.list.m_generic.",1:12,"()",collapse=","),",",paste0("metric.list.m_layout.",1:12,"()",collapse=","),",",paste0("metric.list.m_vertex.",1:12,"()",collapse=","),",",paste0("metric.list.m_edge.",1:12,"()",collapse=","),")") 
#       else if(input$Type2>=input$Type1 & !input$Type2==0) # click Type 2
#       string.use <- paste0("c(",paste0("metric.list.m.2.",1:50,"()",collapse=","),")") else 
      # hasn't click yet 
#       return()
      metricsOptArgVal <- eval(parse(text=string.use))
      metricsOptArgVal
     })
   
   expFun_generic <- reactive({
      tmp <- c(input$mychooser_generic$right)
      tmp2 <- unlist(lapply(l12.list,function(x)x[1]))
      tmp3 <- unlist(lapply(l12.list,function(x)paste(x[1],x[2],sep=":")))
      cat(paste(tmp3[match(tmp,tmp2)],collapse="\n\n"))
      
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
   
   output$diag <- renderPrint({
#      	()
     })
   
   #################################Console message#################################
   output$console <- renderPrint({
#       nw <- isolate({nw.reac()})
#       plotlist <- argFun()
#      tmp <- if(length(plotlist)){","}
#      paste("plot.network(nw",tmp,paste(plotlist,sep=",",collapse=","),")")
      if(input$goButton==0) return(cat("Select Dataset\n"))
      input$goButton
      cat("Help message:\n")
      nw <- isolate({nw.reac()})
      plotlist <- argFun()
      tmp <- if(length(plotlist)){","}
      tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw,coord=coords()",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
   
   
   printPlot <- reactive({  	
      input$goButton
      nw <- isolate({nw.reac()})
      plotlist <- argFun()
      #only add , if there is argument after nw
      tmp <- if(length(plotlist)){","}
      # add error handler
      tryCatch(eval(parse(text=paste("tryCatch(expr=plot.network(nw,coord=coords()",tmp,paste(plotlist,sep=",",collapse=","),"),error=function(cond) {cat('Input value is invalid')})")
          )),error=function(e)cat("Input format is invalid"))
     })
   ########Jul 18, 2014######## generate plot ########
   output$nwplot <- renderPlot({
      printPlot()
     })
   
   
   printPlot2 <- reactive({
      plot(network(10))
     })
   #summary of network attributes
   
   output$attrUI <- renderUI({
      if (input$goButton){
#            if (is.null(input$net.ele))
       verbatimTextOutput('attr')}
     })
   
   
   output$attr <- renderPrint({
#						if (input$goButton == 0){
#							return(cat('Please choose a sample dataset from the side panel'))
#						}
      nw <- isolate(nw.reac())
      return(nw)
     })
   
   output$downloadData<- downloadHandler(
     filename = function() {
      paste('networkplot')
     },
     content = function(file) { 
      png(file)
      #plot(network(10))
      dev.off()
     },contentType="image/png")
   
  })