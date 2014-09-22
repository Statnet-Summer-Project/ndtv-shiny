# TODO: Add comment
# 
# Author: kirk
###############################################################################

library(network)
library(ergm)
library(ndtv)
library(shinyData)
########Jul 18, 2014######## Server function ########
network.arg.vec <- function()names(formals(plot.network.default))
network.arg.vec.val <- function(){
 tmp <- formals(plot.network.default)
 tmp.class <- unlist(lapply(tmp,class))
 tmp[tmp.class%in%c("call","character")] <- 
   paste0("'",tmp[tmp.class%in%c("call","character")],"'")
 tmp
}

#network.arg.vec()
#network.arg.vec.val()
#para <- read.table("para.txt",header=TRUE)
#save(para,file="para.RData")

data(para)
edge.para.vec <- as.character(para[para$tab=="edge","parameter"])
vertex.para.vec <- as.character(para[para$tab=="vertex","parameter"])
layout.para.vec <- as.character(para[para$tab=="layout","parameter"])
generic.para.vec <- c(as.character(para[para$tab=="generic","parameter"]),"...")



generic.arg.vec <- function()intersect(names(formals(plot.network.default)),generic.para.vec)
generic.arg.vec()

generic.arg.vec.val <- function(){
 tmp <- formals(plot.network.default)
 tmp <- tmp[names(tmp) %in% generic.para.vec]
 tmp.class <- unlist(lapply(tmp,class))
 tmp[tmp.class%in%c("call","character")] <- 
   paste0("'",tmp[tmp.class%in%c("call","character")],"'")
 tmp
}


layout.arg.vec <- function()intersect(names(formals(plot.network.default)),layout.para.vec)

layout.arg.vec.val <- function(){
 tmp <- formals(plot.network.default)
 tmp <- tmp[names(tmp) %in% layout.para.vec]
 tmp.class <- unlist(lapply(tmp,class))
 tmp[tmp.class%in%c("call","character")] <- 
   paste0("'",tmp[tmp.class%in%c("call","character")],"'")
 tmp
}

vertex.arg.vec <- function()intersect(names(formals(plot.network.default)),vertex.para.vec)

vertex.arg.vec.val <- function(){
 tmp <- formals(plot.network.default)
 tmp <- tmp[names(tmp) %in% vertex.para.vec]
 tmp.class <- unlist(lapply(tmp,class))
 tmp[tmp.class%in%c("call","character")] <- 
   paste0("'",tmp[tmp.class%in%c("call","character")],"'")
 tmp
}




edge.arg.vec <- function()intersect(names(formals(plot.network.default)),edge.para.vec)

edge.arg.vec.val <- function(){
 tmp <- formals(plot.network.default)
 tmp <- tmp[names(tmp) %in% edge.para.vec]
 tmp.class <- unlist(lapply(tmp,class))
 tmp[tmp.class%in%c("call","character")] <- 
   paste0("'",tmp[tmp.class%in%c("call","character")],"'")
 tmp
}





slice.par.fn <- function(start=0,end=100,interval=1, aggregate.dur=1,rule='latest')list(start=start,end=end,interval=interval, aggregate.dur=aggregate.dur,rule=rule)

slice.par.arg.vec <- function()names(formals(slice.par.fn))

slice.par.arg.vec()

slice.par.arg.vec.val <- function(){formals(slice.par.fn)}

slice.par.arg.vec.val()


guessSlicePar <- function (nd) 
{
 times <- get.change.times(nd)
 if (length(times) == 0) {
  warning("network does not appear to have any dynamic information. Using start=0 end=1")
  slice.par <- list(start = 0, end = 0, interval = 1, aggregate.dur = 1, 
    rule = "latest")
  return(slice.par)
 }
 times[times == Inf] <- NA
 times[times == -Inf] <- NA
 slice.par <- list(start = min(times, na.rm = T), end = max(times, 
     na.rm = T), interval = 1, aggregate.dur = 1, rule = "latest")
 return(slice.par)
}

render.par.fn <- function(tween.frames = 10, show.time = TRUE, 
  show.stats = NULL, extraPlotCmds = NULL, initial.coords = 0)
 list(tween.frames = 10, show.time = TRUE, 
   show.stats = NULL, extraPlotCmds = NULL, initial.coords = 0)

render.par.arg.vec <- function()names(formals(render.par.fn))
render.par.arg.vec()
render.par.arg.vec.val <- function(){formals(render.par.fn)}
render.par.arg.vec.val()




ca.arg.vec <- function()names(formals(compute.animation))
ca.arg.vec()
ca.arg.vec.val <- function(){
 tmp <-formals(compute.animation)
 tmp$slice.par <- NULL
 tmp
}
ca.arg.vec.val()







ra.arg.vec <- function()names(formals(render.animation))
ra.arg.vec()
ra.arg.vec.val <- function(){
 tmp <-formals(compute.animation)
 tmp$plot.par <- NULL
 tmp$render.par <- NULL
 tmp
}
ra.arg.vec.val()



sa.arg.vec <- function()names(formals(saveVideo))
sa.arg.vec()
sa.arg.vec.val <- function(){
 tmp <-formals(saveVideo)
 tmp
}
sa.arg.vec.val()





sh.arg.vec <- function()names(formals(saveHTML))
sh.arg.vec()
sh.arg.vec.val <- function(){
 tmp <-formals(saveHTML)
 tmp$htmlfile <- "www/index.html"
	tmp$imgdir <- "www"
	tmp$autobrowse <- "FALSE"
 tmp
}
sh.arg.vec.val()



sg.arg.vec <- function()names(formals(saveGIF))
sg.arg.vec()
sg.arg.vec.val <- function(){
 tmp <-formals(saveGIF)
 tmp
}
sg.arg.vec.val()


	

help_file <- function(topic, console=FALSE,format=c("text", "html", "latex", "html"),
  lines=NULL, before=NULL, after=NULL) {  
 format=match.arg(format)
 if (!is.character(topic)) topic <- deparse(substitute(topic))
 helpfile = utils:::.getHelpFile(help(topic))
 
 if(console){
 hs <- capture.output(switch(format, 
     text=tools:::Rd2txt(helpfile),
     html=tools:::Rd2HTML(helpfile),
     latex=tools:::Rd2latex(helpfile),
     Rd=tools:::prepare_Rd(helpfile)
   )
 )
 if(!is.null(lines)) hs <- hs[lines]
 hs <- c(before, hs, after)
 cat(hs, sep="\n")
 invisible(hs)
}
else{
 hs <- switch(format, 
   text=tools:::Rd2txt(helpfile),
   html=tools:::Rd2HTML(helpfile),
   latex=tools:::Rd2latex(helpfile),
   Rd=tools:::prepare_Rd(helpfile)
 )
 
 return(hs)
}

}



