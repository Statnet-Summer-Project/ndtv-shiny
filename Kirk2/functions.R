# TODO: Add comment
# 
# Author: kirk
###############################################################################

library(statnet)

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
par.arg.vec <- function()names(par())


#para <- read.table("para.txt",header=TRUE)
#save(para,file="para.RData")
load("para.RData")
edge.para.vec <- para[para$tab=="edge","parameter"]
vertex.para.vec <- para[para$tab=="vertex","parameter"]
layout.para.vec <- para[para$tab=="layout","parameter"]
generic.para.vec <- para[para$tab=="generic","parameter"]



generic.arg.vec <- function()intersect(names(formals(plot.network.default)),generic.para.vec)

generic.arg.vec.val <- function(){
	tmp <- formals(plot.network.default)
	tmp <- tmp[names(tmp) %in% generic.para.vec]
	tmp.class <- unlist(lapply(tmp,class))
	tmp[tmp.class%in%c("call","character")] <- 
			paste0("'",tmp[tmp.class%in%c("call","character")],"'")
	tmp
}

generic.arg.vec.val()


layout.arg.vec <- function()intersect(names(formals(plot.network.default)),layout.para.vec)

layout.arg.vec.val <- function(){
	tmp <- formals(plot.network.default)
	tmp <- tmp[names(tmp) %in% layout.para.vec]
	tmp.class <- unlist(lapply(tmp,class))
	tmp[tmp.class%in%c("call","character")] <- 
			paste0("'",tmp[tmp.class%in%c("call","character")],"'")
	tmp
}

layout.arg.vec.val()



vertex.arg.vec <- function()intersect(names(formals(plot.network.default)),vertex.para.vec)

vertex.arg.vec.val <- function(){
	tmp <- formals(plot.network.default)
	tmp <- tmp[names(tmp) %in% vertex.para.vec]
	tmp.class <- unlist(lapply(tmp,class))
	tmp[tmp.class%in%c("call","character")] <- 
			paste0("'",tmp[tmp.class%in%c("call","character")],"'")
	tmp
}

vertex.arg.vec.val()



edge.arg.vec <- function()intersect(names(formals(plot.network.default)),edge.para.vec)

edge.arg.vec.val <- function(){
	tmp <- formals(plot.network.default)
	tmp <- tmp[names(tmp) %in% edge.para.vec]
	tmp.class <- unlist(lapply(tmp,class))
	tmp[tmp.class%in%c("call","character")] <- 
			paste0("'",tmp[tmp.class%in%c("call","character")],"'")
	tmp
}

edge.arg.vec.val()



