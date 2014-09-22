# TODO: Add comment
# 
# Author: kirk
###############################################################################



#################################
library(tools)
Rd <- parse_Rd("plot.network.Rd")
argument.index <- which(sapply(Rd,function(x)attr(x,"Rd_tag"))=="\\arguments")
Arg <- Rd[[argument.index]]
l1 <- unlist(lapply(Arg,function(x)try(x[[1]],silent = T)))
parse.fn1 <- function(x)paste0(sapply(x[[2]],unlist),collapse="")
l2 <- lapply(Arg, function(x)tryCatch(parse.fn1(x),error=function(e)return()))
l12 <- paste(l1,l2,sep=":")
l12.final <- l12[!grepl(":NULL",l12)]
l12.list <- strsplit(l12.final,":")
save(l12.list, file="plotnetworkExp.RData")

#################################


