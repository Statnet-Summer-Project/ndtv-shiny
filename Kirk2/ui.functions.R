# TODO: Add comment
# 
# Author: kirk
###############################################################################

########Jul 18, 2014######## UI function ########

# function to generate dynamic ui for Type1
UI_G1 <- function(group=NULL){
 tmp <- lapply(1:50,function(ct) {eval(parse(text=paste("uiOutput('para_",group,".",ct,"')",sep="",collapse="")))})
 tmp
}

# function to generate dynamic ui for Type2
UI_G2 <- function(){
 tmp <- lapply(1:50,function(ct) {eval(parse(text=paste("uiOutput('paraT2.",ct,"')",sep="",collapse="")))})
 tmp
}


testfn <- function()  titlePanel("network app")
testfn()
