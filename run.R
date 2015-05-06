# TODO: Add comment
# 
# Author: kirk
###############################################################################
library(network)
library(shiny)
library(shinyData) # devtools::install_github("Statnet-Summer-Project/shinyData")
runApp()

##roxygenize(getwd())
#
##if (!require("devtools"))
##  install.packages("devtools")
##devtools::install_github("rstudio/shinyapps")
####
library(shinyapps)
deployApp(appName = "dynamic-network-visualization")
#
