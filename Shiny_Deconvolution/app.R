#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(shinyjs)

source('ui.R')
source('server.R')


# Run the application 
shinyApp(ui = ui, server = server)

##
#library(rsconnect)
# rsconnect::deployApp("/Users/lgaspard/Documents/Literature/Deconvolution_review/Shiny_Deconvolution")
