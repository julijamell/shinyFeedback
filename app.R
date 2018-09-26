##################
# app.R v1.7.0
# 
# Main controller. 
# Used to import your ui and server components, and initialize the app.
###################

rm(list = ls())

source('./ui.R')
source('./server.R')

shinyApp(ui, server)

