###################
# ui.R
# 
# Initializes the ui. 
# Used to load in your header, sidebar, and body components.
###################

source('./ui/header.R')
source('./ui/sidebar.R')
source('./ui/body.R')

ui <- dashboardPage(
  skin = DASHBOARD_SKIN_COLOR,
  header = header(),
  sidebar =  sidebar(),
  body = body()
)
