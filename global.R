###################
# global.R
# 
# Anything you want shared between your ui and server, define here.
###################

# clear workspace
rm(list = ls())

#load libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(networkD3)
library(igraph)
library(htmltools)
library(dplyr)
library(ggplot2)
library(fmsb)
library(reshape)
library(reshape2)
library(Cairo)
library(RColorBrewer)
library(roxygen2)

# load data and functions
load('./toydata_full.rData')
source('./functions.R')

#set options
options(shiny.usecairo=TRUE)

# set defaults

# dashboard skin color. Choose from ("blue", "black", "green", "purple", "red", "yellow")
DASHBOARD_SKIN_COLOR <- "blue"
# column of users passwords
userPassword <- toydata$ID
# default color of all plots
COLOR_DEFAULT_PLOT <- "#007ba7"
COLOR_DEFAULT_USER <- "red"
# default color of boxes headers. Choose from ("primary", "success", "info", "warning", "danger")
STATUS_COLOR <- "primary"
