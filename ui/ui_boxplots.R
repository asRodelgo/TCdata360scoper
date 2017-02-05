library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(V8)

source("global.R", local = TRUE)

tagList(
  shinyjs::useShinyjs(),
  #includeCSS("css/datascoper.css"),
  
  plotOutput('plotBoxplotBrushed')
  
)
