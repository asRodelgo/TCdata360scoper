# ui
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(V8)

source("global.R", local = TRUE)

tagList(
  shinyjs::useShinyjs(),
  plotOutput('plotTSNE', height = "650px",
             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
             click = clickOpts("plot_click"),
             brush = brushOpts("plot_brush", delay = 100, delayType = "debounce"),
             dblclick = "plot_dblclick"),
  uiOutput("hover_info"),
  uiOutput("click_info")
)
