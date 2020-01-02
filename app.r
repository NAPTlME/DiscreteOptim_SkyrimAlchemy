# Test to get skyrim ingredients and all effects then create a graph
# Nathan Pratt
# 2019-12-31

library(rvest)
library(stringr)
library(ggplot2)
library(igraph)
library(dplyr)
library(tidyr)
library(threejs)
library(htmlwidgets)
library(shiny)
library(shinydashboard)

baseDir = "~/GitHub/DiscreteOptim_SkyrimAlchemy/"
if (!dir.exists(baseDir)){
  baseDir = choose.dir(caption = "Locate Base Repo Directory")
}
DataFile = paste0(baseDir, "/Data/alchemyGraph.rds")
graphFunctionsFile = paste0(baseDir, "graphFx.r")
alchemyGraph = readRDS(DataFile)
#source(getDataFile)
source(graphFunctionsFile)


#### shinyApp ####
ui = dashboardPage(
  dashboardHeader(title = "Sample Alchemy Manager"),
  dashboardSidebar(
    
  ),
  dashboardBody(
    tabItems(
      #dashboard view
      tabItem(tabName = "dashboardTab",
              fluidRow(box())
              )
    )
  )
)