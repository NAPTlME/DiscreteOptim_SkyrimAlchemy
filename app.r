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
DataFile = paste0(baseDir, "Data/alchemyGraph.rds")
graphFunctionsFile = paste0(baseDir, "graphFx.r")
dataVisFunctionsFile = paste0(baseDir, "DataVisFx.r")
alchemyGraph = readRDS(DataFile)
#source(getDataFile)
source(graphFunctionsFile)
source(dataVisFunctionsFile)


#### shinyApp ####
ui = dashboardPage(
  dashboardHeader(title = "Sample Alchemy Manager"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Inventory", tabName = "inventory", icon = icon("table")),
      menuItem("Reveal Effects", tabName = "potionRecommendations", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #dashboard view
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("revealedEffectsPlot", height = 250))
              )
      ),
      # Inventory Screen
      tabItem(tabName = "inventory",
              fluidRow(
                column(12,
                  fluidRow(
                    column(4, # controls for selecting ingredient
                      selectInput("selInputIngredient", "Ingredient", V(alchemyGraph)$name[V(alchemyGraph)$Type == "Ingredient"],
                                  selected = V(alchemyGraph)$name[V(alchemyGraph)$Type == "Ingredient"][1])
                    ),
                    column(8,
                      fluidRow(
                        box(height = 150),
                        numericInput("ingredientCount", "Count", 0, min = 0)
                      ),
                      fluidRow(
                        actionButton("effect1", "No Effects Found", icon = icon("eye-slash")),
                        actionButton("effect2", "No Effects Found", icon = icon("eye-slash"))
                      ),
                      fluidRow(
                        actionButton("effect3", "No Effects Found", icon = icon("eye-slash")),
                        actionButton("effect4", "No Effects Found", icon = icon("eye-slash"))
                      )
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "potionRecommendations",
              fluidRow(
                actionButton("startRecommendation", label = "Start"),
                verbatimTextOutput("recommendationComments", placeholder = T)
              ),
              fluidRow(
                tableOutput("optimizationTable")
              )
      )
    )
  )
)

server = function(input, output, session){
  # selected ingredient for inventory tab
  selectedIngredient_Inventory = reactive({
    ingredient = input$selInputIngredient
    updateNumericInput(session, "ingredientCount", value = getIngredientCount(ingredient, alchemyGraph))
    ingredient
  })# %>% debounce(1000)
  
  newIngredientCount = reactive({
    input$ingredientCount
  })
  
  ingredientCountReactiveHandle <- reactiveValues(v = T) # handle to invalidate counts of ingredients
  KnownIngredientReactiveHandle <- reactiveValues( v = T) # handle to invalidate knownIngredientDf and refresh
  recommendationReactiveHandle <- reactiveValues(
    df = data.frame(),
    comments = ""
  )
  
  knownIngredientEffectsDf = reactive({
    KnownIngredientReactiveHandle$v
    ingredient = selectedIngredient_Inventory()
    if (length(ingredient) != 1){
      return(NULL)
    }
    getIngredientEffectsDf(ingredient, alchemyGraph)
  })
  
  # update text in effects
  observe({
    knownEffectsDf = knownIngredientEffectsDf()
    icons = c("eye-slash", "eye")
    updateActionButton(session, 
                       inputId = "effect1", 
                       label = knownEffectsDf$effect[1], 
                       icon = icon(icons[knownEffectsDf$known[1]+1]))
    updateActionButton(session, 
                       inputId = "effect2", 
                       label = knownEffectsDf$effect[2], 
                       icon = icon(icons[knownEffectsDf$known[2]+1]))
    updateActionButton(session, 
                       inputId = "effect3", 
                       label = knownEffectsDf$effect[3], 
                       icon = icon(icons[knownEffectsDf$known[3]+1]))
    updateActionButton(session, 
                       inputId = "effect4", 
                       label = knownEffectsDf$effect[4], 
                       icon = icon(icons[knownEffectsDf$known[4]+1]))
  })
  # update known effects from button presses
  observeEvent(input$effect1, {
    ingredient = isolate(selectedIngredient_Inventory())
    if(length(ingredient) == 1){
      knownEffectsDf = isolate(knownIngredientEffectsDf())
      print(ingredient)
      print(paste0("Currently Known: ", knownEffectsDf$known[1]))
      print(paste0("Changing to: ", !knownEffectsDf$known[1]))
      print(paste0("Effect: ", knownEffectsDf$effect[1]))
      alchemyGraph <<- setKnownIngredientEffect(ingredient, 
                                              effect = knownEffectsDf$effect[1], 
                                              known = !knownEffectsDf$known[1], 
                                              alchemyGraph)
      print(getIngredientEffectsDf(ingredient, alchemyGraph))
      KnownIngredientReactiveHandle$v = !KnownIngredientReactiveHandle$v # invalidate rv so knowneffectsdf is updated
    }
  })
  observeEvent(input$effect2, {
    ingredient = isolate(selectedIngredient_Inventory())
    if (length(ingredient) == 1){
      knownEffectsDf = isolate(knownIngredientEffectsDf())
      alchemyGraph <<- setKnownIngredientEffect(ingredient, 
                                              effect = knownEffectsDf$effect[2], 
                                              known = !knownEffectsDf$known[2], 
                                              alchemyGraph)
      KnownIngredientReactiveHandle$v = !KnownIngredientReactiveHandle$v # invalidate rv so knowneffectsdf is updated
    }
  })
  observeEvent(input$effect3, {
    ingredient = isolate(selectedIngredient_Inventory())
    if (length(ingredient) == 1){
      knownEffectsDf = isolate(knownIngredientEffectsDf())
      alchemyGraph <<- setKnownIngredientEffect(ingredient, 
                                              effect = knownEffectsDf$effect[3], 
                                              known = !knownEffectsDf$known[3], 
                                              alchemyGraph)
      KnownIngredientReactiveHandle$v = !KnownIngredientReactiveHandle$v # invalidate rv so knowneffectsdf is updated
    }
  })
  observeEvent(input$effect4, {
    ingredient = isolate(selectedIngredient_Inventory())
    if (length(ingredient) == 1){
      knownEffectsDf = isolate(knownIngredientEffectsDf())
      alchemyGraph <<- setKnownIngredientEffect(ingredient, 
                                              effect = knownEffectsDf$effect[4], 
                                              known = !knownEffectsDf$known[4], 
                                              alchemyGraph)
      KnownIngredientReactiveHandle$v = !KnownIngredientReactiveHandle$v # invalidate rv so knowneffectsdf is updated
    }
  })
  
  observe({
    ingredient = isolate(selectedIngredient_Inventory())
    newCount = newIngredientCount()
    if (length(ingredient) == 1){
      currentCount = getIngredientCount(ingredient, alchemyGraph)
      alchemyGraph <<- setIngredientCount(ingredient, newCount, alchemyGraph)
    }
  })
  
  output$revealedEffectsPlot = renderPlot({
    ingredientCountReactiveHandle$v
    plotPercIngredientsOwned(alchemyGraph)
  })
  
  observeEvent(input$startRecommendation, {
    potions = potionsRecommendedForEffectReveal(alchemyGraph)
    numRevealedDf = getEffectRevealMetaData(potions, alchemyGraph)
    numRevealed = sum(numRevealedDf$numRevealedEffects)
    numPotions = length(potions)
    numIngredients = sum(sapply(potions, length))
    recommendationReactiveHandle$df = do.call(rbind, lapply(1:length(potions), function(i){
      df = data.frame(Ingredient1 = potions[[i]][1],
                      Ingredient2 = potions[[i]][2],
                      Ingredient3 = NA)
      if (length(potions[[i]] == 3)){
        df$Ingredient3 = potions[[i]][3]
      }
      df$Revealed_Effects = numRevealedDf$numRevealedEffects[i]
      return(df)
    }))
    recommendationReactiveHandle$comments = paste0("Number of potions: ", numPotions,
                                                   "\nNumber of Effects revealed: ", numRevealed,
                                                   "\nNumber of Ingredients used: ", numIngredients)
  })

  output$recommendationComments = renderPrint({
    recommendationReactiveHandle$comments
  })

  output$optimizationTable = renderTable({
    recommendationReactiveHandle$df
  })
}

shinyApp(ui, server)
