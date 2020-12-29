# Get data and format as graph
# Nathan Pratt 
# 2020-01-01

library(rvest)
library(igraph)
library(stringr)
library(dplyr)
library(tidyr)

ingredientsUrl = "https://elderscrolls.fandom.com/wiki/Ingredients_(Skyrim)"

# get ingredient table
page = read_html(ingredientsUrl)

tables = page %>%
  html_nodes("table")
ingredientTable = html_table(tables[[2]])

# clean up symbols from names (except "'" and space)
ingredientTable$Ingredient = str_replace_all(ingredientTable$Ingredient, "[^a-zA-Z'\\s]", "")

#### Convert to graph format ####

# Nodes
# Ingredients
ingredientNodes = ingredientTable %>%
  select(Ingredient, Obtained) %>%
  mutate(Type = "Ingredient", Name = Ingredient, Comments = Obtained, Count = 0) %>%
  select(Name, Type, Count, Comments)
# effects
effectNodes = data.frame(Name = unique(c(ingredientTable$`Primary Effect`, ingredientTable$`Secondary Effect`,
                                         ingredientTable$`Tertiary Effect`, ingredientTable$`Quaternary Effect`)), 
                         Type = "Effect")
alchemyNodes = bind_rows(ingredientNodes, effectNodes)

# set colors
cols = c("lightsteelblue3", "tomato")
alchemyNodes$Type = factor(alchemyNodes$Type)
alchemyNodes$color = cols[alchemyNodes$Type]

# edges
alchemyEdges = ingredientTable %>%
  mutate(id1 = Ingredient) %>%
  select(id1, `Primary Effect`, `Secondary Effect`, `Tertiary Effect`, `Quaternary Effect`) %>%
  gather(key = effectNumber_remove, value = id2, -id1) %>%
  mutate(Known = F) %>%
  select(id1, id2, Known)

# create graph
alchemyGraph = graph.data.frame(alchemyEdges, directed = T, vertices = alchemyNodes)

potionCombinations1 = c(getAllCombinationsOfRange(ingredientNodes$Name, 2),
                       getAllCombinationsOfRange(ingredientNodes$Name, 3)) # not very efficient, makes all possible combinations of ingredients

# instead get only combinations that will result in a potion effect
usedIngredientsV1 = c()
potionCombinations = c(lapply(ingredientNodes$Name, function(x){
  print(paste0("ingredient: ", x))
  searchResults = bfs(alchemyGraph, V(alchemyGraph)[V(alchemyGraph)$name == x], neimode = "all", order = F, unreachable = F, dist = T)
  adjacentIngredients = names(searchResults$dist[!is.nan(searchResults$dist) & searchResults$dist == 2])
  adjacentIngredients = adjacentIngredients[!(adjacentIngredients %in% usedIngredientsV1)]
  if (length(adjacentIngredients) == 0){
    return(NULL)
  }
  returnPotions = lapply(adjacentIngredients, function(y){
    c(x, y)
  })
  print(paste0("Num 2x potions: ", length(returnPotions)))
  if (length(adjacentIngredients) == 1){
    return(returnPotions)
  }
  # now get 3 ing potion combinations
  ing3_1 = lapply(getAllCombinationsOfRange(adjacentIngredients, 2), function(y) c(x, y))
  print(paste0("Num 3x combo potions: ", length(ing3_1)))
  # now iterate over returnPotions and get any effects that are 2 steps away from the second ingredient but not the first
  ing3_2 = lapply(returnPotions, function(y){
    secondIngredient = y[2]
    searchResults2 = bfs(alchemyGraph, V(alchemyGraph)[V(alchemyGraph)$name == secondIngredient], neimode = "all", order = F, unreachable = F, dist = T)
    adjacentIngredients2 = names(searchResults2$dist[!is.nan(searchResults2$dist) & searchResults2$dist == 2])
    adjacentIngredients2 = adjacentIngredients2[!(adjacentIngredients2 %in% c(adjacentIngredients, usedIngredientsV1)) & adjacentIngredients2 != x]
    if(length(adjacentIngredients2) > 0){
      return (lapply(adjacentIngredients2, function(j){
        c(y, j)
      }))
    } else {
      return(NULL)
    }
  }) %>% unlist(recursive = F)
  print(paste0("Num other 3x potions: ", length(ing3_2)))
  usedIngredientsV1 <<- c(usedIngredientsV1, x)
  return(c(returnPotions, ing3_1, ing3_2))
})) %>% unlist(recursive = F)

potionsDf = do.call(rbind, lapply(1:length(potionCombinations), function(i){
  data.frame(id1 = paste0("potion", i), id2 = potionCombinations[[i]], Known = F) # adding known to match ingredient-effect edges
}))
alchemyEdges2 = rbind(alchemyEdges, potionsDf)
potionNodes = data.frame(Name = unique(potionsDf$id1),
                         Type = "Potion",
                         Count = 0,
                         Comments = NA,
                         color = "goldenrod")
alchemyEdges2 = rbind(alchemyEdges2, data.frame(id1 = "root", id2 = potionNodes$Name, Known = F))
alchemyNodes2 = rbind(alchemyNodes, potionNodes, data.frame(Name = "root", Type = "Root", Count = 0, Comments = NA, color = "black"))
alchemyGraph2 = graph.data.frame(alchemyEdges2, directed = T, vertices = alchemyNodes2)



# remove values that will no longer be used
rm(ingredientsUrl, page, tables)

# save graph
saveRDS(alchemyGraph, file.choose())
