# Function Declarations
# Nathan Pratt 
# 2020-01-01

library(igraph)
library(dplyr)

# Function to get all effects of a potion using the ingredients provided
# ingredients: character vector of length 2 or 3
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2019-12-31
getPotionEffects = function(ingredients, g){
  # fail if not 2 or 3 ingredients
  if (length(ingredients) != 2 && length(ingredients) != 3){
    stop("2 or 3 ingredients must be provided")
  }
  # create subgraph of g using only effects and the listed ingredients
  sg = induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$name %in% ingredients])
  # get effects that are connected to the ingredients (found using degree > 0)
  effectVertices = V(sg)[V(sg)$Type == "Effect"]
  degreesOfEffects = degree(sg, v = effectVertices)
  effects = effectVertices$name[degreesOfEffects > 1]
  return (effects)
}

# Function to get a dataframe of all vertices
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
getVertexDf = function(g){
  igraph::as_data_frame(g, what = "vertices")
}

# Function to get data frame of all ingredient vertices
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
getIngredientVertexDf = function(g){
  vertexDf = getVertexDf(g)
  ingredientVDf = vertexDf %>% 
    filter(Type == "Ingredient") %>%
    select(name, Type, Count, Comments)
  return(ingredientVDf)
}

# Function to get effects (known and unkown for an ingredient)
# ingredient: character vector of length 1 that is an exact string match for 
## the vertex name of the required ingredient
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
getIngredientEffectsDf = function(ingredient, g){
  if (length(ingredient) > 1){
    warning("Will only use the first ingredient")
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient to get effects")
  }
  # get neighbors to ingredient (these will be the 4 ingredients)
  ingredientV = V(g)[V(g)$name == ingredient[1]]
  effectVs = neighbors(g, ingredientV)
  paths = shortest_paths(g, from = ingredientV, to = effectVs, output = "both")
  # iterate over the vpaths and epaths to get the name of the effect and known from the 
  # edge connecting this ingredient to the path
  return(data.frame(effect = sapply(paths$vpath, function(x) x$name[2]),
                    known = sapply(paths$epath, function(x) x$Known)))
}

# Function to update the count of an ingredient in the inventory
# returns the updated graph
# ingredient: character vector of length 1 that is an exact string match for 
## the vertex name of the required ingredient
# newCount: numeric vector of length 1
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
setIngredientCount = function(ingredient, newCount, g){
  if (length(ingredient) > 1){
    warning("Will only use the first ingredient")
    ingredient = ingredient[1]
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient to get effects")
  }
  if (!(class(newCount) %in% c("numeric", "integer")) | length(newCount) != 1){
    stop("newCount must be a numeric vector of length 1")
  }
  if (!(ingredient %in% V(g)$name[V(g)$Type == "Ingredient"])){
    stop(paste0(ingredient, " is not a known ingredient, check spelling and case."))
  }
  V(g)[V(g)$name == ingredient]$Count <- newCount
  return(g)
}

# Function to get the count of an ingredient in the inventory
# ingredient: character vector of length 1 that is an exact string match for 
## the vertex name of the required ingredient
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
getIngredientCount = function(ingredient, g){
  if (length(ingredient) > 1){
    warning("Will only use the first ingredient")
    ingredient = ingredient[1]
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient to get effects")
  }
  return(V(g)[V(g)$name == ingredient]$Count)
}

# Function to update the effect of an ingredient to known/unkonwn
# returns the updated graph
# ingredient: character vector of length 1 that is an exact string match for 
## the vertex name of the required ingredient
# effect: character vector of length 1 that is an exact string match for 
## the vertex name of the required effect
# known: boolean value representing whether or not the effect is known for this ingredient
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
setKnownIngredientEffect = function(ingredient, effect, known, g){
  effect = as.character(effect)
  if (length(ingredient) > 1){
    warning("Will only use the first ingredient")
    ingredient = ingredient[1]
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient")
  }
  if (length(effect) > 1){
    warning("Will only use the first effect")
    effect = effect[1]
  }
  if (length(effect) == 0){
    stop("Must supply one effect")
  }
  if (length(known) > 1){
    warning("Will only use the first value for known")
    effect = known
  }
  if (length(effect) == 0 | class(known) != "logical"){
    stop("Must supply a logical value for known")
  }
  if (!(ingredient %in% V(g)$name[V(g)$Type == "Ingredient"])){
    stop(paste0(ingredient, " is not a known ingredient, check spelling and case."))
  }
  # validate that effect is adjacent to ingredient
  isAdjacent = effect %in% getIngredientEffectsDf(ingredient, g)$effect
  if (isAdjacent){
    # get edge and update value
    E(g, P = V(g)[V(g)$name %in% c(ingredient, effect)])$Known = known
  } else {
    warning(paste0("The effect: ", effect, " is not an effect of ", ingredient, 
                   ".\nIgnoring this operation."))
  }
  return(g)
}

# Function to simulate crafting a potion using the ingredients provided
# Updates values for counts and known effects
# returns updated graph
# ingredients: character vector of length 2 or 3
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2019-12-31
makePotion = function(ingredients, g){
  # fail if not 2 or 3 ingredients
  if (length(ingredients) != 2 && length(ingredients) != 3){
    stop("2 or 3 ingredients must be provided")
  }
  # fail if all ingredients don't have at least one in inventory
  ingredientCounts = sapply(ingredients, function(x) getIngredientCount(x, g))
  if (any(ingredientCounts < 1)){
    stop("Not enough ingredients")
  }
  ingredientEffects = getPotionEffects(ingredients, g)
  
  # reduce all ingredient counts by 1
  for(ingredient in ingredients){
    currCount = getIngredientCount(ingredient, g)
    g = setIngredientCount(ingredient, currCount - 1, g)
    # set all ingredient/effect edges known status to true
    for (effect in ingredientEffects){
      g = setKnownIngredientEffect(ingredient, effect, T, g)
    }
  }
  return(g)
}

# Function to identify the number of ingredient-effect edges revealed (number where Known == F)
# Updates values for counts and known effects
# returns number of edges revealed
# ingredients: character vector of length 2 or 3
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2019-12-31
getEdgesRevealed = function(ingredients, g){
  # fail if not 2 or 3 ingredients
  if (length(ingredients) != 2 && length(ingredients) != 3){
    stop("2 or 3 ingredients must be provided")
  }
  # fail if all ingredients don't have at least one in inventory
  ingredientCounts = sapply(ingredients, function(x) getIngredientCount(x, g))
  if (any(ingredientCounts < 1)){
    stop("Not enough ingredients")
  }
  # create subgraph of all effects and only the ingredients passed to this function
  sg = induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$name %in% ingredients])
  sgEDf = igraph::as_data_frame(sg, "edges")
  sgEDf = sgEDf %>% 
    group_by(to) %>%
    filter(n() > 1) %>%
    ungroup()
  return(sum(!sgEDf$Known))
}

# Function to determine the potion to craft that will reveal the most ingredient effects (min 1)
# This approach does not consider optimizing interactions with ingredients with the fewest 
# quantity in inventory
# There is a chance that this will make a recommendation that when considering subsequent 
# iterations is less than optimal
# returns vector of the ingredients to use
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2019-12-31
recommendPotionForEffectReveal = function(g){
  # get subgraph showing only unknown effects and ingredients with at least one count
  sg = induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$Count > 0])
  # remove effects that only have one edge remaining 
  # (these couldn't be obtained as there are no ingredients in the inventory to pair with)
  sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
  # remove known edges 
  # done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  # get ingredient vertices
  ingredientVs = V(sg)[V(sg)$Type == "Ingredient"]
  if (length(ingredientVs) == 0){ # exit early if there are no more vertices to check
    return(NA)
  }
  ingredientVs_fromSgUnknown = V(sgUnknown)[V(sgUnknown)$Type == "Ingredient"] # only able to treat these as the same because the order of the vertices is intact
  # get ingredient with highest number of edges # returns named numeric vector
  ingredientNumHiddenEffects = degree(sgUnknown, ingredientVs_fromSgUnknown)
  ingredientIndexToUse = which(ingredientNumHiddenEffects == max(ingredientNumHiddenEffects))[1]
  ### get effects this ingredient connects to and find which of these has the highest number of connections
  # get all nodes within distance 2 and 4 from this vertex (these are potential ingredients for the potion)
  searchResults = bfs(sg, root = ingredientVs[ingredientIndexToUse], order = F, unreachable = F, dist = T)
  VerticesToUse = names(searchResults$dist[!is.nan(searchResults$dist) & (searchResults$dist == 2 | searchResults$dist == 4)])
  # get name of first ingredient
  ingredient = ingredientVs$name[ingredientIndexToUse]
  # all combinations of 2 and 3 ingredients from these values
  potionCombinations = c(lapply(VerticesToUse, function(x) c(ingredient, x)), 
                         lapply(getAllCombinationsOfRange(VerticesToUse, 2), function(x) c(ingredient, x)))
  if (length(potionCombinations) == 0){
    # exit as there are no more combinations
    return(NA)
  }
  # calculate number of effects revealed for each, as well as number of ingredients used 
  # and number of ingredients with only one in inventory
  combinationMetadata = do.call(rbind, lapply(1:length(potionCombinations), function(i){
    data.frame(index = i,
               numRevealedEffects = getEdgesRevealed(potionCombinations[[i]], sg), # could also just send g here
               numIngredientsWithOnly1Count = sum(ingredientVs$Count[ingredientVs$name %in% potionCombinations[[i]]] == 1),
               numIngredientsUsed = length(potionCombinations[[i]]))
  }))
  combinationMetadata = combinationMetadata %>% 
    filter(numRevealedEffects > 0) %>%
    arrange(-numRevealedEffects, numIngredientsUsed, -numIngredientsWithOnly1Count)
  if (nrow(combinationMetadata) == 0){
    return(NA)
  }
  return(potionCombinations[[combinationMetadata$index[1]]])
}

# Function to expand all unique pairings of a vector in n-dimensions
# x: a vector of unique values
# n: a numeric value of length 1 no less than 2
# include.equals (allows duplication of values)
# Modified from code found here: https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
# Author: Nathan Pratt
# 2020-01-04
expand.grid.unique_FromSingleVector <- function(x, n = 2, include.equals=FALSE)
{
  g <- function(i, x, n)
  {
    z <- setdiff(x[i:length(x)], x[seq_len(i-include.equals)])
    
    if(length(z)) 
    {
      if (n == 1){
        NextLevel = z
      } else {
        NextLevel = do.call(rbind, lapply(seq_along(z[1:(length(z) - n + 1)]), function(k) g(k, z, n-1)))
      }
      cbind(x[i], NextLevel, deparse.level=0)
    }
  }
  
  do.call(rbind, lapply(seq_along(x[1:(length(x) - n + 1)]), function(k) g(k, x, n - 1)))
}

# Function to get all unique combinations of a range of values
# Combinations of n values
# ij: range of numericValues (or vector of unique values)
# Author: Nathan Pratt
# 2020-01-03
getAllCombinationsOfRange = function(ij, n){
  if (n > length(ij)){
    warning("n cannot exceed the length of range ij.")
    return(ij)
  }
  gridComboDf = expand.grid.unique_FromSingleVector(ij, n)
  returnVal = lapply(1:nrow(gridComboDf), function(i) gridComboDf[i,])
  return(returnVal)
}

# Function to recommend sequence of potions that will unlock the highest number of ingredient effects
# returns list of character vectors, each containing the ingredients for a potion
# g: the graph of all ingredients and effects
# Author: Nathan Pratt
# 2020-01-04
potionsRecommendedForEffectReveal = function(g){
  potionRecommendations = list()
  repeat{
    x = recommendPotionForEffectReveal(g)
    if (is.na(x[1])){
      break
    } else {
      # simulate making the potion and add to list
      g = makePotion(x, g)
      potionRecommendations[[length(potionRecommendations) + 1]] = x
    }
  }
  return(potionRecommendations)
}

# Function to simulate a list of potion ingredients and return metadata regarding the number of effects revealed etc.
# returns dataframe
# potions: list of vectors, each vector is a set of ingredients for a potion
# g: the graph of all ingredients and effects
# Author: Nathan Pratt
# 2020-01-05
getEffectRevealMetaData = function(potions, g){
  dataframeList = list()
  sg = g # remaining graph, will be updated each iteration
  for(ingredients in potions){
    df = data.frame(
      numRevealedEffects = getEdgesRevealed(ingredients, sg)
    )
    sg = makePotion(ingredients, sg)
    dataframeList[[length(dataframeList) + 1]] = df
  }
  return(do.call(rbind, dataframeList))
}

