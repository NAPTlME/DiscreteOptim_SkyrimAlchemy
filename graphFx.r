# Function Declarations
# Nathan Pratt 
# 2020-01-01

library(igraph)
library(dplyr)
library(animation)

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
# Function to get all effects of a potion using the potion provided
# potionV: Vertex of a potion
# g: igraph containing all ingredients and effects and potions
# Author: Nathan Pratt
# 2020-12-01
getPotionEffects_2 = function(potionV, g){
  # get all effects connected to ingredients
  connectedV = neighborhood(g, order = 2, nodes = potionV, mode = "out")[[1]]
  effectVs = connectedV[connectedV$Type == "Effect"]
  # get all shortest paths to effects, group by count and return those with more than one
  paths = all_shortest_paths(g, from = potionV, to = effectVs, mode = "out")
  effects = sapply(paths$res, function(x) {
    names(x[length(x)])
  })
  tbl = table(effects)
  effects = names(tbl[tbl > 1])
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
# Function to identify the number of ingredient-effect edges revealed (number where Known == F)
# Updates values for counts and known effects
# returns number of edges revealed
# ingredients: character vector of length 2 or 3
# g: igraph containing all ingredients and effects and has potion nodes
# Author: Nathan Pratt
# 2019-12-31
getEdgesRevealed_2 = function(potionV, g){
  ingredients = neighbors(g, potionV, mode = "out")
  # fail if all ingredients don't have at least one in inventory
  ingredientCounts = sapply(names(ingredients), function(x) getIngredientCount(x, g))
  if (any(ingredientCounts < 1)){
    stop("Not enough ingredients")
  }
  # get all effects connected to ingredients
  connectedV = neighborhood(g, order = 2, nodes = potionV, mode = "out")[[1]]
  effectVs = connectedV[connectedV$Type == "Effect"]
  # get all shortest paths to effects, group by count and return those with more than one
  paths = all_shortest_paths(g, from = potionV, to = effectVs, mode = "out")
  ingEffectPairs = sapply(paths$res, function(x) x[c(2,3)])
  edgeIds = get.edge.ids(g, ingEffectPairs)
  return(sum(!E(g)$Known[edgeIds]))
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
  searchResults = bfs(sg, root = ingredientVs[ingredientIndexToUse], order = F, unreachable = F, dist = T, neimode = "all")
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
recommendPotionForEffectReveal1.1 = function(g){
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
  searchResults = bfs(sg, root = ingredientVs[ingredientIndexToUse], order = F, unreachable = F, dist = T, neimode = "all")
  vertices2Away = names(searchResults$dist[!is.nan(searchResults$dist) & searchResults$dist == 2])
  # get name of first ingredient
  ingredient = ingredientVs$name[ingredientIndexToUse]
  potionCombinations = lapply(vertices2Away, function(ing2) c(ingredient, ing2))
  if (length(vertices2Away) > 1){
    potionCombinations = c(potionCombinations,
                           lapply(getAllCombinationsOfRange(vertices2Away, 2), function(x) c(ingredient, x)))
  }
  potionCombinations = c(potionCombinations, 
                         lapply(vertices2Away, function(ing2){
                           potion1 = c(ingredient, ing2)
                           ing1Effects = neighbors(sg, V(sg)[V(sg)$name == ingredient], mode = "out")$name
                           ing2Effects = neighbors(sg, V(sg)[V(sg)$name == ing2], mode = "out")$name
                           ing2Effects = setdiff(ing2Effects, ing1Effects) # get effects not common primary ingredient
                           ing2effectDeg = degree(sg, V(sg)[V(sg)$name %in% ing2Effects], mode = "in")
                           ing2EffectDegUnk = degree(sgUnknown, V(sgUnknown)[V(sgUnknown)$name %in% ing2Effects], mode = "in")
                           if (any(ing2EffectDegUnk > 0 & ing2effectDeg > 1)){
                             ing2Effects = ing2Effects[ing2EffectDegUnk > 0 & ing2effectDeg > 1]
                             effectNeighbors = unique(unlist(lapply(ing2Effects, function(effect){
                               neighborNames = neighbors(sg, V(sg)[V(sg)$name == effect], mode = "in")$name
                               neighborNames = setdiff(neighborNames, c(ingredient, ing2))
                               return(neighborNames)
                             })))
                             return(lapply(effectNeighbors, function(ing3){
                               c(ingredient, ing2, ing3)
                             }))
                           } else {
                             return(NULL)
                           }
                           
                           }) %>% unlist(recursive = F))
  potionCombinations = lapply(potionCombinations, sort) # sort each set of ingredients
  potionCombinations = unique(potionCombinations) # remove duplicates
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
recommendPotionForEffectReveal2 = function(g){
  # get subgraph showing only unknown effects and ingredients with at least one count
  sg = induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$Count > 0])
  # remove effects that only have one edge remaining 
  # (these couldn't be obtained as there are no ingredients in the inventory to pair with)
  sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
  # remove known edges 
  # done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  sgKnown = delete.edges(sg, E(sg)[!E(sg)$Known])
  # get ingredient vertices
  ingredientVs = V(sg)[V(sg)$Type == "Ingredient"]
  if (length(ingredientVs) == 0 | sum(!E(sg)$Known) == 0){ # exit early if there are no more vertices to check or all effects are known
    return(NA)
  }
  ingredientVs = V(sgUnknown)[V(sgUnknown)$Type == "Ingredient"]
  similarityMatrix = similarity(sgUnknown, ingredientVs, mode = "all", method = "dice")
  # clear diagonals
  similarityMatrix[row(similarityMatrix) == col(similarityMatrix)] = 0
  if (max(similarityMatrix) > 0){
    topResults = which(similarityMatrix == max(similarityMatrix), arr.ind = T)
    ing1Index = topResults[1,1]
    ing2Index = topResults[1,2]
    potionCombin = ingredientVs[c(ing1Index, ing2Index)]$name
    # look for secondary ingredients that give any added value
    rowMatches = similarityMatrix[ing1Index,]
    colMatches = similarityMatrix[,ing2Index]
    # clear the match identified above
    rowMatches[ing2Index] = 0
    colMatches[ing1Index] = 0
    if (max(colMatches) > max(rowMatches)){
      ing3Index = which(colMatches == max(colMatches))[1]
      potionCombin = c(potionCombin, ingredientVs[ing3Index]$name)
    } else if (max(rowMatches) > 0){
      ing3Index = which(rowMatches == max(rowMatches))[1]
      potionCombin = c(potionCombin, ingredientVs[ing3Index]$name)
    } else { # look for match among any of the known edges of these ingredients
      ings = V(sgKnown)[V(sgKnown)$name %in% potionCombin]
      effects = unique(unlist(lapply(ings, function(ing) neighbors(sgKnown, ing, mode = "out")$name)))
      # try to find ingredients via unknown edges (order by those found the most)
      effectVs = V(sgUnknown)[V(sgUnknown)$name %in% effects]
      unknownEdgeEnds = sort(table(unlist(sapply(effectVs, function(effectV){
        neighbors(sgUnknown, effectV, mode = "in")$name
      }))), decreasing = T)
      if (length(unknownEdgeEnds) > 0){
        potionCombin = c(potionCombin, names(unknownEdgeEnds[1]))
      }
    }
    return(potionCombin)
  } else {
    return(recommendPotionForEffectReveal1.1(g))
  }
}
isEqualPotionCountsDf = data.frame(count1 = numeric(0), count2 = numeric(0), isEqual = logical(0))
recommendPotionForEffectReveal2.2 = function(g){ # effect based optim
  # get subgraph showing only unknown effects and ingredients with at least one count
  sg <<- induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$Count > 0])
  # remove effects that only have one edge remaining 
  # (these couldn't be obtained as there are no ingredients in the inventory to pair with)
  sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
  
  # get ingredient vertices
  ingredientVs = V(sg)[V(sg)$Type == "Ingredient"]
  if (length(ingredientVs) == 0){ # exit early if there are no more vertices to check
    return(NA)
  }
  
  # remove known edges 
  # done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  
  # get effect vertices
  effects = V(sgUnknown)[V(sgUnknown)$Type == "Effect"]
  effectDeg = degree(sgUnknown, effects)
  # convert any degree above 3 to 3
  effectDeg[effectDeg > 3] = 3
  # filter on only those equal to the max
  effectsToCheck = names(effectDeg[effectDeg == max(effectDeg)])
  effectMeta = lapply(effectsToCheck, function(effect){
    searchResult = bfs(sgUnknown, V(sgUnknown)[V(sgUnknown)$name == effect], neimode = "all", unreachable = F, dist = T)
    effectsToPath = names(searchResult$dist[!is.na(searchResult$dist) & (searchResult$dist == 2 | searchResult$dist == 4)])
    
    # all one step away
    ingOneStepAway = names(searchResult$dist[!is.na(searchResult$dist) & searchResult$dist == 1])
    # remove NAs
    potionOneStepAway = list()
    if (length(ingOneStepAway) > 1){
      potionOneStepAway = c(potionOneStepAway, getAllCombinationsOfRange(ingOneStepAway, 2))
    } else if (length(ingOneStepAway) == 1){
      # find another ingredient with the same effect and the highest inv
      vOneStepAway = neighbors(sg, V(sg)[V(sg)$name == effect], mode = "all")
      #filter out the one ingredient we already have
      vOneStepAway = vOneStepAway[vOneStepAway$name != effect]
      #get the one with the highest inv
      if (length(vOneStepAway) > 0){
        potionOneStepAway = c(potionOneStepAway, list(c(ingOneStepAway, vOneStepAway$name[which(vOneStepAway$Count == max(vOneStepAway$Count))[1]])))
      }
    }
    if (length(ingOneStepAway) > 2){
      potionOneStepAway = c(potionOneStepAway, getAllCombinationsOfRange(ingOneStepAway, 3))
    }
    paths = all_shortest_paths(sgUnknown, from = V(sgUnknown)[V(sgUnknown)$name == effect], to = V(sgUnknown)[V(sgUnknown)$name %in% effectsToPath], mode = "all")
    if (length(paths$res) == 0){
      effectsToPath = effectsToCheck[effectsToCheck != effect]
      paths = all_shortest_paths(sg, from = V(sg)[V(sg)$name == effect], to = V(sg)[V(sg)$name %in% effectsToPath], mode = "all")
    }
    potionOneTwoStepAway = list()
    if (length(paths$res) > 0){
      # all one and two steps away
      potionOneTwoStepAway = c(potionOneTwoStepAway, 
                               lapply(paths$res[sapply(paths$res, function(x) length(x) >= 4)], function(x){
                                 return(names(x[c(2,4)]))
                                 }))
    }
    potionOneTwoStepAway = lapply(potionOneTwoStepAway, function(x){
      sort(x)
    })
    potionOneTwoStepAway = potionOneTwoStepAway[!duplicated(potionOneTwoStepAway)]
    # all one and two steps away with all one step away not included
    potionOneTwoOneStepAway = lapply(potionOneTwoStepAway, function(x){
      ingNotInpotion = ingOneStepAway[sapply(ingOneStepAway, function(j) !(j %in% x))]
      lapply(ingNotInpotion, function(y){
        c(x, y)
      })
    }) %>% unlist(recursive = F)
    potionCombin = c(potionOneStepAway, potionOneTwoStepAway, potionOneTwoOneStepAway)
    # order ingredients
    potionCombin = lapply(potionCombin, function(x){
      sort(x)
    })
    potionCombin = potionCombin[!duplicated(potionCombin)]
    # delete duplicates
    ingInPath =  sort(table(sapply(paths$res, function(x) {
      returnIn = 2
      if (length(x) >= 4){
        returnIn = c(returnIn, 4)
      }
      names(x[returnIn])
    }) %>% unlist()), decreasing = T)
    return(list(effect = effect, tbl = ingInPath, potions = potionCombin))
  }) ### test this
  
  effectMetaDf = do.call(rbind, lapply(effectMeta, function(x){
    data.frame(max = max(as.numeric(x$tbl)), mean = mean(as.numeric(x$tbl)), len = length(x$tbl), numPot = length(x$potions))
  })) %>% mutate(id = row_number()) %>% filter(numPot > 0) %>% arrange(-mean, -numPot)
  
  ### could (instead of doing all combinations) only get those 1 step away and pair with those one and 2 steps away, 1 and 2, 1 and 1, 1 and 1 and 2)
  #potionLens = sapply(effectMeta, function(x) length(x$potions))
  
  # ItemToUse = effectMeta[[which(effectMetaDf$max == max(effectMetaDf$max))[1]]] # 9
  ItemToUse = effectMeta[[effectMetaDf$id[1]]] # 12
  # ItemToUse = effectMeta[[which(effectMetaDf$len == max(effectMetaDf$len))[1]]] # 7 -> 10
  # ItemToUse = effectMeta[[which(potionLens == max(potionLens))[1]]] # 10
  print(ItemToUse$effect)
  # 
  # ingToUse = names(ItemToUse$tbl)
  ingToUse = names(ItemToUse$tbl)
  itemAboveMean = ItemToUse$tbl >= mean(ItemToUse$tbl)
  if (sum(itemAboveMean) >= 10){
    ingToUse = ingToUse[itemAboveMean]
  } else {
    ingToUse = head(ingToUse, 10)
  }
  # 
  potionCombinations2 = list()
  if (length(ingToUse) > 1){
    potionCombinations2 = c(potionCombinations2, getAllCombinationsOfRange(ingToUse, 2))
  }
  if (length(ingToUse) > 2){
    potionCombinations2 = c(potionCombinations2, getAllCombinationsOfRange(ingToUse, 3))
  }
  potionCombinations = ItemToUse$potions
  tmpPotions <<- potionCombinations
  
  numRevealed1 = 0
  numRevealed2 = 0
  
  # calculate number of effects revealed for each, as well as number of ingredients used 
  # and number of ingredients with only one in inventory
  
  if (length(potionCombinations2) > 0){
    combinationMetadata2 = do.call(rbind, lapply(1:length(potionCombinations2), function(i){
      data.frame(index = i,
                 numRevealedEffects = getEdgesRevealed(potionCombinations2[[i]], sg), # could also just send g here
                 numIngredientsWithOnly1Count = sum(ingredientVs$Count[ingredientVs$name %in% potionCombinations2[[i]]] == 1),
                 numIngredientsUsed = length(potionCombinations2[[i]]))
    }))
    combinationMetadata2 = combinationMetadata2 %>% 
      filter(numRevealedEffects > 0) %>%
      arrange(-numRevealedEffects, numIngredientsUsed, -numIngredientsWithOnly1Count)
    numRevealed2 = combinationMetadata2$numRevealedEffects[1]
  }
  if (length(potionCombinations) > 0){
    combinationMetadata = do.call(rbind, lapply(1:length(potionCombinations), function(i){
      data.frame(index = i,
                 numRevealedEffects = getEdgesRevealed(potionCombinations[[i]], sg), # could also just send g here
                 numIngredientsWithOnly1Count = sum(ingredientVs$Count[ingredientVs$name %in% potionCombinations[[i]]] == 1),
                 numIngredientsUsed = length(potionCombinations[[i]]))
    }))
    combinationMetadata = combinationMetadata %>% 
      filter(numRevealedEffects > 0) %>%
      arrange(-numRevealedEffects, numIngredientsUsed, -numIngredientsWithOnly1Count)
    numRevealed1 = combinationMetadata$numRevealedEffects[1]
  }
  
  
  isEqualPotionCountsDf <<- rbind(isEqualPotionCountsDf, data.frame(count1 = numRevealed1, count2 = numRevealed2, isEqual = numRevealed1 == numRevealed2))
  if (length(potionCombinations) == 0 || nrow(combinationMetadata) == 0){
    return(NA)
  }
  return(potionCombinations[[combinationMetadata$index[1]]])
}
recommendPotionForEffectReveal2.3 = function(g){ # effect based optim
  # get subgraph showing only unknown effects and ingredients with at least one count
  sg <<- induced_subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$Count > 0])
  # remove effects that only have one edge remaining 
  # (these couldn't be obtained as there are no ingredients in the inventory to pair with)
  sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
  
  # get ingredient vertices
  ingredientVs = V(sg)[V(sg)$Type == "Ingredient"]
  if (length(ingredientVs) == 0){ # exit early if there are no more vertices to check
    return(NA)
  }
  
  # remove known edges 
  # done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  
  # get effect vertices
  effects = V(sgUnknown)[V(sgUnknown)$Type == "Effect"]
  effectDeg = degree(sgUnknown, effects)
  # convert any degree above 3 to 3
  effectDeg[effectDeg > 3] = 3
  # filter on only those equal to the max
  effectsToCheck = names(effectDeg[effectDeg == max(effectDeg)])
  effectMeta = lapply(effectsToCheck, function(effect){
    searchResult = bfs(sgUnknown, V(sgUnknown)[V(sgUnknown)$name == effect], neimode = "all", unreachable = F, dist = T)
    effectsToPath = names(searchResult$dist[!is.na(searchResult$dist) & (searchResult$dist == 2 | searchResult$dist == 4)])
    
    # all one step away
    ingOneStepAway = names(searchResult$dist[!is.na(searchResult$dist) & searchResult$dist == 1])
    # remove NAs
    
    paths = all_shortest_paths(sgUnknown, from = V(sgUnknown)[V(sgUnknown)$name == effect], to = V(sgUnknown)[V(sgUnknown)$name %in% effectsToPath], mode = "all")
    if (length(paths$res) == 0){
      effectsToPath = effectsToCheck[effectsToCheck != effect]
      paths = all_shortest_paths(sg, from = V(sg)[V(sg)$name == effect], to = V(sg)[V(sg)$name %in% effectsToPath], mode = "all")
    }
    
    ingInPath =  sort(table(sapply(paths$res, function(x) {
      returnIn = 2
      if (length(x) >= 4){
        returnIn = c(returnIn, 4)
      }
      names(x[returnIn])
    }) %>% unlist()), decreasing = T)
    return(list(effect = effect, tbl = ingInPath))
  }) ### test this
  
  effectMetaDf = do.call(rbind, lapply(effectMeta, function(x){
    data.frame(max = max(as.numeric(x$tbl)), mean = mean(as.numeric(x$tbl)), len = length(x$tbl))
  })) %>% mutate(id = row_number()) %>% filter(!is.nan(mean)) %>% arrange(-mean, -numPot)
  
  ### could (instead of doing all combinations) only get those 1 step away and pair with those one and 2 steps away, 1 and 2, 1 and 1, 1 and 1 and 2)
  #potionLens = sapply(effectMeta, function(x) length(x$potions))
  
  # ItemToUse = effectMeta[[which(effectMetaDf$max == max(effectMetaDf$max))[1]]] # 9
  ItemToUse = effectMeta[[effectMetaDf$id[1]]] # 12
  # ItemToUse = effectMeta[[which(effectMetaDf$len == max(effectMetaDf$len))[1]]] # 7 -> 10
  # ItemToUse = effectMeta[[which(potionLens == max(potionLens))[1]]] # 10
  print(ItemToUse$effect)
  # 
  # ingToUse = names(ItemToUse$tbl)
  ingToUse = names(ItemToUse$tbl)
  itemAboveMean = ItemToUse$tbl >= mean(ItemToUse$tbl)
  if (sum(itemAboveMean) >= 10){
    ingToUse = ingToUse[itemAboveMean]
  } else {
    ingToUse = head(ingToUse, 10)
  }
  # 
  potionCombinations2 = list()
  if (length(ingToUse) > 1){
    potionCombinations = c(potionCombinations, getAllCombinationsOfRange(ingToUse, 2))
  }
  if (length(ingToUse) > 2){
    potionCombinations = c(potionCombinations, getAllCombinationsOfRange(ingToUse, 3))
  }
  tmpPotions <<- potionCombinations
  
  # calculate number of effects revealed for each, as well as number of ingredients used 
  # and number of ingredients with only one in inventory
  
  if (length(potionCombinations) > 0){
    combinationMetadata = do.call(rbind, lapply(1:length(potionCombinations), function(i){
      data.frame(index = i,
                 numRevealedEffects = getEdgesRevealed(potionCombinations[[i]], sg), # could also just send g here
                 numIngredientsWithOnly1Count = sum(ingredientVs$Count[ingredientVs$name %in% potionCombinations[[i]]] == 1),
                 numIngredientsUsed = length(potionCombinations[[i]]))
    }))
    combinationMetadata = combinationMetadata %>% 
      filter(numRevealedEffects > 0) %>%
      arrange(-numRevealedEffects, numIngredientsUsed, -numIngredientsWithOnly1Count)
    numRevealed1 = combinationMetadata$numRevealedEffects[1]
  } else {
    print("Using legacy function")
    return(recommendPotionForEffectReveal(g)) # fall back to other route
  }
  return(potionCombinations[[combinationMetadata$index[1]]])
}
recommendPotionForEffectReveal3 = function(g){ # assumes the potion layer is present
  # because of limitations in igraph, a proper traversal can't be used, so we will have to subgraph for certain calculations
  # remove potions for which any ingredients are missing
  sg = g
  missingIngredients = V(g)[V(g)$Type == "Ingredient" & V(g)$Count == 0]
  if (length(missingIngredients) > 0){
    # potionsMissingIngredients = sapply(missingIngredients, function(v){
    #   #all_simple_paths(g, from = v, to = V(g)[V(g)$Type == "Potion"], mode = "in")
    #   neighbors(g, v, mode = "in")
    # })
    potionsMissingIngredients = unique(unlist(adjacent_vertices(g, missingIngredients, mode = "in")))
    sg = delete.vertices(sg, potionsMissingIngredients)
  }
  
  # # remove potions for which all effects are known (find those that have missing effects and delete all others)
  # effectEdges = unlist(sapply(V(sg)[V(sg)$Type == "Effect"], function(v){
  #   E(sg)[to(v)]
  # }))
  # unknownEffectEdges = effectEdges[!E(sg)[effectEdges]$Known]
  # pathsWithUnknownEdges = all_shortest_paths(sg, from = V(sg)[V(sg)$name == "Root"], to = unknownEffectEdges, mode = "out")
  # 
  
  # from remaining potions, get revealed effect counts
  # potionVs = V(sg)[V(sg)$Type == "Potion"]
  # startTime = Sys.time()
  # revealedCounts = sapply(potionVs, function(potionV){
  #   getEdgesRevealed_2(potionV, sg)
  # })
  # difftime(Sys.time(), startTime)
  # get subgraph showing only unknown effects and ingredients with at least one count
  sg = induced_subgraph(gs, V(sg)[V(sg)$Type == "Effect" | V(sg)$Count > 0])
  # remove effects that only have one edge remaining 
  # (these couldn't be obtained as there are no ingredients in the inventory to pair with)
  sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
  # remove known edges 
  # done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  # get paths from root to unknown effects
  paths = all_shortest_paths(sg, from = V(sg)[V(sg)$name == "Root"], to = V(sg)[V(sg)$Type == "Effect"])
  tmpN = neighborhood(sg, order = 2, nodes = potionVs, mode = "out")
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
  ## unfortunately, this is everything in this dataset when you have most ingredients
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
potionsRecommendedForEffectReveal1.1 = function(g){
  potionRecommendations = list()
  repeat{
    x = recommendPotionForEffectReveal1.1(g)
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
potionsRecommendedForEffectReveal2 = function(g){
  potionRecommendations = list()
  repeat{
    x = recommendPotionForEffectReveal2(g)
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
potionsRecommendedForEffectReveal2.2 = function(g){
  potionRecommendations = list()
  repeat{
    x = recommendPotionForEffectReveal2.2(g)
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
potionsRecommendedForEffectReveal2.3 = function(g){
  potionRecommendations = list()
  repeat{
    x = recommendPotionForEffectReveal2.2(g)
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

startTime = Sys.time()
potions = potionsRecommendedForEffectReveal(alchemyGraph)
difftime(Sys.time(), startTime)

samplePotion = c("Bear Claws", "Eye of Sabre Cat", "Hanging Moss", "Bee")
sampleGraph = induced_subgraph(alchemyGraph, V(alchemyGraph)[V(alchemyGraph)$Type == "Effect" | V(alchemyGraph)$name %in% samplePotion])
sampleGraph = delete.vertices(sampleGraph, V(sampleGraph)[degree(sampleGraph, V(sampleGraph)) == 0])
sampleGraph = as.undirected(sampleGraph, mode = "each")
# set ingredient counts to 5
V(sampleGraph)[V(sampleGraph)$Type == "Ingredient"]$Count = 5
coords = layout.fruchterman.reingold(sampleGraph)

# set edges to known where effect degree is one
effectVs = V(sampleGraph)[V(sampleGraph)$Type == "Effect"]
effectVsDeg = degree(sampleGraph, effectVs, mode = "in")
effectEdgeIndices = unlist(incident_edges(sampleGraph, v = effectVs[effectVsDeg == 1], mode = "in"))
# as well as a custom one
effectEdgeIndices = c(effectEdgeIndices, 
                      get.edge.ids(sampleGraph, V(sampleGraph)[V(sampleGraph)$name %in% c("Eye of Sabre Cat", "Damage Magicka")]))
E(sampleGraph)[effectEdgeIndices]$Known = T

plot(sampleGraph, 
     layout = coords,
     #vertex.label = NA, 
     vertex.label.color = V(sampleGraph)$color,
     edge.lty = (!E(sampleGraph)$Known) + 1,
     vertex.size = 0)

# get subgraph showing only unknown effects and ingredients with at least one count
sg = induced_subgraph(sampleGraph, V(sampleGraph)[V(sampleGraph)$Type == "Effect" | V(sampleGraph)$Count > 0])
# remove effects that only have one edge remaining 
# (these couldn't be obtained as there are no ingredients in the inventory to pair with)
layout_sg = coords[!(V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1),] # to keep the plot consistent
sg = delete.vertices(sg, V(sg)[V(sg)$Type == "Effect" & degree(sg, V(sg)) == 1])
# remove known edges 
# done in separate graph to allow connections where one ingredient knows the effect and another does not (so the one hidden one can be revealed)
sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])


plot(sg, 
     layout = layout_sg,
     #vertex.label = NA, 
     vertex.label.color = V(sg)$color,
     edge.lty = (!E(sg)$Known) + 1,
     vertex.size = 0)

plot(sgUnknown, 
     layout = layout_sg,
     #vertex.label = NA, 
     vertex.label.color = V(sgUnknown)$color,
     edge.lty = (!E(sgUnknown)$Known) + 1,
     vertex.size = 0)

# get ingredient vertices
ingredientVs = V(sg)[V(sg)$Type == "Ingredient"]
ingredientVs_fromSgUnknown = V(sgUnknown)[V(sgUnknown)$Type == "Ingredient"] # only able to treat these as the same because the order of the vertices is intact
# get ingredient with highest number of edges # returns named numeric vector
ingredientNumHiddenEffects = degree(sgUnknown, ingredientVs_fromSgUnknown)
ingredientIndexToUse = which(ingredientNumHiddenEffects == max(ingredientNumHiddenEffects))[1]
# get name of first ingredient
ingredient = ingredientVs$name[ingredientIndexToUse]
# get all nodes within distance 2 and 4 from this vertex (these are potential ingredients for the potion)
searchResults = bfs(sg, root = ingredientVs[ingredientIndexToUse], order = F, unreachable = F, dist = T, neimode = "all")
vertices2Away = names(searchResults$dist[!is.nan(searchResults$dist) & searchResults$dist == 2])

potionCombinations = lapply(vertices2Away, function(ing2) c(ingredient, ing2))
if (length(vertices2Away) > 1){
  potionCombinations = c(potionCombinations,
                         lapply(getAllCombinationsOfRange(vertices2Away, 2), function(x) c(ingredient, x)))
}
potionCombinations = c(potionCombinations, 
                       lapply(vertices2Away, function(ing2){
                         potion1 = c(ingredient, ing2)
                         ing1Effects = neighbors(sg, V(sg)[V(sg)$name == ingredient], mode = "out")$name
                         ing2Effects = neighbors(sg, V(sg)[V(sg)$name == ing2], mode = "out")$name
                         ing2Effects = setdiff(ing2Effects, ing1Effects) # get effects not common primary ingredient
                         ing2effectDeg = degree(sg, V(sg)[V(sg)$name %in% ing2Effects], mode = "in")
                         ing2EffectDegUnk = degree(sgUnknown, V(sgUnknown)[V(sgUnknown)$name %in% ing2Effects], mode = "in")
                         if (any(ing2EffectDegUnk > 0 & ing2effectDeg > 1)){
                           ing2Effects = ing2Effects[ing2EffectDegUnk > 0 & ing2effectDeg > 1]
                           effectNeighbors = unique(unlist(lapply(ing2Effects, function(effect){
                             neighborNames = neighbors(sg, V(sg)[V(sg)$name == effect], mode = "in")$name
                             neighborNames = setdiff(neighborNames, c(ingredient, ing2))
                             return(neighborNames)
                           })))
                           return(lapply(effectNeighbors, function(ing3){
                             c(ingredient, ing2, ing3)
                           }))
                         } else {
                           return(NULL)
                         }
                         
                       }) %>% unlist(recursive = F))
potionCombinations = lapply(potionCombinations, sort)
potionCombinations = unique(potionCombinations)
# all combinations of 2 and 3 ingredients from these values
# potionCombinations = c(lapply(VerticesToUse, function(x) c(ingredient, x)), 
#                        lapply(getAllCombinationsOfRange(VerticesToUse, 2), function(x) c(ingredient, x)))
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
# would exit if nrow of combinationMetadata were 0
samplePotionCombination = potionCombinations[[combinationMetadata$index[1]]]

plot(sampleGraph, 
     layout = coords, 
     vertex.label.color = V(sampleGraph)$color,
     edge.lty = (!E(sampleGraph)$Known) + 1,
     vertex.size = 0)

sampleGraph_afterPot = makePotion(samplePotionCombination, sampleGraph)

plot(sampleGraph_afterPot, 
     layout = coords,
     #vertex.label = NA, 
     vertex.label.color = V(sampleGraph_afterPot)$color,
     edge.lty = (!E(sampleGraph_afterPot)$Known) + 1,
     vertex.size = 0)

bfs(tmp, V(tmp)[[1]], neimode = "out", order = F, unreachable = F, dist = T)

# add potion

tmp = add.vertices(tmp, 1, attr = list(name = "potion1", color = "goldenrod", Type = "Potion"))
tmp = add_edges(tmp, c(V(tmp)[V(tmp)$name == "potion1"], V(tmp)[V(tmp)$name == tmpPotion[1]],
                       V(tmp)[V(tmp)$name == "potion1"], V(tmp)[V(tmp)$name == tmpPotion[3]]))


plot(tmp, 
     #vertex.label = NA, 
     vertex.label.color = V(tmp)$color,
     vertex.size = 0)
tmpBfs = bfs(tmp, V(tmp)[V(tmp)$name == "potion1"], neimode = "out", order = F, unreachable = F, dist = T)

ingredients = names(tmpBfs$dist)[!is.nan(tmpBfs$dist) & tmpBfs$dist == 1]
allEffects = names(tmpBfs$dist)[!is.nan(tmpBfs$dist) & tmpBfs$dist == 2]

all_shortest_paths(tmp, from = V(tmp)[V(tmp)$name == "potion1"], to = V(tmp)[V(tmp)$name %in% allEffects], mode = "out")
sapply(all_simple_paths(tmp, from = V(tmp)[V(tmp)$name == "potion1"], to = V(tmp)[V(tmp)$name %in% allEffects], mode = "out"), function(x){
  x[length(x)]
})

startTime = Sys.time()
tmp1 = potionsRecommendedForEffectReveal(g)
difftime(Sys.time(), startTime)
startTime = Sys.time()
tmp2 = potionsRecommendedForEffectReveal2.2(g)
difftime(Sys.time(), startTime)
tmpReveal2 = getEffectRevealMetaData(tmp2, alchemyGraph)
startTime = Sys.time()
tmp3 = potionsRecommendedForEffectReveal2.3(g)
difftime(Sys.time(), startTime)
tmpReveal3 = getEffectRevealMetaData(tmp3, alchemyGraph)
startTime = Sys.time()
tmp4 = potionsRecommendedForEffectReveal2(g)
difftime(Sys.time(), startTime)
tmpReveal4 = getEffectRevealMetaData(tmp4, alchemyGraph)
startTime = Sys.time()
tmp5 = potionsRecommendedForEffectReveal1.1(g)
difftime(Sys.time(), startTime)
tmpReveal5 = getEffectRevealMetaData(tmp5, alchemyGraph)

#run1Pot = tmp

alchemyGraph_Full = alchemyGraph
E(alchemyGraph_Full)$Known = F
V(alchemyGraph_Full)$Count[V(alchemyGraph_Full)$Type == "Ingredient"] = 50

alchemyGraph_Partial = alchemyGraph
E(alchemyGraph_Partial)$Known = F
edgeIndicesForKnown = sample(length(E(alchemyGraph_Partial)), size = floor(length(E(alchemyGraph_Partial))/2), replace = F)
E(alchemyGraph_Partial)[edgeIndicesForKnown]$Known = T
V(alchemyGraph_Partial)$Count = 0
ingredientIndices = as.numeric(V(alchemyGraph_Partial)[V(alchemyGraph_Partial)$Type == "Ingredient"])
ingredientIndicesForCountManip = sample(ingredientIndices, size = floor(length(ingredientIndices)*0.75), replace = F)
ingredientCountVals = ceiling(rnorm(length(ingredientIndicesForCountManip), mean = 2, sd = 1))
ingredientCountVals[ingredientCountVals < 0] = 0 # replace any negative numbers with 0
V(alchemyGraph_Partial)$Count[ingredientIndicesForCountManip] = ingredientCountVals

startTime = Sys.time()
run1 = potionsRecommendedForEffectReveal1.1(alchemyGraph_Full)
difftime(Sys.time(), startTime) # ~ 24 min
run1Reveal = getEffectRevealMetaData(run1, alchemyGraph_Full)
length(run1)
sum(run1Reveal$numRevealedEffects)
summary(run1Reveal$numRevealedEffects)

startTime = Sys.time()
run2 = potionsRecommendedForEffectReveal1.1(alchemyGraph_Partial)
difftime(Sys.time(), startTime) # ~ 5 min
run2Reveal = getEffectRevealMetaData(run2, alchemyGraph_Partial)
length(run2)
sum(run2Reveal$numRevealedEffects)
summary(run2Reveal$numRevealedEffects)

startTime = Sys.time()
run3 = potionsRecommendedForEffectReveal2(alchemyGraph_Full)
difftime(Sys.time(), startTime) # ~ 2 min
run3Reveal = getEffectRevealMetaData(run3, alchemyGraph_Full)
length(run3)
sum(run3Reveal$numRevealedEffects)
summary(run3Reveal$numRevealedEffects)

startTime = Sys.time()
run4 = potionsRecommendedForEffectReveal2(alchemyGraph_Partial)
difftime(Sys.time(), startTime) # ~ 30 sec
run4Reveal = getEffectRevealMetaData(run4, alchemyGraph_Partial)
length(run4)
sum(run4Reveal$numRevealedEffects)
summary(run4Reveal$numRevealedEffects)

# V(alchemyGraph_Full)[V(alchemyGraph_Full)$Type == "Ingredient"]$color = "tomato"
# degPartial = degree(alchemyGraph_Partial, V(alchemyGraph_Partial), mode = "all")
# sgUnknown = delete.edges(sg, E(sg)$Known)
# V(alchemyGraph_Partial)[V(alchemyGraph_Partial)$Type == "Ingredient" & degPartial > 0]$color  = "goldenrod"
# V(alchemyGraph_Partial)[V(alchemyGraph_Partial)$Type == "Ingredient" & degPartial == 4]$color  = "tomato"


# make animated graphs
simulatePotionsForGraphAnimation = function(potions, g, layout){
  sg = induced.subgraph(g, V(g)) # remaining graph, will be updated each iteration
  sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
  degUnknown = degree(sgUnknown, V(sgUnknown), mode = "all")
  V(sg)[V(sg)$Type == "Ingredient" & degUnknown > 0]$color  = "goldenrod"
  V(sg)[V(sg)$Type == "Ingredient" & degUnknown == 4]$color  = "tomato"
  V(sg)[V(sg)$Type == "Ingredient" & degUnknown == 0]$color = "limegreen"
  plot(as.undirected(sg, mode = "each"),
                 layout = layout,
                 vertex.label = NA,
                 vertex.color = V(sg)$color,
                 edge.lty = (!E(sg)$Known) + 1, 
                 vertex.size = 4)
  ani.pause()
  for (potion in potions){
    sg = makePotion(potion, sg)
    sgUnknown = delete.edges(sg, E(sg)[E(sg)$Known])
    degUnknown = degree(sgUnknown, V(sgUnknown), mode = "all")
    V(sg)[V(sg)$Type == "Ingredient" & degUnknown > 0]$color  = "goldenrod"
    V(sg)[V(sg)$Type == "Ingredient" & degUnknown == 4]$color  = "tomato"
    V(sg)[V(sg)$Type == "Ingredient" & degUnknown == 0]$color = "limegreen"
    plot(as.undirected(sg, mode = "each"),
                   layout = layout,
                   vertex.label = NA,
                   vertex.color = ifelse(sapply(V(sg)$name, function(x) x %in% potion), "magenta2", V(sg)$color),
                   edge.lty = (!E(sg)$Known) + 1, 
                   vertex.size = 4)
    ani.pause()
  }
  plot(as.undirected(sg, mode = "each"),
                 layout = layout,
                 vertex.label = NA,
                 vertex.color = V(sg)$color,
                 edge.lty = (!E(sg)$Known) + 1, 
                 vertex.size = 4)
  ani.pause()
}
alchemyGraphLayout = layout.fruchterman.reingold(alchemyGraph_Full)

run1PlotList = simulatePotionsForGraphAnimation(run1, alchemyGraph_Full, alchemyGraphLayout)
animation::saveGIF(simulatePotionsForGraphAnimation(run1, alchemyGraph_Full, alchemyGraphLayout), 
                   interval = 0.4,
                   movie.name = "C:\\Users\\NAP\\Downloads\\run1_alg1_full_3.gif",
                   ani.width = 800,
                   ani.height = 800)
animation::saveGIF(simulatePotionsForGraphAnimation(run2, alchemyGraph_Partial, alchemyGraphLayout), 
                   interval = 0.4, 
                   movie.name = "C:\\Users\\NAP\\Downloads\\run2_alg1_partial_3.gif",
                   ani.width = 800,
                   ani.height = 800)
animation::saveGIF(simulatePotionsForGraphAnimation(run3, alchemyGraph_Full, alchemyGraphLayout), 
                   interval = 0.4, 
                   movie.name = "C:\\Users\\NAP\\Downloads\\run3_alg2_full_3.gif",
                   ani.width = 800,
                   ani.height = 800)
animation::saveGIF(simulatePotionsForGraphAnimation(run4, alchemyGraph_Partial, alchemyGraphLayout), 
                   interval = 0.4, 
                   movie.name = "C:\\Users\\NAP\\Downloads\\run4_alg2_partial_3.gif",
                   ani.width = 800,
                   ani.height = 800)