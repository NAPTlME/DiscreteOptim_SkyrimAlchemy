# Function Declarations
# Nathan Pratt 
# 2020-01-01

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
  sg = subgraph(g, V(g)[V(g)$Type == "Effect" | V(g)$name %in% ingredients])
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
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient to get effects")
  }
  if (class(newCount) != "numeric" | length(newCount) != 1){
    stop("newCount must be a numeric vector of length 1")
  }
  V(g)[V(g)$name == ingredient[1]]$Count <- newCount
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
  }
  if (length(ingredient) == 0){
    stop("Must supply one ingredient to get effects")
  }
  return(V(g)[V(g)$name == ingredient[1]]$Count)
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
  # validate that effect is adjacent to ingredient
  isAdjacent = effect %in% getIngredientEffectsDf(ingredient, g)$effect
  if (isAdjacent){
    # get edge
    effectEdge = E(g)[E(g)$from == ingredient & E(g)$to == effect]
    # update value
    effectEdge$Known = known
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
recommendPotion = function(g){
  
}





