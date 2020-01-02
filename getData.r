# Get data and format as graph
# Nathan Pratt 
# 2020-01-01

ingredientsUrl = "https://elderscrolls.fandom.com/wiki/Ingredients_(Skyrim)"

# get ingredient table
page = html(ingredientsUrl)

tables = page %>%
  html_nodes("table")
ingredientTable = html_table(tables[[1]])

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
alchemyGraph = graph.data.frame(alchemyEdges, directed = F, vertices = alchemyNodes)

# remove values that will no longer be used
rm(ingredientsUrl, page, tables)

# save graph
saveRDS(alchemyGraph, file.choose())
