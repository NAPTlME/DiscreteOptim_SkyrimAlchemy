# Data Visualization Function Declarations
# Nathan Pratt 
# 2020-01-01

# Function to show percentage of types of ingredients in inventory
# g: igraph containing all ingredients and effects
# Author: Nathan Pratt
# 2020-01-01
plotPercIngredientsOwned = function(g){
  # get ingredient vertices
  ingredientV = V(g)[V(g)$Type == "Ingredient"]
  # get df of owned vs unowned ingredients
  ingredientCountDf = data.frame(Name = ingredientV$name,
                                 Count = ingredientV$Count) %>%
    mutate(Owned = Count > 0)
  grpedIngredientOwnedDf = ingredientCountDf %>%
    group_by(Owned) %>%
    summarise(Count = n()) %>%
    mutate(Group = ifelse(Owned, "Owned", "Unowned"))
  # Calculate position of labels
  # grpedIngredientOwnedDf <<- grpedIngredientOwnedDf %>%
    # arrange(desc(Group)) %>%
    # mutate(prop = Count / sum(grpedIngredientOwnedDf$Count) * 100) %>%
    # mutate(ypos = cumsum(prop) - 0.5 * prop)
  ggplot(grpedIngredientOwnedDf, aes(x = "", y = Count, fill = Group)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() #+
    #geom_text(aes(y = ypos, label = Count, hjust = 0.75), color = "grey15", size = 6)
}
