# Visualize graph structure and explore data
# Nathan Pratt
# 2020-01-01

library(threejs)
library(htmlwidgets)

#### Data Exploration ####

#### Visualization of Graph ####
plot(alchemyGraph, 
     vertex.label = NA, 
     vertex.size = 4)

outFile = file.choose()

g.js = alchemyGraph
graph_attr(g.js, "layout") = NULL
gjs = graphjs(g.js, 
              vertex.label = V(alchemyGraph)$name,
              vertex.size = 0.5,
              showLabels = T,
              stroke = F,
              attraction = 0.6, repulsion = 2, opacity = 0.4,
              vertex.color = V(alchemyGraph)$color)

saveWidget(gjs, outFile)
browseURL(outFile)
