#!/usr/bin/env Rscript

#TODO: many magic thresholds floating around, make them relative

smallThresh <- 5000000
mediumThresh <- 7000000


# the actual size of nodes is too big with their actual weight
# dividing it by this value makes it easier to look at
node_size_divisor <- 70000

library("networkD3")
library("igraph")

file_name = file.choose()

data_csv = read.csv(file_name, header=TRUE)

data_matrix <- as.matrix(data_csv)

data_frame <- data.frame(data_matrix)

# getting the igraph graph
g  <- graph_from_data_frame(data_frame, directed=FALSE)
g <- set_edge_attr(g, "weight", value=as.numeric(data_csv$weight))

# get the size of each section by finding edge with target or source = THE REST

nodeSize <- array(1:(length(V(g)) - 1))
nodeGroup <- array(1:(length(V(g)) - 1))
linkValue <- array(1:(length(V(g)) - 1))

rest_id <- as.numeric(which(V(g)$name == "THE REST"))

assign_node_group <- function(index, g, e){
  node_weight <- as.numeric(E(g)$weight[e])
  nodeSize[index] <- node_weight/node_size_divisor
  
  if(node_weight < smallThresh){
    nodeGroup[index] = "small"
    } else if(node_weight < mediumThresh) {
      nodeGroup[index] = "medium"
    } else {
        nodeGroup[index] = "big"
    }
}

for (e in E(g)){
  if(head_of(g, e) == rest_id) {
    assign_node_group(tail_of(g, e), g, e)
  } else if (tail_of(g, e) == rest_id) {
    assign_node_group(head_of(g, e), g, e)
  }
}

g<-delete.edges(g,which(as_edgelist(g)=="THE REST",arr.ind=TRUE)[,1])


# removing this because it's trivial that the rest includes everything
g <- delete.vertices(g, "THE REST")

#removing these for better visualization
#g <- delete.vertices(g, "DOCUMENTATION")
#g <- delete.vertices(g, "DOCUMENTATION/ITALIAN")
#g <- delete.vertices(g, "OPEN FIRMWARE AND FLATTENED DEVICE TREE BINDINGS")


eb <- cluster_edge_betweenness(g, weights=NULL, directed=FALSE)

members <- membership(eb)

plot(eb, g)


# source: https://rdrr.io/cran/networkD3/man/igraph_to_networkD3.html

# convert to a suitable object for networkD3

gd3 <- igraph_to_networkD3(g, group=members)


# create my own node list with my assigned values added

nodeList <- cbind(gd3$nodes, nodeSize, nodeGroup)

# creating the link list. Further information to be appended?
linkList <- gd3$links


# create force undirected network plot
forceNetwork(Links = gd3$links, Nodes = nodeList,
             Source = 'source', Target = 'target', NodeID = 'name',
             Value="value",
             Nodesize = "nodeSize",
             fontSize = 50,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             linkDistance = JS("function(d){return ((500 > 10000/d.value ? 1000/d.value : 500) < 100 ? 100 : 10000/d.value);}"),
             #linkWidth = JS("function(d){return d.value/100000}"),
             linkWidth = 5,
             Group = 'nodeGroup', zoom = TRUE
             ) %>%

saveNetwork(file = "forcedNet.html")



##########betweenness of nodes -> not used for now, but perhaps interesting?


# nodebetweenness to control node size
# source for nodebetweenness: https://gist.github.com/Vessy/d0228c983349cf138cefd0ced4098359

# calculating the betweenness of nodes, can I use that later?

betAll <- igraph::betweenness(g, v = igraph::V(g), directed = FALSE) / (((igraph::vcount(g) - 1) * (igraph::vcount(g)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))




rm(betAll, betAll.norm)

