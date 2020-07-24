#!/usr/bin/env Rscript

smallThresh <- 2000
mediumThresh <- 6000

# the actual size of nodes is too big with their actual weight
# this makes it easier to look at
node_size_divisor <- 10

library("networkD3")
library("igraph")

file_name = file.choose()

data_csv = read.csv(file_name, header=TRUE, row.names = 1)

data_matrix <- as.matrix(data_csv)

# getting the igraph graph
g  <- graph.adjacency(data_matrix, weighted=TRUE,
                      diag=TRUE)



# get size of each section by finding edge with source=target

# verticeWeights <- E(g).select(weight=50)

nodeSize <- array(1:(length(V(g))))
nodeGroup <- array(1:(length(V(g))))

E(g)

for (e in E(g)){
  if(head_of(g, e) == tail_of(g, e)) {
    node_weight <- E(g)$weight[e]
    if(node_weight < smallThresh){
      nodeGroup[head_of(g, e)] = "small"
    } else if(node_weight < mediumThresh) {
      nodeGroup[head_of(g, e)] = "medium"
    } else {
      nodeGroup[head_of(g, e)] = "big"
    }
    
    nodeSize[head_of(g, e)] = node_weight/node_size_divisor
  }
}


# calculating the betweenness of nodes, can I use that later?

betAll <- igraph::betweenness(g, v = igraph::V(g), directed = FALSE) / (((igraph::vcount(g) - 1) * (igraph::vcount(g)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))


# getting the igraph node group values

# source: https://rdrr.io/cran/networkD3/man/igraph_to_networkD3.html

# convert to a suitable object for networkD3

gd3 <- igraph_to_networkD3(g)


# nodebetweenness to control node size
# source for nodebetweenness: https://gist.github.com/Vessy/d0228c983349cf138cefd0ced4098359

nodeList <- cbind(gd3$nodes, nodeSize, nodeGroup)

linkList <- gd3$links

rm(betAll, betAll.norm)

# create force undirected network plot

forceNetwork(Links = gd3$links, Nodes = nodeList,
             Source = 'source', Target = 'target', NodeID = 'name',
             #Value="value",
             Nodesize = "nodeSize",
             fontSize = 20,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             # linkDistance = JS("function(d){return 1/(d.value/1000);}"), requires further experimentation
             Group = 'nodeGroup', zoom = TRUE
             )



nodes <- data.frame()

