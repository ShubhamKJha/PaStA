#!/usr/bin/env Rscript

smallThresh <- 7000
mediumThresh <- 14000

library("networkD3")
library("igraph")

file_name = file.choose()

data_csv = read.csv(file_name, header=TRUE, row.names = 1)

data_matrix <- as.matrix(data_csv)

# getting the igraph graph
g  <- graph.adjacency(data_matrix, weighted=TRUE,
                      diag=TRUE)



# get size of each section by finding edge with source=target

verticeWeights <- E(g).select(weight=50)

nodeSize <- array(1:(length(V(g))))

E(g)

for (e in E(g)){
  if(head_of(g, e) == tail_of(g, e)){
    nodeSize[head_of(g, e)] = E(g)$weight[e]
  }
}


# calculating the betweenness of nodes

betAll <- igraph::betweenness(g, v = igraph::V(g), directed = FALSE) / (((igraph::vcount(g) - 1) * (igraph::vcount(g)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))


# getting the igraph node group values

# source: https://rdrr.io/cran/networkD3/man/igraph_to_networkD3.html

wc <- cluster_walktrap(g)

members <- membership(wc)

# convert to a suitable object for networkD3

gd3 <- igraph_to_networkD3(g, group=members)


# nodebetweenness to control node size
# source for nodebetweenness: https://gist.github.com/Vessy/d0228c983349cf138cefd0ced4098359

nodeList <- cbind(gd3$nodes, nodeBetweenness=100*betAll.norm)

linkList <- gd3$links

rm(betAll, betAll.norm)

# create force undirected network plot

forceNetwork(Links = gd3$links, Nodes = nodeList,
             Source = 'source', Target = 'target', NodeID = 'name',
             #Value="value",
             Nodesize = "nodeBetweenness",
             fontSize = 20,
             Group = 'group', zoom = TRUE
             )



nodes <- data.frame()

