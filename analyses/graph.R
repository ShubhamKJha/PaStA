#!/usr/bin/env Rscript


g <- make_ring(10) %>%
  set_vertex_attr("name", value = LETTERS[1:10])
g
plot(g)
V(g)

g2 <- delete_vertices(g, c(1,5)) %>%
  delete_vertices("B")
g2
plot(g2)
V(g2)

smallThresh <- 10000000
mediumThresh <- 7000

# the actual size of nodes is too big with their actual weight
# dividing it by this value makes it easier to look at
node_size_divisor <- 10

library("networkD3")
library("igraph")

file_name = file.choose()

data_csv = read.csv(file_name, header=TRUE)

data_matrix <- as.matrix(data_csv)

data_matrix

data_frame <- data.frame(data_matrix)

data_frame

# getting the igraph graph
g  <- graph_from_data_frame(data_frame, directed=FALSE)
g <- set_edge_attr(g, "weight", value=as.numeric(data_csv$weight))

plot(g)

# get size of each section by finding edge with target or source = THE REST

# verticeWeights <- E(g).select(weight=50)

E(g)

nodeSize <- array(1:(length(V(g)) - 1))
nodeGroup <- array(1:(length(V(g)) - 1))
linkValue <- array(1:(length(V(g)) - 1))



rest_id <- as.numeric(which(V(g)$name == "THE REST"))

assign_node_group <- function(index, g, e){
  node_weight <- as.numeric(E(g)$weight[e])
  print("node_weight:")
  print(node_weight)
  
  if(node_weight < smallThresh){
    print("it's small")
    nodeGroup[index] = "small"
    } else if(node_weight < mediumThresh) {
      
      print("it's medium")
      nodeGroup[index] = "medium"
    } else {
      print("it's big")
        nodeGroup[index] = "big"
    }
}



E(g)

for (e in E(g)){
  print(e)
  if(head_of(g, e) == rest_id) {
    print("made it in")
    assign_node_group(tail_of(g, e), g, e)
  } else if (tail_of(g, e) == rest_id) {
    print("made it in, too")
    assign_node_group(head_of(g, e), g, e)
  }
}

as_edgelist(g)

g<-delete.edges(g,which(as_edgelist(g)=="THE REST",arr.ind=TRUE)[,1])

as_edgelist(g)

plot(g)

g <- delete.vertices(g, "THE REST")

V(g)



# source: https://rdrr.io/cran/networkD3/man/igraph_to_networkD3.html

# convert to a suitable object for networkD3

gd3 <- igraph_to_networkD3(g)


# create my own node list with my assigned values added
nodeList <- cbind(gd3$nodes, nodeSize, nodeGroup)

# creating the link list. Further information to be appended?
linkList <- gd3$links

# create force undirected network plot
forceNetwork(Links = gd3$links, Nodes = nodeList,
             Source = 'source', Target = 'target', NodeID = 'name',
             Value="value",
             Nodesize = "nodeSize",
             fontSize = 20,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             linkDistance = JS("function(d){return 100000/d.value;}"),
             #linkWidth = JS("function(d){return d.value/100000}"),
             linkWidth = 5,
             Group = 'nodeGroup', zoom = TRUE
             ) %>%

saveNetwork(file = "forcedNet.html")

forceNetwork(Links = gd3$links, Nodes = nodeList,
             Source = 'source', Target = 'target', NodeID = 'name',
             Value="value",
             Nodesize = "nodeSize",
             fontSize = 20,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             linkDistance = JS("function(d){return 100000/d.value;}"),
             #linkWidth = JS("function(d){return d.value/100000}"),
             linkWidth = 5,
             Group = 'nodeGroup', zoom = TRUE
)



##########betweenness of nodes -> not used for now, but interesting?


# nodebetweenness to control node size
# source for nodebetweenness: https://gist.github.com/Vessy/d0228c983349cf138cefd0ced4098359

# calculating the betweenness of nodes, can I use that later?

betAll <- igraph::betweenness(g, v = igraph::V(g), directed = FALSE) / (((igraph::vcount(g) - 1) * (igraph::vcount(g)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))




rm(betAll, betAll.norm)

