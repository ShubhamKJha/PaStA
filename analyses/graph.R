#!/usr/bin/env Rscript

#args <- commandArgs(trailingOnly = True)

#file_name <- args[1]
#install.packages("igraph")
#install.packages("networkD3")
library("networkD3")
library("igraph")


file_name = file.choose()
csv_data <- read.csv(file_name, header=TRUE, row.names=1)
data_matrix <- as.matrix(csv_data)

data_graph <- graph.adjacency(data_matrix, mode="undirected"
                          , diag=FALSE, weighted=TRUE)

# source: http://www.vesnam.com/Rblog/viznets6/

# create a data frame obj, disguised as a node list obj
nodeList <- data.frame(ID = c(0:(igraph::vcount(data_graph)-1)),
                       nName = igraph::V(data_graph)$name)

# map node names from edge list to node IDs

getNodeID <- function(x){
  which(x == igraph::V(data_graph)$name) - 1
}

edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))

edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                        function(x) data.frame(F1(x)))



plot(data_graph, edge.label=E(data_graph)$weight,
     edge.label.cex=0.75)

D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                         Nodes = nodeList, # data frame that contains info about nodes
                                         Source = "SourceID", # ID of source node 
                                         Target = "TargetID", # ID of target node
                                         Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                         NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                         height = 500, # Size of the plot (vertical)
                                         width = 1000,  # Size of the plot (horizontal)
                                         fontSize = 20, # Font size
                                         linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         opacity = 0.85, # opacity
                                         zoom = TRUE, # ability to zoom when click on the node
                                         opacityNoHover = 0.1, # opacity of labels when static
                                         linkColour = edges_col) # edge colors


################################## for igraph
# data_matrix <- as.matrix(csv_data)

# data_graph <- graph.adjacency(data_matrix, mode="undirected"
#                            , diag=FALSE, weighted=TRUE)

# plot(data_graph, edge.label=E(data_graph)$weight)

################################### for igraph end





