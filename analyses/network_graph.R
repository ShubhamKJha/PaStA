#!/usr/bin/env Rscript

#ATTENTION: Code copied and pasted from graph.R, fix that

node_size_divisor <- 100
keywords <- c("NETWORKING", "MEMORY", "DRIVERS", "ARM32")

#install.packages("ColorBrewer")
#install.packages("intergraph")
#install.packages("ggplot2")

library("igraph")
library("network")
library("sna")
library("GGally")
library("qgraph")
library("visNetwork")

file_name = file.choose()

data_csv = read.csv(file_name, header=TRUE)

data_matrix <- as.matrix(data_csv)

data_frame <- data.frame(data_matrix)
data_frame$weight <- as.numeric(data_frame$weight)

g  <- graph_from_data_frame(data_frame, directed=FALSE)

g <- igraph::delete.edges(g,which(as_edgelist(g)=="THE REST",arr.ind=TRUE)[,1])

adjacency <- get.adjacency(g)


# removing this because it's trivial that the rest includes everything
g <- igraph::delete.vertices(g, "THE REST")

clust  <- cluster_walktrap(g)

assign_vertex_group <- function(g){
  for(v in V(g)){
    print("processing vertex:")
    print(vertex_attr(g, "name", v))
    set_vertex_attr(g, "group", v, "UNKNOWN")
    for(k in keywords){
      if(grepl(k, vertex_attr(g, "name", v))){
        print("assigning keyword:")
        print(k)
        set_vertex_attr(g, "group", v, k)
      }
    }
  }
}

assign_vertex_group(g)

edge_quantiles <- quantile(E(g)$weight)

quantile(E(g)$weight)


#l <- qgraph.layout.fruchtermanreingold(adjacency,vcount=vcount(g))
#                                       niter=1000,
#                                       repulse.rad = vcount(g)^2)


l <- layout.reingold.tilford(g, circular=TRUE)

l <- layout.kamada.kawai(g)

plot.igraph(g,
            mark.groups=groups(clust),
            #mark.groups = V(g)$group,
            vertex.size=2,
            vertex.label.dist=1, vertex.label.cex=0.35,
            layout=l
            #layout=layout_with_mds
            )

legend("topleft",bty = "n",
       legend=levels(),
       fill=pal, border=NA)

#plot(g)

visIgraphLayout(visIgraph(g), layout=layout_with_mds)

visIgraph(g) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

################### TODO: use later for node size calculations ##########
#ggnet2(g, label=True)

# get the size of each section by finding edge with target or source = THE REST

nodeSize <- array(1:(length(V(g)) - 1))
nodeGroup <- array(1:(length(V(g)) - 1))
linkValue <- array(1:(length(V(g)) - 1))

rest_id <- as.numeric(which(V(g)$name == "THE REST"))

assign_node_group <- function(index, g, e){
  node_weight <- as.numeric(E(g)$weight[e])
  nodeSize[index] <- node_weight/node_size_divisor
}

for (e in E(g)){
  if(head_of(g, e) == rest_id) {
    assign_node_group(tail_of(g, e), g, e)
  } else if (tail_of(g, e) == rest_id) {
    assign_node_group(head_of(g, e), g, e)
  }
}

g <- igraph::delete.edges(g,which(as_edgelist(g)=="THE REST",arr.ind=TRUE)[,1])


# removing this because it's trivial that the rest includes everything
g <- igraph::delete.vertices(g, "THE REST")