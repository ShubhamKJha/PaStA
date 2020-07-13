#!/usr/bin/env Rscript

library(igraph)

args <- commandArgs(trailingOnly = True)

file_name <- args[1]
csv_data <- read.csv(file_name, header=TURE, row.names=1)
data_matrix <- as.matrix(csv_data)

data_network <- graph.adjacency(data_matrix, mode="undirected", diag=FALSE)
# print it
data_network

plot(data_network)
