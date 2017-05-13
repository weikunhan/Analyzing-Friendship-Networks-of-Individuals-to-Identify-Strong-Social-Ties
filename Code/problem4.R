# EE232E Project 1 Problem 4
# Name: Weikun Han, Duzhi Chen
# Date: 5/10/2017
# Reference:
#  - https://google.github.io/styleguide/Rguide.xml#indentation
#  - http://snap.stanford.edu/data/egonets-Facebook.html

library("igraph")

# Setup the file path to load data
file_path <- "~/Documents/project1/facebook_combined.txt"

# Load the graph from the path
graph <- read.graph(file = file_path, format = "ncol", directed = FALSE)

# Create a graph that only consists of node 1 neighbors and the edges that have
# both ends within this set of nodes
g_remove_node1 = induced.subgraph(graph, neighbors(graph, 1))

# Use fast greedy method to find the community structure
fc <- fastgreedy.community(g_remove_node1)
vertexcolor1 <- membership(fc) + 1

# Plot information
plot(g_remove_node1,
     vertex.color = vertexcolor1,
     vertex.label = NA,
     vertex.size = 5)

# Base on edge betweenness to find the community structure
ebc <- edge.betweenness.community(g_remove_node1)
vertexcolor2 <- membership(ebc) + 1

# Plot information
plot(g_remove_node1,
     vertex.color = vertexcolor2,
     vertex.label = NA,
     vertex.size = 5)


# Use Infomap method to find the community structure
ic <- infomap.community(g_remove_node1)
vertexcolor3 <- membership(ic) + 1

# Plot information
plot(g_remove_node1,
     vertex.color = vertexcolor3,
     vertex.label = NA,
     vertex.size = 5)
