# EE 219 Project 1 Problem 3
# Name: Weikun Han, Duzhi Chen
# Date: 5/9/2017
# Reference:
#  - https://google.github.io/styleguide/Rguide.xml#indentation
#  - http://snap.stanford.edu/data/egonets-Facebook.html

library("igraph")

# Setup the file path to load data
file_path <- "~/Documents/project1/facebook_combined.txt"

# Load the graph from the path
graph <- read.graph(file = file_path, format = "ncol", directed = FALSE)

# Initial veriables
core_index <- numeric(0)
core_degree <- numeric(0)

# Use degree fucntion to find a numeric vector of the same length as argument v
degree <- degree(graph)

# Find nodes in the graph that have more than 200 neighbors
for (i in 1:length(degree)) {
    if (length(neighbors(graph, i)) > 200) {
        core_index <- c(core_index, i)
        core_degree <- c(core_degree, length(neighbors(graph, i)))
    }
}

# Print information
cat("-------------------------Processing Finshed 1----------------------------------\n",
    "The average degree of these core nodes: ", mean(core_degree), "\n",
    "-------------------------------------------------------------------------------\n")

# Create a graph that consists of node 1 and its neighbors and the edges that have
# both ends within this set of nodes
graph_node1 <- induced.subgraph(graph, c(1, neighbors(graph, 1)))

# Use fast greedy method to find the community structure
fc <- fastgreedy.community(graph_node1)
vertexcolor1 <- membership(fc) + 1

# Plot information
plot(graph_node1,
     vertex.color = vertexcolor1,
     vertex.label = NA,
     vertex.size = 5)

# Base on edge betweenness to find the community structure
ebc <- edge.betweenness.community(graph_node1)
vertexcolor2 <- membership(ebc) + 1

# Plot information
plot(graph_node1,
     vertex.color = vertexcolor2,
     vertex.label = NA,
     vertex.size = 5)


# Use Infomap method to find the community structure
ic <- infomap.community(graph_node1)
vertexcolor3 <- membership(ic) + 1

# Plot information
plot(graph_node1,
     vertex.color = vertexcolor3,
     vertex.label = NA,
     vertex.size = 5)
