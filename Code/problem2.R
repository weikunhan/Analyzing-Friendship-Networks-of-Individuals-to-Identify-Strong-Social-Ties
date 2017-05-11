# EE232E Project 1 Problem 2
# Name: Weikun Han, Duzhi Chen
# Date: 5/8/2017
# Reference:
#  - https://google.github.io/styleguide/Rguide.xml#indentation
#  - http://snap.stanford.edu/data/egonets-Facebook.html

library("igraph")

# Setup the file path to load data
file_path <- "~/Documents/project1/facebook_combined.txt"

# Load the graph from the path
graph <- read.graph(file = file_path, format = "ncol", directed = FALSE)

# Create a graph that consists of node 1 and its neighbors and the edges that have
# both ends within this set of nodes
graph_node1 <- induced.subgraph(graph, c(1, neighbors(graph, 1)))
vertexcolor <- rep("blue", vcount(graph_node1))
vertexcolor[1] <- "red"

# Plot information
plot(graph_node1,
     vertex.size = 5,
     vertex.color = vertexcolor,
     vertex.label = NA)

# Print information
cat("-------------------------Processing Finshed 1----------------------------------\n",
    "Number of nodes this graph have: ", vcount(graph_node1), "\n",
    "Number of edges this graph have: ", ecount(graph_node1), "\n",
    "-------------------------------------------------------------------------------\n")
