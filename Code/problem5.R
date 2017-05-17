# EE232E Project 1 Problem 5
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

# Initial veriables
core_index <- numeric(0)
core_degree <- numeric(0)
embeddedness_total <- numeric(0)
dispersion_total <- numeric(0)

# Use degree fucntion to find a numeric vector of the same length as argument v
degree <- degree(graph)
V(graph)$name <- V(graph)

# Find nodes in the graph that have more than 200 neighbors
for (i in 1:length(degree)) {
    if (length(neighbors(graph, i)) > 200) {
        core_index <- c(core_index, i)
        core_degree <- c(core_degree, length(neighbors(graph, i)))
    }
}

# Print information
cat("-------------------------Processing Finshed 1----------------------------------\n",
    "The number of core nodes (more than 200 neighbors) is: ", length(core_index), "\n",
    "The average degree of these core nodes is: ", mean(core_degree), "\n",
    "-------------------------------------------------------------------------------\n")

# Find every pair of the mutual friends a node shares with core node
for(i in 1:length(core_index)) {

    # For debug
    print(i)

    # Find neighbors for code node and graph for node i
    core_neighbors <- neighbors(graph, core_index[i])
    graph_nodei <- induced.subgraph(graph, c(core_index[i], core_neighbors))

    # Initial veriables
    embeddedness <- numeric(0)
    dispersion <- numeric(0)

    # Embeddedness and dispersion
    for(j in 1:length(core_neighbors)) {
        mutual_friends <- intersect(core_neighbors, neighbors(graph, core_neighbors[j]))
        embeddedness <- c(embeddedness, length(mutual_friends))
        core_name <- which(V(graph_nodei)$name == core_index[i])
        node_name <- which(V(graph_nodei)$name == core_neighbors[j])
        graph_remove_2nodes = delete.vertices(graph_nodei,c(core_name, node_name))
        dispersion = c(dispersion, distances(graph_remove_2nodes))
    }

    # Calculate totol embeddedness and dispersion
    embeddedness_total <- c(embeddedness_total, embeddedness)
    dispersion_total <- c(dispersion_total, dispersion)

    # Plot onw personal networks change i to plot difference one
    if (i == 1000) {
        max_dispersion <- which.max(dispersion)
        max_embeddedness <- which.max(embeddedness)
        max_dispersion_embeddedness <- which.max(dispersion * ( 1 / embeddedness))


        # Find community structure
        fc <- fastgreedy.community(graph_nodei)

        # Hightlight max dispresion / embeddedness color and size
        vertexcolor1 <- membership(fc) + 1
        vertexsize1 <- rep(5, length(vertexcolor1))
        vertexcolor1[max_dispersion_embeddedness] <- 0
        vertexsize1[max_dispersion_embeddedness] <- 10
        vertexcolor1[which(V(graph_nodei)$name == core_index[i])] <- 0
        vertexsize1[which(V(graph_nodei)$name == core_index[i])] <- 10

        # Disable color for nonrelate node
        E(graph_nodei)$color <- "grey"

        # Show line betweenness code node and max dispresion / embeddedness node
        E(graph_nodei,P = c(max_dispersion_embeddedness, which(V(graph_nodei)$name == core_index[i])))$color <- "red"
        E(graph_nodei,P = c(max_dispersion_embeddedness, which(V(graph_nodei)$name == core_index[i])))$width <- 10

        # Plot infomation
        plot(graph_nodei,
             vertex.color = vertexcolor1,
             vertex.label = NA,
             vertex.size = vertexsize1)

        # Hightlight max dispresion color and size
        vertexcolor2 <- membership(fc) + 1
        vertexsize2 <- rep(5, length(vertexcolor2))
        vertexcolor2[max_dispersion] <- 0
        vertexsize2[max_dispersion] <- 10
        vertexcolor2[which(V(graph_nodei)$name == core_index[i])] <- 0
        vertexsize2[which(V(graph_nodei)$name == core_index[i])] <- 10

        # Disable color for nonrelate node
        E(graph_nodei)$color <- "grey"

        # Show line betweenness code node and max dispresion / embeddedness node
        E(graph_nodei,P = c(max_dispersion, which(V(graph_nodei)$name == core_index[i])))$color <- "red"
        E(graph_nodei,P = c(max_dispersion, which(V(graph_nodei)$name == core_index[i])))$width <- 10

        # Plot infomation
        plot(graph_nodei,
             vertex.color = vertexcolor2,
             vertex.label = NA,
             vertex.size = vertexsize2)

        # Hightlight max embeddedness color and size
        vertexcolor3 <- membership(fc) + 1
        vertexsize3 <- rep(5, length(vertexcolor3))
        vertexcolor3[max_embeddedness] <- 0
        vertexsize3[max_embeddedness] <- 10
        vertexcolor3[which(V(graph_nodei)$name == core_index[i])] <- 0
        vertexsize3[which(V(graph_nodei)$name == core_index[i])] <- 10

        # Disable color for nonrelate node
        E(graph_nodei)$color <- "grey"

        # Show line betweenness code node and max dispresion / embeddedness node
        E(graph_nodei,P = c(max_embeddedness, which(V(graph_nodei)$name == core_index[i])))$color <- "red"
        E(graph_nodei,P = c(max_embeddedness, which(V(graph_nodei)$name == core_index[i])))$width <- 10

        # Plot infomation
        plot(graph_nodei,
             vertex.color = vertexcolor3,
             vertex.label = NA,
             vertex.size = vertexsize3)
    }
}
# Plot information
h <- hist(embeddedness_total,
          breaks = 50,
          freq = FALSE,
          main = "Degree distribution for embeddedness",
          ylab = "Frequency",
          xlab = "Embeddedness")
h <- hist(dispersion_total[which(dispersion_total != Inf)],
          breaks = 100,
          freq = FALSE,
          main = "Degree distribution for dispersion",
          ylab = "Frequency",
          xlab = "Dispersion")
