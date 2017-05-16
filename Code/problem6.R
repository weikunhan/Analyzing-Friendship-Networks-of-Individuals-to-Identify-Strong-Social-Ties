# EE232E Project 1 Problem 6
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
max_average_degree <- numeric(0)
max_clustering_coefficient <- numeric(0)
max_density <- numeric(0)
min_average_degree <- numeric(0)
min_clustering_coefficient <- numeric(0)
min_density <- numeric(0)
max_index_average_degree <- numeric(0)
max_index_clustering_coefficient <- numeric(0)
max_index_density <- numeric(0)
min_index_average_degree <- numeric(0)
min_index_clustering_coefficient <- numeric(0)
min_index_density <- numeric(0)

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
    "The number of core nodes (more than 200 neighbors) is: ", length(core_index), "\n",
    "The average degree of these core nodes is: ", mean(core_degree), "\n",
    "-------------------------------------------------------------------------------\n")

# Calculate each variable
for(i in 1:length(core_index)) {

    # Initial veriables
    community_number_10up <- numeric(0)
    average_degree <- numeric(0)
    global_clustering_coefficient <- numeric(0)
    density <- numeric(0)

    # Find neighbors for code node and graph for node i
    core_neighbors <- neighbors(graph, core_index[i])
    graph_nodei <- induced.subgraph(graph, c(core_index[i], core_neighbors))

    # Find community structure
    fc <- fastgreedy.community(graph_nodei)

    for(j in 1:length(fc)) {
        community_number <- V(graph_nodei)[which(membership(fc) == j)]
        if(length(community_number) > 10) {
            community_number_10up <- c(community_number_10up, j)
        }
    }

    for(j in 1:length(community_number_10up)) {
        graph_community <- induced.subgraph(graph_nodei, V(graph_nodei)[which(membership(fc) == community_number_10up[j])])
        average_degree <- c(average_degree, mean(degree(graph_community)) / vcount(graph_community))
        global_clustering_coefficient <- c(global_clustering_coefficient, transitivity(graph_community, type = "global"))
        density <- c(density, graph.density(graph_community))
    }

    # Calulate index and result
    max_index_average_degree <- c(max_index_average_degree, community_number_10up[which.max(average_degree)])
    max_average_degree <- c(max_average_degree, max(average_degree))
    max_index_clustering_coefficient <- c(max_index_clustering_coefficient, community_number_10up[which.max(global_clustering_coefficient)])
    max_clustering_coefficient <- c(max_clustering_coefficient, max(global_clustering_coefficient))
    max_index_density <- c(max_index_density, community_number_10up[which.max(density)])
    max_density <- c(max_density, max(density))
    min_index_average_degree <- c(min_index_average_degree, community_number_10up[which.min(average_degree)])
    min_average_degree <- c(min_average_degree, min(average_degree))
    min_index_clustering_coefficient <- c(min_index_clustering_coefficient, community_number_10up[which.min(global_clustering_coefficient)])
    min_clustering_coefficient <- c(min_clustering_coefficient, min(global_clustering_coefficient))
    min_index_density<-c(min_index_density, community_number_10up[which.min(density)])
    min_density <- c(min_density, min(density))
}

# Create the table for results
max_index_average_degree <- t(data.matrix(max_index_average_degree))
max_average_degree <- t(data.matrix(max_average_degree))
max_index_clustering_coefficient <- t(data.matrix(max_index_clustering_coefficient))
max_clustering_coefficient <- t(data.matrix(max_clustering_coefficient))
max_index_density <- t(data.matrix(max_index_density))
max_density <- t(data.matrix(max_density))
min_index_average_degree <- t(data.matrix(min_index_average_degree))
min_average_degree <- t(data.matrix(min_average_degree))
min_index_clustering_coefficient <- t(data.matrix(min_index_clustering_coefficient))
min_clustering_coefficient <- t(data.matrix(min_clustering_coefficient))
min_index_density <- t(data.matrix(min_index_density))
min_density <- t(data.matrix(min_density))

# Print infomation
print(max_index_average_degree)
print(max_average_degree)
cat("-------------------------Processing Finshed 2----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the maximum feature value.\n",
    "Secode table is max average dagree in this community.\n",
    "-------------------------------------------------------------------------------\n")
print(max_index_clustering_coefficient)
print(max_clustering_coefficient)
cat("-------------------------Processing Finshed 3----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the maximum feature value.\n",
    "Secode table is max clustering coefficient in this community.\n",
    "-------------------------------------------------------------------------------\n")
print(max_index_density)
print(max_density)
cat("-------------------------Processing Finshed 4----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the maximum feature value.\n",
    "Secode table is max density in this community.\n",
    "-------------------------------------------------------------------------------\n")
print(min_index_average_degree)
print(min_average_degree)
cat("-------------------------Processing Finshed 5----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the minimum feature value.\n",
    "Secode table is min average dagree in this community.\n",
    "-------------------------------------------------------------------------------\n")
print(min_index_clustering_coefficient)
print(min_clustering_coefficient)
cat("-------------------------Processing Finshed 6----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the minimum feature value.\n",
    "Secode table is min clustering coefficient in this community.\n",
    "-------------------------------------------------------------------------------\n")
print(min_index_density)
print(min_density)
cat("-------------------------Processing Finshed 7----------------------------------\n",
    "Total 40 code nodes done!\n",
    "First table is index of community which has the minimum feature value.\n",
    "Secode table is min density in this community.\n",
    "-------------------------------------------------------------------------------\n")
