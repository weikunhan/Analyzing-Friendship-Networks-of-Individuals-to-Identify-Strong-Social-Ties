# EE232E Project 1 Problem 7
# Name: Weikun Han, Duzhi Chen
# Date: 5/10/2017
# Reference:
#  - https://google.github.io/styleguide/Rguide.xml#indentation
#  - http://snap.stanford.edu/data/egonets-Gplus.html

library("igraph")

# Setup the file path to load data
file_path <- "~/Documents/project1/gplus/"

# In file_path directory, using UNIX shell to run this line to generate the ego node ID:
# >>ls | grep .edges | awk 'BEGIN{FS="."}{print $1}'| sort >> ../gplus_ID.txt
file_id <- read.table("~/Documents/project1/gplus_ID.txt", numerals = "no.loss")

# Initial veriables
circle_count <- numeric(0)

# In file_path directory, run UNIX shell script circlescount.sh to run to generate .circlescount file:
# >>./circlescount.sh
for (i in 1:length(file_id$V1)) {
    ego_node_id <- file_id$V1[i]
    circles_file_list <- paste(file_path, ego_node_id, ".circles", sep = "")
    circlescount_file_list <- paste(file_path, ego_node_id, ".circlescount", sep="")
    number_circle <- read.table(circlescount_file_list, numerals = "no.loss")
    circle_count <- c(circle_count, number_circle$V1[1])
}

# Create the table for results
circle_count <- t(circle_count)

# Print information
print(circle_count)
cat("-------------------------Processing Finshed 1----------------------------------\n",
    "Total number ego node done is: ", length(file_id$V1), "\n",
    "The table is show index of ego node with number of circle in each ego node.\n",
    "-------------------------------------------------------------------------------\n")

# From the result, we want selet number of circle is more than 2
# Here, I selet ego node index is 16 and 24
#index <- 16
index <- 12

# Begin program
ego_node_id <- file_id$V1[index]
edge_file_list <- paste(file_path , ego_node_id  , ".edges", sep="")
circles_file_list <- paste(file_path, ego_node_id, ".circles", sep = "")
circlescount_file_list <- paste(file_path, ego_node_id, ".circlescount", sep="")
number_circle <- read.table(circlescount_file_list, numerals = "no.loss")

# Load the graph from edge file list and create graph add ego node id
graph1 <- read.graph(edge_file_list, format = "ncol", directed = TRUE)
graph2 <- add.vertices(graph1, 1, name = ego_node_id)
for (i in 1:length(V(graph1))) {
    graph2 <- add.edges(graph2, c(vcount(graph2), i))
}

# Open circles file list to input
fp <- file(circles_file_list, open = "r")
content <- readLines(fp)
close(fp)
circle <- list()
for (i in 1:length(content)) {
    circle[i] <- strsplit(content[i], "\t")
 }

# Find community structure
wc <- walktrap.community(graph2)
ic <- infomap.community(graph2)

# Change condiion to select use difference community structure
# FALSE for Walktrap algorithm, TRUE for Infomap algorithm
condition <- FALSE

# Main
if (condition) {

    # Find percentage in each community
    for (i in 1:max(membership(ic))) {

        # Initial veriables
        select <- numeric(0)
        percentage <- numeric(0)

        # Find percentage in each circle
        for (j in 1:length(membership(ic))) {
            if (membership(ic)[j] == i) {
                select <- c(select, (ic$name[j]))
            }
        }
        for (j in 1:number_circle$V1[1]) {
            intersection <- intersect(select, circle[[j]])
            temp <- length(intersection) / length(select)
            percentage <- c(percentage, temp)
        }

        # Create the table for results
        percentage <- t(percentage)

        # Print information
        print(percentage)
        cat("-------------------------Processing Finshed", i, "----------------------------------\n",
            "The index of ego node is: ", index, ".\n",
            "The index of community is: ", i, "\n",
            "The table have index shows the circle ID for ego node.\n",
            "And and reslut in table shows percentage of overlap using infomap algorithm.\n",
            "-------------------------------------------------------------------------------\n")
    }
} else {

    # Find percentage in each community
    for (i in 1:max(membership(wc))) {

        # Initial veriables
        select <- numeric(0)
        percentage <- numeric(0)

        # Find percentage in each circle
        for (j in 1:length(membership(wc))) {
            if (membership(wc)[j] == i) {
                select <- c(select, (ic$name[j]))
            }
        }
        for (j in 1:number_circle$V1[1]) {
            intersection <- intersect(select, circle[[j]])
            temp <- length(intersection) / length(select)
            percentage <- c(percentage, temp)
        }

        # Create the table for results
        percentage <- t(percentage)

        # Print information
        print(percentage)
        cat("-------------------------Processing Finshed", i, "----------------------------------\n",
            "The index of ego node is: ", index, "\n",
            "The index of community is: ", i, "\n",
            "The table have index shows the circle ID for ego node.\n",
            "And and reslut in table shows percentage of overlap using Walktrap algorithm.\n",
            "-------------------------------------------------------------------------------\n")
    }
}
