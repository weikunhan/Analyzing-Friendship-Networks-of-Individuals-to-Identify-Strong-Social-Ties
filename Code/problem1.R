# EE232E Project 1 Problem 1
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

# Check the graph connectivity
if(is.connected(graph)) {

    # Print information
    cat("-------------------------Processing Finshed 1----------------------------------\n",
        "Sucessfully load the graph from path.\n",
        "Connectiveity: Connected\n",
        "-------------------------------------------------------------------------------\n")
} else {

    # Print information
    cat("-------------------------Processing Finshed 1----------------------------------\n",
        "Sucessfully load the graph from path.\n",
        "Connectiveity: Not connected\n",
        "-------------------------------------------------------------------------------\n")
}

# Check the determeters for graph
diameter <- diameter(graph, directed = FALSE, unconnected = FALSE, weights = NULL)
path <- get.diameter(graph, directed = FALSE, unconnected = FALSE, weights = NULL)

# Print information
cat("-------------------------Processing Finshed 2----------------------------------\n",
    "Graph have the diameter is: ", diameter, "\n",
    "And a path with this diameter is: ", path, "\n",
    "-------------------------------------------------------------------------------\n")

# Use degree fucntion to find a numeric vector of the same length as argument v
degree <- degree(graph)

# Plot infomation
h <- hist(degree,
          breaks = seq(-0.5, by = 1 , length.out = max(degree) + 2),
          freq = FALSE,
          main = "Degree distribution for graph (social circles: Facebook)",
          ylab = "Fraction of nodes",
          xlab = "Degree")
plot(degree.distribution(graph),
     main = "Degree distribution for graph (social circles: Facebook)",
     ylab = "Fraction of nodes",
     xlab = "Degree")

# Calculate X and Y
x <- h$mids[1:max(degree) + 1]
y <- h$density[1:max(degree) + 1]

# Create model to fit a curve on it
model = nls(y ~ I(exp(1)^(a + b * x)), start = list(a = 0, b = 0))

# Plot information
plot(data.frame(x=h$mids, y=h$density),
     type = "o",
     main = "Degree distribution for graph (social circles: Facebook) fit a curve",
     ylab = "Fraction of nodes",
     xlab = "Degree")
x_axis = seq (from = 1, to = max(degree), by=1)
lines(x_axis, predict(model, list(x = x_axis)), col="red")

# Calculate the total mean squared error
MSE = sum(residuals(model)^2) / max(degree)

# Print information
print(summary(model))
cat("-------------------------Processing Finshed 3----------------------------------\n",
    "Curve’s total mean squared error is: ", MSE, "\n",
    "Average degree is: ", mean(degree), "\n",
    "-------------------------------------------------------------------------------\n")
