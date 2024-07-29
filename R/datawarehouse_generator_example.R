# Install and load networkD3 if you haven't already
install.packages("networkD3")
library(networkD3)

# Generate data
generate_data <- function(dataset_names, number_of_tables, number_of_relationships) {
  # Helper function to generate a random 7-letter string
  random_string <- function() {
    paste(sample(c(letters, LETTERS),
                 7,
                 replace = TRUE), 
          collapse = "")
  }
  
  # Helper function to generate table names
  generate_table_names <- function(dataset_names, number_of_tables) {
    table_names <- character(number_of_tables)
    for (i in 1:number_of_tables) {
      dataset <- sample(dataset_names, 1)
      table_names[i] <- paste("project_demo",
                              dataset,
                              paste0("table_",
                                     random_string()),
                              sep = ".")
    }
    return(table_names)
  }
  
  # Generate the list of table names
  table_names <- generate_table_names(dataset_names, number_of_tables)
  
  # Generate relationships
  relationships <- data.frame(
    table_name = character(number_of_relationships),
    downstream_table = character(number_of_relationships),
    upstream_table = character(number_of_relationships),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:number_of_relationships) {
    table_name <- sample(table_names, 1)
    downstream_table <- sample(table_names[table_names != table_name], 1)
    upstream_table <- sample(table_names[table_names != table_name], 1)
    
    relationships[i, ] <- c(table_name, downstream_table, upstream_table)
  }
  
  return(relationships)
}

# Parameters
number_of_tables <- 10
min_relationships <- 2 * number_of_tables  # For example, 1.5 times the number of tables

df <- generate_data(dataset_names, number_of_tables, min_relationships)
print(df)

# Convert relationships to networkD3 format
nodes <- data.frame(name = unique(c(df$downstream_table, df$upstream_table)))
links <- data.frame(
  source = match(df$downstream_table, nodes$name) - 1,
  target = match(df$upstream_table, nodes$name) - 1
)

library(networkD3)

# Create the interactive network graph
forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "name",
  opacity = 0.8,
  zoom = TRUE,
  legend = TRUE,
  arrows = TRUE,
  bounded = TRUE
)

library(igraph)

# Convert the relationships data frame into an edge list for igraph
edges <- data.frame(
  from = df$downstream_table,
  to = df$upstream_table,
  stringsAsFactors = FALSE
)

# Create a directed graph from the edge list
g <- graph_from_data_frame(edges, directed = TRUE)

# Calculate the in-degrees (number of incoming edges) for each vertex
in_degrees <- degree(g, mode = "in")

# Define vertex sizes based on in-degrees
vertex_sizes <- 10 + 5 * in_degrees  # Adjust the multiplier as needed for better visualization

# Plot the graph
plot(g, 
     vertex.label = V(g)$name, 
     vertex.size = vertex_sizes, 
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     edge.arrow.size = 0.5,
     main = "Table Relationships")

# Identify the most referenced table
most_referenced_table <- names(sort(in_degrees, decreasing = TRUE))[1]

# Print the most referenced table
print(paste("The most referenced table is:", most_referenced_table))