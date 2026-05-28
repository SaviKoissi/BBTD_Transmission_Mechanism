#============================================
# Phase 1: Node Analysis and connectivity 
#============================================

inspect_network_structure <- function(phase1_obj) {
  # 1. Convert the raw adjacency matrix to an igraph object
  # (Assumes directed if trade flows/vectors matter, change to 'undirected' if symmetric)
  g <- graph_from_adjacency_matrix(phase1_obj$adjacency_matrix, 
                                   mode = "directed", 
                                   weighted = TRUE)
  
  # 2. Calculate fundamental network centrality metrics
  net_stats <- data.frame(
    Node_ID    = 1:vcount(g),
    In_Degree  = degree(g, mode = "in"),
    Out_Degree = degree(g, mode = "out"),
    Betweenness = betweenness(g, directed = TRUE),
    Eigenvector = eigen_centrality(g, directed = TRUE)$vector
  )
  
  # Add spatial coordinates for mapping later
  if(!is.null(phase1_obj$coords)) {
    net_stats$X <- phase1_obj$coords[,1]
    net_stats$Y <- phase1_obj$coords[,2]
  }
  
  # 3. Print Top 5 Choke Point Nodes
  cat("==================================================\n")
  cat("         TOP 5 NETWORK SUPER-SPREADER NODES       \n")
  cat("==================================================\n")
  print(net_stats %>% arrange(desc(Eigenvector)) %>% head(5))
  cat("==================================================\n")
  
  # 4. Plot the Degree Distribution to prove it is Non-Random
  p <- ggplot(net_stats, aes(x = In_Degree + Out_Degree)) +
    geom_histogram(bins = 30, fill = "#1f77b4", color = "white", alpha = 0.8) +
    labs(title = "Network Degree Distribution",
         subtitle = "A long tail proves a non-random, scale-free architecture",
         x = "Total Degree Connectivity", 
         y = "Node Count") +
    theme_minimal()
  
  print(p)
  return(net_stats)
}

# Run the inspection
network_metrics <- inspect_network_structure(phase1)

