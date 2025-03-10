# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Here we have the set of functions that define a problem
# (Let's take a look to the functions)
source("../problem/CanibalesYMisioneros-problem.R")

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Solve the problem using different algorithms
solve.problem <- function(problem) {
  
  # Let's see the difference between distinct algorithms
  #bfs_ts <- breadth.first.search(problem, max_iterations = 2000, count_print = 50)   
  bfs_gs <- breadth.first.search(problem, max_iterations = 2000, count_print = 50, graph_search = TRUE)
  #dfs_ts <- depth.first.search(problem, max_iterations = 2000, count_print = 50)
  dfs_gs <- depth.first.search(problem, max_iterations = 2000, count_print = 50, graph_search = TRUE)
  #dls_ts <- depth.limited.search(problem, max_iterations = 2000, count_print = 50, depth_limit = 20)
  dls_gs <- depth.limited.search(problem, max_iterations = 2000, count_print = 50, depth_limit = 20, graph_search = TRUE)
  #ids_ts <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 50)
  ids_gs <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 50, graph_search = TRUE)
  
  # Analyze the result of all the executions
  results <- analyze.results(list(bfs_gs,dfs_gs,dls_gs,ids_gs), problem)
  # Print results in an HTML Table
  kable_material(kbl(results, caption = "Misioneros y Canibales"), c("striped", "hover", "condensed", "responsive"))
}

# Create different problem instances

problem <- initialize.problem(3, 3, 2)
# Solve each problem
solve.problem(problem)

problem <- initialize.problem(5, 5, 3)
# Solve each problem
solve.problem(problem)

problem <- initialize.problem(10, 10, 4)
# Solve each problem
solve.problem(problem)