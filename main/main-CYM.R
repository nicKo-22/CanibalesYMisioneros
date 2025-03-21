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

source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Solve the problem using different algorithms
solve.problem <- function(problem) {
  
  # Let's see the difference between distinct algorithms
  bfs_gs <- breadth.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  dfs_gs <- depth.first.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  dls_gs <- depth.limited.search(problem, max_iterations = 2000, count_print = 1000, depth_limit = 20, graph_search = TRUE)
  ids_gs <- iterative.deepening.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  
  ucs_ts   <- uniform.cost.search(problem, max_iterations = 2500, count_print = 1000)
  ucs_gs  <- uniform.cost.search(problem, max_iterations = 2000, count_print = 1000, graph_search = TRUE)
  gbfs_ts  <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000)
  gbfs_gs <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  a_star_ts <- a.star.search(problem, max_iterations = 2500, count_print = 1000)
  a_star_gs<- a.star.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
  
  # Analyze the result of all the executions
  results <- analyze.results(list(bfs_gs,
                                  dfs_gs,
                                  dls_gs,
                                  ids_gs,
                                  ucs_ts,
                                  ucs_gs,
                                  gbfs_ts,
                                  gbfs_gs, 
                                  a_star_ts, 
                                  a_star_gs), problem)
  # Print results in an HTML Table
  kable_material(kbl(results, caption = "Misioneros y Canibales"), c("striped", "hover", "condensed", "responsive"))
}

# Create different problem instances

problem <- initialize.problem(3, 3, 2)
# Solve each problem
solve.problem(problem)

problem <- initialize.problem(5, 5, 3)
#  Solve each problem
solve.problem(problem)
# 
problem <- initialize.problem(10, 10, 4)
#  Solve each problem
solve.problem(problem)