#' Create a tidygraph object
#'
#' Create a tidygraph object from node and edge data. There should be one row
#' for each unique combination of node and edge.
#'
#' @param data data.frame with columns to be used as nodes and edges
#' @param node column name to use as nodes
#' @param edge column name to use as edges
#' @param groups list of column names to use as grouping variables
#' @param directed logical; if `TRUE`, graph edges are directed
#' @param counts logical; if `TRUE`, the edge object is transformed to counts of
#'   edges between pairs of nodes within each `group`
#' @param loops logical; if `TRUE`, nodes with edges to themselves are drawn,
#'   otherwise they are removed
#' @return an object of class "tbl_graph" from `tidygraph`
create_tbl_graph <- function(data, node, edge, groups = NULL,
                             directed = FALSE, counts = FALSE, loops = TRUE) {
  # Distinct node ids and labels
  nodes <- data %>%
    dplyr::distinct(label = !!rlang::sym(node)) %>%
    tibble::rowid_to_column("id")

  # All pairwise nodes found within group and edge
  edges <- data %>%
    dplyr::group_by_at(dplyr::vars(!!!groups, !!edge)) %>%
    dplyr::summarise(
      edge = !!rlang::sym(node) %>%
        match(nodes[["label"]]) %>% {
          if (length(.) == 1) rep(., 2) else .
        } %>%
        combn(2) %>%
        purrr::array_branch(margin = 1) %>%
        purrr::set_names(c("from", "to")) %>%
        tibble::as_tibble() %>%
        list()
    ) %>%
    tidyr::unnest()

  # Count number of edges between every pair of nodes, within each group
  if (counts) {
    edges <- dplyr::count(edges, from, to)
  }

  # Create tidygraph object
  graph <- tidygraph::tbl_graph(nodes = nodes,
                                edges = edges,
                                directed = directed)

  # Remove loops from graph object
  if (loops) graph else remove_loops(graph)
}

#' Remove loops from graph
#'
#' Remove loops and nodes with no edges (zero-degree nodes) from graph
#'
#' @param graph a graph object of class "tbl_graph"
#' @return a graph with its loops removed from the edge data and zero-degree
#'   nodes removed from the node data
remove_loops <- function(graph) {
  graph %>%
    tidygraph::activate(edges) %>%
    dplyr::filter(!igraph::which_loop(.)) %>%
    tidygraph::activate(nodes) %>%
    dplyr::filter(tidygraph::centrality_degree() > 0)
}

#' Create ggplot object with graphing layers
#'
#' Create ggplot object with graphing layers to modify node and edge attributes
#' for plotting from a tidygraph object.
#'
#' @param graph an object of class "tbl_graph"
#' @param layout layout of graph
#' @param color column name to map to color aesethetic
#' @param linetype column name to map to linetype aesthetic
#' @param alpha level of transparency
#' @param title plot title
#' @param facet column name for variable to split edges on
#' @return an object of class `ggplot` with the graph plot
create_ggraph <- function(graph, layout = "kk", color = NULL, linetype = NULL,
                          alpha = 0.5, title = NULL, facet = NULL) {
  # Layers that are always present
  base_layers <- list(
    ggraph::geom_node_point(),
    ggraph::geom_node_text(ggplot2::aes(label = label), size = 2, repel = TRUE),
    ggraph::scale_edge_width(range = c(0.2, 2)),
    ggplot2::labs(title = title),
    ggraph::theme_graph(base_family = "sans")
  )
  # If "n" is in the edge data column names we know it contains counts
  edge_names <- graph %>%
    tidygraph::activate(edges) %>%
    tibble::as_tibble() %>%
    names()
  # Use thickness of links and loops to show number of edges, or
  # use a fan to show all edges
  if ("n" %in% edge_names) {
    edge_layer <- list(
      ggraph::geom_edge_link(
        ggplot2::aes_string(color = color, linetype = linetype, width = "n"),
        alpha = alpha
      ),
      ggraph::geom_edge_loop(
        ggplot2::aes_string(color = color, linetype = linetype, width = "n"),
        alpha = alpha
      )
    )
  } else {
    edge_layer <- list(
      ggraph::geom_edge_fan(
        ggplot2::aes_string(color = color, linetype = linetype),
        alpha = alpha
      )
    )
  }
  # Combine edge layers before base layers so node text is not covered
  gg_layers <- purrr::splice(edge_layer, base_layers)
  # Facetting variable
  if (!is.null(facet)) {
    gg_layers <- gg_layers %>%
      purrr::splice(ggraph::facet_edges(ggplot2::vars(!!rlang::sym(facet))))
  }
  # Return ggplot object
  ggraph::ggraph(graph, layout = layout) + gg_layers
}
