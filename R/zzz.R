# Suppress R CMD check notes for NSE variables used in dplyr/sf operations
utils::globalVariables(c(
  "kind",
  "child_id", "parent_id", "transition_year",
  "end", "from", "geom_id", "geom_id2",
  "boundaries", "relations", "hierarchy", "parish_meta", "parish_link", "parish_codes",
  "parish_registry", "unit_variants",
  "geomid", "hist_town",
  "name", "parent_geom_id", "child_geom_id", "child_type", "parent_name",
  "parent_ref_code", "p_name", "top_parent_id",
  "start", "tom", "type", "type_id",
  "dist", "dist_norm", "priority",
  # unit_history
  "n_from", "max_siblings", "from_ids", "to_ids", "n_children", "event",
  # plot_hierarchy
  "x", "y", "x1s", "y1s", "x2s", "y2s", "label", "geom_ids", "relation", "source"
))

#' @importFrom utils data
#' @importFrom stats setNames
NULL
