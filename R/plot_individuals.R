#' Observations, individual predictions and population predictions plotted against 
#' the independent variable for every individual
#'
#' @description Observations (DV), individual predictions (IPRED) and population predictions (PRED) plotted against 
#' the independent variable for every individual
#' 
#' @inheritParams dv_vs_pred
#' @param ncol Number of columns per page.
#' @param nrow Number of rows per page.
#' @param page Page to draw.
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' # Basic example
#' ind_plots(xpdb_ex_pk)
#' 
#' # Example with mapping and facetting
#' ind_plots(xpdb_ex_pk, aes(x = TAD), 
#'           facets = OCC~ID, 
#'           panel_labeller = 'label_both')
#' 
#' @export
ind_plots <- function(xpdb,
                      mapping  = NULL,
                      group    = 'variable',
                      type     = 'lp',
                      facets   = NULL,
                      title    = 'Individual plots | @run',
                      subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                      caption  = '@dir',
                      log      = NULL,
                      nrow     = 3,
                      ncol     = 3,
                      page     = 1,
                      problem,
                      quiet,
                      ...) {
  # Check input
  check_xpdb(xpdb, check = 'data')
  if (missing(problem)) problem <- default_plot_problem(xpdb)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- xp_var(xpdb, problem, type = 'id')$col
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(problem = problem, source = 'data',
                               filter = only_obs(xpdb, problem, quiet),
                               tidy = TRUE,
                               value_col = xp_var(xpdb, problem, 
                                                  type = c('dv', 'pred', 'ipred'))$col),
                mapping = aes_c(aes_string(x = xp_var(xpdb, problem, type = 'idv')$col, 
                                           y = 'value', line_color = 'variable', 
                                           line_linetype = 'variable', point_color = 'variable', 
                                           point_alpha = 'variable'), mapping),
                type = type, panel_facets = facets,
                xscale = check_scales('x', log),
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]), 
                panel_nrow = nrow, panel_ncol = ncol, ...) +
    scale_alpha_manual(values = c(0.8, 0, 0)) +
    scale_color_manual(values = c('grey60', 'deepskyblue3', 'deepskyblue3')) +
    scale_linetype_manual(values = c('dashed', 'solid', '52'))
}
