#' Parameter value or gradient vs. iterations
#'
#' @description Change of parameter value or gradient vs. iterations.
#'
#' @inheritParams dv_vs_pred
#' @param subprob The sub-problem number to be used. By default returns 
#' the last sub-problem associated with the selected problem.
#' 
#' @inheritSection xplot_scatter Layers mapping
#' @inheritSection xplot_scatter Template titles
#' @seealso \code{\link{xplot_scatter}}
#' @examples
#' prm_vs_iteration(xpdb_ex_pk)
#' 
#' grd_vs_iteration(xpdb_ex_pk)
#' 
#' @name minimization_plots
#' @export
prm_vs_iteration <- function(xpdb,
                             mapping  = NULL,
                             group    = 'variable',
                             type     = 'pl',
                             facets   = NULL,
                             title    = 'Parameter @y vs. @x | @run',
                             subtitle = 'Method: @method, minimization time: @runtime\nTermination message: @term',
                             caption  = '@dir',
                             log      = NULL,
                             guides   = FALSE,
                             problem,
                             subprob,
                             quiet,
                             ...) {
  # Check input
  check_xpdb(xpdb, check = 'files')
  if (missing(problem)) problem <- last_file_problem(xpdb, 'ext')
  if (missing(subprob)) subprob <- last_file_subprob(xpdb, 'ext', problem)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  
  x_var <- 'ITERATION'
  if(xpdb$software=='nlmixr') {
    x_var <- 'iter'
  }
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(problem = problem, 
                               source = 'ext',
                               filter = function(x) {
                                 x %>% 
                                   dplyr::filter(.[, x_var] >= 0) %>% 
                                   dplyr::select_if(.predicate = function(x) dplyr::n_distinct(x) > 1)
                               }, tidy = TRUE, index_col = x_var),
                mapping = aes_c(aes_string(x = x_var, y = 'value'), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                panel_scales = 'free_y', ...)
}

#' @rdname minimization_plots
#' @export
grd_vs_iteration <- function(xpdb,
                             mapping  = NULL,
                             group    = 'variable',
                             type     = 'pl',
                             facets   = NULL,
                             title    = 'Gradient @y vs. @x | @run',
                             subtitle = 'Method: @method, minimization time: @runtime\nTermination message: @term',
                             caption  = '@dir',
                             log      = NULL,
                             guides   = FALSE,
                             problem,
                             subprob,
                             quiet,
                             ...) {
  # Check input
  check_xpdb(xpdb, check = 'files')
  if (missing(problem)) problem <- last_file_problem(xpdb, 'grd')
  if (missing(subprob)) subprob <- last_file_subprob(xpdb, 'grd', problem)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(facets)) facets <- 'variable'
  x_var <- 'ITERATION'
  
  xplot_scatter(xpdb = xpdb, group = group, quiet = quiet,
                opt = data_opt(problem = problem, 
                               source = 'grd',
                               filter = function(x) {
                                 x %>% 
                                   dplyr::filter(.[, x_var] >= 0) %>% 
                                   dplyr::select_if(.predicate = function(x) dplyr::n_distinct(x) > 1)
                               }, tidy = TRUE, index_col = x_var),
                mapping = aes_c(aes_string(x = x_var, y = 'value'), mapping),
                type = type, guides = guides, panel_facets = facets, 
                xscale = check_scales('x', log), 
                yscale = check_scales('y', log), 
                title = title, subtitle = subtitle, caption = caption,
                plot_name = as.character(match.call()[[1]]),
                panel_scales = 'free_y', ...)
}
