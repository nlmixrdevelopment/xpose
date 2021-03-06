#' Draw an xpose_plot object
#' 
#' @description This function explicitly draw an xpose_plot and interprets keywords 
#' contained in labels.
#' 
#' @param x An \code{xpose_plot} object.
#' @param ... Options to be passed on to the ggplot2 print method.
#' 
#' @method print xpose_plot
#' @examples
#' my_plot <- dv_vs_ipred(xpdb_ex_pk) +
#'             labs(title = 'A label with keywords @nind, @nobs')

#' # Using the print function
#' print(my_plot)
#' 
#' # Or simply by writting the plot object name
#' my_plot
#' 
#' @export
print.xpose_plot <- function(x, ...) {
  if (is.xpose.plot(x)) {
    var_map <- as.character(x$mapping)
    x$labels$title <- append_suffix(x$xpose, x$labels$title, 'title')
    x$labels$subtitle <- append_suffix(x$xpose, x$labels$subtitle, 'subtitle')
    x$labels$caption  <- append_suffix(x$xpose, x$labels$caption, 'caption')
    x$labels <- x$labels %>% 
      purrr::map_if(stringr::str_detect(., '@'),
                    .f = parse_title, xpdb = x$xpose,
                    problem = x$xpose$problem, quiet = x$xpose$quiet,
                    extra_key = c('plotfun', 'timeplot', names(var_map)), 
                    extra_value = c(x$xpose$fun, 
                                    format(Sys.time(), "%a %b %d %X %Z %Y"), 
                                    var_map))
  }
  print.ggplot(x, ...)
}

print.ggplot <- get('print.ggplot', envir = asNamespace('ggplot2'))
