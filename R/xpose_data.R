#' Import NONMEM output into R
#'
#' @description Gather model outputs into a R database
#'
#' @param runno Run number to be evaluated.
#' @param dir Location of the model files.
#' @param file Full file name preferably a `.lst` file. Alternative argument to \code{dir}, \code{prefix},
#' \code{runno} and \code{ext}.
#' @param prefix Prefix of the model file name.
#' @param ext Extension of the model file. Should be one of ".lst" (default), ".out", ".res", ".mod" or ".ctl" for NONMEM.
#' @param gg_theme A ggplot2 theme object (eg. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param simtab If \code{TRUE} only reads in simulation tables, if \code{FALSE} only reads estimation tables. 
#' Default \code{NULL} reads all tables. Option not compatible with manual_import.
#' @param manual_import If \code{NULL} (default) the names of the output tables to import will be obtained from the model file. 
#' To manually import files as in previous versions of xpose, the check the function \code{\link{manual_nm_import}}.
#' @param skip Character vector be used to skip the import/generation of: 'data', 'files', 'summary' or any
#' combination of the three.
#' @param extra_files A vector of additional output file extensions to be imported. Default is ".ext", ".cov", ".cor", ".phi", 
#' ".grd" for NONMEM.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_delim}} functions.
#'
#' @examples
#' \dontrun{
#' xpdb <- xpose_data(file = 'run001.lst')
#' }
#' 
#' @export
xpose_data <- function(runno         = NULL,
                       dir           = NULL,
                       file          = NULL,
                       prefix        = 'run',
                       ext           = '.lst',
                       gg_theme      = theme_readable(),
                       xp_theme      = theme_xp_default(),
                       simtab        = NULL,
                       manual_import = NULL,
                       skip          = NULL,
                       extra_files,
                       quiet,
                       ...) {
  
  if (is.null(runno) && is.null(file)) {
    stop('Argument `runno` or `file` required.', call. = FALSE)
  }
  
  if (is.null(file)) {
    ext  <- make_extension(ext)
    file <- file_path(dir, stringr::str_c(prefix, runno, ext))
  }
  
  if (missing(quiet)) quiet <- !interactive()
  
  if (ext %in% c('.lst', '.out', '.res', '.mod', '.ctl')) {
    software <- 'nonmem'
    model_code <- read_nm_model(file, runno, dir, prefix, ext, quiet)
    
    if (is.null(manual_import)) {
      tbl_names <- list_nm_tables(model_code)
    } else {
      tbl_names <- list_nm_tables_manual(file = file, prefix = prefix, tab_list = manual_import)
    }
  } else {
    stop('Model file currently not supported by xpose.', call. = FALSE)
  }  
  
  # Import estimation tables
  if ('data' %in% skip) {
    msg('Skipping data import', quiet)
    data <- NULL
  } else if (software == 'nonmem') {
    data <- read_nm_tables(files = tbl_names, quiet = quiet, simtab = simtab, ...)
  }
  
  # Generate model summary
  if ('summary' %in% skip) {
    msg('Skipping summary generation', quiet)
    summary <- NULL
  } else if (software == 'nonmem') {
    summary <- summarise_nm_model(file, model_code, software, rounding = xp_theme$rounding)
  }
  
  # Import output files
  if ('files' %in% skip) {
    msg('Skipping output files import', quiet)
    out_files <- NULL
  } else if (software == 'nonmem') {
    if (missing(extra_files)) {
      extra_files <- c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk')
    }
    out_files <- update_extension(file, '') %>% 
      stringr::str_c(make_extension(extra_files)) %>% 
      read_nm_files(quiet = quiet)
  }
  
  # Label themes
  attr(gg_theme, 'theme') <- as.character(substitute(gg_theme)) 
  attr(xp_theme, 'theme') <- as.character(substitute(xp_theme)) 
  
  # Output xpose_data
  list(code = model_code, summary = summary, data = data,
       files = out_files, gg_theme = gg_theme, xp_theme = xp_theme,
       options = list(dir = dirname(file), quiet = quiet, 
                      manual_import = manual_import), software = 'nonmem') %>% 
    structure(class = c('xpose_data', 'uneval'))
}

