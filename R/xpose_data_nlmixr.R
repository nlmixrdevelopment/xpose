#' Import nlmixr output into R
#'
#' @description Gather model outputs into an xpose database
#'
#' @param obj nlmixr fit object to be evaluated.
#' @param gg_theme A ggplot2 theme object (eg. \code{\link[ggplot2]{theme_classic}}).
#' @param xp_theme An xpose theme or vector of modifications to the xpose theme
#' (eg. \code{c(point_color = 'red', line_linetype = 'dashed')}).
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#' @param skip Character vector be used to skip the import/generation of: 'data', 'files', 'summary' or any
#' combination of the three.
#' @param ... Additional arguments to be passed to the \code{\link[readr]{read_delim}} functions.
#'
#' @examples
#' \dontrun{
#' xpdb <- xpose_data_nlmixr(obj = fit1)
#' }
#' 
#' @export
xpose_data_nlmixr <- function(obj         = NULL,
                       gg_theme      = theme_readable(),
                       xp_theme      = theme_xp_default(),
                       quiet,
                       skip          = NULL,
                       ...) {
  
  
  get_wres <- function(res, dv, pred) {
    res / (sqrt(cov(dv, pred)))
  }
  
  if (is.null(obj)) {
    stop('Argument `obj` required.', call. = FALSE)
  }
  
  if (missing(quiet)) quiet <- !interactive()
  
  objok <- FALSE
  
  if (("nlmixr_nlme" %in% class(obj)) | ("nlmixr.ui.nlme" %in% class(obj))) { 
    mtype <- "nlme"
    software <- "nlmixr"
    objok <- TRUE
  }
  
  if ("nlmixr.ui.saem" %in% class(obj)) { 
    mtype <- "saem"
    software <- "nlmixr"
    objok <- TRUE
  }
  
  if (objok == FALSE) {
    stop('Model type currently not supported by xpose.', call. = FALSE)
  }  
  
  if ("nlmixr_nlme" %in% class(obj)) { 
  data <- obj$call[[3]]
  
  data$PRED  <- obj$fitted[,1]
  data$IPRED <- obj$fitted[,2]
  data$RES   <- obj$residuals[,1]
  data$IRES  <- obj$residuals[,2]
  
  data_a <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(WRES = get_wres(res = RES, dv = DV, pred=PRED))
  
  data_a <- tibble::as.tibble(data_a)
  }
  
  if (("nlmixr.ui.saem" %in% class(obj)) | ("nlmixr.ui.nlme" %in% class(obj))) { 
    data <- as.data.frame(obj)
    data_a <- data %>%
      dplyr::group_by(ID) 
    
    data_a <- tibble::as.tibble(data_a)
  }
  
  data <- NULL
  data_ind <- data_a %>% 
    colnames() %>% 
    dplyr::tibble(table = 'nlmixr',
                  col   = ., 
                  type  = NA_character_, 
                  label = NA_character_,     # Feature to be added in future release
                  units = NA_character_) %>% # Feature to be added in future release
    dplyr::mutate(type = dplyr::case_when(
      .$col == 'ID' ~ 'id',
      .$col == 'DV' ~ 'dv',
      .$col == 'TIME' ~ 'idv',
      .$col == 'OCC' ~ 'occ',
      .$col == 'DVID' ~ 'dvid',
      .$col == 'AMT' ~ 'amt',
      .$col == 'MDV' ~ 'mdv',
      .$col == 'EVID' ~ 'evid',
      .$col == 'IPRED' ~ 'ipred',
      .$col == 'PRED' ~ 'pred',
      .$col %in% c('RES', 'WRES', 'CWRES', 'IWRES', 'EWRES', 'NPDE','IRES') ~ 'res',
      stringr::str_detect(.$col, 'ETA\\d+|ET\\d+|eta.*') ~ 'eta'))
  
  data <- list()
  data <- dplyr::tibble(problem = 1,
               simtab = F,
               index = list(data_ind),
               data = list(data_a),
               modified = F)
  
  # Generate model summary
  if ('summary' %in% skip) {
    msg('Skipping summary generation', quiet)
    summary <- NULL
  } else if (software == 'nlmixr') {
    summary <- summarise_nm_model(obj, '', software, rounding = xp_theme$rounding)
  }
  
  # The weighted residuals are calculated by dividing the vector of each
  # individual's residuals (res_i) by the square root of the matrix of
  # covariances of that individual's data conditional on the population model:
  #   
  #   WRES_i = RES_i / SQRT(COV(data_i | F_pop))
  # 
  # This means that for each WRES calculated we include covariances between data
  # points of an individual.  If the correlations between some of these data
  # points are negative then the resulting WRES could also be negative, while
  # the RES could be positive. 
  # 
  # -Andy  
  
  # Import estimation tables
  # if ('data' %in% skip) {
  #   msg('Skipping data import', quiet)
  #   data <- NULL
  # } else if (software == 'nonmem') {
  #   data <- read_nm_tables(files = tbl_names, quiet = quiet, simtab = simtab, ...)
  # }
  
  # Generate model summary
  # if ('summary' %in% skip) {
  #   msg('Skipping summary generation', quiet)
  #   summary <- NULL
  # } else if (software == 'nonmem') {
  #   summary <- summarise_nm_model(file, model_code, software, rounding = xp_theme$rounding)
  # }
  
  # Import output files
  # if ('files' %in% skip) {
  #   msg('Skipping output files import', quiet)
  #   out_files <- NULL
  # } else if (software == 'nonmem') {
  #   if (missing(extra_files)) {
  #     extra_files <- c('.ext', '.cor', '.cov', '.phi', '.grd', '.shk')
  #   }
  #   out_files <- update_extension(file, '') %>% 
  #     stringr::str_c(make_extension(extra_files)) %>% 
  #     read_nm_files(quiet = quiet)
  # }
  
  # Label themes
  attr(gg_theme, 'theme') <- as.character(substitute(gg_theme)) 
  attr(xp_theme, 'theme') <- as.character(substitute(xp_theme)) 
  
  # Output xpose_data
  list(code = NULL, summary = summary, data = data,
       files = NULL, gg_theme = gg_theme, xp_theme = xp_theme,
       options = list(dir = NULL, quiet = quiet, 
                      manual_import = NULL)) %>% 
    structure(class = c('xpose_data', 'uneval'))
}

