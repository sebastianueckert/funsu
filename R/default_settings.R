# The code in this file manages the settings for the different methods

#'@export
settings.mc <- function(n_samples = 1000, seed = 123) make_settings()

#'@export
settings.gq <- function(quad_points = 5) make_settings()

#'@export
settings.irt_expected <- function(gq.quad_points = 5) make_settings()

#' Make a settings function
#'
#' The function simplifies the creation of settings functions which return their arguments as a list. It works by
#' combining the formals of the calling function with the arguments of the acutal call and returns
#' the results as a list
#'
#' @return A list
#' @export
#'
#' @examples
#' my_settings <- function(opt_1 = 1) make_settings()
#' settings <- my_settings()
#' settings2 <- my_settings(opt_1 = 2)
make_settings <- function() {
  fmls_caller <- as.list(formals(sys.function(sys.parent(n = 1))))
  args_caller <- as.list(match.call(sys.function(sys.parent(n = 1)), sys.call(sys.parent(n = 1))))[-1]
  purrr::list_modify(fmls_caller, !!!args_caller) %>%
    rapply(eval, how = "replace")
}

# This function checks whether all required settings are available in the list
check_required_settings <- function(settings, required){
  not_available <- required[!required %in% names(settings)]
  msg <- paste0("The following required settings were not provided: ", paste(not_available, collapse = ","))
  if(length(not_available)!=0) rlang::abort(msg)
}


filter_settings <- function(settings, prefix){
  filtered <- settings[grepl(paste0(prefix,"."), names(settings))]
  names(filtered) <- gsub(paste0(prefix,"."),"", names(filtered))
  filtered
}

