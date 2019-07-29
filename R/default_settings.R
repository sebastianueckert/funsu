# The code in this file manages the settings for the different methods

#'@export
defaults.mc <- function(n_samples = 1000, seed = 123) rlang::fn_fmls()

#'@export
defaults.gq <- function(quad_points = 5) rlang::fn_fmls()
# function combines the formals of the calling function with the arguments of the acutal call and returns
# the results as a list
merge_fmls_args <- function() {
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
