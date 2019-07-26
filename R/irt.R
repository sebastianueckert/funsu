#' Calculate expected item response for a population
#'
#' The function calculates the expected item score for a given mirt IRT model and a specified population. If mean and variance are not provided, they are taken from the
#' mirt model object.
#'
#' @param model A \code{mirt} model
#' @param mu The mean of latent variable values
#' @param sigma The variance-covariance matrix of the latent variable values
#'
#' @return A vector with the expected score for each item
#' @export
#'
#' @examples
#' fit <- mirt::mirt()
irt_expected_item_response <- function(model, mu = NULL, sigma = NULL){
  if(!requireNamespace("mirt", quietly = TRUE)) stop("The mirt package is need for this function.", call. = F)
  if(is.null(mu))  mu <- mirt::coef(model, simplify = T)$means
  if(is.null(sigma)) sigma <- mirt::coef(model, simplify = T)$cov
  dim <- model@Model$nfact
  stopifnot(dim == length(mu))
  stopifnot(dim == NROW(sigma))
  nitems <- model@Data$nitems
  item_names <- colnames(model@Data$data)
  item_objs <- purrr::map(seq_len(nitems), ~mirt::extract.item(model, .x))
  p_i <-  purrr::map(item_objs, ~expect_norm_gq(mirt::probtrace, x = .x, dimensions = dim, mu = mu, sigma = sigma))
  expect_i <- purrr::map_dbl(p_i, ~sum(.x * (seq_along(.x)-1))) %>% purrr::set_names(item_names)
  return(expect_i)
}
