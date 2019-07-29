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
#' @seealso \code{\link{irt_expected_cov_matrix}}
#' @export
#'
#' @examples
#' fit <- mirt::mirt(mirt::Science, 1)
#' irt_expected_item_response(fit)
irt_expected_item_response <- function(model, mu = NULL, sigma = NULL, settings = settings.irt_expected()){
  if(!requireNamespace("mirt", quietly = TRUE)) stop("The mirt package is need for this function.", call. = F)
  if(is.null(mu))  mu <- mirt::coef(model, simplify = T)$means
  if(is.null(sigma)) sigma <- mirt::coef(model, simplify = T)$cov
  gq_settings <- filter_settings(settings, "gq")
  dim <- model@Model$nfact
  stopifnot(dim == length(mu))
  stopifnot(dim == NROW(sigma))
  nitems <- model@Data$nitems
  item_names <- colnames(model@Data$data)
  item_objs <- purrr::map(seq_len(nitems), ~mirt::extract.item(model, .x))
  p_i <-  purrr::map(item_objs, ~expect_norm_gq(mirt::probtrace, x = .x, dimensions = dim, mu = mu, sigma = sigma, settings = gq_settings))
  expect_i <- purrr::map_dbl(p_i, ~sum(.x * (seq_along(.x)-1))) %>% purrr::set_names(item_names)
  return(expect_i)
}


#' Calculate expected item covariance matrix for a population
#'
#' @param model A \code{mirt} model
#' @param mu The mean of latent variable values
#' @param sigma The variance-covariance matrix of the latent variable values
#'
#' @return The variance covariance matrix of the assessment
#' @seealso \code{\link{irt_expected_item_response}}
#' @export
#'
#' @examples
#' fit <- mirt::mirt(mirt::Science, 1)
#' irt_expected_cov_matrix(fit)
irt_expected_cov_matrix <- function(model, mu = NULL, sigma = NULL, settings = settings.irt_expected()){
  if(!requireNamespace("mirt", quietly = TRUE)) stop("The mirt package is need for this function.", call. = F)
  if(is.null(mu))  mu <- mirt::coef(model, simplify = T)$means
  if(is.null(sigma)) sigma <- mirt::coef(model, simplify = T)$cov
  dim <- model@Model$nfact
  stopifnot(dim == length(mu))
  stopifnot(dim == NROW(sigma))
  gq_settings <- filter_settings(settings, "gq")
  nitems <- model@Data$nitems
  item_names <- colnames(model@Data$data)
  item_objs <- purrr::map(seq_len(nitems), ~mirt::extract.item(model, .x))
  p_i <-  purrr::map(item_objs, ~expect_norm_gq(mirt::probtrace, x = .x, dimensions = dim, mu = mu, sigma = sigma, settings = gq_settings))
  expect_i <- purrr::map_dbl(p_i, ~sum(.x * (seq_along(.x)-1)))
  var_i <- purrr::imap_dbl(p_i, ~sum(.x * ((seq_along(.x)-1) - expect_i[.y])^2))
  cov_matrix <- diag(var_i)
  for(i in seq_len(nitems)){
    for(j in seq_len(nitems)){
      if(j>=i) next
      prob_ij <- function(psi) {
        p_i <- mirt::probtrace(item_objs[[i]], psi)
        p_j <- mirt::probtrace(item_objs[[j]], psi)
        p_ij <- matrix(nrow = NROW(psi), ncol = ncol(p_i)*ncol(p_j))
        for(n in seq_len(NROW(psi))) p_ij[n,] <- as.vector(p_i[n,]%*%t(p_j[n,]))
        return(p_ij)
      }
      p_ij <- expect_norm_gq(prob_ij, dimensions = dim, mu = mu, sigma = sigma, settings = gq_settings) %>%
        matrix(nrow = 4)
      covar <- 0
      for(k_i in seq_len(item_objs[[i]]@ncat)){
        for(k_j in seq_len(item_objs[[j]]@ncat)){
          covar <- covar + p_ij[k_i, k_j]*(k_i-1-expect_i[i])*(k_j-1-expect_i[j])
        }
      }
      cov_matrix[i,j] <- cov_matrix[j,i] <- covar
    }
  }
  colnames(cov_matrix) <- rownames(cov_matrix) <- item_names
  cov_matrix
}



#'@export
#'@rdname irt_expected_item_response
settings.irt_expected <- function(gq.quad_points = 5) make_settings()
