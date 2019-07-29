

#' Calculate the expected value of f(X) with X~N(mu, sigma)
#'
#' @param fun Function to integrate
#' @param dimensions Number of dimensions of X
#' @param mu Mean of X
#' @param sigma Variance of X
#' @param settings Settings
#' @param ... Arguments to f
#'
#' @return Numeric value of the integral
#' @export
#'
#' @describeIn expect_norm_gq Calculation using Gaussian quadrature
#' @examples
#' f <- function(x) x^2
#' expect_norm_gq(f)
expect_norm_gq <- function(fun, dimensions = 1, mu=rep(0,dimensions),
                         sigma=diag(1,dimensions), settings=settings.gq(), ...){
  if(!requireNamespace("statmod", quietly = TRUE)) stop("The 'statmod' package needs to be installed to use this function.", call. = FALSE)

  additional.args <- c(list(), list(...))
  check_required_settings(settings, "quad_points")
  n.quad.points <- settings$quad_points
  if(is.character(dimensions)) {
    ndim <- length(dimensions)
  }else{
    ndim <- dimensions
  }
  if(dimensions==0) {
    n.quad.points <- 1
    ndim <- 1
  }
  gq <- statmod::gauss.quad.prob(n.quad.points, dist = "normal")
  std_grid_matrix <- matrix(rep(gq$nodes, ndim), ncol=ndim, byrow = F)
  std_grid_points <- do.call(expand.grid, as.data.frame(std_grid_matrix))

  eig <- eigen(sigma)
  rot <- eig$vectors %*% diag(sqrt(eig$values), nrow = ndim, ncol = ndim)
  scaled_grid_points <- t(rot %*% t(std_grid_points) + mu)


  std_weights_matrix <- matrix(rep(gq$weights, ndim), ncol=ndim, byrow = F)
  std_weights <-do.call(expand.grid, as.data.frame(std_weights_matrix))

  weights <- apply(std_weights, 1, prod)
  call.args <- c(additional.args, list(scaled_grid_points))
  # evaluate function on all grid points
  grid.results <- do.call(fun, call.args)
  # transpose evaluation results to allow easier definition of f
  grid.results <- t(grid.results)
  # calculate weighted sum
  drop(grid.results %*% weights)
}



#' @describeIn expect_norm_gq Calculation using Monte-Carlo sampling
#' @export
expect_norm_mc <- function(fun, dimensions, mu=rep(0,dimensions),
                           sigma=diag(1,dimensions), settings=settings.mc(), ...){
  additional.args <- c(list(), list(...))
  check_required_settings(settings, "n_samples")
  n.samples <- settings$n_samples
  if(!is.null(settings$seed)) set.seed(settings$seed)
  if(is.character(dimensions)) {
    ndim <- length(dimensions)
  }else{
    ndim <- dimensions
  }
  if(ndim>0){
    param.samples <- matrix(rnorm(n.samples*ndim, 0, 1), ncol=ndim)
  }else{
    param.samples <- matrix(nrow=n.samples)
  }
  if(is.character(dimensions)) {
    colnames(param.samples) <- dimensions
  }

  eig <- eigen(sigma)
  rot <- eig$vectors %*% diag(sqrt(eig$values), nrow = ndim, ncol = ndim)
  scaled_samples <- t(rot %*% t(param.samples) + mu)

  call.args <- c(additional.args, list(scaled_samples))
  mc.results <- do.call(fun, call.args)
  if(!is.matrix(mc.results)) return(mean(mc.results))
  return(drop(colMeans(mc.results)))
}

integrate_mc_lhs <- function(fun, dimensions, settings=filter_settings(defaults.agq(), "mc"), ...){
  require(lhs)
  additional.args <- c(list(), list(...))
  n.samples <- settings$n_samples
  if(is.character(dimensions)) {
    ndim <- length(dimensions)
  }else{
    ndim <- dimensions
  }
  #cells <- vapply(seq_len(ndim), function(x) sample(seq_len(n.samples)), rep(0, n.samples))
  #inv_samples <- 1/n.samples*(cells-1+matrix(runif(n.samples*ndim), nrow = n.samples))
  inv_samples <- randomLHS(n.samples,dimensions)
  call.args <- c(additional.args, list(inv_samples))
  mc.results <- do.call(fun, call.args)
  if(!is.matrix(mc.results)) return(mean(mc.results))
  return(rowMeans(mc.results))
}

get_sobol_closure <- function(){
  .dim <- 0
  function(samples, dim){
    if(.dim==dim) sobol(samples, dim = dim, scrambling = T, init=F)
    else{
      .dim <<- dim
      sobol(samples, dim = dim, scrambling = T, init=T)
    }
  }
}

my_sobol <- get_sobol_closure()

integrate_qrmc <- function(fun, dimensions, settings=filter_settings(defaults.agq(), "qrmc"), ...){
  require(randtoolbox)
  additional.args <- c(list(), list(...))
  n.samples <- settings$n_samples
  if(is.character(dimensions)) {
    ndim <- length(dimensions)
  }else{
    ndim <- dimensions
  }

  inv_samples <- my_sobol(n.samples, dimensions)
  #sobol(n.samples, dim = dimensions, scrambling = 1, init = F)
  if(dimensions==1) inv_samples <- matrix(inv_samples, ncol=1)
  #if(T) inv_samples <- (inv_samples + matrix(runif(n.samples*ndim), nrow = n.samples)) %% 1
  call.args <- c(additional.args, list(inv_samples))
  mc.results <- do.call(fun, call.args)
  if(!is.matrix(mc.results)) return(mean(mc.results))
  return(rowMeans(mc.results))
}
