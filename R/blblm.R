#' @import purrr
#' @import stats
#' @import utils
#' @importFrom furrr future_map
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @aliases NULL
#' @details
#' Regression models with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' BLB Linear regression
#' @param formula regression formula to fit
#' @param data data or data folder directory
#' @param m splitted data size
#' @param B times of bootstrap
#' @export
blblm <- function(formula, data, m = 10, B = 5000) {
  if(class(data) == "character"){
    data_list <- read_data(data)
  }
  else{
    data_list <- split_data(data, m)
  }
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(.), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' BLB Linear regression with paralization.
#' @param formula regression formula to fit
#' @param data data
#' @param m splitted data size
#' @param B times of bootstrap
#' @export
par_blblm <- function(formula, data, m = 10, B = 5000) {
  if(class(data) == "character"){
    data_list <- read_data(data)
  }
  else{
    data_list <- split_data(data, m)
  }
  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(.), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' BLBGLM
#' @param formula regression formula to fit
#' @param data data
#' @param m splitted data size
#' @param B times of bootstrap
#' @param family family in glm
#' @export
blbglm <- function(formula, data, m = 10, B = 5000, family) {
  if(class(data) == "character"){
    data_list <- read_data(data)
  }
  else{
    data_list <- split_data(data, m)
  }
  estimates <- map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B, family))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}


#' BLBGLM with paralization.
#' @param formula regression formula to fit
#' @param data data
#' @param m splitted data size
#' @param B times of bootstrap
#' @param family family in glm
#' @export
par_blbglm <- function(formula, data, m = 10, B = 5000, family) {
  if(class(data) == "character"){
    data_list <- read_data(data)
  }
  else{
    data_list <- split_data(data, m)
  }
  estimates <- future_map(
    data_list,
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B, family))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}


#' split data into m parts of approximated equal sizes
#' @param data data
#' @param m splitted data size
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' read csv folder
#' @param folder_directory directory of a folder
read_data = function(folder_directory){
  file.path(folder_directory, list.files(folder_directory, pattern = "csv$")) %>%
    map(read_csv)
}


#' compute the estimates for linear regression
#' @param formula regression formula to fit
#' @param data data
#' @param n number of random vectors to draw
#' @param B times of bootstrap
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the linear regression estimates for a blb dataset
#' @param formula regression formula to fit
#' @param data data
#' @param n number of random vectors to draw
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' estimate the linear regression estimates based on given the number of repetitions
#' @param formula regression formula to fit
#' @param data data
#' @param freqs weights for fitting
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = coef(fit), sigma = sigma(fit))
}


#' compute the estimates for GLM
#' @param formula regression formula to fit
#' @param data data
#' @param n number of random vectors to draw
#' @param B times of bootstrap
#' @param family family in glm
glm_each_subsample <- function(formula, data, n, B, family) {
  replicate(B, glm_each_boot(formula, data, n, family), simplify = FALSE)
}


#' compute the GLM estimates for a blb dataset
#' @param formula regression formula to fit
#' @param data data
#' @param n number of random vectors to draw
#' @param family family in glm
glm_each_boot <- function(formula, data, n, family) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs, family)
}

#' estimate the GLM estimates based on given number of repetitions
#' @param formula regression formula to fit
#' @param data data
#' @param freqs weights for fitting
#' @param family family in glm
glm1 <- function(formula, data, freqs, family) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, family = family, weights = freqs)
  list(coef = coef(fit), sigma = sigma(fit))
}


#' print the formula
#' @param x fitted model
#' @param ... extra conditions
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat(class(x), "model:", capture.output(x$formula))
  cat("\n")
}


#' sigma for blb regression model
#' @param object fitted model
#' @param confidence True/False
#' @param level confidence level
#' @param ... extra conditions
#' @export sigma.blblm
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- map(object$estimates,~map(.,"sigma")%>%reduce(.,rbind))
  sigma <- est %>%
    map(.,~apply(.,2,mean)) %>%
    reduce(`+`)/length(object$estimates)
  #confidence interval
  if (confidence) {
    alpha <- 1 - level
    limits <- object$estimates %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  }
  else{
    return(sigma)
  }
}


#' coefficients for blb regression model
#' @param object fitted model
#' @param ... extra conditions
#' @export coef.blblm
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' confidence interval for each terms
#' @param object fitted model
#' @param parm TRUE/FALSE
#' @param level confidence level
#' @param ... extra conditions
#' @export confint.blblm
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}


#' predict the model by fiven new data
#' @param object fitted model
#' @param new_data data
#' @param confidence True/False
#' @param level confidence level
#' @param ... extra conditions
#' @export predict.blblm
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
