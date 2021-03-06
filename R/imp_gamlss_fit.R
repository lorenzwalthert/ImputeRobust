#' GAMLSS imputation fit
#'
#' @description
#' This function takes uses a data set to fit a gamlss model and
#' another to predict the expected parameters values. It returns a
#' function that will generate a vector of random observations for the
#' predicted parameters. The amount of random obervations is the
#' number of units on the dataset used to get such predictions.
#'
#' @param data Completely observed data frame to be used to fit a
#'   gamlss model estimate.
#' @param new.data Data frame used to predict the parameter values for
#'   some given right side x-values on the gamlss model.
#' @param family Family to be used for the response variable on the
#'   GAMLSS estimation.
#' @param n.ind.par Number of individual parameters to be
#'   fitted. Currently it only allows one or two because of stability
#'   issues for more parameters.
#' @param lin.terms Character vector specifying which (if any)
#'   predictor variables should enter the model linearly.
#' @param forceNormal Flag that if set to 'TRUE' will use a normal
#'   family for the gamlss estimation as a last resource.
#' @param trace whether to print at each iteration (TRUE) or not (FALSE)
#' @param form_int either a list with the objects \code{formula},
#'   \code{sigma.formula}, \code{nu.formula} and \code{tau.formula} or just one
#'   string that will be passed to \code{forula}, the other parameters will 
#'   be determined internally. 
#' @param prefit_gam Whether or not to fit a gam first and extract degrees of 
#'   freedom to provide a starting point for gamlss.
#' @param ... extra arguments for the control of the gamlss fitting
#'   function
#'
#' @return Returns a method to generate random samples for the fitted
#'   gamlss model using "new.data" as covariates.
ImpGamlssFit <- function(data, new.data, family, n.ind.par, lin.terms = NULL,
                         forceNormal = FALSE, trace = FALSE, form_int, 
                         prefit_gam = TRUE, ...) {

  # Family last will be the distribution family of the last fitted
  # gamlss model if forceNormal is TRUE, a normal distribution is
  # used.
  family.last <- family
  if (forceNormal) family.last <- NO

  # Really ugly way of specifying the formulas for the gamlss fitting
  # function
  mu.f1 <- ModelCreator(data, "pb", lin.terms = lin.terms)
  mu.f0 <- ModelCreator(data, "pb", par = list(degree = 3, order = 0),
                        lin.terms = lin.terms)
  mu.lin <- ModelCreator(data, "linear")

  sigma.f1 <- switch(n.ind.par, ~1, mu.f1, mu.f1, mu.f1)
  sigma.f0 <- switch(n.ind.par, ~1, mu.f0, mu.f0, mu.f0)
  sigma.lin <- switch(n.ind.par, ~1, mu.lin, mu.lin, mu.lin)

  nu.f1 <- switch(n.ind.par, ~1, ~1, mu.f1, mu.f1)
  nu.f0 <- switch(n.ind.par, ~1, ~1, mu.f0, mu.f0)
  nu.lin <- switch(n.ind.par, ~1, ~1, mu.lin, mu.lin)

  tau.f1 <- switch(n.ind.par, ~1, ~1, ~1, mu.f1)
  tau.f0 <- switch(n.ind.par, ~1, ~1, ~1, mu.f0)
  tau.lin <- switch(n.ind.par, ~1, ~1, ~1, mu.lin)

  # allow user-specified formula
  if (!is.null(form_int)) {
    if (length(form_int == 1)) {
      user_mu.f1 <- form_int
      user_sigma.f1 <- sigma.f1
      user_nu.formula = nu.f1
      user_tau.formula = tau.f1
    } else {
      user_mu.f1 <- form_int$formula
      user_sigma.f1 <- form_int$sigma.formula
      user_nu.formula = form_int$nu.f1
      user_tau.formula <- form_int$tau.formula
    }
    
    if (!is.null(prefit_gam)) {
      message(paste0("prefitting gam", user_mu.f1, "\n"))
      browser()
      # fit the gam model in advance and derive formula via edf
      fit <- gam(as.formula(user_mu.f1), family = prefit_gam$family, data = data)
      user_mu.f1 <- extract_formula(fit, response = "y")
      cat("\n the gam formula is", user_mu.f1, "\n")
    }
    
  }
  tryCatch(
  {
    # Fit gamlss model with given formula for all four moments
    # (ignored if not needed) and the given distribution family
    fit <- tryCatch(
      gamlss(formula = user_mu.f1,
             sigma.formula = user_sigma.f1,
             nu.formula = user_nu.f1,
             tau.formula = user_tau.f1,
             family = family,
             data = data,
             control = gamlss.control(trace = trace , ...),
             i.control = glim.control(...)),
      error = function(e) { 
        tryCatch(
        gamlss(formula = mu.f1,
               sigma.formula = sigma.f1,
               nu.formula = nu.f1,
               tau.formula = tau.f1,
               family = family,
               data = data,
               control = gamlss.control(trace = trace , ...),
               i.control = glim.control(...)),
        error = function(e) {
          tryCatch(
          {
            gamlss(formula = mu.f0,
                   sigma.formula = sigma.f0,
                   nu.formula = nu.f0,
                   tau.formula = tau.f0,
                   family = family,
                   data = data,
                   control = gamlss.control(trace = trace , ...),
                   i.control = glim.control(...))
          },
          error = function(e) {
            gamlss(formula = mu.lin,
                   sigma.formula = sigma.lin,
                   nu.formula = nu.lin,
                   tau.formula = tau.lin,
                   family = family.last,
                   data = data,
                   control = gamlss.control(trace = trace , ...),
                   i.control = glim.control(...))
          }
          )
        }
        )
      }
    )

    # Predict the parameters values for the units with missings
    capture.output(predictions <- predictAll(fit, new.data, type="response",
                                             data = data),
                   file = "/dev/null")

    # Return wrapper that will call the generation function
    # corresponding to the distribution family of the gamlss
    # model
    function(...) {
      r <- paste("r", fit$family[1], sep = "")

      predictions$y <- NULL
      do.call(r,
              args = c(predictions,
                       n = length(predictions$mu)
                       )
              )
    }
  }, error = function(e) {
    function(...) {do.call(rep, args = list(NA, nrow(new.data)))}
  }
  )

}
