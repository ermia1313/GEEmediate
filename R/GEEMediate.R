#' Causal mediation analysis for generlized linear models using the difference method.
#'
#' Estimation of natural direct and indirect effects for generalized linear models. The function utilizes a data-duplication algorithm to fit
#' marginal and conditional GLMs in a way that allow for consistent variance estimation. The function
#' produce point estimates, confidence intervals and p-values for the natural indirect effect and the mediation proportion
#
#'
#' @param formula A formula expression as for
#' other regression models, of the form response ~ predictors. See the documentation of \code{\link[stats]{lm}} and \code{\link[stats]{formula}} for details.
#' predictors should include exposure/treatment and mediator.
#' @param exposure The exposure (string).
#' @param mediator The mediator (string).
#' @param df A name of a data frame where all variables mentioned in formula are stored.
#' @param family A \code{family} object to be used in gee: a list of functions and expressions for defining link and variance functions
#' see the gee documentation. Default is gaussian. See also \code{\link[gee]{gee}} and \code{\link[stats]{glm}}.
#' @param corstr A working correlation structure. See \code{\link[gee]{gee}} and \code{\link[stats]{glm}}.
#' @param pres Presentation of the coefficient tables. "tog" for a single table, "sep" for two separated tables.
#' @param conf.level Confidence level for all confidence intervals (default 0.95)
#' @param surv Is the outcome survival (not supported)
#' @param niealternative Alternative hypothesis for testing that the nie=0. Either "two-sided" (default) or
#' "one-sided" for alternative nie>0.
#' @param ... Further arguments for the \code{gee} call.
#' @return The output contains the following components:
#' \item{call}{The call.}
#' \item{GEE.fit}{Results of fitting the GEE for the duplicated data.}
#' \item{nie}{The natural indirect effect estimate.}
#' \item{nie.pval}{P-value for tesing mediation using the NIE.}
#' \item{nde}{The natural direct effect estimate.}
#' \item{nie.ci}{Confidence interval in for the NIE in confidence level conf.level.}
#' \item{pm}{The mediation proportion estimate.}
#' \item{pm.pval}{P-value for tesing one-sided mediation using the mediation proportion.}
#' \item{pm.ci}{Confidence interval for the mediation proportio in confidence level conf.level.}
#' @examples
#' \dontrun{
#' SimNormalData <- function(n,beta.star = 1, p = 0.3, rho =0.4, inter =  0)
#'{
#'  beta2 <- (p/rho)*beta1.star
#'  beta1 <- (1-p)*beta1.star
#'  XM <- mvrnorm(n, mu = c(0,0), Sigma = matrix(c(1,rho,rho,1),2,2))
#'  X <- XM[,1]
#'  M <- XM[,2]
#'  beta <- c(inter, beta1, beta2)
#'  print(beta)
#'  Y <- cbind(rep(1,n),XM)%*%beta+rnorm(n,0,sd = 1)
#'  return(data.frame(X = X, M = M, Y = Y))
#'}
#' set.seed(314)
#' df <- SimNormalData(500)
#' GEEmediate(Y ~ X + M, exposure = "X", mediator = "M", df = df)
#' }
#'
#'
#' @importFrom stats qnorm pnorm as.formula gaussian update printCoefmat
#' @export
GEEmediate <- function(formula, exposure, mediator, df, family = gaussian,  corstr = "independence",
                       conf.level = 0.95, surv = F,  pres = "tog",
                       niealternative = "two-sided",...)
{
  alp.conf <- (1 - conf.level)/2
  n.vars <- length(all.vars(formula))
  indep.vars <- all.vars(formula)[2:n.vars]
  if (!(pres %in% c("tog","sep")))
  {
    message("Using pres='tog'. pres argument is unknown.")
    pres <- "tog"
  }
  if (!(niealternative %in% c("two-sided","one-sided")))
  {
    warning(gettextf("niealternative = '%s' is not supported. Using 'two-sided' for nie test."))
    niealternative <- "two-sided"
  }
  if (!(exposure %in% indep.vars))
  {
    stop("The exposure/treatment should be included in the model formula" )
  }
  if (!(mediator %in% indep.vars))
  {
    stop("The mediator should be included in the model formula" )
  }
  if (is.null(df))
  {
    stop("Argument 'df' was null. Data must be part of a data frame" )
  }
  back <- list()
  back$call <- match.call()
  back$alter <- niealternative
  fit <- invisible(GEEmediateFit(formula = formula, exposure, mediator,
                       data = df, surv = surv, family = family,
                       corstr = corstr,...))
  back$GEEfit <- fit
  nde <- fit$coefficients[names(fit$coefficients)==exposure]
  nie <- fit$coefficients[names(fit$coefficients)==paste0(exposure,".star")] - nde
  te <- nde + nie
  pm <- nie/te

  cov.gee <- fit$robust.variance
  v <- cov.gee[dimnames(fit$robust.variance)[[1]]==exposure,
                        dimnames(fit$robust.variance)[[2]]==exposure]
  v.star <- cov.gee[dimnames(fit$robust.variance)[[1]]==paste0(exposure, ".star"),
                        dimnames(fit$robust.variance)[[2]]==paste0(exposure, ".star")]
  covar <- cov.gee[dimnames(fit$robust.variance)[[1]]==exposure,
                                dimnames(fit$robust.variance)[[2]]==paste0(exposure, ".star")]
    if (pm < 0 | pm >1) {
      warning("Crude PM estimate not within [0,1], no formal statistical infernece \n
              Either NIE and NDE are in oppsoing directions or
              M is not a meidator.
              ")
      var.nie <-  v + v.star - 2 * covar
        warning("Calculating two-sided p-value for the null NIE=0 ")
        nie.pval <- pnorm(2*abs(nie)/sqrt(var.nie),lower.tail = F)
        back$alter <- "two-sided"

      back$nie.pval <- nie.pval
      back$nie.ci <- nie.ci
    } else {
      var.pm <- v/te^2 + v.star * (nde^2) / (te^4) -
        2 * covar * nde / (te^3) # Var(PM) by the delta method
      var.nie <-  v + v.star - 2 * covar
      pm.pval <- pnorm(pm/sqrt(var.pm),lower.tail = F)
      if (niealternative=="two-sided") {
      nie.pval <- pnorm(2*abs(nie)/sqrt(var.nie),lower.tail = F)
      } else {
        nie.pval <- pnorm(nie/sqrt(var.nie),lower.tail = F)
      }
      pm.ci <- pm + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.pm)
      if (pm.ci[1] < 0) {pm.ci[1] <- 0}
      if (pm.ci[2] > 1) {pm.ci[2] <- 1}
      nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.nie)
      back$pm.pval <- pm.pval
      back$pm.ci <- pm.ci
      back$nie.pval <- nie.pval
      back$nie.ci <- nie.ci
    }
back$nde <- nde
back$nie <- nie
back$pm <- pm
back$pres <- pres
names(back$pm) <- "Prop Med"
class(back) <- "GEEmediate"
back
}
