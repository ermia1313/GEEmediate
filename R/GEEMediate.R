GEEMediate <- function(formula, exposure, mediator, NIE = T, prop = T, conf.level = 0.95,
                       data = parent.frame(), surv = F, family = gaussian, pres = "tog",
                       niealternative = "two-sided", corstr = "independence",...)
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
  back <- list()
  back$call <- match.call()
  back$alter <- niealternative
  fit <- invisible(GEEMediateFit(formula = formula, exposure, mediator,
                       data = data, surv = surv, family = family,
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
class(back) <- "GEEMediate"
back
}
