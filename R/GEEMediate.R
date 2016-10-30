GEEMediate <- function(formula, exposure, mediator, NIE = T, prop = T, conf.level = 0.95,
                       data = parent.frame(), surv = F, family = gaussian, presentseprate = F,
                       niealternative = c("two-sided","one-sided"),
                       corstr = "independence",...)
{
  alp.conf <- (1 - conf.level)/2
  back <- list()
  back$call <- match.call()
  fit <- GEEMediateFit(formula = formula, exposure, mediator,
                       data = data, surv = surv, family = gaussian,
                       corstr = "independence",...)
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
      nie.pval <- pnorm(abs(nie)/sqrt(var.nie),lower.tail = F)
      nie.ci <- nie + c(qnorm(alp.conf), qnorm(1 - alp.conf)) * sqrt(var.nie)
      back$nie.pval <- nie.pval
      back$nie.ci <- nie.ci
    } else {
      var.pm <- v/te^2 + v.star * (nde^2) / (te^4) -
        2 * covar * nde / (te^3) # Var(PM) by the delta method
      var.nie <-  v + v.star - 2 * covar
      pm.pval <- pnorm(pm/sqrt(var.pm),lower.tail = F)
      nie.pval <- pnorm(abs(nie)/sqrt(var.nie),lower.tail = F)
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
return(back)
}
