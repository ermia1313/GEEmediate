substrRight <- function(x, n=5){
  substr(x, nchar(x)-n+1, nchar(x)) ### Thank you Andrie (http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r)
}
#' @export
print.GEEmediate <- function(x, digits = max(options()$digits - 4, 3),...)
{

  cat("Call:\n")
  print(x$call)
  cat("\n")
  if(x$pres=="tog")
  {
    cat("Coefficients:\n")
    coeffs <- x$GEEfit$coefficients
    cat("\n")
    coeffs.names <- names(coeffs)
    covmat <- x$GEEfit$robust.variance
    sd.err <- sqrt(diag(covmat))
    zvalue <- coeffs/sd.err
    pvalue <- 2*pnorm(abs(zvalue),lower.tail = F)
    coef.table <- cbind(coeffs, sd.err, zvalue, pvalue)
    dimnames(coef.table) <- list(coeffs.names,
                                 c("EStimate", "Std. Error", "z value", "Pr(>|z|)"))
    printCoefmat(coef.table, digits = digits)
  } else if (x$pres=="sep") {
    cat("Conditional Model (Model with the Mediator):\n \n")
    cat("Coefficients:\n")
    coeffs <- x$GEEfit$coefficients
    coeffs.names <- names(coeffs)
    stars <- which(sapply(coeffs.names, substrRight)==".star")
    no.stars <- which(sapply(coeffs.names, substrRight)!=".star")
    covmat <- x$GEEfit$robust.variance
    sd.err <- sqrt(diag(covmat))
    zvalue <- coeffs/sd.err
    pvalue <- 2*pnorm(abs(zvalue),lower.tail = F)
    coef.table <- cbind(coeffs, sd.err, zvalue, pvalue)
    dimnames(coef.table) <- list(names(coeffs),
                                 c("EStimate", "Std. Error", "z value", "Pr(>|z|)"))
    stats::printCoefmat(coef.table[stars,], digits = digits)
    cat("\nMarginal Model (Model without the Mediator):\n")
    stats::printCoefmat(coef.table[no.stars,],  digits = digits)
  }
  cat("\n---------------------------")
  cat("\nNatural Indirect Effect: ", format(x$nie, digits = digits),
      "\np=", format(x$nie.pval, digits = 2),
      " for ", paste0(x$alter), " test for mediation \n", sep = "")
  cat("Confidence Interval = [", format(x$nie.ci[1],digits = digits),",",format(x$nie.ci[2],digits = digits),"]", sep = "")
  cat("\n---------------------------")
  cat("\nNatural Direct Effect:", format(x$nde, digits = digits))
  cat("\n---------------------------")
  if(x$pm >=0 & x$pm < 1)
  {
  cat("\nMediation Proportion:", format(100*x$pm,digits = 3),"%",
      "\np=", format.pval(x$pm.pval, digits = 2),
      " for one-sided test for mediation \n", sep = "")

  cat("Confidence Interval = [", format(100*x$pm.ci[1],digits = 3),"%",",",format(100*x$pm.ci[2],digits = 3),"%","]", sep = "")
  } else
  {
    cat("\nMediation Proportion:", format(100*x$pm,digits = 3),"%")
  }
   cat("\n---------------------------")
}
