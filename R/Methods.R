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
                                 c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    printCoefmat(coef.table, digits = digits)
  } else if (x$pres=="sep") {
    cat("------------------------------------------------------------------------------")
    cat("\n------------------------------------------------------------------------------")
    cat("\nMarginal Model (Model without the Mediator):\n")
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
                                 c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    cond.table <- coef.table[no.stars,]
    rownames(cond.table)[1] <- "(Intercept)"
    marg.table <- coef.table[stars,]
    rownames(marg.table)[1] <- "(Intercept)"
    rownames(marg.table)[-1] <- sapply(rownames(marg.table)[-1], function(x) substr(x,1,nchar(x)-5))
    #cat(row.names(marg.table), "\n")
    stats::printCoefmat(marg.table, digits = digits)
    cat("\n------------------------------------------------------------------------------")
    cat("\n------------------------------------------------------------------------------")
    cat("\nConditional Model (Model with the Mediator):\n \n")
    stats::printCoefmat(cond.table,  digits = digits)
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
#' @export
predict.GEEmediate <- function (object, newdata = NULL, model.pred = c("cond", "marg"),type = c("link", "response", "terms"),
                                se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...)
{
  model.pred <- match.arg(model.pred)
  type <- match.arg(type)
  if (se.fit==T) {warning("se.fit=T is currently not supported for GEEmediate")}
  if (type=="terms") {
    warning("type='terms' is currently not supported for GEEmediate, using type='response' intead")
    type <- "response"
  }
  gee.object <- object$GEEfit
  if(!missing(newdata))
  {
    newdf <- newdata[gee.object$xnames[substrRight(gee.object$xnames)!=".star" & substrRight(gee.object$xnames)!="INT"]]
    dupl.df.new <- DupliData(df = newdf, mediator = object$call[[4]], surv = F)
    dupl.df.new <- dupl.df.new[gee.object$xnames]
    eta <- gee.object$linear.predictors
    if (type=="response") {
    pred <- make.link(tolower(gee.object$model$link))$linkinv(eta)
    } else {
      pred <- eta
    }
    if (model.pred=="cond") {out <- pred[1:(gee.object$nobs/2)]}
    if (model.pred=="marg") {out <- pred[(gee.object$nobs/2+1):gee.object$nobs]}
  } else {
    pred <- predict.glm(object = gee.object, type = type, se.fit = F, na.action = na.action, ...)
    if (model.pred=="cond") {out <- pred[1:(gee.object$nobs/2)]}
    if (model.pred=="marg") {out <- pred[(gee.object$nobs/2+1):gee.object$nobs]}}
  out
}
