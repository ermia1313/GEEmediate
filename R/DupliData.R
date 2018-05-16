######
#
#DupliData function expects a data frame, the name of a single expousre (X), a mediator (M) and outcome (Y) [optional], and unless told otherwise
#
######

DupliData <- function(df, mediator = NULL, outcome = NULL, case = NULL , Time = NULL,  surv = F, ID = NULL)
{
  if (surv==T & (is.null(case) | is.null(Time))) {stop("For survival data, both case and time are needed")}
  #if (surv==F & (is.null(outcome) | is.null(Time))) {stop("For survival data, both case and time are needed")}

  if (surv == F) {
     if(missing(outcome))
     {
       n.row <- nrow(df)
       df.star <- df
       df.star[[paste0(mediator)]] <- rep(0,n.row)
       names.covar <- names(df)[!(names(df)==mediator)]
       names(df.star)[!(names(df)==mediator)] <- paste0(names.covar,".star")
       for (var.name in names.covar)
       {
         df[[paste0(var.name,".star")]]  <- rep(0,n.row)
         df.star[[paste0(var.name)]] <- rep(0,n.row)
       }
       df$INT<- df.star$INT.star <- c(rep(1,n.row))
       df$INT.star <- df.star$INT <- c(rep(0,n.row))
       df.dupl <- rbind(df[c("INT","INT.star", names.covar, paste0(names.covar,".star"),
                             paste0(mediator))],
                        df.star[c("INT","INT.star", names.covar, paste0(names.covar,".star"),
                                  paste0(mediator))])
       if (is.null(ID)) {df.dupl$ID <- rep(1:n.row,2)}
   }  else {
     n.row <- nrow(df)
    df.star <- df
    df.star[[paste0(mediator)]] <- rep(0,n.row)
    names.covar.nofactor <- names(df)[!(names(df) %in% c(outcome,mediator)) & !sapply(df, is.factor)]
    names(df.star)[!(names(df) %in% c(outcome,mediator)) &  !sapply(df, is.factor)] <- paste0(names.covar.nofactor,".star")
    for (var.name in names.covar.nofactor)
    {
      df[[paste0(var.name,".star")]]  <- rep(0,n.row)
      df.star[[paste0(var.name)]] <- rep(0,n.row)
    }
    names.covar.factor <- names(df)[!(names(df) %in% c(outcome, mediator)) & sapply(df, is.factor)]
    names(df.star)[!(names(df) %in% c(outcome, mediator)) &  sapply(df, is.factor)] <- paste0(names.covar.factor,".star")
    for (var.name in names.covar.factor)
    {
      df[[paste0(var.name,".star")]]  <- factor(rep(levels(df[[var.name]])[1],n.row), levels = levels(df[[var.name]]))
      df.star[[paste0(var.name)]] <- factor(rep(levels(df[[var.name]])[1],n.row), levels(df[[var.name]]))
    }
    #df$INT<- df.star$INT.star <- c(rep(1,n.row))
    #df$INT.star <- df.star$INT <- c(rep(0,n.row))
    # df.dupl <- rbind(df[c("INT","INT.star", names.covar.nofactor, names.covar.factor,
    #                       paste0(c(names.covar.nofactor,names.covar.factor),".star"),
    #                       paste0(c(mediator,outcome)))],
    #                  df.star[c("INT","INT.star", names.covar.nofactor, names.covar.factor,
    #                            paste0(c(names.covar.nofactor,names.covar.factor),".star"),
    #                            paste0(c(mediator,outcome)))])
    df.dupl <- rbind(df[c(names.covar.nofactor, names.covar.factor,
                          paste0(c(names.covar.nofactor,names.covar.factor),".star"),
                          paste0(c(mediator,outcome)))],
                     df.star[c(names.covar.nofactor, names.covar.factor,
                               paste0(c(names.covar.nofactor,names.covar.factor),".star"),
                               paste0(c(mediator,outcome)))])
    df.dupl$INT <- factor(c(rep("",n.row), rep(".star", n.row)))
    df.dupl <- df.dupl[c("INT", paste0(c(outcome, mediator)), names.covar.nofactor, names.covar.factor,
                                                              paste0(c(names.covar.nofactor,names.covar.factor),".star"))]
    if (is.null(ID)) {df.dupl$ID <- rep(1:n.row,2)}
  }
  } else {
    {
print("survival is yet to be included")
    }

#    df.return <- data.frame(IDdupl, model,  Xdupl1, Xdupl2, Mdupl, casedupl, Timedupl)
    }

  return(df.dupl)
}
