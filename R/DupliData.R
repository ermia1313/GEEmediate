######
#
#DupliData function expect a data frame, a single expousre (X), a mediator (M) and outcome (Y), and unless told other wise
# it uses
# Set alter to be T if to want the duplicate observarions to be next ot each other
# that is Id = 1 1 2 2 and so on
#  - end data
######

DupliData <- function(df, mediator = NULL, outcome = NULL, case = NULL , Time = NULL,  surv = F, ID = NULL)
{
  if (surv==T & (is.null(case) | is.null(Time))) {stop("For survival data, both case and time are needed")}
  #if (surv==F & (is.null(outcome) | is.null(Time))) {stop("For survival data, both case and time are needed")}

  if (surv == F) {
  {
    n.row <- nrow(df)
    df.star <- df
    df.star[[paste0(mediator)]] <- rep(0,n.row)
    names.covar <- names(df)[!(names(df) %in% c(outcome,mediator))]
    names(df.star)[!(names(df) %in% c(outcome,mediator))] <- paste0(names.covar,".star")

    for (var.name in names.covar)
    {
      df[[paste0(var.name,".star")]]  <- rep(0,n.row)
      df.star[[paste0(var.name)]] <- rep(0,n.row)
    }
    df$INT<- df.star$INT.star <- c(rep(1,n.row))
    df$INT.star <- df.star$INT <- c(rep(0,n.row))
    df.dupl <- rbind(df[c("INT","INT.star", names.covar, paste0(names.covar,".star"),
                          paste0(c(mediator,outcome)))],
                     df.star[c("INT","INT.star", names.covar, paste0(names.covar,".star"),
                               paste0(c(mediator,outcome)))])
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
