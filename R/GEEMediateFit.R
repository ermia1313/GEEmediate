#' @importFrom stats pnorm qnorm
GEEmediateFit <- function(formula, df, exposure, mediator,
                          surv = F, family = gaussian, corstr  = "independence", ID = ID,...)
{
  outcome.name <- all.vars(formula)[1]
#  ID <- NULL
  df.relevant <- model.frame(formula = formula, data =  df)
  dupl.df <- DupliData(df = df.relevant, mediator = mediator, outcome = outcome.name,  surv = F)
  dupl.formula <- as.formula(  paste(outcome.name, paste(colnames(dupl.df)[!(colnames(dupl.df)%in%c(outcome.name,"ID"))], collapse=" + "), sep=" ~ "))
  dupl.formula <- update(dupl.formula, ~. -1)
#  sink("NUL")
#  print("You may ignore the following message and vector output")
  dupl.fit <-  gee::gee(formula = dupl.formula, id = ID, data = dupl.df, family = family,
                        corstr = corstr)
 # sink()
  return(dupl.fit)
}

