GEEMediateFit <- function(formula, data, exposure, mediator,
                          surv = F, family = gaussian, corstr  = "independence",...)
{

  outcome.name <- all.vars(formula)[1]
  dupl.df <- DupliData(df = data, mediator = mediator, outcome = outcome.name,  surv = F)
  dupl.formula <- as.formula(  paste(outcome.name, paste(colnames(dupl.df)[!(colnames(dupl.df)%in%c(outcome.name,"ID"))], collapse=" + "), sep=" ~ "))
  dupl.formula <- update(dupl.formula, ~. -1)
  sink("NUL")
  dupl.fit <-  gee::gee(formula = dupl.formula, id = ID, data = dupl.df, family = family,
                        corstr = corstr)
  sink()
  return(dupl.fit)
}

