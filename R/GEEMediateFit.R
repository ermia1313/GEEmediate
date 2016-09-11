GEEMediateFit <- function(formula = formula(data), exposure, mediator, data = parent.frame(),surv = F, )
{
  outcome.name <- all.vars(formula)[1]
  dupl.df <- DupliData(df = data, mediator = mediator, outcome = outcome.name,  surv = F)
  dupl.formula <- as.formula(  paste(outcome.name, paste(colnames(dupl.df)[!(colnames(dupl.df)%in%c(outcome.name,"ID"))], collapse=" + "), sep=" ~ "))
  dupl.formula <- update(dupl.formula, ~. -1)
  dupl.fit <- gee::gee(formula = dupl.formula, id = ID, data = dupl.df)
  return(dupl.fit)
}

