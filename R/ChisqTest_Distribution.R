#'Loss and solution sources distribution informations
#'@description Indicates the distribution of loss and solution sources: aggregate, random or regular.
#'@usage ChisqTest_Distribution(Data)
#'@param Data It is a data frame object containing data from total number per repetition of loss source and solution source.
#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return Return distribution of loss and solution sources: aggregate, random, or regular. This information is important to check
#' whether the problem or solution occurs randomly or not. This has an impact on the decision making associated with increasing
#' or reducing of the problem.
#'@seealso  \code{\link{EffectivenessOfSolution}} ,  \code{\link{LossSource}}  ,  \code{\link{ReductionDamage}}
#'@importFrom stats lm var
#'@export
#'@examples
#\dontrun{
#' data("DataLossSource")
#' ChisqTest_Distribution(DataLossSource)
#'
#' data("DataSolutionSource")
#' ChisqTest_Distribution(DataSolutionSource)


ChisqTest_Distribution=function(Data){
  SolutionData=Data
  verbose=FALSE

  D=SolutionData
  n=colSums(D)
  pv=Class.=NULL
  for(i in 1:ncol(D)){
    chisq=suppressWarnings(chisq.test(D[,i]))
    pv=c(pv,chisq$p.value)
    pv2=100*(chisq$p.value)
    class="Random"
    if(pv2<2.5){
      class="Aggregated"
    }

    if(pv2>97.5){
      class="Regular"
    }

    Class.=c(Class.,class)
  }
  Var=apply(D,2,FUN = "var")
  Mean=apply(D,2,mean)
  p.Value=pv
  Res2=data.frame(Var=Var,Mean=Mean,p.Value=p.Value,Distribution=Class.)

  return(Res2=Res2)
}
