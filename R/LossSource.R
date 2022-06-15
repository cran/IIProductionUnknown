#'Obtaining indices associated with sources of loss
#'@description These functions allow to calculate the total n of the L.S. (n),
#' R.P., ks, c, ds, n.I.I., Sum.n.I.I., and percentage of I.I. (P.I.I.) by each L.S..\cr
#'Equations:
#'R.P. = Damage or defoliation \cr
#'n=total n per sample \cr
#'k.s.= R.P./n \cr
#'c = SUM of occurrence of L.S. on the samples, where, absence = 0 or presence = 1.\cr
#'ds = 1 - P of the chi-square test of L.S. on the samples.\cr
#'n.I.I.=ks x c x ds \cr
#'Sum.n.I.I. = sum of all n.I.I.\cr
#'Percentage of I.I. (P.I.I.)=(n.I.I. of each L.S./sum of all n.I.I.)*100

#'@usage LossSource(DataLoss,DataResult,Cols=c(1,3,5),verbose)
#'@param DataLoss It is an matrix object containing data from loss sources.
#'@param DataResult Matrix with loss sources.
#'@param Cols Most important data loss columns.
#'@param verbose Logical value (TRUE/FALSE). TRUE displays the results of the analysis.

#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return The function returns several indices associated with the loss source.
#'@seealso  \code{\link{EffectivenessOfSolution}} ,  \code{\link{SolutionSource}}



#'@importFrom stats lm
#'@export


#'@examples
#\dontrun{
#' data("DataLossSource")
#' ChisqTest_Distribution(DataLossSource)
#'
#' data("DataSolutionSource")
#' ChisqTest_Distribution(DataSolutionSource)
#'
#' data("DataDefoliation")
#' data("DataDamage")
#'
#' DataResult=cbind(DataDefoliation,DataDamage$D.L.S.2,DataDefoliation,
#' DataDamage$D.L.S.4,DataDefoliation)
#' ResultLossSource=LossSource(DataLoss = DataLossSource,DataResult =DataResult,
#' Cols=c(1,3,5),verbose=TRUE)
#'
#' EOS=EffectivenessOfSolution(DataLossSource =DataLossSource,
#'                             DataSolutionSource =DataSolutionSource,
#'                             ResultLossSource = ResultLossSource)
#'
#' EOS
#' #Put: y and y
#' # ID=SelectEffectivenessOfSolution(EOS)
#' ID=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
#' FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)

#' ResultSolutionSource=SolutionSource(SolutionData =DataSolutionSource,Production =DataResult,
#'                                     EffectivenessOfSolution =EOS ,Id = ID,Verbose = TRUE  )
#' ResultSolutionSource
#'
#' # Put: y,n,y,n,y,n and y
#' # ReductionAbundance(ResultSolutionSource,ResultLossSource,
#' #                  EffectivenessOfSolution=EOS)
#'
#' ###################################################
#' EOSDamage=EffectivenessOfSolution(DataLossSource =DataDamage,
#'                                   DataSolutionSource =DataSolutionSource,
#'                                   ResultLossSource = NULL)
#'
#'
#' EOSDamage
#'
#' # Put: y, n and y
#' #ReductionDamage(ResultSolutionSource,LossSource=DataDamage,
#' #                EffectivenessOfSolution=EOSDamage)
# }
#'


LossSource=function(DataLoss,DataResult,Cols=c(1,3,5),verbose){
  Prod=DataResult
  Prod0=DataResult
  D=DataLoss
  n=colSums(D)

  RP=R.P(D,Prod,verbose=verbose)
 # RP
  Cols=c(1,3,5)
  DataDefoliation2=DataLoss[,Cols]
  Sum=colSums(DataDefoliation2)
  TotalSum=sum(colSums(DataDefoliation2))
  Proportion= Sum/ TotalSum
  Average=mean(unlist(DataDefoliation2))/100
  ProportionPer_LS=Proportion*Average

  Result1=cbind(Sum,TotalSum,Proportion,Average,ProportionPer_LS)


  RP[Cols][RP[Cols]==0]=Result1[RP[Cols]==0,5]

  KS=RP/n
  c=colSums(D>0)

  ds=1-suppressWarnings(apply(D,2,function(x) chisq.test(x)$p.value))

  n.I.I=ds*c*KS
  Sum_n.I.I=sum(n.I.I)
  Perc_I.I=100*n.I.I/Sum_n.I.I

  Result_LossSource=cbind(n,Da=RP,KS,c,ds,n.I.I,Sum_n.I.I,Perc_I.I)
  if(verbose==TRUE){
    print(Result1)
    print(Result_LossSource)

  }

  return(list(Res1=Result1,Res2=Result_LossSource))

  }


