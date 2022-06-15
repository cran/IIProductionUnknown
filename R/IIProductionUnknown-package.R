#' Analyzing data through of percentage of importance indice-production unknow and its derivations.
#'
#'
#'
#'
#' @description  The Percentage of Importance Indice-production unknow (Percentage_I.I.P.U.) bases in magnitudes, frequencies,
#' and distributions of occurrence of an event. This index can detect the key loss sources (L.S)
#'  and solution sources (S.S.), classifying them according to their importance in terms of
#'  damage or damage reduction in the system. The
#'  Percentage_I.I.P.U. = ((ks1 x c1 x ds1)/SUM(ks1 x c1 x ds1) + (ks2 x c2 x ds2) + (ksn x cn x dsn)) x 100.
#'  key source (ks) is obtained using simple regression analysis and magnitude (abundance). Constancy (c)
#'  is SUM of occurrence of L.S. or S.S. on the samples (absence = 0 or presence = 1),
#'   and distribution source (ds) is obtained using chi-square test.
#'   This index has derivations: i.e., i) Reduction of the total n. of the L.S. (R.L.S.)/Total n. of the solution source  and
#'   ii) Percentage of the R.L.S. per S.S..
#'
#' @docType package
#' @name IIProductionUnknown package
#' @aliases IIProductionUnknown
#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)

#' @references
#' Demolin-Leite, G. L. (2024), Percentage of importance indice-production unknown:
#' loss and solution sources identification on system. Brazilian Journal of Biology 84, e253218.
#'  <https://doi.org/10.1590/1519-6984.253218>.

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



NULL
