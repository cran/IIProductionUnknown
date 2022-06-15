#'Estimate of the abundance reduction
#'@description Function to estimate of the abundance reduction
#'@usage ReductionAbundance(ResultSolutionSource,ResultLossSource,EffectivenessOfSolution)
#'@param ResultSolutionSource Output of the SolutionSource function.
#'@param ResultLossSource Output of the LossSource function.
#'@param EffectivenessOfSolution Output of the EffectivenessOfSolution function.

#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return The function returns the estimate of the reduction in abundance.
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






ReductionAbundance=function(ResultSolutionSource,ResultLossSource,
                            EffectivenessOfSolution) {
EOS=EffectivenessOfSolution


  #  colSums(DataDamage)






  n1=ResultSolutionSource$Res1[as.numeric(as.factor(EOS[,1])),1]
  n2=ResultLossSource[[2]][as.numeric(as.factor(EOS[,2])),1]

  RLSSS.abun=as.numeric(EOS[,3])*n1
  Perc_RLSSS.abun=100*RLSSS.abun/n2
  Res1=cbind(EOS,n1,RLSSS.abun,Perc_RLSSS.abun)
  val=round(Res1[,6],5)
  val[val==0]="."
  res2=data.frame(t(matrix(val,nrow=nrow(ResultLossSource[[2]]))))
  colnames(res2)=rownames(ResultLossSource[[2]])
  rownames(res2)=rownames(ResultSolutionSource$Res1)

  for(i in 1:nrow(res2)){
    for(j in 1:ncol(res2)){
      if(res2[i,j]!="."){
     Pergunta=paste0("Is the association of ", rownames(res2)[i], " with ",
             colnames(res2)[j], "  important (",res2[i,j],")? (y/n): ")
     bk=readline(prompt = Pergunta)

     if(bk=="n"){res2[i,j]="."}
    }
    }
  }

    res2b=res2

    res2b[res2=="."]=NA

    res2b=data.frame(res2b)

    Sum=apply(res2b,2, function(x) sum(na.omit(as.numeric(x))))
    res2c=rbind(res2,Partial_Sum=Sum)
  Total_Sum=sum(na.omit(as.numeric(unlist(res2b))))

  list(Result=res2c, Total_Sum=Total_Sum)
}
