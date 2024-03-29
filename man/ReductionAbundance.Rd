% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReductionAbundance.R
\name{ReductionAbundance}
\alias{ReductionAbundance}
\title{Estimate of the abundance reduction}
\usage{
ReductionAbundance(ResultSolutionSource,ResultLossSource,EffectivenessOfSolution)
}
\arguments{
\item{ResultSolutionSource}{Output of the SolutionSource function.}

\item{ResultLossSource}{Output of the LossSource function.}

\item{EffectivenessOfSolution}{Output of the EffectivenessOfSolution function.}
}
\value{
The function returns the estimate of the reduction in abundance (loss source) due to solution source.
}
\description{
Function to estimate of the abundance reduction.
In this function you are asked to indicate 'y' or 'n' in order to define
whether the association of the source of solution with the source of loss is important.
}
\examples{
data("DataLossSource")
ChisqTest_Distribution(DataLossSource)

data("DataSolutionSource")
ChisqTest_Distribution(DataSolutionSource)

data("DataDefoliation")
data("DataDamage")

DataResult<-cbind(DataDefoliation,DataDamage$D.L.S.2,DataDefoliation,
DataDamage$D.L.S.4,DataDefoliation)
ResultLossSource<-LossSource(DataLoss = DataLossSource,DataResult =DataResult,
Cols=c(1,3,5),verbose=TRUE)

EOS<-EffectivenessOfSolution(DataLossSource =DataLossSource,
                            DataSolutionSource =DataSolutionSource,
                            ResultLossSource = ResultLossSource)

EOS
#Put: y and y
# ID=SelectEffectivenessOfSolution(EOS)
ID<-c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
ResultSolutionSource<-SolutionSource(SolutionData =DataSolutionSource,Production =DataResult,
                                    EffectivenessOfSolution =EOS ,Id = ID,Verbose = TRUE  )
ResultSolutionSource

# Put: y,n,y,n,y,n and y
# ReductionAbundance(ResultSolutionSource,ResultLossSource,
#                  EffectivenessOfSolution=EOS)

###################################################
EOSDamage<-EffectivenessOfSolution(DataLossSource =DataDamage,
                                  DataSolutionSource =DataSolutionSource,
                                  ResultLossSource = NULL)


EOSDamage

# Put: y, n and y
#ReductionDamage(ResultSolutionSource,LossSource=DataDamage,
#                EffectivenessOfSolution=EOSDamage)

}
\seealso{
\code{\link{EffectivenessOfSolution}} ,  \code{\link{SolutionSource}}
}
\author{
Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
}
