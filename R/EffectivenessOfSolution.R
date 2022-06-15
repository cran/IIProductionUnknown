#' Function to estimate the effectiveness of solution sources (S.S.) by loss source (Percentage_I.I. > 0.00) in the production system.
#' @description This function allows to calculate E.S. of each S.S. by L.S. (significant in the reduction of defoliation or damage) in
#' the system. Equation: E.S. = R2 x (1 - P) when it is of the first degree, or E.S. = ((R2 x (1 - P))x(B2/B1) when
#' it is of the second degree. Where, R2 = determination coefficient and P = significance of ANOVA, B1 = regression coefficient,
#' and B2 = regression coefficient (variable2), of the simple regression equation of the S.S..
#' @usage EffectivenessOfSolution (DataLossSource,DataSolutionSource,ResultLossSource, verbose=TRUE)
#' @param DataLossSource It is an matrix object containing data from loss sources.
#' @param DataSolutionSource It is an matrix object containing data from solution sources.
#' @param ResultLossSource Output of LossSource function.
#'@param verbose Logical value (TRUE/FALSE). TRUE displays the results of the effectiveness of solution
#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)



#' @return The function returns several indices associated with the loss source.
#' @seealso  \code{\link{ReductionDamage}} ,  \code{\link{SolutionSource}}
#' @importFrom stats anova chisq.test coefficients cor pf predict
#' @importFrom crayon green
#' @export


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






EffectivenessOfSolution=function(DataLossSource,DataSolutionSource,ResultLossSource, verbose=TRUE){

  D1=DataLossSource
  D2=DataSolutionSource

B2=Escolhido="None"

if(is.null(ResultLossSource)==FALSE){
  id=ResultLossSource[[2]][,8]>0
  D1b=as.matrix(D1[,id],nrow=nrow(Prod))
}

if(is.null(ResultLossSource)==TRUE){
  D1b=as.matrix(D1[,],nrow=nrow(Prod))
}



es=NULL
D=D2
for(i in 1:ncol(D)){
  for(j in 1:ncol(D1b)){
    x=D[,i]
    Prod=as.matrix(D1b[,j])
    m=lm(Prod~x)
    m2=lm(Prod~x+I(x^2))

    ANOVA=anova(m)
    ANOVA2=anova(m2)


    ms1=summary(m)
    ms2=summary(m2)

    if(verbose==TRUE){
      cat(green("################################################ \n"))
      cat(green(colnames(D)[i]," VS ",colnames(D1b)[j],"\n"))
      cat(green("################################################ \n"))
      print(ms1)
      cat(green("_______________________________________________ \n"))
      print(ms2)





    }

    pvf1=1-pf(ms1$fstatistic[1],1,nrow(D)-2)
    pvf2=1-pf(ms2$fstatistic[1],2,nrow(D)-3)

    if(nrow(coefficients(ms2))==2){pvf2=1}

    hipo=sum(pvf1<=0.05,pvf2<=0.05)
    util=hipo>0


    if(util==TRUE){
      if(hipo==1){
        if(pvf1<=0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))
          if(B2>0){res=-res}
          Escolhido="Linear"

        }

        if(pvf2<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))
          Escolhido="Quadratic"
        }

      }
      if(hipo==2){
        pv1= ms1$coefficients[,4]
        pv2= ms2$coefficients[,4]
        if(pv2[length(pv2)]<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))
          param=FALSE
          Escolhido="Quadratic"
          if((B2>0)&(B1<0)){
            ANOVA=anova(m)
            p=pvf1
            R2=cor(predict(m),Prod)^2
            B1=coefficients(m)[1]
            B2=coefficients(m)[2]

            res=abs(R2*(1-p))
            Escolhido="Linear"
            if(B2>0){res=-res}

            param=TRUE
          }


        }
        if(pv2[length(pv2)]>0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))
          if(B2>0){res=-res}
          Escolhido="Linear"


        }

      }

    }




    if(is.na(B2)){B2=100}
    #if((B2>0)&(positivo==FALSE)){util=FALSE}
    if(util==FALSE){res=0}

    res=c(colnames(D2)[i],colnames(D1b)[j],round(res,6))

    es=rbind(es,res)

    if(verbose==TRUE){
      cat(green("_______________________________________ \n"))
      cat(paste0("Chosen: ",Escolhido,"\n"))
    }


  }
}

X= data.frame(es)
colnames(X)=c("InimigoNatural","Praga","ES")
X
}
