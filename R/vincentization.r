#' @title Vincentization
#'
#' @description This function vincentize da data frame
#' @param dataframe the data frame  df containing data
#' @param rt response time, ex: df$rt
#' @param accuracy accuracy, 1 for correct trials, 0 for errors. Ex: df$accuracy
#' @param var list variables within which vincentization will be performed
#' @param quantiles vector of quantiles used for vincentization
#' @keywords vincentization
#' @export
#' @examples
#'  vincentize(data,data$rt,data$acc,list(sujet=data$sujet, compatible=data$compatible),c(0.2,0.4,0.6,0.8))
#' compute the means for quantiles (0-0.2,0.2-0.4,0.4-0.6,0.6-0.8,0.8-1)
#' for the variables data$rt and data$acc 
#' for each Sujet, in each modalities of compatible
#' return new variables:
#'  q = quantile
#' nq = proportion of trials within quantile
#' rt_m = means of response time by quantile
#' accuracy_m = means of accuracy by quantile

vincentize <- function (dataframe, rt, accuracy = NA, var, quantiles) {
  if (is.na(accuracy[1])){accuracy <- rep(1,length(rt))}  
  n <- length(quantiles)
  q <- paste('q', 1:n, sep='')
  b <- paste("b", 1:n,sep='')


  for (i in (1:n)) {
    b <- aggregate(list(q=rt),by=var,FUN = quantile, probs  = quantiles[i])
    names(b)[names(b)=="q"] <- paste('q',i,sep='')
    assign(paste('b',i,sep=''),b)
  }

  data1 <- dataframe
  data1$rt <- rt
  data1$accuracy <- accuracy

  for (i in (1:n)) {
    expr <- substitute(paste('merge(data1,','b',k,',all=TRUE,sort=FALSE)',sep=''),list(k=i))
    chaine <- eval(expr)
    expr_chaine <- parse(text = chaine)
    data1 <- eval(expr_chaine)
  }

  data1$q <- 0
  data1$q0 <-0

  for (i in (0:n-1)) {
    expr <- substitute(
      paste('data1$q[data1$rt>+data1$q',k,'] <-',k+1,sep=''),list(k=i))
    chaine <- eval(expr)
    expr_chaine <- parse(text = chaine)
    eval(expr_chaine)
  }



  data1$nq <- 0
  for (i in (1:n)) {
    data1$nq[data1$q==i] <- length(data1$q[data1$q==i])/length(data1$rt)
  }

  expr <- paste('list(')
  for (i in (1:length(var))){
    expr <- paste(expr,names(var[i]),'=','data1$',names(var[i]),',',sep='')
  }
  expr <- paste(expr,'q=data1$q)',sep='')
  chaine <- eval(expr)
  expr_chaine <- parse(text = chaine)
  var2 <- eval(expr_chaine)
  data_final <- aggregate(list(rt_m=data1$rt, accuracy_m=data1$accuracy,nq=data1$nq), by=var2, FUN = mean)

  return(data_final)
}
