#' @title ELF
#'
#' @description This function compute Error Location Function
#' @param rt Response time
#' @param accuracy accuracy, 1 for correct trials, 0 for errors. Ex: df$accuracy
#' @param na.rm should missing data be disregarded (if there is no error). Default is TRUE
#' @keywords Error Location Function
#' @export


elf <- function (rt, accuracy, na.rm = TRUE) {
  p=(1:length(rt))/length(rt)
  errors <- rt[accuracy==0]
  correct <- rt[accuracy==1]
    if (length(correct) == length(rt)){ L <- NA }
  else{
  c_error <- ecdf(errors)
  L <- c_error(quantile(rt,probs = p, na.rm = na.rm))
  return(L)
  }
}


#' @title ELI
#'
#' @description This function compute Error Location Index
#' @param L an ELF function
#' @param alpha weight of ELI function. Default is 1. The greater alpha, the more weight is given to fast responses
#' @keywords Error Location Index
#' @export
#' @examples
#' eli(L) 

## ** response capture index
eli <- function (L, alpha = 1, na.rm = FALSE){
  n <- length(L)
  a <- round(alpha*n)
  LL <- L[1:a]
  V <- (1/n)*(1/alpha)*sum(LL, na.rm = na.rm)
  return(V)
}


#' @title ELF group
#'
#' @description This function compute aggregated  Error Location Index for groups
#' @param RT response time
#' @param accuracy accuracy, 1 for correct trials, 0 for errors. Ex: df$accuracy
#' @param sujet name of subjects Ex: df$sujet
#' @keywords Error Location Index
#' @export
#' @examples
#' elf.g(df$RT, df$accuracy, df$sujet) 

elf.g <- function(RT,accuracy,sujet, na.rm = FALSE){
        df <- data.frame(RT, accuracy, sujet)
        suj <- unique(sujet)
        ns <- length(suj)
        y <- 0
        for (s in suj) {
                d <- df[df$sujet == s,]
                x  <- elf(d$RT, d$accuracy) %>%  quantile(.,c(1:100)/100,  na.rm = na.rm)
                if (is.na(x[1])  == TRUE){
                    x <- 0
                    ns <- ns -1
                }
                y <- y+x       
                        }
        y <- c(0,y/ns)
        return(y)
}
