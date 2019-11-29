#' @title delta
#'
#' @description This function compute classical delta plots
#' @param rt Response time
#' @param compatible compatible vs incompatible trials, 'c' for compatible trials, 'i' for incompatible
#' @param sujet subject names. Default is NULL
#' @param quant quantiles
#' @keywords dela plot
#' @export
#' @examples
#' delta(df$rt,  df$compatible, df$sujet, quant = c(1:5)/5) 
#' Return L(p)  proportion of errors among the pth first quantile of response time (computed on all responses)


delta <- function(rt,compatible,sujet = NULL, quant = c(1:5)/5){
    assign('compatible',compatible)
    assign('rt',rt)
    if (is.null(sujet)){sujet <- rep('x',length(rt))}
    assign('sujet',sujet)
    data <- data.frame(rt,compatible,sujet)
    data.in <-  data[data$compatible == 'i',]
    data.co <- data[data$compatible == 'c' ,]
    vin.co <- vincentize(dataframe = data.co, rt = data.co$rt, var= list(compatible = data.co$compatible, sujet= data.co$sujet),quantiles = quant)
    colnames(vin.co)[colnames(vin.co)=="rt_m"] <- "rt.co"
    vin.co <- vin.co[c('sujet','q','rt.co')]
    vin.in <- vincentize(dataframe = data.in, rt =  data.in$rt, var = list(compatible = data.in$compatible, sujet = data.in$sujet),quantiles = quant)
    colnames(vin.in)[colnames(vin.in)=="rt_m"] <- "rt.in"
    vin.in <- vin.in[c('sujet','q','rt.in')]   
    dv <-  merge(vin.co, vin.in, by = c('q','sujet'))
    dv$delta <- dv$rt.in - dv$rt.co
    dv$rt <- (dv$rt.co + dv$rt.in)/2
    return(dv)
}



## compute continuous delta plot
qdelta <- function (rt, compatible){
    data <- data.frame(rt = rt, compatible = compatible)
    dc <- data %>%
        filter(compatible == 'c') 
    di <- data %>%
        filter(compatible == 'i')
    n  <- min(nrow(dc),nrow(di))
    qc <- quantile(dc$rt, c(1:n)/n)
    qi <- quantile(di$rt, c(1:n)/n)    
    qtot <- data.frame(p = c(1:n)/n, qc = qc, qi = qi) %>%
        mutate(delta = qi - qc) %>%
        mutate(m = (qi+qc)/2)
    return(qtot)
}

## ** Lorenz-Delta plots and inhib index
#' @title Lorenz-Delta
#'
#' @description This function compute  Lorenz-Dela plot
#' @param rt Response time
#' @param comp compatible vs incompatible trials, 'c' for compatible trials, 'i' for incompatible
#' @keywords Lorenz-Delta plots
#' @export
#' @examples lorenz(rt, comp)
#' Return  inhibition index and Lorenz-Delta plot

lorenz  <- function(rt, comp){
    q  <- qdelta(rt, comp) %>%
        mutate(c = cumsum(delta)/sum(delta))
     inhib  <-   2*MESS::auc(q$p, q$c)-1
     out  <- list('inhib' = inhib, 'q' = q)
    return(out)
}

#' @title inhibibition indices
#' @description This function compute inhibition indices
#' @param data dataframe with rt, comp,  sujet, cond
#' @param dquantile number of quantiles used to compute the slope of last segment
#' @keywords dela plot inhibition index
#' @export
#' @examples
#' inhib.delta(rt, comp, cond, dquantile = 20) 
#' rt: response time
#' comp: compatible vs incompatible ('c' vs 'i')
#' sujet: subjects id (transformed into factor if needed)
#' cond: experimental conditions (transformed into factor if needed)
#' dquantile: number of quantiles to compute last segment slope and linear approximation
#' Output: last segment delta slope, delta slopes, linear approximation (trend = coefficient, intercept), inhibition index
#' q, delta.slope, index

inhib.delta <- function(rt,  comp, sujet = NA, cond = NA, dquantile = 20){
    ## data
    if (is.na(cond[1])){cond <- rep('C',length(rt))}
    if (is.na(sujet[1])){sujet <- rep('A',length(rt))}
    data <- data.frame(rt = rt,  comp = comp, sujet = as.factor(sujet), cond = as.factor(cond))
    
    ## inhib
    i  <- data.frame(sujet = factor(), cond = factor(), index = numeric())
    for (s in unique(data$sujet)){
        for (c in unique(data$cond)){
            d  <- data %>%
                filter(sujet == s, cond == c)
            l  <- lorenz(d$rt, d$comp)$inhib[1]
            i  <-  add_row(i, sujet = s, cond = c, index = l)
            }
    }
    delta.slope  <- data.frame(sujet = character(), cond = character(), slope = numeric())
    for (c in unique(data$cond)){
        data0  <- data %>% filter(cond == c)
        d  <- with(data0, delta(rt = rt, compatible = comp, sujet = sujet, quant = (1:dquantile)/dquantile)) %>%
            mutate(slope = 0)
        l <- lm(delta ~ rt, data = d)
        for (s in unique(data$sujet)){
        d[d$sujet == s,]$slope  <-
            (d[d$sujet == s  & d$q == dquantile,]$d - d[d$sujet == s & d$q == dquantile-1,]$d)/
            (d[d$sujet == s & d$q == dquantile,]$rt - d[d$sujet == s & d$q == dquantile-1,]$rt)
        }
        
        d  <- d %>%
            filter(q == 1) %>%
            select(sujet, slope) %>%
            mutate(cond = c) %>%
            mutate(trend = summary(l)$coefficients[2]) %>%
            mutate(intercept =  summary(l)$coefficients[1]) %>%
            mutate(delta.slope = rbind(delta.slope, d))
    }
    return(list('i'= i,  'delta.slope' = delta.slope))
}
