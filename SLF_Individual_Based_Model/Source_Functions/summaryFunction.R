#summary function creates table of n, mean, var, SD, and SE
summaryFunction <- function(DataIn, factor, response){
  require(plyr)
  summaryOut <- ddply(DataIn, factor, .fun = function(xx){
    c(n = length(xx[,response]),
      mean = mean(xx[,response],na.rm=TRUE),
      var = var(xx[,response],na.rm=TRUE),
      SD = sd(xx[,response],na.rm=TRUE),
      SE = sqrt(var(xx[,response])/length(xx[,response])))
  })
  return(summaryOut)
  dev.off()
}
