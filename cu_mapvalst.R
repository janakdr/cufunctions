cu_mapvalst = function(pval, pvcuts)
  return(ifelse(pval>pvcuts[2],"ns", ifelse(pval>pvcuts[3],"*",
         ifelse(pval>pvcuts[4],"**", ifelse(pval>pvcuts[5],"***","****")))))
