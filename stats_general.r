
library(data.table)
# HH_QUANTILES -------------------------------------------------------------

hh_list <- as.list(1:48)
hh_quantiles <- lapply(hh_list, function(hh_el){
	vals <- dat[hh == hh_el, c(-1,-2), with = F]
	vals <- as.vector(as.matrix(vals))
	return(quantile(x = vals,probs = seq(0,1,0.1)))
})

hh_quantiles <- matrix(unlist(hh_quantiles), byrow = F, ncol = 48)
matplot(t(hh_quantiles[-11,]), type = 'l')