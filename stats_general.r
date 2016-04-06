
library(data.table)
library(scales)
setkey(x = dat, day, hh)
hhs_list <- as.list(1:48)
days_list <- as.list(1:364) # needs to be repaired
quant_by <- 0.1
times_vec <- cbind(rep(0:23, each = 2),rep(c('00','30'), times = 24))
times_vec <- unlist(lapply(as.list(1:48), function(i){
		return(paste(times_vec[i,], collapse=":"))
}))

# MEAN SD plot ------------------------------------------------------------

dat.mat <- as.matrix(dat[day != 0,3:dim(dat)[2],with =F])
dat.mat <- matrix(as.numeric(dat.mat), nrow = 48*7)
dat.mat <- colSums(dat.mat)
dat.mat2 <- matrix(dat.mat,nrow = 4640,ncol = 52,byrow = F)
sds <- apply(dat.mat,1,sd)
means <- apply(dat.mat,1,mean)
pdf(file = '../img/Mean_SD.pdf',width = 10, height = 8)
par(cex=1.5)
plot(means,sds, col= alpha('blue', 0.4),pch=16, 
     xlab = 'Mean Weekly Usage (kWh)', ylab = 'Standard Deviation')
dev.off()
rm(dat.mat)

# HH_QUANTILES -------------------------------------------------------------
quant_by = 0.1
hhs_quantiles <- lapply(hhs_list, function(hh_el){
	vals <- dat[hh == hh_el, c(-1,-2), with = F]
	vals <- as.vector(as.matrix(vals))
	return(quantile(x = vals,probs = seq(0,1-quant_by,quant_by)))
})
hhs_quantiles <- matrix(unlist(hhs_quantiles), byrow = F, ncol = 48)
matplot(hhs.vec, t(hhs_quantiles), 
	type = 'l',
	xlab = 'Time of Day (HHs)', ylab = 'Quantile of consumptions (by 10%)')

hhs_sds <- lapply(hhs_list, function(hh_el){
	vals <- dat[hh == hh_el, c(-1,-2), with = F]
	vals <- as.vector(as.matrix(vals))
	return(sd(vals))
})
hhs_sds <- unlist(hhs_sds)
plot(hhs.vec, hhs_sds, 
     type = 'b', pch = 4,
     xlab = 'Time of Day (HHs)', ylab = 'Standard Deviation of consumption')

hhs_means <- lapply(hhs_list, function(hh_el){
	vals <- dat[hh == hh_el, c(-1,-2), with = F]
	vals <- as.vector(as.matrix(vals))
	return(mean(vals))
})
hhs_means <- unlist(hhs_means)
plot(hhs.vec, hhs_means, 
     type = 'b', pch = 4, xaxt = "n",
     xlab = 'Time of Day (Hour)', ylab = 'Mean consumption (kWh)')
axis(side = 1, at = seq(0,24,6))

# GRINDROD'S TYPE OF PLOT -------------------------------------------------
min.vals <- as.list(seq(0.9,11,0.2))

days_maxs <- lapply(days_list, function(day_el){
	vals <- dat[day == day_el, c(-1,-2), with = F]
	vals <- as.matrix(vals)
	#max.vals <- apply(X = vals, MARGIN = 2, FUN = max, na.rm = T)
	max.val.ind <- apply(X = vals, MARGIN = 2, FUN = function(col_el){
			max.val <- max(col_el)[1]
			max.ind <- which(col_el == max.val)[1]
			return(list(max.val, max.ind))
	})
#	max.val.ind <- unlist(max.val.ind, recursive = F)
	return(max.val.ind)
})
days_maxs <- unlist(days_maxs, recursive = F)
days_maxs <- matrix(unlist(days_maxs), byrow = T, ncol = 2)

hhs_grinds <- lapply(hhs_list, function(hh_el){
	max.vals <- days_maxs[days_maxs[,2] == hh_el,1]
	max.ind <- hh_el
	max.proports <- lapply(min.vals, function(min.val){
		return(sum( max.vals <= min.val ))	
	})
	return(max.proports)
})

hhs_grinds <- matrix(unlist(hhs_grinds), byrow = T, ncol = length(min.vals))
hhs_grinds <- hhs_grinds/rowSums(hhs_grinds)
matplot(hhs.vec, hhs_grinds, type = 'l', xaxt = "n", 
	lwd = 2,
	xlab = "Time of Day", 
	ylab = "Proportion of Peaks")
axis(1, at = hhs.vec, labels = times_vec)

# GRINDROD 2 --------------------------------------------------------------
min.vals <- as.list(seq(0.9,11,0.2))
quant_by = 0.1
greater.prop <- lapply(as.list(1:(7*48)), function(list_el){
	vals <- dat.mat[list_el,]
	vals <- as.matrix(vals)
	greater.num <- quantile(x = vals,
				probs = seq(0,1-quant_by,quant_by))
	#greater.num <- lapply(min.vals, function(min.val){
	#	return(sum(vals <= min.val)/length(vals))	
	#})
	return(unlist(greater.num))
})

greater.prop.mat <- matrix(unlist(greater.prop), 
			   byrow = T, 
			   ncol = length(seq(0,1-quant_by,quant_by)))

pdf(file = '../img/Quantiles.pdf',width = 10, height = 8)
par(cex=1.5)
matplot(1:(7*48), greater.prop.mat, type = 'l', xaxt = "n", 
	xlab = "Time of Day", 
	ylab = "Quantile values (by 10%)", lty = 1, col = 'black')
# legend('topleft', 
#        legend = paste('>=', unlist(min.vals)[seq(1,51,by = 10)])
#        )
#axis(1, at = hhs.vec, labels = times_vec)
axis(1, at = (1:7)*48, labels = F)
axis(1, at = (1:7)*48-24, labels = weekdays(2:8),tick = F)
dev.off()


# SEASONS -----------------------------------------------------------------
winter.ind <- c(1:63,344:364)
spring.ind <- c(64:154)
summer.ind <- c(155:252)
autumn.ind <- c(253:343)

dat.winter.mat <- as.matrix(dat[day %in% winter.ind,c(-1,-2), with =F])
dat.winter.mat <- matrix(as.numeric(dat.winter.mat), nrow = 48*7)

dat.spring.mat <- as.matrix(dat[day %in% spring.ind,c(-1,-2), with =F])
dat.spring.mat <- matrix(as.numeric(dat.spring.mat), nrow = 48*7)

dat.summer.mat <- as.matrix(dat[day %in% summer.ind,c(-1,-2), with =F])
dat.summer.mat <- matrix(as.numeric(dat.summer.mat), nrow = 48*7)

dat.autumn.mat <- as.matrix(dat[day %in% autumn.ind & hh %in% (1:48),c(-1,-2), with =F])
dat.autumn.mat <- matrix(as.numeric(dat.autumn.mat), nrow = 48*7)

quant.winter <- lapply(as.list(1:(7*48)), function(list_el){
	vals <- dat.winter.mat[list_el,]
	vals <- as.matrix(vals)
#	greater.num <- quantile(x = vals,
#				probs = seq(0,1-quant_by,quant_by))
	greater.num <- lapply(min.vals, function(min.val){
		return(sum(vals <= min.val)/length(vals))	
	})
	return(unlist(greater.num))
})

quant.spring <- lapply(as.list(1:(7*48)), function(list_el){
	vals <- dat.spring.mat[list_el,]
	vals <- as.matrix(vals)
#	greater.num <- quantile(x = vals,
#				probs = seq(0,1-quant_by,quant_by))
	greater.num <- lapply(min.vals, function(min.val){
		return(sum(vals <= min.val)/length(vals))	
	})
	return(unlist(greater.num))
})

quant.summer <- lapply(as.list(1:(7*48)), function(list_el){
	vals <- dat.summer.mat[list_el,]
	vals <- as.matrix(vals)
	#greater.num <- quantile(x = vals,
	#			probs = seq(0,1-quant_by,quant_by))
	greater.num <- lapply(min.vals, function(min.val){
		return(sum(vals <= min.val)/length(vals))	
	})
	return(unlist(greater.num))
})

quant.autumn <- lapply(as.list(1:(7*48)), function(list_el){
	vals <- dat.autumn.mat[list_el,]
	vals <- as.matrix(vals)
	#greater.num <- quantile(x = vals,
	#			probs = seq(0,1-quant_by,quant_by))
	greater.num <- lapply(min.vals, function(min.val){
		return(sum(vals <= min.val)/length(vals))	
	})
	return(unlist(greater.num))
})
len <- length(min.vals)
quant.winter.mat <- matrix(unlist(quant.winter), 
			   byrow = T, 
			   ncol = len)
quant.spring.mat <- matrix(unlist(quant.spring), 
			   byrow = T, 
			   ncol = len)
quant.summer.mat <- matrix(unlist(quant.summer), 
			   byrow = T, 
			   ncol = len)
quant.autumn.mat <- matrix(unlist(quant.autumn), 
			   byrow = T, 
			   ncol = len)

pdf(file = '../img/Peaks_seasons.pdf',width = 10, height = 8)
par(mfrow = c(2,2),mar=c(5,3,2,1.5) + 0.1, cex.axis=1.3)
matplot(1:(7*48), 1-quant.winter.mat, type = 'l', xaxt = "n", 
	xlab = "Day of Week", 
	ylab = "Proportion of peaks", lty = 1, col = 'black', 
	main = 'Winter', ylim = c(0,0.45))
axis(1, at = (1:7)*48, labels = F)
axis(1, at = (1:7)*48-24, labels = weekdays(2:8),tick = F)

matplot(1:(7*48), 1-quant.spring.mat, type = 'l', xaxt = "n", 
	xlab = "Day of Week", 
	ylab = "Proportion of peaks", lty = 1, col = 'black', 
	main = 'Spring', ylim = c(0,0.45))
axis(1, at = (1:7)*48, labels = F)
axis(1, at = (1:7)*48-24, labels = weekdays(2:8),tick = F)

matplot(1:(7*48), 1-quant.summer.mat, type = 'l', xaxt = "n", 
	xlab = "Day of Week", 
	ylab = "Proportion of peaks", lty = 1, col = 'black', 
	main = 'Summer', ylim = c(0,0.45))
axis(1, at = (1:7)*48, labels = F)
axis(1, at = (1:7)*48-24, labels = weekdays(2:8),tick = F)

matplot(1:(7*48), 1-quant.autumn.mat, type = 'l', xaxt = "n", 
	xlab = "Day of Week", 
	ylab = "Proportion of peaks", lty = 1, col = 'black', 
	main = 'Autumn', ylim = c(0,0.45))
axis(1, at = (1:7)*48, labels = F)
axis(1, at = (1:7)*48-24, labels = weekdays(2:8),tick = F)
dev.off()
