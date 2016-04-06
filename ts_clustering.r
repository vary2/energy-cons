library(dtw)
library(wavelets)
library(ggfortify)

# prepare data row: sample, column: feature
N_samp = dim(dat)[2]-2
#sig <- t(as.matrix(dat[day >= 1 & day <= 7, (1:N_samp)+2, with = F]))
sig <- t(as.matrix(dat[((day %in% summer.ind) | (day %in% spring.ind)) & 
		       	(hh %in% (1:48)), (1:N_samp)+2, with = F]))
sig <- t(as.matrix(dat[day %in% winter.ind & 
		       	(hh %in% (1:48)), (1:N_samp)+2, with = F]))
sig <- matrix(data = t(sig), byrow = T, ncol = 48*7)
maxes <- apply(sig,1,max)
means <- apply(sig,1,mean)
data_glm <- data.frame(means = means, maxes = maxes)
ys <- rep(c(1,1,0,0,0,0,0),N_samp*length(winter.ind)/7)
#ys <- numeric(length(winter.ind)/7 + length(summer.ind)/7)
#winter.ind[winter.ind %% 7 == 0]/7
#ys[ c(1:9,24,25,26) ] = 1
#ys <- rep(ys, N_samp)
res <- glm(ys ~ means + maxes,family=binomial(link='logit'), data = data_glm)
plot(means, maxes, col = alpha('blue', 0.1),pch=16, type = 'p')
abline(a = -res$coefficients[1]/res$coefficients[3], b = -res$coefficients[2]/res$coefficients[3], lwd = 3, col = 'red')

library(ROCR)
pred<-prediction(res$score, res$Active)
perf<-performance(pred,"tpr","fpr")
writeMat('sig.mat',sig = sig)
# DWT ---------------------------------------------------------------------
sig_pp <- sig
wtData <- NULL
for (i in 1:nrow(sig_pp)) {
	a <- sig_pp[i,]
	wt <- dwt(a, filter='la8', boundary='periodic',n.levels = 2)
	wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))

}
wtData <- as.data.frame(wtData)

N_clust  = 9
km <- kmeans(wtData, centers = N_clust)
pdf(file = '../img/AggrClustersWT.pdf',width = 10, height = 10)
par(mfrow=c(3,3), mar=c(5,4,2,1.5) + 0.1, cex.lab = 1.3)
for (i in 1:N_clust){
	plot(colSums(sig[km$cluster == i,])/sum(km$cluster == i),
	     	type = 'l', xlab = 'HH', ylab = 'Aggregated consumption',
	     	main = paste('Cluster',i,'with',sum(km$cluster == i),'members.'))
}
dev.off()

N_clust  = 9
km2 <- kmeans(sig, centers = N_clust)
pdf(file = '../img/AggrClusters.pdf',width = 10, height = 10)
par(mfrow=c(3,3), mar=c(5,4,2,1.5) + 0.1, cex.lab = 1.3)
for (i in 1:N_clust){
	plot(colSums(sig[km2$cluster == i,])/sum(km2$cluster == i),
	     type = 'l', xlab = 'HH', ylab = 'Aggregated consumption',
	     main = paste('Cluster',i,'with',sum(km$cluster == i),'members.'))
}
dev.off()

hc <- hclust(dist(wtData),method = 'ward.D')
hc <- cutree(hc, h=max(hc$height/7))
pdf(file = '../img/AggrClustersWardWT.pdf',width = 10, height = 10)
par(mfrow=c(3,3), mar=c(5,4,2,1.5) + 0.1, cex.lab = 1.3)
for (i in 1:N_clust){
	plot(colSums(sig[hc == i,])/sum(hc == i), type = 'l')
}
dev.off()
plot(sig[km$cluster == 5,])
#plot PCA
pdf(file = '../img/PCA_Clusters.pdf',width = 10, height = 10)
par(mfrow=c(1,3), mar=c(5,4,2,1.5) + 0.1)
pal <- rainbow(N_clust, alpha = 0.6)
ggplot2::autoplot(stats::prcomp(wtData,scale. = T), colour = pal[km$cluster], label = F, loadings = F)
ggplot2::autoplot(stats::prcomp(wtData,scale. = T), colour = pal[hc], label = F, loadings = F)
ggplot2::autoplot(stats::prcomp(sig,scale. = T), colour = pal[km2$cluster], label = F, loadings = F)
dev.off()


# embeddings --------------------------------------------------------------

# try to find embedding dimension

embedd.dim <- seq(1,24,by = 2)
lamb <- numeric(length(embedd.dim))
for (i in 1:length(embedd.dim)){
	XX <- embed(sig[1,], embedd.dim[i])
	MTM <- t(XX) %*% XX
	lamb[i] <- max(eigen(MTM)$values)
	plot(eigen(MTM)$values,type ='l')
}

library(ForeCA)
XX <- embed(sig[1,], 24)
XX <- ts(XX, freq = 48)
s <- ts(sig[1:50,], freq = 48)
mod.foreca <- foreca(s, n.comp = 4, 
		     spectrum.control = list(method = "EM"))
mod.foreca$scores <- ts(mod.foreca$scores, start = start(XX), 
			freq = frequency(XX))

plot(mod.foreca$scores)
plot(mod.foreca)

par(mfrow=c(4,1),mar = rep(2, 4))
plot(sig[1,]-mean(sig[1,]),type = 'l')
lines(rowSums(mod.foreca$scores), col = 'red', type = 'l', lwd = 2)
plot(mod.foreca$scores[,1],type = 'l')
plot(mod.foreca$scores[,2],type = 'l')
plot(mod.foreca$scores[,3],type = 'l')


#tsdist <- diss( sig , "ACF", p=0.05)
tsdist.mat <- as.matrix(tsdist)
tsdist.mat[is.na(tsdist.mat)] <- 10
hc <- hclust(as.dist(tsdist.mat[1:400,1:400]),method = 'ward.D2')


N_clust  = 16
groups<-cutree(hc, k=N_clust)
pdf(file = '../img/AggrClustersACF.pdf',width = 10, height = 10)
par(mfrow=c(4,4), mar=c(5,4,2,1.5) + 0.1, cex.lab = 1.3)
for (i in 1:N_clust){
	plot(colSums(sig[groups == i,])/sum(groups == i),
	     type = 'l', xlab = 'HH', ylab = 'Aggregated consumption',
	     main = paste('Cluster',i,'with',sum(groups == i),'members.'))
}
dev.off()

