library(autoencoder)
nl=3
unit.type = "logistic"
Nx.patch=10
Ny.patch=10
N.input = Nx.patch*Ny.patch
N.hidden = 10*10
lambda = 0.0002
beta=6
rho = 0.01
epsilon <- 0.001
max.iterations = 2000

data('training_matrix_N=5e3_Ninput=100')

autoencoder.object <- autoencode(X.train=training.matrix,nl=nl,N.hidden=N.hidden,
				unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
				optim.method="L-BFGS-B",max.iterations=max.iterations,
				rescale.flag=TRUE,rescaling.offset=0.001)

cat("autoencode(): mean squared error for training set: ",
    round(autoencoder.object$mean.error.training.set,3),"\n")


# signals -----------------------------------------------------------------
N_samp = 4000#dim(dat)[2]-3
sig <- t(as.matrix(dat[day >= 1 & day <= 7, (0:N_samp)+3, with = F]))

#sig <- t(apply(sig,1,function(row){ return(row/max(row))}))
sig <- apply(sig,2,function(col){
	col <- col-mean(col)
	col <- col/max(col)
	return(col)
	})

nl=3
unit.type = "logistic"
Nx.patch=10
Ny.patch=10
N.input = Nx.patch*Ny.patch
N.hidden = 10*10
lambda = 0.0002
beta=6
rho = 0.01
epsilon <- 0.001
max.iterations = 2000

autoencoder.object <- autoencode(X.train=sig, nl=nl, N.hidden=N.hidden,
				 unit.type=unit.type, lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
				 optim.method="L-BFGS-B",max.iterations=max.iterations,
				 rescale.flag=TRUE, rescaling.offset=0.001)



output <- SAENET.train(sig, unit.type = 'logistic', 'L-BFGS-B',
		       n.nodes = c(25), 
		       lambda = 1e-5, 
		       beta = 1e-5, 
		       rho = 0.01, 
		       epsilon = 0.01)

W <- autoencoder.object$W
nl <- autoencoder.object$nl
sl <- autoencoder.object$sl


op <- par(no.readonly = TRUE)
par(mfrow = c( floor(sqrt(sl[[nl - 1]])),floor(sqrt(sl[[nl -1]]))), 
    mar = c(0.2, 0.2, 0.2, 0.2), oma = c(3,3, 3, 3))
													    
for (i in 1:sl[[nl - 1]]) {
	denom <- sqrt(sum(W[[nl - 2]][i, ]^2))
	sig_clus <- W[[nl - 2]][i, ]/denom
	plot(sig_clus, type = 'l')
}
par(op)

