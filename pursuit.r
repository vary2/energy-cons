N_samp = 50#dim(dat)[2]-2
sig <- t(as.matrix(dat[day >= 1 & day <= 7, (1:N_samp)+2, with = F]))

sig_dwt1 <- wavDWT(sig[1,],n.levels = 3)
sig_dwt2 <- wavDWT(sig[2,],n.levels = 3)