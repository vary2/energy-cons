
# INIT IMPORT -------------------------------------------------------------

library(data.table)

hhs.vec <- seq(from = 0, to = 23.5, by = 0.5)
pathToData <- '../data/'
# IMPORT ------------------------------------------------------------------
# find unique household_ids and datetimes
household_ids <- character(0)
datetimes <- character(0)
for (i in (1:6)){
	pathToFile <- paste(pathToData,'File',i,'.txt', sep = '')
	file_tmp <- fread(pathToFile, data.table = T, select = c('V1','V2'))
	household_ids <- c(household_ids, unique(file_tmp$V1))
	datetimes <- c(datetimes, unique(file_tmp$V2))
	rm(file_tmp)
}
rm(i)
household_ids <- as.numeric(sort(unique(household_ids)))
household_N <- length(household_ids)

datetimes <- as.numeric(sort(unique(datetimes)))
datetimes_N <- length(datetimes)


# CREATE DATA.TABLE -------------------------------------------------------
dat <- matrix(-2, ncol = household_N+2, nrow = datetimes_N)
# -2 will correspond to missing value
colnames(dat) <- c('day','hh',as.character(household_ids))
dat <- data.table(dat)

dat[,day := sapply(datetimes, function(datetime){substr(datetime,1,3)})]
dat[,hh  := sapply(datetimes, function(datetime){substr(datetime,4,5)})]


# LOAD STUFF --------------------------------------------------------------
for (i in (1:6)){
	pathToFile <- paste(pathToData,'File',i,'.txt', sep = '')
	file_tmp <- fread(pathToFile, data.table = T)
	household_ids_tmp <- sort(unique(file_tmp$V1))
	household_ids_N_tmp <- length(household_ids_tmp)
	pb <- txtProgressBar(title = file_tmp)
	for (j in 1:household_ids_N_tmp){
		household_id <- household_ids_tmp[j]
		setTxtProgressBar(pb, j/household_ids_N_tmp)
		vals <- file_tmp[file_tmp$V1 == household_id]$V3
		datetimes_tmp <- unique(file_tmp[V1 == household_id, V2])
		datetimes_ind <- match(x = datetimes_tmp, table = datetimes)
		dat[datetimes_ind, (which(household_id == household_ids) + 2) := vals, with = F]
	}
	close(pb)
	rm(file_tmp)
}

dat$hh <- as.numeric(dat$hh)
dat$day <- as.numeric(dat$day)
setkey(x = dat, day, hh)

# MISSING ? -----------------------------------------------------------------

# check 1st Jan to 31st Dec 2010:
day.start <- 366#min(dat[,day])
day.end <- day.start+365
dat.matrix <- as.matrix(dat[day <= day.end & day >= day.start, c(-1,-2), with = F])
ind.miss <- apply(dat.matrix, MARGIN = 2, FUN = function(col_el){
	return(sum(col_el == -2))
})
# Number of households without missing values:
sum(ind.miss == 0)
# it is 4640

# FILTER DAT --------------------------------------------------------------
# keep only non-missing stuff
household_ids.del <- household_ids[ind.miss != 0]
household_ids.keep <- household_ids[ind.miss == 0]
datetimes.keep <- datetimes[datetimes >= day.start*100 & datetimes <= day.end*100]
dat[,c(as.character(household_ids.del)) := NULL]
rm(dat.matrix)
datetimes <- datetimes.keep
datetimes_N <- length(datetimes)
household_ids <- household_ids.keep
household_N <- length(household_ids)
days.vec <- day.start:day.end

# move days to start from 1:
dat <- dat[day >= day.start & day <= day.end,]
days <- dat$day-day.start
days.vec <- days.vec - day.start + 1
days.vec <- days.vec(-day.start)
dat[,day := days]
rm(days)


# Days fix ----------------------------------------------------------------
fix1 <- as.matrix(dat[day == 86 & hh == 1, c(-1,-2), with = F])
fix2 <- as.matrix(dat[day == 86 & hh == 4, c(-1,-2), with = F])
fixm <- colMeans(rbind(fix1,fix2))
dat <- rbind(dat,as.list(c(day = 86, hh = 2,fixm)))
dat <- rbind(dat,as.list(c(day = 86, hh = 3,fixm)))
rm(fix1, fix2, fixm)
setkey(x = dat, day, hh)

# SAVE STUFF --------------------------------------------------------------
save(dat, 
     datetimes, datetimes_N, 
     days.vec, hhs.vec,
     household_ids, household_N, 
     pathToData,
     file = paste(pathToData,'dat.RData',sep=''))
