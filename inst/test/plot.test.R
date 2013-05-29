library(dodo)
library(reshape2)
library(ggplot2)

stgs <- letters[1:4]

o <- within(data = list(), expr = {
	stages = stgs
	n_bins = rbinom(n=length(stgs), size=5, prob=0.4) + 1
	obj <- new('block_projection', stages=stages, bins=n_bins)
})


combinations <- as.matrix(expand.grid(o$obj$stage_names,o$obj$stage_names))
 
dat_list <- apply(
	X=combinations, MARGIN=1, 
	FUN=function(row, M) { 
		nom <- paste(row[1], "->", row[2], sep='')
		mat <- M[row[1],row[2]]
		cols_start <- min(as.numeric(colnames(mat)))
		cols_stop  <- max(as.numeric(colnames(mat)))
		rows_start <- min(as.numeric(rownames(mat)))
		rows_stop  <- max(as.numeric(rownames(mat)))
		ul <- as.numeric(c(cols_start-1	,  (rows_start-1)))
		ll <- as.numeric(c(cols_start-1	,   rows_stop))
		ur <- as.numeric(c(cols_stop 		,  (rows_start-1)))
		lr <- as.numeric(c(cols_stop 		,   rows_stop))
		dat <- data.frame(
			id = rep(nom,4),
			from = rep(row[1],4),
			to = rep(row[2],4),
			x = c(ul[1], ll[1], lr[1], ur[1]),
			y = c(ul[2], ll[2], lr[2], ur[2])
		)
		return(dat)
	},
	M = o$obj
)

dat_list <- do.call(what=rbind, args=dat_list)

values <- data.frame(
						id = unique(dat_list[,c('id')]), 
						value = 1:nrow(combinations))

middles <- do.call(what=rbind, args=by(
	data=dat_list, 
	INDICES=dat_list['id'], 
	FUN=function(dat) {
		o <- data.frame(
			id = unique(dat[['id']]),
			ctr_x = mean(dat[['x']]),
			ctr_y = mean(dat[['y']]))
		return(o)
	},
	simplify=TRUE
))

values <- merge(x=values, y=middles, by='id')
dat_list[['y']] <- dat_list[['y']] * -1

pl <- ggplot(values, aes(fill = value)) + scale_y_reverse() +
  geom_map(aes(map_id = id), map = dat_list) +
#	expand_limits(dat_list) + 
	geom_text(aes(x=ctr_x, y=ctr_y, label=id, value=id)) + 
	xlab('') + ylab('') + theme_bw() 





