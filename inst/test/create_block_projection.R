library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	n_bins = rbinom(n=length(letters), size=5, prob=0.3) + 1
	obj <- new('block_projection', stages=stages, bins=n_bins)
})

obj <- o$obj

library(reshape2)
library(ggplot2)

set <- data.frame(
	x = sample(x=1:ncol(obj$A), size=30),
	y = sample(x=1:nrow(obj$A), size=30),
	value = rbinom(n=30, size=10, prob=0.3)
)
for ( i in 1:nrow(set)) { 
	obj[set[i,'y'], set[i,'x']] <- set[i,'value']
}

obj$plot

## From: http://activeintelligence.org/blog/archive/matplotlib-sparse-matrix-plot/
#pl <- ggplot(
#	data = melt(as.matrix(o$obj$A)), 
#	aes(Var1,Var2, fill=value)
#) + geom_raster() +
#		scale_fill_gradient2(low='red', high='black', mid='white') + 
#		theme_bw() + xlab("From") + ylab("To")
#print(pl)


