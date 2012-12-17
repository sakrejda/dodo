source('allGenerics.R')
source('allClasses.R')
source('initializeMethods.R')
source('projectionMethods.R')
source('graphicalMethods.R')


# Initialize the object:
tp <- size_distribution(
	seed_sample = runif(1000,-5,5),
	n_bins = 200, 
	limits = c(min=-15,max=35),
	bw = 5)
plot(tp); Sys.sleep(5); dev.off()

# Initialize egg size object:
tpE <- egg_size_distribution(
	seed_sample = runif(1000,-5,5),
	n_bins = 200, 
	limits = c(min=-15,max=35),
	bw = 5)
plot(tpE)

# Make a survival model for it:
N <- 300
some_data <- data.frame(
	sizes = rnorm(N, 0, 10)
)
some_data[['pr']] <- plogis(
	5 + .3*some_data[['sizes']] - 0.2*some_data[['sizes']]^2
)
some_data[['y']] <- sapply( 
	X = some_data[['pr']],
	FUN = function(x) {
		sample(x=c(0,1), size=1, replace=TRUE, prob=c(1-x, x))
	})
some_data[['n']] <- 1 - some_data[['y']]
modS <- glm(formula=cbind(y,n) ~ 1 + sizes + I(sizes^2), data = some_data,
					 family="binomial")

# Make a growth model for it (independent data set):
N <- 100
more_data <- data.frame(
	sizes = rnorm(N)
)
more_data[['new_size']] <- rnorm(
	n = N, 
	mean = more_data$sizes + 2 + .2*more_data$sizes, 
	sd=1
)
modG <- lm(formula=new_size ~ 1 + sizes, data = more_data)


ylim = c(0,max(tp@sizes))
while ( names(dev.cur()) != 'null device' ) dev.off()
plot(tp);
for ( i in 1:200 ) {
	tp <- survive(tp, modS, list())
	while ( names(dev.cur()) != 'null device' ) dev.off()
	cat("survived!\n")
	plot(tp, ylim=ylim, type = 'l'); Sys.sleep(4)
	tp <- grow(tp, modG, list())
	cat("grew!\n")
	while ( names(dev.cur()) != 'null device' ) dev.off()
	plot(tp, ylim=ylim, type='l'); Sys.sleep(4)
}



