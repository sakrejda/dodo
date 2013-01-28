library(dodo)
s <- readRDS('/var/lib/store/store/analysis-ahj97n-2012-11-19_15:00:18.485035/s3.rds')

data('life_cycle-ATS-smolt')
rm(pop)

## Utility link function for mortality models from survival coef's
rlogit <- list(
	linkfun 	= function(mu) {qlogis(1-mu)},
	linkinv 	= function(eta) {plogis(-1*eta)},
	mu.eta  	= function(eta) {},
	valideta	= function(eta) TRUE,
	name 			= "rlogit"
)
class(rlogit) <- "link-glm"

## Aging:
pa <- new('pGLM',
	formula = ~ 1,
	family = gaussian(link="identity"),
	coefficients = list(
		"(Intercept)" = 1
	),
	epsilon = function(n=1) {return(rep(0,n))},
	samp = FALSE
)


## Stocking:
sp <- new('pGLM', 
	formula = ~ 1 + season,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)" = 1.3, 
		season2 = -100,
		season3 = -100,
		season4 = -100
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
sf <- new('pGLM',
	formula = ~ 1,
	family = poisson(link=log),
	coefficients = list("(Intercept)"=2),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.01) },
	samp = FALSE
)
ss <- new('pGLM',
	formula = ~ 1,
	family = gaussian(),
	coefficients = list("(Intercept)" = 5),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.5) },
	samp = FALSE
)

## Fry growth:
fg <- new('pGLM',
	formula = ~ 1,
	family = gaussian(),
	coefficients = list(
		"(Intercept)"  = 55
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)

## Fry survival:
fm <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)"  = -0.9
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.2) },
	samp = FALSE
)


## Growth of juveniles:
jg <- new('pGLM',
	formula = ~ 1 + ageInSamples + offset(sizes),
	family = gaussian(),
	coefficients = list(
		"(Intercept)"  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 1],
		ageInSamples2  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 2],
		ageInSamples3  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 3],
		ageInSamples4  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 4],
		ageInSamples5  = s[['gr_beta_0']],
		ageInSamples6  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 6],
		ageInSamples7  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 7],
		ageInSamples8  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 8],
		ageInSamples9  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 9],
		ageInSamples10 = s[['gr_beta_0']] +  s[['gr_beta_ais']][10],
		ageInSamples11 = s[['gr_beta_0']] +  s[['gr_beta_ais']][11],
		ageInSamples12 = s[['gr_beta_0']] +  s[['gr_beta_ais']][12],
		ageInSamples13 = s[['gr_beta_0']] +  s[['gr_beta_ais']][13],
		ageInSamples14 = s[['gr_beta_0']] +  s[['gr_beta_ais']][14],
		ageInSamples15 = rep(0, length(s[['gr_beta_0']]))
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)

## Mortality of juveniles:
jm <- new('pGLM', 
	formula = ~ 1 + ageInSamples,
	family = binomial(link=rlogit),
	coefficients = list(
		"(Intercept)"  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 1],
		ageInSamples2  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 2],
		ageInSamples3  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 3],
		ageInSamples4  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 4],
		ageInSamples5  = s[['phi_beta_0']],
		ageInSamples6  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 6],
		ageInSamples7  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 7],
		ageInSamples8  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 8],
		ageInSamples9  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 9],
		ageInSamples10 = s[['phi_beta_0']] +  s[['phi_beta_ais']][10],
		ageInSamples11 = s[['phi_beta_0']] +  s[['phi_beta_ais']][11],
		ageInSamples12 = s[['phi_beta_0']] +  s[['phi_beta_ais']][12],
		ageInSamples13 = s[['phi_beta_0']] +  s[['phi_beta_ais']][13],
		ageInSamples14 = s[['phi_beta_0']] +  s[['phi_beta_ais']][14],
		ageInSamples15 = rep(-100,length(s[['phi_beta_0']]))
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)


## Smolting of juveniles:
js1 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = -2.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js2 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 0.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js3 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 1.5
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

js4 <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)"  = 1.0
	),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)


## Make a blank population, this has to happen once:
pop <- population$new(
	stages = stages, parents = parents, transformations = transformations)

## Build/Add the models:

## Stock:
pop$add_model( 
	stage						=	'stock',							
	transformation	=	'stock',
	model						=	staged_reproduction_factory('stock','fry', sp, sf, ss))

## Fry:
pop$add_model(
	stage						=	'fry',							
	transformation	=	'grow',
	model						=	staged_growth_factory('fry', fg))
pop$add_model(
	stage						=	'fry',
	transformation	=	'die',
	model						= staged_transition_factory('fry', 'dead_fry', fm))
pop$add_model(
	stage						= 'fry',
	transformation 	= 'age',
	model						= staged_transition_factory('fry','zero_summer_parr', pa))

## Zero plus.
pop$add_model(
	stage						=	'zero_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_summer_parr', jg))
pop$add_model(
	stage						=	'zero_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_summer_parr', 'dead_zero_summer_parr', jm))
pop$add_model(
	stage						=	'zero_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_summer_parr', 'zero_autumn_parr', pa))

pop$add_model(
	stage						=	'zero_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_autumn_parr', jg))
pop$add_model(
	stage						=	'zero_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_autumn_parr', 'dead_zero_autumn_parr', jm))
pop$add_model(
	stage						=	'zero_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_autumn_parr', 'zero_winter_parr', pa))


pop$add_model(
	stage						=	'zero_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('zero_winter_parr', jg))
pop$add_model(
	stage						=	'zero_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('zero_winter_parr', 'dead_zero_winter_parr', jm))
pop$add_model(
	stage						=	'zero_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('zero_winter_parr', 'one_spring_parr', pa))

## One plus:
pop$add_model(
	stage						=	'one_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_spring_parr', jg))
pop$add_model(
	stage						=	'one_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_spring_parr', 'dead_one_summer_parr', jm))
pop$add_model(
	stage						=	'one_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_spring_parr', 'one_summer_parr', pa))

pop$add_model(
	stage						=	'one_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_summer_parr', jg))
pop$add_model(
	stage						=	'one_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_summer_parr', 'dead_one_summer_parr', jm))
pop$add_model(
	stage						=	'one_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_summer_parr', 'one_autumn_parr', pa))

pop$add_model(
	stage						=	'one_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_autumn_parr', jg))
pop$add_model(
	stage						=	'one_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_autumn_parr', 'dead_one_autumn_parr', jm))
pop$add_model(
	stage						=	'one_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_autumn_parr', 'one_winter_parr', pa))


pop$add_model(
	stage						=	'one_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('one_winter_parr', jg))
pop$add_model(
	stage						=	'one_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('one_winter_parr', 'dead_one_winter_parr', jm))
pop$add_model(
	stage						=	'one_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('one_winter_parr', 'one_spring_parr', pa))

## Two plus:
pop$add_model(
	stage						=	'two_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_spring_parr', jg))
pop$add_model(
	stage						=	'two_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_spring_parr', 'dead_two_spring_parr', jm))
pop$add_model(
	stage						=	'two_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_spring_parr', 'two_summer_parr', pa))

pop$add_model(
	stage						=	'two_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_summer_parr', jg))
pop$add_model(
	stage						=	'two_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_summer_parr', 'dead_two_summer_parr', jm))
pop$add_model(
	stage						=	'two_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_summer_parr', 'two_autumn_parr', pa))

pop$add_model(
	stage						=	'two_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_autumn_parr', jg))
pop$add_model(
	stage						=	'two_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_autumn_parr', 'dead_two_autumn_parr', jm))
pop$add_model(
	stage						=	'two_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_autumn_parr', 'two_winter_parr', pa))

pop$add_model(
	stage						=	'two_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('two_winter_parr', jg))
pop$add_model(
	stage						=	'two_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('two_winter_parr', 'dead_two_winter_parr', jm))
pop$add_model(
	stage						=	'two_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('two_winter_parr', 'three_spring_parr', pa))

## Three plus:
pop$add_model(
	stage						=	'three_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_spring_parr', jg))
pop$add_model(
	stage						=	'three_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_spring_parr', 'dead_three_spring_parr', jm))
pop$add_model(
	stage						=	'three_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_spring_parr', 'three_summer_parr', pa))

pop$add_model(
	stage						=	'three_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_summer_parr', jg))
pop$add_model(
	stage						=	'three_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_summer_parr', 'dead_three_summer_parr', jm))
pop$add_model(
	stage						=	'three_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_summer_parr', 'three_autumn_parr', pa))

pop$add_model(
	stage						=	'three_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_autumn_parr', jg))
pop$add_model(
	stage						=	'three_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_autumn_parr', 'dead_three_autumn_parr', jm))
pop$add_model(
	stage						=	'three_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_autumn_parr', 'three_winter_parr', pa))

pop$add_model(
	stage						=	'three_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('three_winter_parr', jg))
pop$add_model(
	stage						=	'three_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('three_winter_parr', 'dead_three_winter_parr', jm))
pop$add_model(
	stage						=	'three_winter_parr',
	transformation	=	'age',
	model						= staged_transition_factory('three_winter_parr', 'four_spring_parr', pa))

## Four plus:
pop$add_model(
	stage						=	'four_spring_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_spring_parr', jg))
pop$add_model(
	stage						=	'four_spring_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_spring_parr', 'dead_four_spring_parr', jm))
pop$add_model(
	stage						=	'four_spring_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_spring_parr', 'four_summer_parr', pa))

pop$add_model(
	stage						=	'four_summer_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_summer_parr', jg))
pop$add_model(
	stage						=	'four_summer_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_summer_parr', 'dead_four_summer_parr', jm))
pop$add_model(
	stage						=	'four_summer_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_summer_parr', 'four_autumn_parr', pa))

pop$add_model(
	stage						=	'four_autumn_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_autumn_parr', jg))
pop$add_model(
	stage						=	'four_autumn_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_autumn_parr', 'dead_four_autumn_parr', jm))
pop$add_model(
	stage						=	'four_autumn_parr',
	transformation	=	'age',
	model						= staged_transition_factory('four_autumn_parr', 'four_winter_parr', pa))

pop$add_model(
	stage						=	'four_winter_parr',							
	transformation	=	'grow',
	model						=	staged_growth_factory('four_winter_parr', jg))
pop$add_model(
	stage						=	'four_winter_parr',
	transformation	=	'die',
	model						= staged_transition_factory('four_winter_parr', 'dead_four_winter_parr', jm))

## Smolting:
pop$add_model(
	stage						= 'two_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('two_spring_parr', 'two_spring_riverine', js2))
pop$add_model(
	stage						= 'three_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('three_spring_parr', 'three_spring_riverine', js3))
pop$add_model(
	stage						= 'four_spring_parr',
	transformation	= 'smolt',
	model						=	staged_transition_factory('four_spring_parr', 'four_spring_riverine', js4))

################################################################################
################################################################################
## To get everything above this line, the website code would do:
##		data("life_cycle-ATS-smolt")
##
################################################################################
################################################################################



pop$add(
	stage="stock", 
	args=list(
		n_bins=250, 
		limits=c(min=0, max=250), 
		density=function(y) dnorm(x=y, mean=15, sd=1)
	)
)

#### SO: after two (?) steps, there are two sizes of fry because one set
##       got to grow and the other didn't.  Add seasonality! (through
##       env) ... also, "survive" transformation should be "mortality"
##			 transformation and there should be a separate "age"
##			 transformation... (maybe that can also do stocking since that
##			 _is_ aging for the stock source...

#### SO: Added seasonality to stocking (spring only...) need to add
##			 aging models.

envir <- cbind(read.csv('~/downloads/seasonal_streamflow.csv'),
							 read.csv('~/downloads/seasonal_streamtemp.csv')
					)
envir$season <- as.factor(envir$season)


for ( i in 1:5 ) { 
	pop$run(n=1, e=envir[i,], o='/tmp') # Run one step of the model
	for ( j in 1:length(pop$sub_pops) ) { 
		plot(pop$sub_pops[[j]]);
		print(pop$sub_pops[[j]]@stage_name); 
		readline(); dev.off() 
	}
}


pop$clear()
pop$add(
  stage="stock",
  args=list(
    n_bins=250,
    limits=c(min=0, max=250),
    density=function(y) dnorm(x=y, mean=15, sd=1)
  )
)
pop$run(n=nrow(envir), e=envir, o='/tmp')

# Alternatively, if you want to see what it looks like before sync:
#pop$step(synchronize=FALSE)
#pop$sync()

# Save the whole population object to a file, or into a directory (under a
# safe/random name:
#pop$save(path="/tmp")

# Save the "sub-populations" to a file, or into a directory (under a
# safe/random name:
#pop$save_populations(path="/tmp")

# Load environmental data, run a hundred iterations.
#env_file='~/env.csv'
#library(dodo); data('life_cycle-TSD')
#pop$run(n=100, e=read.csv(env_file), o='/tmp')

# Clear the model to restart from a fresh population (blank, need to add
# some individuals to start it again:
#pop$clear()



