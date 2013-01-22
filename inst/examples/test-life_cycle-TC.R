library(dodo)

## Utility link function for mortality models from survival coef's
rlogit <- list(
	linkfun 	= function(mu) {qlogis(1-mu)},
	linkinv 	= function(eta) {plogis(-1*eta)},
	mu.eta  	= function(eta) {},
	valideta	= function(eta) TRUE,
	name 			= "rlogit"
)
class(rlogit) <- "link-glm"

## Define states, transitions, and transition model names
## This is all defined when the line "data('life_cycle-ATS')"
## is run.
stages <- data.frame(
	stage_name 		= c(
		"juvenile", "adult", "dead_juvenile", "dead_adult"),
	age_in_months = c(
						0,      12, 						  0,					 12 ),
	moy						= c(
					  0,      12,  							0,					 12 )
)

parents <- data.frame(
	stage_name	  = c(
		"juvenile",	"adult", 		"adult", "dead_juvenile", "dead_adult"),
	parent_stage  = c(
		"adult",		"juvenile",	"adult", "juvenile",			"adult")
)

transformations <- data.frame(
	order = 1:4,
	model = c("reproduction", "maturation", "growth", "mortality")
)



## Growth of juveniles:
jg <- new('pGLM',
	formula = ~ 1 + offset(sizes),
	family = gaussian(),
	coefficients = list("(Intercept)" = 10),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)
jgf <- staged_growth_factory('juvenile',jg)

## Mortality of juveniles:
jm <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=rlogit),
	coefficients = list("(Intercept)" = -0.5),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
jmf <- staged_transition_factory('juvenile','dead_juvenile',jm)

## Maturation of juveniles:
ja <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list("(Intercept)"=1.2),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
jaf <- staged_transition_factory('juvenile','adult',ja)

## Growth of adults
ag <- new('pGLM',
	formula = ~ 1 + offset(sizes),
	family = gaussian(),
	coefficients = list("(Intercept)"=10),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=1) },
	samp = FALSE
)
agf <- staged_growth_factory('adult',ag)

## Mortality of adults:
am <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=rlogit),
	coefficients = list("(Intercept)" = 1.3),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
amf <- staged_transition_factory('adult','dead_adult',am)

## Adult reproduction:
ar <- new('pGLM', 
	formula = ~ 1,
	family = binomial(link=rlogit),
	coefficients = list("(Intercept)" = 1.3),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)
af <- new('pGLM',
	formula = ~ 1 + sizes,
	family = poisson(link=log),
	coefficients = list("(Intercept)"=2,sizes=1),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.01) },
	samp = FALSE
)
js <- new('pGLM',
	formula = ~ 1,
	family = poisson(link=log),
	coefficients = list("(Intercept)" = 10),
	epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.1) },
	samp = FALSE
)

arf <- staged_reproduction_factory('adult','juvenile', ar, af, js)

## Make a blank population, this has to happen once:
pop <- population$new(
	stages = stages, parents = parents, transformations = transformations)

## Add the models:
pop$add_model('juvenile',		'growth'			,			jgf)
pop$add_model('juvenile',		'mortality'		,			jmf)
pop$add_model('juvenile',		'maturation'	,			jaf)
pop$add_model('adult'   ,		'growth'			,			agf)
pop$add_model('adult'		,		'mortality'		,			amf)
pop$add_model('adult'		,		'reproduction',			arf)

################################################################################
################################################################################
## To get everything above this line, the website code would do:
##		data("life_cycle-ATS")
##		data("life_cycle-TC")  (for the two-stage test population).
##
################################################################################
################################################################################


juveniles <- new('juvenile_size_distribution', 
		Object_ = FALSE,
		seed_sample = runif(1000,5,10), 
		n_bins=100, 
		limits = c(min=-20,max=50),
		bw=5
)

#adults <- new('adult_size_distribution',
#		Object_ = FALSE,
#		seed_sample = runif(1000,10,20),
#		n_bins=100,
#		limits = c(min=-20, max=50),
#		bw=5
#)


pop <- pop + juveniles    # Add some juveniles to the population.

# Run one step of the model:
pop$step()

# Alternatively, if you want to see what it looks like before sync:
pop$step(synchronize=FALSE)
pop$sync()

# Save the whole population object to a file, or into a directory (under a
# safe/random name:
pop$save(path="/tmp")

# Save the "sub-populations" to a file, or into a directory (under a
# safe/random name:
pop$save_populations(path="/tmp")

# Load environmental data, run a hundred iterations.
env_file='~/env.csv'
library(dodo); data('life_cycle-TSD')
pop$run(n=100, e=read.csv(env_file), o='/tmp')

# Clear the model to restart from a fresh population (blank, need to add
# some individuals to start it again:
pop$clear()



