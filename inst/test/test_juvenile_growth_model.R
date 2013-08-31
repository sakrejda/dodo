library(dodo)
data('estimates-ATS-smolt-M4')
data('stages.ats')

stages.ats$ageInSamples <- factor(stages.ats$ageInSamples)
stages.ats$season <- factor(stages.ats$season)

## Data:
stages <- structure(
	list(
	standardize_size = function(df) {
		df[['std_sizes']] <- (df[['sizes']] - df[['sizeAtAge']])/df[['sdAtAge']]
		return(df)
	},
	flow_conversion = function(df) {
		flowMeans <- c(spring=28,summer=11,autumn=12,winter=26)
		flowSds <- c(spring=9,summer=5,autumn=6,winter=7.5)
		df[['flow_std']] <- 
			(df[['flow']] - flowMeans[df[['season']]]) / flowSds[df[['season']]]
		return(df)
	},
	temp_conversion = function(df) {
		tempMeans <- c(spring=18.02234,summer=18.50568,autumn=18.06938,winter=17.96012)
		tempSds <- c(spring=0.019765602, summer=0.031461813, autumn=0.022657542, winter=0.00111111)
		df[['temp_std']] <-
			(df[['WB']] - tempMeans[df[['season']]]) / tempSds[df[['season']]]
		return(df)
	}

	),
	.Names = c("standardize_size", "flow_conversion", "temp_conversion") 

)

stages <- lapply( X=stages.ats[['stage']],
	FUN=function(stage) {
		o2 <- as.list(stages.ats[stages.ats[['stage']] == stage,])
		if (!is.na(stages.ats[stages.ats[['stage']] == stage, 'ageInSamples'])) {
			o1 <- stages
			return(c(o1,o2))
		} else {
			return(o2)
		}
	}
)
names(stages) <- stages.ats[['stage']]


pop <- new('population', 
	stages = stages.ats[['stage']],
	bins = stages.ats[['n_bins']],
	minima = stages.ats[['minima']],
	maxima = stages.ats[['maxima']],
	projections = c('survive', 'stretch', 'smolt', 'grow'),
	traits = stages 
)

## AS A TEST:
e <- new.env()
e$flow <- 0
e$WB <- 0
pop$set_env(e)
##


juvenile_growth_mu_model <- new('pGLM',
  formula = ~ 1 + ageInSamples,
  family = gaussian(link="identity"),
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
  epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_growth_sd_model <- new('pGLM',
	formula = ~ -1 + season,
	family = gaussian(link="log"),
	coefficients = list(
		seasonautumn = s[['gr_sigma_beta_autumn']],
		seasonspring = s[['gr_sigma_beta_spring']],
		seasonsummer = s[['gr_sigma_beta_summer']],
		seasonwinter = s[['gr_sigma_beta_winter']]
	),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_offset_model <- new('pGLM',
	formula = ~ 1 + offset(sizes),
	family = gaussian(),
	coefficients = list("(Intercept)" = 0),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_growth_models <- list()
for ( i in 1:18) {
	juvenile_growth_models[[i]] <- offset_dlnorm_projection_factory(
		mean_model = juvenile_growth_mu_model,
		sd_model = juvenile_growth_sd_model,
		offset_model = juvenile_offset_model,
		target_dims = c(
			n_bins = stages.ats[['n_bins']][i+4], 
			minimum = stages.ats[['minima']][i+4], 
			maximum = stages.ats[['maxima']][i+4]
		)
	)
	pop$add_transition(from=stages.ats[['stage']][i+3], to=stages.ats[['stage']][i+4],
		projection = list(grow = juvenile_growth_models[[i]]))
}


################################################################################
pop$make_matrix()
pop$projection[1:10,1:10]



