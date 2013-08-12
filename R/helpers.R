
path_helper <- function(path, timestamp, iteration = NULL) {
		ts <- paste(strsplit(x=date(), split=' ')[[1]], sep='_', collapse='_')
		if (is.null(path)) stop("Target file or directory must be supplied.")
		if (file.exists(path)) {
			fi <- file.info(path)
			if (fi$isdir) {
				pattern <- 'pops'
				if (timestamp) {
					pattern <- paste(pattern, "-", ts, sep='')
				}
				if (is.numeric(iteration) && (length(iteration)==1)) {
					pattern <- paste(pattern, "-ITER:", iteration, sep='')
				}
				target <- tempfile(pattern=paste(pattern,'-',sep=''), tmpdir=path, fileext='.rds')
			} else {
				stop("Target file already exists.")		
			}
		} else {
			target <- path
			if (timestamp) {
				target <- strsplit(x=target, split='.', fixed=TRUE)[[1]]
				if (length(target) > 1) {
					target <- paste(target[1],ts,paste(target[2:length(target)], collapse='-'),sep='-') 
				} else {
					target <- paste(target,ts,sep='-')
				}
			}
		}
		return(target)
}

list.2.environment <- function(x) {
	o <- new.env()
	sapply(X=names(x), FUN=function(e,x) assign(x=e, value=x[[e]], envir=o), x=x)
	return(o)
}

data.frame.2.lists <- function(x) {
	o <- lapply(1:nrow(x), function(i,x) as.list(x[i,]), x=x)
	return(o)
}


rlogit <- list(
  linkfun   = function(mu) {qlogis(1-mu)},
  linkinv   = function(eta) {plogis(-1*eta)},
  mu.eta    = function(eta) {},
  valideta  = function(eta) TRUE,
  name      = "rlogit"
)
class(rlogit) <- "link-glm"



get_regexpr <- function(pattern, text, offset = 0, ...) {
  m <- regexpr(pattern=pattern, text=text, ...)
  matches <- mapply(
    FUN = substr,
    x = text,
    start = m + offset,
    stop =  m + attr(m, 'match.length') - 1
  )
  return(matches)
}

as.data.frame.size_distribution <- function(x, ..., stringsAsFactors) {
  header <- data.frame(
    stage = x@stage,
    stage_name = x@stage_name
  )
  header <- cbind(
		header, 
		as.data.frame(x@traits[!sapply(x@traits,is.function)])
	)
  dens <- as.data.frame(t(as.matrix(x@densities)))
  colnames(dens) <- x@midpoints
  x <- cbind(header, dens)
  return(x)
}

as.matrix.block_projection <- function(x, ...) { return(x$A) }






