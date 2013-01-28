
path_helper <- function(path, timestamp) {
		ts <- paste(strsplit(x=date(), split=' ')[[1]], sep='_', collapse='_')
		if (is.null(path)) stop("Target file or directory must be supplied.")
		if (file.exists(path)) {
			fi <- file.info(path)
			if (fi$isdir) {
				pattern <- 'pops-'
				if (timestamp) {
					pattern <- paste(pattern, ts, sep='')
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



