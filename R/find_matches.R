#' @importFrom adagio assignment
find_matches <- function(x,y, 
		method, C, caliper, lsap, verbose=FALSE, ...){
	
	Nx <- nrow(x)
	Ny <- nrow(y)

	if( is.null(Nx) | is.null(Ny) ){
		Nx <- length(x)
		Ny <- length(y)
	}
	
	if((Nx<Ny)) stop("nrow(x)>=nrow(y)!")

	# Calculate cost matrix 
	m <- calc_dist(x,y,method=method,matrix=TRUE,...)
	m <- m*C

	caliper <- caliper * C
	
	if(verbose){
		cat("Summary statistics for values in distance matrix:\n")
		print(summary(as.vector(m)))
		cat("\n")
	}


	maxcost <- max(m) 
	if(caliper>maxcost) caliper <- maxcost+0.5

	# Add columns to make 
	# cost matrix quadratic
	if(Nx!=Ny){ 
		add_cols <- matrix(maxcost+1, Nx, Nx-Ny)
		m <- cbind(m, add_cols)
		} 
	
	# Get permutations 
	# perm : row id in the order of x 
	if(lsap==TRUE){
		if(ncol(m)==1 & nrow(m)==1){
			perm <- 1 
			distance <- m[1,1]
		} else {
			m_ass <- assignment(m)
			if(m_ass$err<0) warnings("Integer overflow occured. Solution not reliable.")
			perm <- m_ass$perm
			distance <- m[cbind(1:Nx, perm)]
		}
	} else {
		perm <- apply(m, 1, which.min)
		distance <- apply(m, 1, min)
	}

	# Remove added columns 
	perm[perm>Ny] <- NA
	distance[is.na(perm)] <- NA
	
	# Apply caliper 
	perm_extra <- perm[distance>caliper & !is.na(distance)]
	perm[distance>caliper & !is.na(distance)] <- NA
	distance[distance>caliper & !is.na(distance)] <- NA

	res <- list(
		"x"=1:Nx,
		"y"=perm,
		"distance"=distance/C,
		"y_extra"=perm_extra)

	return(res)
	}
