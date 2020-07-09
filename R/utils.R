# Takes an ID and replaces 
# each missing value with a new
# ID starting at the maximum observed
# ID value 
complete_id <- function(id){
	na_k <- sum(is.na(id))
	if(na_k!=length(id)){
		id_max <- max(id,na.rm=TRUE)
	} else { 
		id_max <- 0 
	}
	id[is.na(id)] <- (id_max+1):(id_max+na_k)
	return(id)
	}


construct_names <- function(
		x_names,
		y_names,
		x_suffix,
		y_suffix,
		x_strata=NULL,
		y_strata=NULL) {

	x_overlap <- x_names %in% y_names
	y_overlap <- y_names %in% x_names

	if(!is.null(x_strata)){
		x_overlap <- x_overlap==TRUE & !(x_names %in% x_strata)
	} 

	if(!is.null(y_strata)){
		y_overlap <- y_overlap==TRUE & !(y_names %in% y_strata)
	}

	names(x_names) <- x_names
	names(y_names) <- y_names	

	names(x_names)[x_overlap] <- paste0(x_names[x_overlap],x_suffix)
	names(y_names)[y_overlap] <- paste0(y_names[y_overlap],y_suffix)

	return( list(x=x_names,
			y=y_names))
	
	}

make_named <- function(vec){
	if(is.null(names(vec))){
		names(vec) <- vec
		return(vec)
	} else {
		name_miss <- is.na(names(vec))
		names(vec)[name_miss] <- vec[name_miss]
		return(vec)
	}
	}



# Distance Functions #
######################

str_methods <- c("osa", "lv", "dl", "hamming", "lcs", 
		"qgram", "cosine", "jaccard", "jw", "soundex") 

geo_methods <- c("haversine", "geo", "geocosine", "meeus", 
		"vincentysphere", "vincentyellipsoid")

all_methods <- c(str_methods,geo_methods)


calc_dist <- function(x,y,method, matrix, ...){

	check_dist_packages(method)

	if( is.vector(x) ) Kx <- length(x)
	if( is.vector(y) ) Ky <- length(y)

	if( !is.vector(x) ) Kx <- ncol(x)
	if( !is.vector(y) ) Ky <- ncol(y)

	if(method %in% geo_methods){
	  	
	  	if(!(Kx==2 & Ky==2) ) {
	  		stop("For this method, 'by' must be of length 2 (longitude/latitude)")}

		  if(method == "geo") {
		  	if(matrix){
			    dist <- geosphere::distm(x,y,fun=distGeo,...)
			  } else {
			  	dist <- geosphere::distGeo(x,y,...)
			  }
		  }
		  else if(method == "haversine") {
		  	if(matrix){
		    	dist <- geosphere::distm(x,y,fun=distHaversine,...)
		    } else {
			  	dist <- geosphere::distHaversine(x,y,...)
			  }
		  }
		  else if(method == "geocosine") {
		  	if(matrix){
			    dist <- geosphere::distm(x,y,fun=distCosine,...)
			  } else {
			  	dist <- geosphere::distCosine(x,y,...)
			  }
		  }
		  else if(method == "meeus") {
		  	if(matrix){
		    	dist <- geosphere::distm(x,y,fun=distMeeus,...)
			  } else {
			  	dist <- geosphere::distMeeus(x,y,...)
			  }
		  }
		  else if(method == "vincentysphere") {
		  	if(matrix){
		    	dist <- geosphere::distm(x,y,fun=distVincentySphere,...)
			  } else {
			  	dist <- geosphere::distVincentySphere(x,y,...)
			  }
		  }
		  else if(method == "vincentyellipsoid") {
		  	if(matrix){
		    	dist <- geosphere::distm(x,y,fun=distVincentyEllipsoid,...)
			  } else {
			  	dist <- geosphere::distVincentyEllipsoid(x,y,...)
			  }
			}
	}
		
	else if(method %in% str_methods) {

			if(!(Kx==1 & Ky==1) ) {
				stop("For this method, by must be of length 1.") }

			x <- unlist(x)
			y <- unlist(y)

			if(matrix){
				dist <- stringdist::stringdistmatrix(x,y,
					method=method,...)
			} else {
			  dist <- stringdist::stringdist(x,y,
			  	method=method,...)
			  }
	} 

	else {

		if(matrix){
			dist <- proxy::dist(x,y,method=method,...)
		} else{
			dist <- NULL
		}

	}

	return(dist)
	}

#' @importFrom utils install.packages
check_dist_packages <- function(method){

	if(!method %in% all_methods) {
		if(!require(proxy)){
    install.packages("proxy")
    requireNamespace(proxy)
		}
	}

	if(method %in% geo_methods){
		if(!require(geosphere)){
    install.packages("geosphere")
    requireNamespace(geosphere)
		}
	}

	if(method %in% str_methods){
		if(!require(stringdist)){
    install.packages("stringdist")
    requireNamespace(stringdist)
		}
	}

}