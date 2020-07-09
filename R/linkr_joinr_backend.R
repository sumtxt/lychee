# linkr_join by strata
#' @importFrom rlang rep_along
#' @importFrom vctrs vec_slice
#' @importFrom tidyr unnest nest
linkr_joinr_strata <- function(
	x,y,by, 
	strata,
	method,
	assignment,
	output,
	add_distance,
	suffix,
	full,
	na_matches,
	caliper,
	C, verbose, ...){

		strata <- make_named(strata)
		strata_x <- names(strata)
		strata_y <- strata
		names(strata_y) <- NULL
		
		x <- group_by(x,across(all_of(strata_x)))
		y <- group_by(y,across(all_of(strata_y)))
		xy <- full_join(nest(x),nest(y), 
		 by=strata, na_matches=na_matches)
		xy <- rowwise(xy) 
		
		x_ <- filter(xy, is.null(data.y))
		y_ <- filter(xy, is.null(data.x))
		xy <- filter(xy, 
				!is.null(data.y) & 
				!is.null(data.x))

		if(nrow(xy)>0){

			xy <-	mutate(xy, 
				data=list(linkr_joinr(
					a=data.x,b=data.y,
					by=by,method=method, 
					assignment=assignment,
					output=output,
					add_distance=add_distance, 
					suffix=suffix,
					full=full,
					caliper=caliper, C=C,
					verbose=verbose, ...)))

			xy <- select(xy, -data.x, -data.y)
			xy <- unnest(xy,'data')
			xy <- ungroup(xy)

		} else {
			warning("No overlap after stratification.")
			return(NULL)
		}

		x_ <- select(x_, -data.y)
		x_ <- unnest(x_, 'data.x')

		y_ <- select(y_, -data.x)
		y_ <- unnest(y_, 'data.y')

		if(output=="joinr"){

			names <- construct_names(
						x_names=colnames(x),
						y_names=colnames(y),
						x_suffix=suffix[1],
						y_suffix=suffix[2],
						x_strata=strata_x,
						y_strata=strata_y)

			if(nrow(x_)>0){
				x <- rename(x, names$x)		
			} 
			if(nrow(y_)>0){
				y <- rename(y, names$y)
			}
	
		} 

		if(nrow(x_)!=0 & nrow(y_)!=0){
			xy <- bind_rows(xy,x_,y_)
		}
		if(nrow(x_)!=0 & nrow(y_)==0){
			xy <- bind_rows(xy,x_)
		}
		if(nrow(x_)==0 & nrow(y_)!=0){
			xy <- bind_rows(xy,y_)
		}

	return(xy)
	}


# Backend 
linkr_joinr <- function(
	a,b,by, 
	method,
	assignment,
	output,
	add_distance,
	suffix,
	full,
	caliper,
	C, verbose, ...){

	if(!(length(caliper)==1 & is.numeric(caliper))) {
			stop("caliper must be a scalar.") }

	by <- make_named(by)

  if (nrow(a) >= nrow(b)) {

    x <- a 
    y <- b 
    by_x <- names(by)
    by_y <- by
    suffix_x <- suffix[1]
    suffix_y <- suffix[2]
    
  } else {

    x <- b
    y <- a
    by_x <- by
    by_y <- names(by)
    suffix_x <- suffix[2]
    suffix_y <- suffix[1]

 	}

	results <- find_matches(
			x=x[,by_x],
			y=y[,by_y], 
			method=method, 
			lsap=assignment,
			caliper=caliper,
			C=C, verbose=verbose, ...)

	if(output=="joinr"){

		names <- construct_names(
				x_names=colnames(x),
				y_names=colnames(y),
				x_suffix=suffix_x,
				y_suffix=suffix_y)

		x <- rename(x, names$x)
		y <- rename(y, names$y)
	
	  if ( length(results$y_extra) > 0L & full==TRUE) {

	    x_slicer <- c(results$x, 
	    	rep_along(results$y_extra, NA_integer_))

	    y_slicer <- c(results$y, results$y_extra)

	    match_distance <- c(results$distance, 
	    	rep_along(results$y_extra, NA_integer_))
	 
	  } else {

	    x_slicer <- results$x
	    y_slicer <- results$y
	    match_distance <- results$distance
	  
	  }

	  out <- vctrs:::vec_slice(x, x_slicer)
	  if(add_distance){
		  out$match_dist <- match_distance
		}
	  out[names(y)] <- vctrs:::vec_slice(y, y_slicer)

	  return(out)

		} 

	if(output=="linkr"){

		x$match_id <- results$x[results$y]
		y$match_id <- 1:nrow(y)
		y$match_id[!(y$match_id %in% results$y)] <- NA

		if(add_distance){
			x$match_dist <- results$distance
			y$match_dist <- x$match_dist[match(y$match_id,x$match_id)] 
			}

		return(bind_rows(x,y))

		}

	}