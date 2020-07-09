#' Locally Optimal Linking Of Many Data Frames 
#' 
#' Links a series of data frames sequentially: At each iteration, the function selects one element from all already matched tuples (found by linking data frame \code{1...d}) and links it to the next data frame \code{d+1} until no more data frames are available. All elements of a tuple are assigned the same identifier in the stacked data frame. Each tuple will include at most one element from every data frame \code{d}. The solution is a local approximation to the globally optimal solution. 
#' 
#' @param df  data frame to link.
#' @param by character vector of the key variable(s) to join by. To join by different variables on x and y, use a named vector. For example, by = c("a" = "b") will match x$a to y$b.
#' @param slice used to split \code{df} into a list of data frames. 
#' @param strata character vector of variables to join exactly if any. Can be a named vector as for \code{by}. 
#' @param method the name of the distance metric to measure the similarity between the key columns.
#' @param assignment should one-to-one assignments be constructed?
#' @param na_matches should NA and NaN values match one another for any exact join defined by \code{strata}?
#' @param pool one of four string values: "previous", "average", "last" or "random" (see details).
#' @param caliper caliper value on the same scale as the distance matrix (before multipled by \code{C}). 
#' @param C scaling parameter for the distance matrix.
#' @param verbose print distance summary statistic.
#' @param ... parameters passed to distance metric function.
#' 
#' @details 
#' Splits \code{df} by \code{slice} into a list of data frames (indexed 1,...,d,...,D) and applies \code{linkr} to every element of this list. Each data frame \code{d} is linked to a pool of candidates. The candidate pool is defined by one observation from each matched tuple (which might only have a single element, i.e. a singleton) found in the data frames indexed \code{1...(d-1)}. By default, the last observation for each matched tuple is used (\code{pool='last'}). Other options to construct the candidate pool include: 
#'
#' \itemize{
#'   \item \code{pool='random'}: pool includes a randomly drawn element from each matched tuple. 
#'   \item \code{pool='previous'}: pool includes all observations from the data frame indexed \code{d-1}. 
#'	 \item \code{pool='average'}: pool includes a new observation with the average value per key variable for every matched tuple. This option will only work when the variable(s) defined by the parameter \code{by} are numeric. 
#' }
#' 
#' For more details see the help file of \code{\link[lychee]{linkr}}.
#' 
#' 
#' @seealso 
#' \code{\link[adagio]{assignment}} \code{\link[lychee]{linkr}}
#' 
#' 
#' @examples 
#' 
#' library(dplyr)
#' data(greens3)
#' 
#' linkr_multi(
#'   df=filter(greens3, election=="BTW"), 
#'   by='city', 
#'   slice='year',
#'   method='lcs',
#'   caliper=15) %>% 
#' arrange(match_id,year) %>% 
#'  data.frame
#' 
#' 
#' @import dplyr
#' @importFrom igraph components graph_from_data_frame
#' @importFrom stats na.omit
#' @export 
linkr_multi <- function(df, by, slice,
	strata=NULL,
	method="osa", 
	assignment=TRUE,
	na_matches='na',
	pool="last",
	caliper=Inf, 
	C=1, 
	verbose=FALSE, ...){
	
	if("row_id" %in% colnames(df)) {
		warning("Variable row_id will be removed.")
	}
	if("match_id" %in% colnames(df)) {
		warning("Variable match_id will be overwritten.")
	}

	if(length(method)!=1){
		stop("method must be of length 1.")
	} 

	if(length(caliper)!=1){
		stop("caliper must be of length 1.")
	}

	df[['row_id']] <- as.character(1:nrow(df))
	lst <- split(df, df[slice])

	L <- length(lst)
	el <- list()

	x <- bind_rows(lst[[1]])

	for(l in 2:L){

		y <- bind_rows(lst[[l]])

		m <- joinr(x=x,y=y,
				by=by,
				strata=strata,
				method=method, 
				assignment=assignment,
				add_distance=FALSE, 
				suffix=c(".x",".y"),
				full=TRUE,
				na_matches=na_matches,
				caliper=caliper, C=C,
				verbose=verbose, ...)

		if(!is.null(m)){
			el[[length(el)+1]] <- na.omit(m[,c('row_id.x', 'row_id.y')])
		}

		if( pool == "previous" ){
			x <- bind_rows(lst[[l]])
		}	else {
			x <- bind_rows(lst[c(1:l)])
			x <- construct_pool(d=x,e=el,slice=slice,pool=pool,by=by)
		}

	} 

	df <- assign_match_id(d=df,e=el)
	df[['row_id']] <- NULL

	return(df)
	}


# Takes a data frame of  
# edges and converts 
# it into a data frame of 
# vertices that have a 
# common ID if they are connect
connected_vertices <- function(d, 
		name='row_id', 
		value='match_id'){
	d <- bind_rows(d)
	g <- graph_from_data_frame(d)
	g_comp <- components(g)$membership
	d <- tibble(
				val=g_comp, 
				nam=names(g_comp))
	colnames(d) <- c(value,name)
	return(d)
	}

assign_match_id <- function(d,e){
	v <- connected_vertices(e)
	d <- full_join(d, v, by='row_id')
	d[['match_id']] <- complete_id(d[['match_id']])
	return(d)
	}

construct_pool <- function(d, e, slice, pool,by){
	d <- assign_match_id(d,e)
	d <- group_by(d, .data$match_id)
	if(pool=="last"){
		d <- mutate(d, flag=max(.data[[slice]])==.data[[slice]])
		d <- filter(d, .data$flag==1)
		d[['flag']] <- NULL
	} 
	if(pool=="random") {
		d <- sample_n(d,1)
	}
	if(pool=="average") {
		d <- mutate(d,across(all_of(by), mean))
		d <- sample_n(d,1)  
	}
	d[['match_id']] <- NULL
	return(d)
	}


