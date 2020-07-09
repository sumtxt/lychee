#' Optimal Joining And Linking Of Two Data Frames 
#' 
#' Join and link two data frames by columns that are similar by some definition such that the similarity across all matches is maximized and each observation is matched at most to one other observation. The function \code{linkr} stacks two data frames and finds an optimal one-to-one pairing of rows in one data frame with rows in the other data frame. The output is a data frame with as many rows as there are in the two datasets and a common identifier for each matched pair. The complementary function is \code{joinr} which, instead of stacking and assigning a common identifier, joins two data frames similar to the \link[base]{merge} function or \link[dplyr]{full_join}. 
#' 
#' @param x,y  data frames to join
#' @param by character vector of the key variable(s) to join by. To join by different variables on x and y, use a named vector. For example, by = c("a" = "b") will match x$a to y$b.
#' @param strata character vector of variables to join exactly if any. Can be a named vector. 
#' @param method the name of the distance metric to measure the similarity between the key variables.
#' @param assignment should one-to-one matches be constructed?
#' @param add_distance add a distance column to the final data frame?
#' @param suffix character vector of length 2 used to disambiguate non-joined duplicate variables in x and y.
#' @param full retain all unjoined observation from the shorter data frame?
#' @param na_matches should NA and NaN values match one another for any exact join defined by \code{strata}?
#' @param caliper caliper value on the same scale as the distance matrix (before multipled by \code{C}). 
#' @param C scaling parameter for the distance matrix
#' @param verbose print distance summary statistic 
#' @param ... parameters passed to distance metric function 
#' 
#' 
#' @details
#' Matches are constructed using a fast version of the \href{https://en.wikipedia.org/wiki/Hungarian_algorithm}{Hungarian method} as implemented in the \link[adagio]{assignment} function. Only the integer part of the distance matrix is used. To increase precision, use the parameter \code{C}. A warning is printed if the distance matrix does not consist of integers but real values ("Warning in assignment(m): Matrix 'cmat' not integer; will take floor of it."). 
#' 
#' If \code{strata} is not \code{NULL}, optimal one-to-one matches are constructed within the strata defined by the variables in \code{strata}. 
#' 
#' The method for computing distance can by any of the string distances implemented as part of the \code{stringdist} package (see \link[stringdist]{stringdist-metrics} for a list), a geographic distance from the \code{\link[geosphere]{geosphere}} package (\link[geosphere]{distGeo}, \link[geosphere]{distCosine}, \link[geosphere]{distHaversine}, \link[geosphere]{distVincentySphere}, \link[geosphere]{distVincentyEllipsoid}), or a distance metric from the \link[proxy]{registry} package (run \code{summary(proxy::pr_DB)} for a list). Users may also supply their own distance metric. 
#' 
#' For geographic distances, \code{by} must be of length 2 with the names of the variables that include the longitude/latitude coordinates (first one is longitude, second is latitude). For string distances, \code{by} must be of length 1. 
#' 
#' @seealso
#' \code{\link[adagio]{assignment}}
#' 
#' 
#' @examples 
#' 
#' library(dplyr)
#' data(greens3)
#' 
#' btw17 <- filter(greens3,
#'    year==2017 & 
#'    election=="BTW") %>% 
#'  select(-year, -election, 
#'     -city_clean)
#' 
#' btw13 <- filter(greens3,
#'    year==2013 & 
#'    election=="BTW") %>% 
#'  select(-year, -election, 
#'     -city_clean)
#' 
#' joinr(btw13,btw17,by=c("city"), 
#'    suffix=c("94","17"),
#'    method='lcs',
#'    caliper=12, 
#'    add_distance=TRUE)
#' 
#' linkr(btw13,btw17,by=c("city"), 
#'    method='lcs',
#'    caliper=12, 
#'    add_distance=TRUE)
#' 
#' @name linkr_joinr
NULL

#'@rdname linkr_joinr
#'@import dplyr
#'@export 
joinr <- function(x,y,by,
		strata=NULL,
		method="osa", 
		assignment=TRUE,
		add_distance=FALSE, 
		suffix = c(".x", ".y"),
		full=TRUE,
		na_matches='na',
		caliper=Inf, C=1, 
		verbose=FALSE, ...){


	if( is.null(strata) ){

		xy <- linkr_joinr(a=x,b=y,by=by,
			method=method, 
			assignment=assignment,
			output='joinr',
			add_distance=add_distance, 
			full=full,
			suffix=suffix,
			caliper=caliper, C=C,
			verbose=verbose, ...)

		return(xy)

	} else {

		xy <- linkr_joinr_strata(
			x=x,y=y, by=by,
			strata=strata,
			method=method, 
			assignment=assignment,
			output='joinr',
			add_distance=add_distance, 
			full=full,
			suffix=suffix,
			na_matches=na_matches,
			caliper=caliper, C=C,
			verbose=verbose, ...)

		return(xy)

	}

	}

#'@rdname linkr_joinr
#'@import dplyr
#'@export 
linkr <- function(x,y,by,
		strata=NULL,
		method="osa", 
		assignment=TRUE,
		add_distance=FALSE, 
		na_matches='na',
		caliper=Inf,C=1,
		verbose=FALSE,...){

	if( is.null(strata) ){

		xy <- linkr_joinr(a=x,b=y,by=by,
			method=method, 
			assignment=assignment,
			output='linkr',
			add_distance=add_distance, 
			full=NULL,
			suffix=NULL,
			caliper=caliper, C=C,
			verbose=verbose, ...)

	} else {

		xy <- linkr_joinr_strata(
			x=x,y=y, by=by,
			strata=strata,
			method=method, 
			assignment=assignment,
			output='linkr',
			add_distance=add_distance, 
			full=NULL,
			suffix=NULL,
			na_matches=na_matches,
			caliper=caliper, C=C,
			verbose=verbose, ...)

	}

	xy$match_id <- complete_id(xy$match_id)

	return(xy)
	}

