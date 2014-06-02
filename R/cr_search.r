#' Search the CrossRef Metatdata API.
#'
#' @import httr
#' @importFrom plyr ldply llply
#' @param query Query terms.
#' @param doi Search by a single DOI or many DOIs.
#' @param page Page to return from results.
#' @param rows Number of records to return.
#' @param sort Sort either by "score" or "year".
#' @param year Year to search.
#' @details See \url{http://search.labs.crossref.org/help/api} for more info on this
#' 		Crossref API service.
#' @seealso \code{\link{cr_r}}, \code{\link{cr_citation}}, \code{\link{cr_search_free}}
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' cr_search(query = c("renear", "palmer"))
#'
#' # limit to 4 results
#' cr_search(query = c("renear", "palmer"), rows = 4)
#'
#' # get more results than standard
#' cr_search(query = c("renear", "palmer"), rows = 40)
#'
#' # sort results by score
#' cr_search(query = c("renear", "palmer"), rows = 10, sort = "score")
#'
#' # sort results by year
#' cr_search(query = c("renear", "palmer"), rows = 10, sort = "year")
#'
#' # get results for a certain year
#' cr_search(query = c("renear", "palmer"), year = 2010)
#'
#' # search by a single DOI
#' cr_search(doi = "10.1890/10-0340.1")
#'
#' # search for many DOI's
#' cr_search(doi = c("10.1890/10-0340.1","10.1016/j.fbr.2012.01.001","10.1111/j.1469-8137.2012.04121.x"))
#' }
#' @export
cr_search <- function(query, doi = NULL, page = NULL, rows = NULL,
	sort = NULL, year = NULL)
{
	url = "http://search.labs.crossref.org/dois"

	replacenull <- function(x){
		x[sapply(x, is.null)] <- NA
		x
	}
	if(!is.null(doi)){ doi <- as.character(doi) } else {doi <- doi}
	if(is.null(doi)){
		args <- cr_compact(list(q=query, page=page, rows=rows, sort=sort, year=year))
		out <- content(GET(url, query=args))
		out2 <- llply(out, replacenull)
		output <- ldply(out2, function(x) as.data.frame(x))
		if(nrow(output)==0){"no results"} else{output}
	} else
		{
			doicall <- function(x) {
				args <- cr_compact(list(q=x, page=page, rows=rows, sort=sort, year=year))
				out <- content(GET(url, query=args))
				out2 <- llply(out, replacenull)
				output <- ldply(out2, function(x) as.data.frame(x))
				if(nrow(output)==0){"no results"} else{output}
			}
			ldply(doi, doicall)
		}
}