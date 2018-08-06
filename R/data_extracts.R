

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param url PARAM_DESCRIPTION, Default: 'https://fantasy.premierleague.com/drf/bootstrap-static'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[jsonlite]{fromJSON}}
#' @rdname pullFplSeasonJson
#' @export 
#' @importFrom jsonlite fromJSON
pullFplSeasonJson <- function(url = "https://fantasy.premierleague.com/drf/bootstrap-static"){
  fpl_season_json <- jsonlite::fromJSON(url)
  return(fpl_season_json)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fpl_season_json PARAM_DESCRIPTION
#' @param url PARAM_DESCRIPTION, Default: 'https://fantasy.premierleague.com/drf/element-summary'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[jsonlite]{fromJSON}}
#' @rdname pullFplHistoryDt
#' @export 
#' @importFrom jsonlite fromJSON
pullFplHistoryDt <- function(fpl_season_json, url = "https://fantasy.premierleague.com/drf/element-summary"){
  
  fpl_season_dt <- setDT(copy(fpl_season_json$elements))
  all_player_ids <- unique(fpl_season_dt$id)
  
  pb <- txtProgressBar(0, length(all_player_ids), style = 3)
  fpl_player_hist <- lapply(seq_along(all_player_ids), function(index){
    setTxtProgressBar(pb, index)
    out <- tryCatch(jsonlite::fromJSON(glue("{url}/{all_player_ids[index]}")), 
                    error = function(e){NULL})$history_past
    if(class(out) == "list"){
      out <- NULL
    }
    if(!is.null(out)){
      out$player_key <- all_player_ids[index]
    }
    out
  })
  fpl_player_hist <- rbindlist(fpl_player_hist)
  return(fpl_player_hist)
}
