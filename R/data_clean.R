#' @import data.table


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dt PARAM_DESCRIPTION
#' @param dummies PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cleanPlayerData
#' @export 
cleanPlayerData <- function(dt, dummies){
  dt <- setDT(dt)
  dt <- dt[minutes > 0,]
  dt <- dt[total_points > 0,]
  # dt <- dt[total_points > quantile(total_points, 0.1),]
  dt[, team_code := NULL]
  dt[, log_price := log(now_cost)]
  dt[, name := sprintf("%s %s", first_name, second_name)]
  dt[, num_games := round(minutes/90, 1)]
  dt[, team := as.character(team)]
  
  dt <- dummy_cols(dt, select_columns = dummies)
  
  num_cols <- c("total_points", "bps", "bonus", "saves",
                "yellow_cards", "red_cards", "goals_scored",
                "minutes", "assists", "clean_sheets", "goals_conceded",
                "own_goals", "penalties_saved", "penalties_missed",
                "influence", "creativity", "threat", "ict_index",
                "selected_by_percent", "now_cost", "log_price")
  dt <- addMinMaxCols(dt, num_cols)
  return(dt)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dt PARAM_DESCRIPTION
#' @param num_cols PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname addMinMaxCols
#' @export 
addMinMaxCols <- function(dt, num_cols){
  
  mm_num_cols <- paste0("mm_", num_cols)
  dt[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  dt[, (mm_num_cols) := lapply(.SD, FUN = function(x){
    (x-min(x))/(max(x)-min(x))
  }), .SDcols = num_cols]
  return(dt)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fpl_season_json PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringi]{stri_trans_general}}
#' @rdname playerLookup
#' @export 
#' @importFrom stringi stri_trans_general
playerLookup <- function(fpl_season_json){
  
  # extract lookup from the json
  lookup <- as.data.table(fpl_season_json$elements)
  lookup[, player_name := sprintf("%s %s", first_name, second_name)]
  lookup[, player_name := stringi::stri_trans_general(player_name, "Latin-ASCII")]
  keep_cols <- c("id", "player_name", "team", "element_type")
  lookup <- lookup[, keep_cols, with = FALSE]
  setnames(lookup, "team", "team_id")
  setnames(lookup, "id", "player_key")
  
  # merge to get team name
  team_lookup <- as.data.table(fpl_season_json$teams)[, .(team_id = id, team = name)]
  lookup <- merge(lookup, team_lookup, by = "team_id")
  
  # merge to get position
  pos_lookup <- as.data.table(fpl_season_json$element_types)[, .(element_type = id, 
                                                                 position = singular_name_short)]
  lookup <- merge(lookup, pos_lookup, by = "element_type")
  
  # tidy 
  lookup[, c("team_id", "element_type") := NULL]

  return(lookup)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fpl_player_hist PARAM_DESCRIPTION
#' @param fpl_season_json PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname extractBpsData
#' @export 
extractBpsData <- function(fpl_player_hist, fpl_season_json){
  
  player_dt <- playerLookup(fpl_season_json)
  output <- merge(fpl_player_hist, player_dt, by = "player_key")

  # trim
  output[, c("id", "season", "element_code") := NULL]
  
  return(output)
}
