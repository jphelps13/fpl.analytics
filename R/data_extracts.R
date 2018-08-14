#' @title pullFplSeasonJson
#' @rdname pullFplSeasonJson
#' @export 
#' @importFrom jsonlite fromJSON
pullFplSeasonJson <- function(url = "https://fantasy.premierleague.com/drf/bootstrap-static"){
  fpl_season_json <- jsonlite::fromJSON(url)
  return(fpl_season_json)
}

#' @title pullFplHistoryDt
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

#' @title storePackageData
#' @rdname storePackageData
#' @export 
storePackageData <- function(root_path = getwd()){
  
  # data forked from https://github.com/vaastav/Fantasy-Premier-League
  
  data_loc <- file.path(root_path, "data")
  
  # element_type/position
  fpl_positions <- data.table(id = 1:4,
                              full_name = c("Goalkeeper", "Defender", 
                                            "Midfielder", "Forward"),
                              short_name = c("GKP", "DEF", "MID", "FWD"))
  
  seasons <- c("2017-18")
  
  lapply(seasons, function(season){
    
    # path
    season_loc <- file.path(data_loc, sprintf("%s/", season))
    files      <- list.files(file.path(season_loc, "players"), full.names = TRUE)
    players    <- list.files(file.path(season_loc, "players"), full.names = FALSE)
    
    # extract players data, for games where they played
    pb <- txtProgressBar(min = 0, max = length(players), style = 3)
    combined_list <- lapply(seq_along(files), function(i){
      setTxtProgressBar(pb, i)
      dt <- fread(file.path(files[i], "gw.csv"))
      dt <- dt[minutes > 0, ]
      set(dt, j = "player_name", value = players[i])
      dt[, c("kickoff_time", "kickoff_time_formatted", "transfers_balance",
             "loaned_in", "loaned_out", "ea_index", "element") := NULL]
      dt
    })
    combined <- rbindlist(combined_list)
    combined[, player_name := stringi::stri_trans_general(player_name, "Latin-ASCII")]
    combined[, simple_name := gsub(" ", "", trimws(tolower(player_name)))]
    
    # add team
    # swap <- function(vec, from, to) {
    #   tmp <- to[ match(vec, from) ]
    #   tmp[is.na(tmp)] <- vec[is.na(tmp)]
    #   return(tmp)
    # }
    # combined[, team := swap(opponent_team, unique(opponent_team), rev(unique(opponent_team))),
    #          by = fixture]
    
    # get core season info
    player_dt <- fread(file.path(season_loc, "players_raw.csv"), encoding = "UTF-8")
    player_dt[, player_name := paste(first_name, second_name, sep = "_")]
    player_dt[, player_name := stringi::stri_trans_general(player_name, "Latin-ASCII")]
    player_dt[, simple_name := gsub(" ", "", trimws(tolower(player_name)))]
    
    player_dt <- merge(player_dt, fpl_positions[, c("id", "short_name")],
                       by.x = "element_type", by.y = "id")
    setnames(player_dt, c("short_name"), c("position"))

    teams_dt <- seasonTeams(season)
    player_dt <- merge(player_dt, teams_dt, by.x = "team", by.y = "id")
    player_dt[, team := NULL]
    setnames(player_dt, "names", "team")
    
    # merge in to combined - position and team
    combined <- merge(combined, player_dt[, .(simple_name, position, team)], by = "simple_name")
    combined[, simple_name := NULL]
    
    # change opponent team
    combined <- merge(combined, teams_dt, by.x = "opponent_team", by.y = "id")
    combined[, opponent_team := NULL]
    setnames(combined, "names", "opponent_team")
    
    data_obj <- sprintf("fpl_%s_player_history", gsub("-", "_", season))
    assign(data_obj, combined)
    save(list = data_obj, file = file.path(data_loc, sprintf("%s.rda", data_obj)))
  })

  return(invisible())
} 

seasonTeams <- function(season){
  
  team_dt <- switch (season,
    "2016-17" = {
      data.table(id = 1:20,
                 names = c("Arsenal", "Bournemouth", "Brighton", "Burnley", "Chelsea", 
                           "Crystal Palace", "Everton", "Huddersfield", "Leicester", "Liverpool", 
                           "Man City", "Man Utd", "Newcastle", "Southampton", "Stoke",
                           "Swansea", "Spurs", "Watford", "West Brom", "West Ham"))
    },
    "2017-18" = {
      data.table(id = 1:20,
                 names = c("Arsenal", "Bournemouth", "Brighton", "Burnley", "Chelsea", 
                           "Crystal Palace", "Everton", "Huddersfield", "Leicester", "Liverpool", 
                           "Man City", "Man Utd", "Newcastle", "Southampton", "Stoke",
                           "Swansea", "Spurs", "Watford", "West Brom", "West Ham"))
    },
    "2018-19" = {
      data.table(id = 1:20,
                 names = c("Arsenal", "Bournemouth", "Brighton", "Burnley", "Cardiff", 
                           "Chelsea", "Crystal Palace", "Everton", "Fulham", "Huddersfield", 
                           "Leicester", "Liverpool", "Man City", "Man Utd", "Newcastle", 
                           "Southampton", "Spurs", "Watford", "West Ham", "Wolves"))
    }
  )
  
}
