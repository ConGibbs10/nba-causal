# construct pre-treatment blocked data frame
create_pretrt_blocks <- function(.data, quietly = FALSE) {
  require(tidyverse)
  require(foreach)
  
  # extract window data
  windows <- .data %>%
    dplyr::select(.,
                  game_id,
                  play_id,
                  analyze_to,
                  pretrt_win,
                  game_time,
                  posttrt_win)
  
  # get number of plays listed
  n <- nrow(.data)
  
  # loop through number of plays
  blk_df <- foreach::foreach(i = 1:n) %do% {
    # print progress
    if (!quietly) {
      if(i%%100000 == 0 | i == n){
        cat("[", i, "|", n, "]", "\n")
        flush.console()
      }
    }
    .data %>%
      # filter data dynamically to capture the pretreatment windows
      dplyr::filter(
        .,
        game_id == windows$game_id[i],
        game_time <= windows$game_time[i],
        game_time >= windows$pretrt_win[i]
      ) %>%
      # create a column indicating which play the window is for
      dplyr::mutate(., blk = dplyr::last(play_id))
  } %>%
    # bind list elementwise, rowwise
    dplyr::bind_rows(.)
  
  return(blk_df)
}

#- construct post-treatment blocked data frame
create_posttrt_blocks <- function(.data, quietly = FALSE) {
  require(tidyverse)
  require(foreach)
  
  windows <- .data %>%
    dplyr::select(.,
                  game_id,
                  play_id,
                  analyze_to,
                  pretrt_win,
                  game_time,
                  posttrt_win)
  
  n <- nrow(.data)
  blk_df <- foreach::foreach(i = 1:n) %do% {
    # print progress
    if (!quietly) {
      if(i%%100000 == 0 | i == n){
        cat("[", i, "|", n, "]", "\n")
        flush.console()
      }
    }
    .data %>%
      # filter data dynamically to capture the posttreatment windows
      dplyr::filter(
        .,
        game_id == windows$game_id[i],
        game_time >= windows$game_time[i],
        game_time <= windows$posttrt_win[i]
      ) %>%
      # create a column indicating which play the window is for
      dplyr::mutate(., blk = dplyr::first(play_id))
  } %>%
    dplyr::bind_rows(.)
  
  return(blk_df)
}