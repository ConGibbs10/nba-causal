y_area <- function(pool, posttrt_win = 1) {
  # function to calculate outcome
  y_value <- function(f, rally, game_time, posttrt_win) {
    return(-sign(rally) * suppressWarnings(integrate(
      f, lower = game_time,
      upper = posttrt_win
    ))$value)
  }
  tc <- dplyr::filter(pool, trt_group != "neither")
  ret_obj <- list()
  for (i in 1:nrow(tc)) {
    if(i%%100000 == 0 | i == nrow(tc)){
      cat("[", i, "|", nrow(tc), "]", "\n")
      flush.console()
    }
    event_t <- dplyr::slice(tc, i)
    runsi_df <- pool %>%
      dplyr::filter(., game_id == event_t$game_id) %>%
      dplyr::select(., game_time, margin_score, rally)
    f <-
      approxfun(x = runsi_df$game_time,
                y = runsi_df$margin_score - event_t$margin_score)
    ret_t <-
      dplyr::tibble(
        game_id = event_t$game_id,
        play_id = event_t$play_id,
        game_time = event_t$game_time
      ) %>%
      dplyr::mutate(., y_area = tryCatch(
        y_value(
          f = f,
          rally = event_t$rally,
          game_time = event_t$game_time,
          posttrt_win = event_t$posttrt_win
        ),
        error = function(e)
          NA
      ))
    ret_obj[[i]] <- ret_t
  }
  return(dplyr::bind_rows(ret_obj))
}
