append_runs <- function(.pretrt_blk, run_criterion = 9) {
  require(data.table)
  require(dtplyr)
  require(dplyr, warn.conflicts = FALSE)
  source("fns/helper_fns.R")
  
  pre_lz <- dtplyr::lazy_dt(.pretrt_blk)
  
  # keep time
  tic <- Sys.time()
  
  runs <- pre_lz %>%
    # conduct all calculation within blocks, being plays.
    dplyr::group_by(blk) %>%
    # recall, margin_score is defined as home score less away score
    # rally := create a vector of margin_score at time t less margin_score at time t-1, t-2,
    #  ..., t-n such that t-n is the first play in the pretreatment window. Now,
    #  find the most extreme (i.e. largest in absolute value while maintaining sign).
    # d_run := take time t less time t_i where t_i is the time at which the rally
    # occured. If the rally occured multiple times in the pretreatment window, return
    # the smallest difference in time (i.e. use the time in which t_i is closest to t)
    dplyr::mutate(
      rally = max_abs(dplyr::last(margin_score) - margin_score),
      d_run = dplyr::last(game_time) - game_time[which_max_abs(dplyr::last(margin_score) - margin_score)]
    ) %>%
    # return game time t
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    # identify rallys which are runs by definition
    dplyr::mutate(
      run = abs(rally),
      run_ind = run >= run_criterion,
      trt_team = dplyr::case_when(rally < 0 ~ "Home",
                                  rally > 0 ~ "Away",
                                  rally == 0 ~ "Either"),
      Delta_t = dplyr::case_when(
        trt_team == "Home" ~ home_score - away_score,
        trt_team == "Away" ~ away_score - home_score,
        # if there was no rally, i.e. no run, default to home - score for Delta_t
        # these observations will be filtered out in the future since they don't
        # fit run criterion
        trt_team == "Either" ~ home_score - away_score
      )
    ) %>%
    dplyr::select(
      game_id:inst_score,
      to_called,
      analyze_to,
      rally,
      trt_team,
      Delta_t,
      run_ind,
      run,
      d_run,
      dplyr::everything()
    ) %>%
    dplyr::as_tibble()
  
  toc <- Sys.time()
  print(toc - tic)
  
  return(runs)
}
