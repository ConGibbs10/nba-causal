# remove unit if there is a timeout in the window
reduce_timeoutNwindow <-
  function(tc_df,
           pbp,
           pre_win = 2,
           post_win = 1,
           quietly = FALSE) {
    require(tidyverse)
    
    red <- list()
    for (i in 1:nrow(tc_df)) {
      if (!quietly) {
        if(i%%100000 == 0 | i == nrow(tc_df)){
          cat("[", i, "|", nrow(tc_df), "]", "\n")
          flush.console()
        }
      }
      # get the ith row in data set holding potential treatments and controls
      crow_i <- dplyr::slice(tc_df, i)
      # get all instances of timeouts within the pre-treatment or post-treatment window
      tosNwin <- pbp %>%
        # get all plays in the window of the potential control or treatment, except
        # for the potential control or treatment itself
        dplyr::filter(
          .,
          game_id == crow_i$game_id,
          game_time >= crow_i$game_time - pre_win,
          game_time <= crow_i$game_time + post_win,
          game_time != crow_i$game_time
        ) %>%
        # isolate only plays involving timeouts (regardless of whether there was a
        # run at this time)
        dplyr::filter(., to_in_play)
      
      # return nothing if there were no other timeouts in the window
      if (dim(tosNwin)[1] == 0) {
        red[[i]] <- NA
      }
      # return the potential control or treatment associated with a timeout in the window;
      # these will be removed from the pool
      else{
        red[[i]] <- crow_i
      }
    }
    # keep only the unique plays which have a timeout in the window
    tc2remove <-
      dplyr::distinct(dplyr::bind_rows(Filter(Negate(anyNA), red)), .keep_all = TRUE)
    # remove the plays which don't fit criteria 2 or 3 from the pool
    if (is_empty(tc2remove)) {
      print("No plays removed because of other timeouts in windows.")
      tc_red <- tc_df
    } else{
      tc_red <- dplyr::anti_join(tc_df, tc2remove)
    }
    
    return(tc_red)
  }

# # write function to find the maximal disjoint set of intervals for each game
# MDS <- function(arranged_pool){
#   require(intervals)
#
#   # ensure they are arranged
#   stopifnot(identical(dplyr::arrange(arranged_pool, game_id, game_time), arranged_pool))
#
#   # get ids to loop over, last for printing, and list for results
#   ids <- unique(arranged_pool$game_id)
#   last <- tail(ids, 1)
#   units_mds <- list()
#
#   # write function to find maximumal set of disjoint intervals
#   calc_max_disjoint <- function(grouped_pool){
#
#     intervals <- grouped_pool %>%
#       dplyr::select(., pretrt_win, posttrt_win) %>%
#       as.matrix() %>%
#       Intervals(., closed = c(TRUE, TRUE), type = 'R')
#     final <- intervals[1]
#     intervals <- intervals[-1]
#
#     N <- nrow(intervals)
#     while(!purrr::is_empty(intervals)){
#       if(purrr::is_empty(interval_intersection(final, intervals[N]))){
#         final <- c(final, intervals[N])
#         intervals <- intervals[-N]
#         N <- nrow(intervals)
#       }else{
#         final2rm <- which(interval_overlap(from = final, to = intervals[N]) == 1)
#         final <- c(final[-final2rm], intervals[N])
#         intervals <- intervals[-N]
#         N <- nrow(intervals)
#       }
#     }
#     final <- as.matrix(final)
#     grouped_pool_times <- dplyr::select(grouped_pool, pretrt_win, posttrt_win)
#
#     to_keep <- !is.na(match(paste0(grouped_pool_times$pretrt_win, grouped_pool_times$posttrt_win),
#                             paste0(final[,1], final[,2])))
#
#     return(as.data.frame(dplyr::filter(grouped_pool, to_keep)))
#   }
#
#   # execute function across groups
#   for(i in 1:length(ids)){
#     cat(ids[i], 'of', last, '\n')
#     units_mds[[i]] <- arranged_pool %>%
#       dplyr::filter(., game_id == ids[i]) %>%
#       calc_max_disjoint(.)
#   }
#
#   units_mds <- dplyr::bind_rows(units_mds)
#   return(units_mds)
# }
#
# # write function to assess whether there is a control in the pretreatment window
# controlNpre <- function(units, units2search = units, pbp, col_name = 'conN'){
#   pbp <- dplyr::filter(pbp, play_desc != "Plotting")
#   conN <- vector(length = nrow(units))
#   # get all of the controls from the pool of units
#   units_tmp <- units2search %>%
#     dplyr::filter(., trt_group == 'control') %>%
#     dplyr::select(., game_id, play_id, trt_group)
#   units2search_str <- paste0(units_tmp$game_id, units_tmp$play_id, units_tmp$trt_group)
#   for(i in 1:nrow(units)){
#     cat("[", i, "|", nrow(units), "]", "\n"); flush.console()
#     crow_i <- dplyr::slice(units, i)
#     # get the pre-treatment window of the ith unit
#     win <- pbp %>%
#       # don't include the unit under consideration
#       dplyr::filter(., game_id == crow_i$game_id,
#              game_time >= crow_i$pretrt_win,
#              game_time <= crow_i$game_time,
#              play_id != crow_i$play_id) %>%
#       dplyr::select(., game_id, play_id, trt_group)
#
#     # do any controls from the pool of units appear in the pre-treatment window of
#     # the ith unit, not considering the ith unit?
#     conN[i] <- !all(is.na(match(units2search_str,
#                                 paste0(win$game_id, win$play_id, win$trt_group))))
#   }
#   conN_df <- tibble::add_column(units, !!col_name := conN)
#   return(conN_df)
# }
