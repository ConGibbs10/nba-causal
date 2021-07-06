#- CLEAN AND PREPARE DATA

prep_pbp <-
  function(.data,
           pre_win = 2,
           post_win = 1,
           exp_delta = 1,
           plotting_delta = 0.00001) {
    
    require(tidyverse)
    require(dtplyr)
    source("fns/helper_fns.R")
    as.na <- function(x) {
      x[] <- NA
      x
    }
    
    prepped_pbp <- .data %>%
      # filter out overtime and misreported games
      dplyr::filter(., numberPeriod <= 4, idGame %!in% c(21700025)) %>%
      # fill scores by game
      dplyr::group_by(., idGame) %>%
      dplyr::mutate(.,
                    scoreHome = replace(scoreHome, 1, 0),
                    scoreAway = replace(scoreAway, 1, 0)) %>%
      tidyr::fill(., scoreHome, scoreAway) %>%
      dplyr::ungroup(.) %>%
      # condense play description, descriptor of the team yielding the play description,
      # round time to be as precise as the plotting_delta, and see if the play was a timeout
      dplyr::mutate(
        .,
        minuteGame = round(minuteGame, ndecimal_places(plotting_delta)),
        play_desc = dplyr::case_when(
          minuteGame == 0 ~ 'Begin Regulation',
          minuteGame == 48 ~ 'End Regulation',
          TRUE ~ dplyr::if_else(
            is.na(descriptionPlayHome),
            descriptionPlayVisitor,
            descriptionPlayHome
          )
        ),
        play_team = dplyr::if_else(is.na(descriptionPlayHome), "Away", "Home"),
        to_called = stringr::str_detect(play_desc, "Timeout")
      ) %>%
      # get rid of non-plays
      dplyr::filter(.,!is.na(play_desc)) %>%
      # identify if there was a timout at any point at a fixed time where there are
      # simultaneous events
      dplyr::group_by(., idGame, minuteGame) %>%
      dplyr::mutate(., to_in_play = any(to_called)) %>%
      # take the last scoring play
      dplyr::slice(., dplyr::n()) %>%
      dplyr::ungroup(.)
    
    # make fake plays that allow for plotting runs and accurately calculating outcomes
    pbp_exp.l <- list()
    for (i in 1:nrow(prepped_pbp)) {
      if(i%%100000 == 0 | i == nrow(prepped_pbp)){
        cat("[", i, "|", nrow(prepped_pbp), "]", "\n")
        flush.console()
      }
      # get ith play
      tmp <- prepped_pbp[i, ]
      # get (i-1)th play, making one up if necessary
      if (i != 1) {
        tmp_before <- prepped_pbp[(i - 1),]
      } else{
        tmp_before <-
          dplyr::mutate(
            tmp,
            scoreHome = NA,
            scoreAway = NA,
            margin_score = NA
          )
      }
      # create plotting play
      newrow <- tmp %>%
        # start with ith row, making all fields NA
        dplyr::mutate_all(., as.na) %>%
        # create fields appropriate for intermediary of the (i-1)th and ith row
        dplyr::mutate(
          .,
          idGame = tmp$idGame,
          minuteGame = tmp$minuteGame - plotting_delta,
          scoreHome = tmp_before$scoreHome,
          scoreAway = tmp_before$scoreAway,
          play_desc = 'Plotting',
          play_team = 'Neither'
        )
      # create a dataframe of the plotting play and the ith row; save to a list
      # which is to be combined
      pbp_exp.l[[i]] <- dplyr::bind_rows(newrow, tmp)
    }
    
    # create data frame of real plays and plotting plays from list of pairs
    pbp_exp.t <- pbp_exp.l %>%
      dplyr::bind_rows(.) %>%
      # get rid of edge effect for each game by filtering out BS game times
      dplyr::filter(., minuteGame >= 0, minuteGame <= 48) %>%
      # round to ensure that the plotting plays and real plays are measured to the
      # same machine precision
      dplyr::mutate(., minuteGame = round(minuteGame, ndecimal_places(plotting_delta)))
    
    # expand the plays for every exp_delta of a minute
    pbp_exp <- prepped_pbp %>%
      dplyr::distinct(., idGame) %>%
      dplyr::group_by(., idGame) %>%
      dplyr::do(
        .,
        data.frame(
          idGame = .$idGame,
          minuteGame = seq(0L, 48L, by = exp_delta),
          play_desc = "Expanded",
          stringsAsFactors = FALSE
        )
      ) %>%
      # combine the fine grid with the real data
      dplyr::bind_rows(pbp_exp.t, .) %>%
      # keep fine grid and real data on the same machine precision
      dplyr::mutate(
        .,
        minuteGame = round(minuteGame, ndecimal_places(plotting_delta)),
        # create helper variable which is the combined score
        tot_score = scoreHome + scoreAway
      ) %>%
      # sort rows by game, game time, and total score; any NA for total score will
      # will become the last row in groups defined by the game and game time
      dplyr::arrange(., idGame, minuteGame, tot_score) %>%
      # fill down some game specific variables; filling scoreHome and scoreAway after
      # the arrange will turn the NA into the most up to date score
      dplyr::group_by(., idGame) %>%
      tidyr::fill(
        .,
        timeStringWC,
        numberPeriod,
        scoreHome,
        scoreAway,
        numberEvent,
        numberEventMessageType,
        numberEventActionType
      ) %>%
      dplyr::ungroup(.) %>%
      # calculate overwrite margin score
      dplyr::mutate(
        .,
        marginScore = scoreHome - scoreAway,
        # create unique ID which is defined from the game and game time
        play_id = dplyr::group_indices(., idGame, minuteGame),
        # pre-treatment window
        pretrt_win = dplyr::case_when(
          numberPeriod == 1 ~ pmax(minuteGame - pre_win, 0),
          numberPeriod == 2 ~ pmax(minuteGame - pre_win, 12),
          numberPeriod == 3 ~ pmax(minuteGame - pre_win, 24),
          numberPeriod == 4 ~ pmax(minuteGame - pre_win, 36)
        ),
        # post-treatment window
        posttrt_win = dplyr::case_when(
          numberPeriod == 1 ~ pmin(minuteGame + post_win, 12),
          numberPeriod == 2 ~ pmin(minuteGame + post_win, 24),
          numberPeriod == 3 ~ pmin(minuteGame + post_win, 36),
          numberPeriod == 4 ~ pmin(minuteGame + post_win, 48)
        ),
        # is the post-treatment window non-censored?
        analyze_to = dplyr::case_when(
          numberPeriod == 1 & minuteGame <= (12 - post_win) ~ TRUE,
          numberPeriod == 2 &
            minuteGame <= (24 - post_win) ~ TRUE,
          numberPeriod == 3 &
            minuteGame <= (36 - post_win) ~ TRUE,
          numberPeriod == 4 &
            minuteGame <= (48 - post_win) ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      # remove extra rows if they exist and recheck for timeouts
      dplyr::group_by(., idGame, minuteGame) %>%
      # if the expanded play falls on a real play, keep data from real play
      dplyr::mutate(
        .,
        play_team = coalesce_rows(play_team),
        to_called = coalesce_rows(to_called),
        to_in_play = coalesce_rows(to_in_play)
      ) %>%
      # keep only the last scoring play
      dplyr::slice(., dplyr::n()) %>%
      dplyr::ungroup(.) %>%
      # if the expanded play doesn't fall on a real play, input data
      # calculate instantaneous score, i.e. change in score attributed to that specific play
      dplyr::mutate(
        .,
        play_team = ifelse(is.na(play_team), 'Neither', play_team),
        to_called = ifelse(is.na(to_called), FALSE, to_called),
        to_in_play = ifelse(is.na(to_in_play), FALSE, to_in_play),
        inst_score = dplyr::if_else(
          marginScore != lag(marginScore, default = 0),
          marginScore - lag(marginScore, default = 0),
          0
        )
      ) %>%
      # rename, reorder, and remove columns, as desired
      dplyr::select(
        .,
        game_id = idGame,
        play_id,
        event = numberEvent,
        event_msg = numberEventMessageType,
        event_act = numberEventActionType,
        period = numberPeriod,
        clock_time = timeStringWC,
        pretrt_win,
        game_time = minuteGame,
        posttrt_win,
        play_team,
        play_desc,
        home_score = scoreHome,
        away_score = scoreAway,
        margin_score = marginScore,
        inst_score,
        analyze_to,
        to_called,
        to_in_play = to_in_play
      )
    
    return(pbp_exp)
  }