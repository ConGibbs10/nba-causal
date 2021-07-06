 # dependencies
library(Matching)
library(data.table)
library(lubridate)
library(dtplyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(mgcv)
library(tidyverse)
source("fns/data_cleanse_fns.R")
source("fns/treatment_windows_fns.R")
source("fns/helper_fns.R")
source("fns/append_runs_fns.R")
source("fns/calc_outcomes_fns.R")
source("fns/clean_tc.R")

################################################################################
# function of complete analysis from cleaning to matching with arguments to
# control directory
################################################################################

nba_causal <-
  function(data_from,
           data_to,
           point_thres,
           pretrtwin_lb) {
    # #-------------------------------------------------------------------------------
    # # clean play by play
    # # 00.0_data_pull_21w
    # #-------------------------------------------------------------------------------
    pbp17 <-
      file.path(data_from, 'pbp-nba-stats-correct-2018.rds') %>%
      readr::read_rds() %>%
      prep_pbp(
        .,
        pre_win = pretrtwin_lb,
        post_win = 1,
        exp_delta = 1 / 12,
        plotting_delta = 0.00001
      )

    pbp18 <-
      file.path(data_from, 'pbp-nba-stats-correct-2019.rds') %>%
      readr::read_rds() %>%
      prep_pbp(
        .,
        pre_win = pretrtwin_lb,
        post_win = 1,
        exp_delta = 1 / 12,
        plotting_delta = 0.00001
      )

    pbp <- dplyr::bind_rows(pbp17, pbp18, .id = "season") %>%
      dplyr::mutate(., season = dplyr::if_else(season == "1", "2017.18", "2018.19"))

    readr::write_rds(pbp, file.path(data_to, 'pbp1-21w-prep.rds'), compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished data prep.')

    #-------------------------------------------------------------------------------
    # create pre- and post- treatment blocks for 2017
    # 00.1_post17 and 00.1_pre17
    #-------------------------------------------------------------------------------
    pbp17 <- file.path(data_to, 'pbp1-21w-prep.rds') %>%
      readr::read_rds() %>%
      dplyr::filter(., season == "2017.18") %>%
      dplyr::select(., -season)

    posttrt17 <- create_posttrt_blocks(pbp17)
    readr::write_rds(posttrt17,
                     file.path(data_to, 'post-trt-blocks-1718s-21w.rds'),
                     compress = "xz")
    rm(posttrt17)

    pretrt17 <- create_pretrt_blocks(pbp17)
    readr::write_rds(pretrt17,
                     file.path(data_to, 'pre-trt-blocks-1718s-21w.rds'),
                     compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished constructing blocks for 2017.')

    #-------------------------------------------------------------------------------
    # clean play by play and create pre- and post- treatment blocks for 2018
    # 00.1_post18 and 00.1_pre18
    #-------------------------------------------------------------------------------
    pbp18 <- file.path(data_to, 'pbp1-21w-prep.rds') %>%
      readr::read_rds() %>%
      dplyr::filter(., season == "2018.19") %>%
      dplyr::select(., -season)

    posttrt18 <- create_posttrt_blocks(pbp18)
    readr::write_rds(posttrt18,
                     file.path(data_to, 'post-trt-blocks-1819s-21w.rds'),
                     compress = "xz")
    rm(posttrt18)

    pretrt18 <- create_pretrt_blocks(pbp18)
    readr::write_rds(pretrt18,
                     file.path(data_to, 'pre-trt-blocks-1819s-21w.rds'),
                     compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished constructing blocks for 2018.')

    #-------------------------------------------------------------------------------
    # calculate runs
    # 01_calculate_runs
    #-------------------------------------------------------------------------------
    runs17 <- file.path(data_to, "pre-trt-blocks-1718s-21w.rds") %>%
      readr::read_rds() %>%
      append_runs(., run_criterion = point_thres) %>%
      dplyr::select(.,
                    game_id:to_called,
                    to_in_play,
                    analyze_to:d_run,
                    dplyr::everything())

    runs18 <- file.path(data_to, "pre-trt-blocks-1819s-21w.rds") %>%
      readr::read_rds() %>%
      append_runs(., run_criterion = point_thres) %>%
      dplyr::select(.,
                    game_id:to_called,
                    to_in_play,
                    analyze_to:d_run,
                    dplyr::everything())

    runs <- dplyr::bind_rows(runs17, runs18, .id = "season") %>%
      dplyr::mutate(., season = dplyr::if_else(season == "1", "2017.18", "2018.19"))

    readr::write_rds(runs, file.path(data_to, "pbp2-21w-runs.rds"), compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished calculating runs.')

    # #-------------------------------------------------------------------------------
    # # calculate and/or append covariates
    # # 02_append_covariates
    # #-------------------------------------------------------------------------------
    # create start of season date object
    sos17 <- lubridate::mdy("10-17-2017")
    sos18 <- lubridate::mdy("10-16-2018")

    pbp <- readr::read_rds(file.path(data_to, "pbp2-21w-runs.rds"))

    # append covariates
    pbp17 <- pbp %>%
      dtplyr::lazy_dt() %>%
      dplyr::filter(., season == "2017.18") %>%
      dplyr::left_join(., readr::read_rds(file.path(data_from, "win-prob-1718s.rds"))) %>%
      dplyr::left_join(., readr::read_csv(file.path(data_from, "odds-1718s.csv"))) %>%
      dplyr::as_tibble() %>%
      # fill in the missing win probabilities with the upper adjacent
      dplyr::group_by(., game_id) %>%
      tidyr::fill(., home_wp, away_wp, .direction = "down") %>%
      dplyr::ungroup(.) %>%
      dplyr::mutate(
        .,
        wis = lubridate::interval(sos17, date) %>%
          lubridate::as.duration(.) %>%
          {
            . %/% 604800
          } %>%
          as.integer(.) %>%
          magrittr::add(., 1)
      )
    print('UPDATE: Finished bind on 2017-2018 play-by-play.')

    pbp18 <- pbp %>%
      dtplyr::lazy_dt() %>%
      dplyr::filter(., season == "2018.19") %>%
      dplyr::left_join(., readr::read_rds(file.path(data_from, "win-prob-1819s.rds"))) %>%
      dplyr::left_join(., readr::read_csv(file.path(data_from, "odds-1819s.csv"))) %>%
      dplyr::as_tibble() %>%
      # tidyr::fill in the missing win probabilities with the upper adjacent
      dplyr::group_by(., game_id) %>%
      tidyr::fill(., home_wp, away_wp, .direction = "down") %>%
      dplyr::ungroup(.) %>%
      # get week in season
      dplyr::mutate(
        .,
        wis = lubridate::interval(sos18, date) %>%
          lubridate::as.duration(.) %>%
          {
            . %/% 604800
          } %>%
          as.integer(.) %>%
          magrittr::add(., 1)
      )
    print('UPDATE: Finished bind on 2018-2019 play-by-play.')

    pbp <- dplyr::bind_rows(pbp17, pbp18, .id = "season") %>%
      mutate(., season = dplyr::if_else(season == "1", "2017.18", "2018.19")) %>%
      dplyr::select(
        .,
        season,
        date,
        game_id,
        event,
        event_msg,
        event_act,
        play_id:pretrt_win,
        game_time,
        posttrt_win:play_desc,
        home_id,
        home_name,
        home,
        home_fin,
        home_score,
        margin_score,
        inst_score,
        away_score,
        away_fin,
        away,
        away_name,
        away_id,
        to_called,
        to_in_play,
        analyze_to,
        trt_team,
        rally,
        run,
        run_ind,
        d_run,
        spread,
        OU,
        money,
        home_wp,
        away_wp,
        wis,
        dplyr::everything(),-W,-L,-div_W,-div_L,-status,-Delta_t,-blk
      ) %>%
      dplyr::group_by(., game_id) %>%
      tidyr::fill(
        .,
        date,
        dplyr::starts_with('event'),
        dplyr::starts_with('home'),
        dplyr::starts_with('away'),
        spread,
        OU,
        money,
        wis
      ) %>%
      dplyr::ungroup()

    # write results
    readr::write_rds(pbp, file.path(data_to, "pbp3-21w-runswcov.rds"), compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished appending covariates.')
    
    # #-------------------------------------------------------------------------------
    # # calculate outcomes for every point in time
    # # 03_calculate_outcomes
    # #-------------------------------------------------------------------------------
    pbp <- file.path(data_to, "pbp3-21w-runswcov.rds") %>%
      readr::read_rds() %>%
      dplyr::mutate(.,
                    trt_group = ifelse(
                      play_desc == "Plotting",
                      "neither",
                      dplyr::case_when(
                        analyze_to == TRUE &
                          run_ind == TRUE & to_in_play == TRUE ~ "treatment",
                        analyze_to == TRUE &
                          run_ind == TRUE & to_in_play == FALSE ~ "control",
                        TRUE ~ "neither"
                      )
                    ))

    # calculate area
    outcomes <- y_area(pbp, posttrt_win = 1)
    pbp4 <- dplyr::left_join(pbp, outcomes)

    # save results
    readr::write_rds(pbp4,
                     file.path(data_to, "pbp4-21w-outcomes.rds"),
                     compress = "xz")

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished calculating outcomes.')

    #-------------------------------------------------------------------------------
    # split observations into treatment groups
    # 05_define_groups
    #-------------------------------------------------------------------------------
    pbp <- file.path(data_to, "pbp4-21w-outcomes.rds") %>%
      readr::read_rds() %>%
      dplyr::filter(., play_desc != "Plotting")

    # read in pbp dataset which has been expanded and includes observed outcome
    pool <- pbp %>%
      # remove misreported games and plays which
      dplyr::filter(., trt_group != "neither") %>%
      # redefine based on the treated team
      dplyr::mutate(
        .,
        eor = margin_score,
        bor = margin_score - rally,
        margin_bor = dplyr::case_when(
          bor != 0 & trt_team == "Home" ~ bor,
          bor != 0 &
            trt_team == "Away" ~ -bor,
          bor == 0 ~ 0
        ),
        margin_eor = dplyr::case_when(
          eor != 0 & trt_team == "Home" ~ eor,
          eor != 0 &
            trt_team == "Away" ~ -eor,
          eor == 0 ~ 0
        ),
        time_left = 48 - game_time,
        trt_wp = dplyr::case_when(trt_team == "Home" ~ home_wp,
                                  trt_team == "Away" ~ away_wp),
        spread = dplyr::case_when(trt_team == "Home" ~ spread,
                                  trt_team == "Away" ~ -spread),
        money = dplyr::case_when(trt_team == "Home" ~ money,
                                 trt_team == "Away" ~ -money),
        trt_home = dplyr::if_else(trt_team == "Home", 1, 0)
      ) %>%
      # choose variables to keep
      dplyr::select(
        .,
        date,
        season,
        game_id,
        play_id,
        dplyr::starts_with('event'),
        play_desc,
        clock_time,
        period,
        pretrt_win,
        game_time,
        posttrt_win,
        home_id,
        home,
        home_name,
        home_fin,
        home_score,
        away_score,
        away_fin,
        away_name,
        away,
        away_id,
        margin_score,
        inst_score,
        to_in_play,
        analyze_to,
        trt_group,
        trt_team,
        rally,
        run,
        run_ind,
        d_run,
        margin_bor,
        margin_eor,
        time_left,
        wis,
        spread,
        OU,
        money,
        trt_wp,
        trt_home,
        y_area
      )

    # write results
    readr::write_rds(pool, file.path(data_to, "pool1-21w-overlap.rds"))

    # remove overlap
    pool_no <-
      reduce_timeoutNwindow(pool, pbp, pre_win = pretrtwin_lb, post_win = 1)

    # write results
    readr::write_rds(pool_no, file.path(data_to, "pool2-21w-nooverlap.rds"))

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished defining treatment groups.')
     
    # #-------------------------------------------------------------------------------
    # # define possession as a covariate
    # # 06_possession
    # #-------------------------------------------------------------------------------
    df <-
      readr::read_rds(file.path(data_to, "pool2-21w-nooverlap.rds"))

    pbp_df <-
      dplyr::bind_rows(readr::read_rds(file.path(
        data_from, "pbp-nba-stats-correct-2018.rds"
      )),
      readr::read_rds(file.path(
        data_from, "pbp-nba-stats-correct-2019.rds"
      )))

    poss_team <- dplyr::tibble(
      orig = vector(mode = "character", dim(df)[1]),
      corr = vector(mode = "character", dim(df)[1]),
      desc_home = vector(mode = "character", dim(df)[1]),
      desc_away = vector(mode = "character", dim(df)[1])
    )

    N <- length(poss_team$corr)

    for (i in 1:N) {
      if(i%%20 == 0 | i == N){
        cat("[", i, "|", N, "]", "\n")
        flush.console()
      }
      tmp <- pbp_df %>%
        dplyr::filter(idGame == df$game_id[i], numberEvent == df$event[i]) %>%
        dplyr::select(minuteGame) %>%
        dplyr::pull()
      tmp_2 <- pbp_df %>%
        dplyr::filter(idGame == df$game_id[i], minuteGame > tmp)

      next_play <- tmp_2[1, ]
      play_desc <- dplyr::if_else(
        is.na(next_play$descriptionPlayHome),
        next_play$descriptionPlayVisitor,
        next_play$descriptionPlayHome
      )
      def_foul <- dplyr::if_else(
        grepl("foul", play_desc, ignore.case = T) &
          !grepl("offensive", play_desc, ignore.case = T) &
          !grepl("S.FOUL", play_desc, ignore.case = T) &
          !grepl("shooting block foul", play_desc, ignore.case = T),
        TRUE,
        FALSE
      )
      poss_team$orig[i] <-
        dplyr::if_else(is.na(next_play$descriptionPlayHome),
                       "away", "home")
      poss_team$corr[i] <- poss_team$orig[i]
      if (def_foul == TRUE) {
        poss_team$corr[i] <-
          dplyr::if_else(poss_team$orig[i] == "away", "home", "away")
      }

      poss_team$desc_home[i] <- next_play$descriptionPlayHome
      poss_team$desc_away[i] <- next_play$descriptionPlayVisitor
    }

     df_amend <- dplyr::bind_cols(df, poss_team)

    poss <- df_amend %>%
      dplyr::mutate(
        .,
        trt_poss = dplyr::case_when(
          trt_team == "Home" & corr == "home" ~ 1,
          trt_team == "Away" &
            corr == "away" ~ 1,
          trt_team == "Home" &
            corr == "away" ~ 0,
          trt_team == "Away" &
            corr == "home" ~ 0
        )
      ) %>%
      dplyr::select(., game_id, play_id, trt_poss)

    # save results to treatment and control
    pool_no <- df %>%
      dplyr::left_join(., poss) %>%
      dplyr::select(., season, date:trt_home, trt_poss, y_area)

    readr::write_rds(pool_no, file.path(data_to, "pool3-21w-nooverlapcovs.rds"))

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished defining possesion.')

    #-------------------------------------------------------------------------------
    # address any concerns with positivity
    # 07_validate_units
    #-------------------------------------------------------------------------------
    # read units and save
    units <- file.path(data_to, "pool3-21w-nooverlapcovs.rds") %>%
      readr::read_rds() %>%
      # positivity
      dplyr::filter(., abs(money) < 2400)

    readr::write_rds(units, file.path(data_to, "units-21w.rds"))

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished addressing positivity issues.')

    #-------------------------------------------------------------------------------
    # fit gam model
    # 08_propensity_scores
    #-------------------------------------------------------------------------------

    # load data
    pbp <- file.path(data_to, "pbp4-21w-outcomes.rds") %>%
      readr::read_rds() %>%
      dplyr::filter(., play_desc != "Plotting")
    tc <- file.path(data_to, "units-21w.rds") %>%
      readr::read_rds() %>%
      dplyr::mutate(., treat = dplyr::if_else(trt_group == "treatment", 1, 0),
                    BiT_team = dplyr::if_else(trt_team == "Home", home, away),
                    oppo_team = dplyr::if_else(trt_team == "Home", away, home)) %>%
      # should be empty: including just in case
      dplyr::filter(., game_id %!in% c(21700770, 21800010))

    # generalized additive model
    # Note: Need to remove one of run, margin_bor, margin_eor

    withCallingHandlers({
      conv <- 1
      prop_gam_mod <-
        gam(
          treat ~ s(run, k = 3) + s(d_run) + s(margin_bor) + s(time_left) +
            s(wis) + s(trt_wp) + trt_home + trt_poss + s(spread) + s(OU) +
            s(money) + BiT_team + oppo_team,
          data = tc,
          family = "binomial"
        )
    }, warning = function(w){
      conv <<- 0
    })

    # include in dataset
    tc <-
      tibble::add_column(tc,
                         prop_gam.logodds = as.numeric(predict(prop_gam_mod, type = "link")),
                         prop_gam.odds = as.numeric(predict(prop_gam_mod, type = "response")),
                         prop_conv = conv)

    readr::write_rds(tc, file.path(data_to, "units-21w-covs.rds"))

    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished propensity score matching.')

    # #-------------------------------------------------------------------------------
    # # match on covariates
    # # 09.2_matching_wodds
    # #-------------------------------------------------------------------------------
    # standard matches
    pop_size <- 8000
    max_gen <- 100
    wait_gen <- 4
    hard_max <- TRUE
    dist_tol <- 0.00001

    # troublesome matches (denoted by asterick)
    # pop_size <- 1000
    # max_gen <- 15
    # wait_gen <- 2
    # hard_max <- TRUE
    # dist_tol <- 0.1

    # get data
    pbp <- file.path(data_to, "pbp4-21w-outcomes.rds") %>%
      readr::read_rds() %>%
      dplyr::filter(., play_desc != "Plotting")
    tc <- readr::read_rds(file.path(data_to, "units-21w-covs.rds"))

    # data for matching
    X <-
      dplyr::select(
        tc,
        run,
        d_run,
        margin_bor,
        margin_eor,
        time_left,
        wis,
        spread,
        OU,
        money,
        trt_wp,
        trt_home,
        trt_poss,
        prop_gam.odds
      )
    Y <- dplyr::pull(tc, y_area)
    treat <- tc %>%
      dplyr::mutate(., trt_group = dplyr::if_else(trt_group == "treatment", TRUE, FALSE)) %>%
      dplyr::pull(., trt_group)

    # get matching weights
    genout <- Matching::GenMatch(
      Tr = treat,
      X = X,
      estimand = "ATT",
      pop.size = pop_size,
      max.generations = max_gen,
      hard.generation.limit = hard_max,
      distance.tolerance = dist_tol,
      wait.generations = wait_gen,
      int.seed = 888,
      unif.seed = 6283
    )
    readr::write_rds(genout, file.path(data_to, "genmatch-obj-wodds.rds"))

    # conduct matching by weights
    mout <-
      Matching::Match(
        Y = Y,
        Tr = treat,
        X = X,
        Weight.matrix = genout,
        estimand = "ATT"
      )
    summary(mout)

    # view results of matching
    mb <-
      Matching::MatchBalance(
        treat ~ run + d_run + margin_bor + margin_eor + time_left + wis +
          spread + OU + money + trt_wp + trt_home + trt_poss,
        nboots = 1000,
        match.out = mout,
        data = tc
      )

    # clear variables
    rm(mb, Y, treat)

    # create matched data frames
    matches <- mout %>%
      .$MatchLoopC %>%
      .[, 1:2] %>%
      as.data.frame(.) %>%
      dplyr::mutate(., pair = 1:n()) %>%
      dplyr::rename(., trt = V1, con = V2) %>%
      tidyr::gather(., key = "index_type", value = "index", -pair)

    tc_matched <- tc %>%
      dplyr::mutate(., index = row_number()) %>%
      dplyr::left_join(., matches, by = "index") %>%
      dplyr::filter(., !is.na(pair)) %>%
      dplyr::select(., pair, trt_group, dplyr::everything(), -index, -index_type) %>%
      dplyr::arrange(., pair, trt_group)

    # write_results
    readr::write_rds(tc_matched, file.path(data_to, "matched-units-wodds.rds"))

    # weight covariate
    wts <-
      data.frame(cov = names(X),
                 wgt = diag(genout$Weight.matrix) / sum(diag(genout$Weight.matrix))) %>%
      dplyr::arrange(., wgt)
    readr::write_rds(wts, file.path(data_to, "genmatch-wgts-wodds.rds"))


    rm(list = setdiff(ls(), c('nba_causal', 'params', 'index', 'data_to', 'data_from', 'point_thres', 'pretrtwin_lb')))
    print('UPDATE: Finished matching.')
    print('UPDATE: Finished! Use data generated by this process in conjunction with nba-causal-analysis.')
  }


