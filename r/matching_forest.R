library(tidyverse)
library(Matching)
library(batch)

# get summit arguments
parseCommandArgs()

# create function for slurm apply
run_match <- function(int.seed, unif.seed){
  
  # fixed arguments
  pop_size <- 8000
  max_gen <- 100
  tc <- readr::read_rds("data/pt9pre2/units-21w-covs.rds")
  
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
  
  # get matching weights dependent on random seeds
  genout <- Matching::GenMatch(
    Tr = treat,
    X = X,
    estimand = "ATT",
    pop.size = pop_size,
    max.generations = max_gen,
    int.seed = int.seed,
    unif.seed = unif.seed
  )
  
  mout <-
    Matching::Match(
      Y = Y,
      Tr = treat,
      X = X,
      Weight.matrix = genout,
      estimand = "ATT"
    )
  
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
  rm(Y, treat)
  
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
  
  # weight covariate
  wts <-
    data.frame(cov = names(X),
               wgt = diag(genout$Weight.matrix) / sum(diag(genout$Weight.matrix))) %>%
    dplyr::arrange(., wgt)
  
  return(list(wts = wts, genout = genout, mout = mout, mbalance = mb, matched = tc_matched))
}

# data frame for random seeds
match_grid <- data.frame(int.seed = 886:905,
                         unif.seed = 6281:6300)

# run function
result <- run_match(int.seed = match_grid[index,]$int.seed,
                    unif.seed = match_grid[index,]$unif.seed)

# write results
write_rds(result, paste0('data/pt9pre2/matching-forest/int', match_grid[index,]$int.seed, '_unif', match_grid[index,]$unif.seed, '.rds'))
