library(tidyverse)
library(batch)
# load function
source('r/driver.R')

# get summit arguments
parseCommandArgs()

# define grid to conduct analysis over
params <- expand.grid(pts = 7:10, pretrt = seq(1.5, 3, 0.5)) %>%
  dplyr::mutate(file2 = stringr::str_c('data/pt', pts, 'pre', pretrt) %>%
                  stringr::str_replace_all(., pattern = '\\.', replacement = 'y')) %>%
  dplyr::select(file2, pts, pretrt)

# run function
nba_causal(data_from = 'data',
           data_to = params[index,]$file2,
           point_thres = params[index,]$pts,
           pretrtwin_lb = params[index,]$pretrt)

# library(furrr)
# plan(multisession)
# furrr::future_imap(1:16, ~{
#   index <- .x
#   nba_causal(data_from = 'data',
#              data_to = params[index,]$file2,
#              point_thres = params[index,]$pts,
#              pretrtwin_lb = params[index,]$pretrt)
# })

# library(purrr)
# purrr::imap(1:16, ~{
#   index <- .x
#   cat(index, '\n')
#   nba_causal(data_from = 'data',
#              data_to = params[index,]$file2,
#              point_thres = params[index,]$pts,
#              pretrtwin_lb = params[index,]$pretrt)
# })
