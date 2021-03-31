library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)



all_help_no_teams <- all_help_no_teams %>%
  mutate(label= total_wins)

model_data <- all_help_no_teams %>%
  select(-total_wins)

smp_size <- floor(0.80 * nrow(model_data))
set.seed(123)
ind <- sample(seq_len(nrow(model_data)), size = smp_size)
ind_train <- model_data[ind, ]
ind_test <- model_data[-ind, ]

full_train <- xgboost::xgb.DMatrix(as.matrix(ind_train %>% select(-label)), label = as.integer(ind_train$label))

set.seed(123)
cv_pwaa <- xgboost::xgb.cv( params = params, 
                          data = full_train, 
                          nrounds = 2000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)


nrounds <- 338
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 17,
    eta = .025,
    gamma = 2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 1
  )

pwaa_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

imp <- xgb.importance(colnames(ind_train), model = pwaa_model)
xgb.plot.importance(imp)

model_data <- model_data %>%
  mutate(index = row_number())

predicted_wins <- stats::predict(pwaa_model,
                                    as.matrix(model_data %>%
                                                select(season, sos, rush_epa, 
                                                       def_epa, pblk, recv))) %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(purrr::map_dfr(seq_along(model_data$index), function(x) {
    tibble::tibble("wins_pred" = 0:16,
                   "season" = model_data$season[[x]],
                   "sos" = model_data$sos[[x]],
                   "rush_epa" = model_data$rush_epa[[x]],
                   "def_epa" = model_data$def_epa[[x]],
                   "pblk" = model_data$pblk[[x]],
                   "recv" = model_data$recv[[x]],
                   "index" = model_data$index[[x]])
  })) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(cum_prob = cumsum(.data$prob),
                prob = .data$prob) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::summarise(pred_wins = sum(.data$prob * .data$wins_pred)) %>%
  ungroup()   

all_help <- all_help %>%
  mutate(index = row_number())

main_pwaa <- all_help %>%
  inner_join(predicted_wins) 

main_pwaa <- main_pwaa %>%
  mutate(pwaa = total_wins - pred_wins)

