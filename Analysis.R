rm(list = ls())
library(dplyr)
library(xgboost)
library(caret)
library(ggplot2)
library(tidyr)
set.seed(123)

# Honestly this is just to prevent me from having to wait 5 minutes every single time I try rendering this
if (!dir.exists("models")) {
  dir.create("models")
}

model_data <- readRDS("Data.rds")

# Steal model is here
steal_model_path <- "models/steal_model.rds"
xsteal_prob_path <- "models/xsteal_prob.rds"

if (file.exists(steal_model_path) && file.exists(xsteal_prob_path)) {
  cv_results <- readRDS(steal_model_path)
  model_data$xSteal_Prob <- readRDS(xsteal_prob_path)
} else {
  
  steal_features <- c(
    "StoneAdv", "ClosestAdv",
    "Team1_In_House", "Team2_In_House",
    "Center_Line_Blockers", "Button_Is_Open",
    "Team1_Closest", "Team2_Closest",
    "Team1_Guard_Quality_Score", "Team2_Guard_Quality_Score",
    "ShotID", "Stones_On_Ice", "Hammer"
  )
  
  valid_indices <- which(!is.na(model_data$Steal_Happened) &
                           !is.infinite(model_data$Steal_Happened))
  
  dtrain_cv <- xgb.DMatrix(
    data = as.matrix(model_data[valid_indices, steal_features]),
    label = model_data$Steal_Happened[valid_indices]
  )
  
  cv_results <- xgb.cv(
    data = dtrain_cv,
    nrounds = 600,
    nfold = 5,
    max_depth = 7,
    eta = 0.05,
    objective = "binary:logistic",
    verbose = 0,
    prediction = TRUE
  )
  
  model_data$xSteal_Prob <- NA
  model_data$xSteal_Prob[valid_indices] <- cv_results$pred
  
  global_mean_prob <- mean(model_data$xSteal_Prob, na.rm = TRUE)
  model_data$xSteal_Prob[is.na(model_data$xSteal_Prob)] <- global_mean_prob
  
  saveRDS(cv_results, steal_model_path)
  saveRDS(model_data$xSteal_Prob, xsteal_prob_path)
}

model_data <- model_data %>%
  mutate(
    Hammer_x_StealProb = Hammer * xSteal_Prob,
    Steal_Pressure = xSteal_Prob * (11 - ShotID)
  )

# Main model starts from here
main_model_path <- "models/main_model.rds"
cv_preds_path <- "models/cv_predictions.rds"
importance_path <- "models/importance_matrix.rds"

final_features <- c(
  "ScoreDiff", "EndsRemaining", "ScoreDiff_x_Hammer", "ScoreDiff_x_EndsRemaining",
  "Hammer", "StoneAdv", "ClosestAdv", "Stones_On_Ice", "ShotID",
  "Current_Score_Team1",
  "Center_Line_Blockers", "Button_Is_Open",
  "LateralOffsetAdv", "StonesHiddenAdv",
  "GuardQualityAdv",
  "xSteal_Prob", "Steal_Pressure", "Hammer_x_StealProb",
  "Team1_Using_PowerPlay", "Team2_Using_PowerPlay",
  "Team1_Cluster_Score", "Team2_Cluster_Score", "Cluster_Diff",
  "Team1_Vertical_Spread", "Team2_Vertical_Spread"
)

model_data <- model_data %>% filter(!is.na(Win), !is.na(Hammer))

final_matrix <- as.matrix(model_data[, final_features])
final_y <- model_data$Win
final_matrix[is.na(final_matrix)] <- 0

unique_games <- unique(model_data$GameID)
k <- 5 
set.seed(123)
game_folds <- sample(cut(seq_along(unique_games), breaks = k, labels = FALSE))
game_fold_map <- data.frame(GameID = unique_games, Fold = game_folds)

model_data_with_folds <- model_data %>%
  left_join(game_fold_map, by = "GameID") %>%
  mutate(Fold = factor(Fold))

train_control_indices <- lapply(1:k, function(i) {
  which(model_data_with_folds$Fold != i)
})
names(train_control_indices) <- paste0("Fold", 1:k)

train_control <- trainControl(
  method = "cv",
  number = k,
  index = train_control_indices, 
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

if (file.exists(main_model_path) && file.exists(cv_preds_path)) {
  xgb_model_cv <- readRDS(main_model_path)
  oof_preds_df <- readRDS(cv_preds_path)
  importance_matrix <- readRDS(importance_path)
} else {
  
  xgb_model_cv <- train(
    x = final_matrix,
    y = factor(final_y, levels = c(0, 1), labels = c("Loss", "Win")),
    method = "xgbTree",
    trControl = train_control,
    metric = "ROC",
    tuneGrid = data.frame(
      nrounds = 600,
      max_depth = 7,
      eta = 0.05,
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
  )
  
  oof_preds_df <- xgb_model_cv$pred %>% arrange(rowIndex)
  
  xgb_model_final <- xgboost(
    data = final_matrix,
    label = final_y,
    nrounds = 600,
    max_depth = 7,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = "binary:logistic",
    eval_metric = "logloss",
    verbose = 0
  )
  
  importance_matrix <- xgb.importance(feature_names = final_features, model = xgb_model_final)
  saveRDS(xgb_model_cv, main_model_path)
  saveRDS(oof_preds_df, cv_preds_path)
  saveRDS(importance_matrix, importance_path)
}

pred_probs_oof <- oof_preds_df$Win
pred_class_oof <- ifelse(pred_probs_oof > 0.5, 1, 0)

conf_mat <- confusionMatrix(factor(pred_class_oof), factor(final_y[oof_preds_df$rowIndex]), positive="1")

results_df <- data.frame(
  ShotID = model_data_with_folds$ShotID[oof_preds_df$rowIndex],
  Actual = final_y[oof_preds_df$rowIndex],
  Predicted = pred_class_oof
)

accuracy_by_shot <- results_df %>%
  group_by(ShotID) %>%
  summarise(
    Total_Shots = n(),
    Correct = sum(Actual == Predicted),
    Accuracy = Correct / Total_Shots
  ) %>%
  ungroup()

# This sections mostly useful for visualization later on
shot_9 <- model_data %>%
  filter(ShotID == 9) %>%
  select(CompetitionID, SessionID, GameID, EndID, Team1_Has_Closest) %>%
  distinct(CompetitionID, SessionID, GameID, EndID, .keep_all = TRUE) %>%
  mutate(Won_After_Shot9 = ifelse(Team1_Has_Closest, 1, 0)) %>%
  select(-Team1_Has_Closest)

final_outcome <- model_data %>%
  filter(ShotID == 10) %>%
  select(CompetitionID, SessionID, GameID, EndID, Win) %>%
  distinct(CompetitionID, SessionID, GameID, EndID, .keep_all = TRUE) %>%
  rename(Final_Win = Win)

matrix_data <- shot_9 %>%
  inner_join(final_outcome, by = c("CompetitionID", "SessionID", "GameID", "EndID")) %>%
  group_by(Won_After_Shot9, Final_Win) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(
    Shot9 = ifelse(Won_After_Shot9 == 1, "T1 Ahead", "T2 Ahead"),
    Final = ifelse(Final_Win == 1, "T1 Won", "T2 Won"),
    Percentage = (Count / sum(Count)) * 100
  ) %>%
  select(Shot9, Final, Count, Percentage)

matrix_data <- head(matrix_data, 4)