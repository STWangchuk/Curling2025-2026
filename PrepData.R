
library(dplyr)
library(data.table)

Competition <- read.csv("data/Competition.csv")
Competitors <- read.csv("data/Competitors.csv")
Ends        <- read.csv("data/Ends.csv")
Games       <- read.csv("data/Games.csv")
Stones      <- read.csv("data/Stones.csv")
Teams       <- read.csv("data/Teams.csv")
setDT(Stones)

# Shots are coded weirdly
recode_map <- c(`7` = 1, `8` = 2, `9` = 3, `16` = 4, `17` = 5, 
                `18` = 6, `19` = 7, `20` = 8, `21` = 9, `22` = 10)
Stones[, ShotID := recode_map[as.character(ShotID)]]

stone_x_cols <- paste0("stone_", 1:12, "_x")
stone_y_cols <- paste0("stone_", 1:12, "_y")

for (i in 1:12) {
  x_col <- paste0("stone_", i, "_x")
  y_col <- paste0("stone_", i, "_y")
  Stones[, (paste0("Stone_", i, "_Shot")) := !(get(x_col) == 0 & get(y_col) == 0)]
  Stones[, (paste0("Stone_", i, "_OutOfPlay")) := (get(x_col) == 4095 | get(y_col) == 4095)]
}

# Replace 0 and 4095 with NA
Stones[, (stone_x_cols) := lapply(.SD, function(x) { x[x == 0 | x == 4095] <- NA; x }), .SDcols = stone_x_cols]
Stones[, (stone_y_cols) := lapply(.SD, function(x) { x[x == 0 | x == 4095] <- NA; x }), .SDcols = stone_y_cols]

# Distances
for (i in 1:12) {
  x_col <- paste0("stone_", i, "_x")
  y_col <- paste0("stone_", i, "_y")
  Stones[, (paste0("Stone_", i, "_Distance")) := sqrt((get(x_col) - 750)^2 + (get(y_col) - 800)^2)]
  Stones[, (paste0("Stone_", i, "_Behind")) := get(y_col) < 800]
  Stones[, (paste0("Stone_", i, "_Left")) := get(x_col) < 750]
}

setDT(Ends)
setDT(Games)

Ends_Team1 <- merge(
  Ends,
  Games[, .(CompetitionID, SessionID, GameID, TeamID1, TeamID2)],
  by = c("CompetitionID", "SessionID", "GameID"),
  all.x = TRUE
)[, Is_Team1 := (TeamID == TeamID1)
][, .(TeamID1 = first(TeamID1),
      TeamID2 = first(TeamID2),
      Result_Team1 = sum(ifelse(Is_Team1, Result, -Result)),
      Team1_PowerPlay = first(ifelse(Is_Team1, PowerPlay, NA_character_)),
      Team2_PowerPlay = first(ifelse(!Is_Team1, PowerPlay, NA_character_))),
  by = .(CompetitionID, SessionID, GameID, EndID)
]

# Calculate Score Differential at End Level
Ends_Team1 <- Ends_Team1 %>%
  group_by(GameID) %>%
  arrange(EndID) %>%
  mutate(
    ScoreDiffBeforeEnd = cumsum(lag(Result_Team1, default = 0))
  ) %>%
  ungroup() %>%
  select(CompetitionID, SessionID, GameID, EndID, Result_Team1, ScoreDiffBeforeEnd, 
         Team1_PowerPlay, Team2_PowerPlay)

Stones <- merge(Stones, Ends_Team1,
                by = c("CompetitionID", "SessionID", "GameID", "EndID"),
                all.x = TRUE)

Stones[, `:=`(
  Team1_Using_PowerPlay = !is.na(Team1_PowerPlay) & Team1_PowerPlay != "",
  Team2_Using_PowerPlay = !is.na(Team2_PowerPlay) & Team2_PowerPlay != "",
  PowerPlay_Active = (!is.na(Team1_PowerPlay) & Team1_PowerPlay != "") |
    (!is.na(Team2_PowerPlay) & Team2_PowerPlay != "")
)]

# Hammer Logic (Determined by first shots)
first_shots <- Stones[ShotID == 1, .(
  Stone2 = any(Stone_2_Shot, na.rm = TRUE),
  Stone8 = any(Stone_8_Shot, na.rm = TRUE)
), by = .(CompetitionID, SessionID, GameID, EndID)]

first_shots[, Team1_Hammer := fifelse(Stone2 == TRUE & Stone8 != TRUE, 0,
                                      fifelse(Stone8 == TRUE & Stone2 != TRUE, 1, NA_real_))]

Stones <- merge(Stones, first_shots[, .(CompetitionID, SessionID, GameID, EndID, Team1_Hammer)],
                by = c("CompetitionID", "SessionID", "GameID", "EndID"),
                all.x = TRUE)

# Filter to focus on the game starting from the middle, 
# as it's harder to predict beyond that
Stones <- Stones[ShotID > 4]

dist_cols <- paste0("Stone_", 1:12, "_Distance")
y_matrix    <- as.matrix(Stones[, ..stone_y_cols])
x_matrix    <- as.matrix(Stones[, ..stone_x_cols])
dist_matrix <- as.matrix(Stones[, ..dist_cols])

# Engineering some geometric features

blocker_matrix <- (x_matrix > 610 & x_matrix < 890) & 
  (y_matrix > 1200) & 
  (!is.na(x_matrix))
Stones[, Center_Line_Blockers := rowSums(blocker_matrix, na.rm = TRUE)]
Stones[, Button_Is_Open := ifelse(Center_Line_Blockers == 0, 1, 0)]

Stones[, Team1_In_House := rowSums(dist_matrix[, 1:6, drop=FALSE] < 600, na.rm = TRUE)]
Stones[, Team2_In_House := rowSums(dist_matrix[, 7:12, drop=FALSE] < 600, na.rm = TRUE)]
Stones[, Team1_Closest := do.call(pmin, c(as.data.frame(dist_matrix[, 1:6, drop=FALSE]), na.rm = TRUE))]
Stones[, Team2_Closest := do.call(pmin, c(as.data.frame(dist_matrix[, 7:12, drop=FALSE]), na.rm = TRUE))]
Stones[, Team1_Guards := rowSums(!is.na(y_matrix[, 1:6, drop=FALSE]) & (y_matrix[, 1:6, drop=FALSE] > 1200) & (dist_matrix[, 1:6, drop=FALSE] >= 600), na.rm = TRUE)]
Stones[, Team2_Guards := rowSums(!is.na(y_matrix[, 7:12, drop=FALSE]) & (y_matrix[, 7:12, drop=FALSE] > 1200) & (dist_matrix[, 7:12, drop=FALSE] >= 600), na.rm = TRUE)]

Stones[, Team1_Guard_Lateral_Offset := {
  apply(cbind(x_matrix[,1:6], y_matrix[,1:6], dist_matrix[,1:6]), 1, function(z) {
    xvals <- z[1:6]; yvals <- z[7:12]; dvals <- z[13:18]
    is_guard <- !is.na(yvals) & !is.na(dvals) & (yvals > 1200) & (dvals >= 600)
    if (any(is_guard)) mean(abs(xvals[is_guard] - 750), na.rm = TRUE) else 999
  })
}]
Stones[, Team2_Guard_Lateral_Offset := {
  apply(cbind(x_matrix[,7:12], y_matrix[,7:12], dist_matrix[,7:12]), 1, function(z) {
    xvals <- z[1:6]; yvals <- z[7:12]; dvals <- z[13:18]
    is_guard <- !is.na(yvals) & !is.na(dvals) & (yvals > 1200) & (dvals >= 600)
    if (any(is_guard)) mean(abs(xvals[is_guard] - 750), na.rm = TRUE) else 999
  })
}]
Stones[, Team1_Guard_Quality_Score := ifelse(Team1_Guards > 0, 1000 / (1 + Team1_Guard_Lateral_Offset), 0)]
Stones[, Team2_Guard_Quality_Score := ifelse(Team2_Guards > 0, 1000 / (1 + Team2_Guard_Lateral_Offset), 0)]

Stones[, Team1_Stones_Behind_Guards := {
  apply(cbind(y_matrix[,1:6], dist_matrix[,1:6]), 1, function(z) {
    yvals <- z[1:6]; dvals <- z[7:12]
    is_guard <- !is.na(yvals) & !is.na(dvals) & (yvals > 1200) & (dvals >= 600)
    if (!any(is_guard)) return(0L)
    min_guard_y <- min(yvals[is_guard], na.rm = TRUE)
    is_stone_not_guard <- !is.na(yvals) & !is_guard
    sum(is_stone_not_guard & (yvals < min_guard_y), na.rm = TRUE)
  })
}]
Stones[, Team2_Stones_Behind_Guards := {
  apply(cbind(y_matrix[,7:12], dist_matrix[,7:12]), 1, function(z) {
    yvals <- z[1:6]; dvals <- z[7:12]
    is_guard <- !is.na(yvals) & !is.na(dvals) & (yvals > 1200) & (dvals >= 600)
    if (!any(is_guard)) return(0L)
    min_guard_y <- min(yvals[is_guard], na.rm = TRUE)
    is_stone_not_guard <- !is.na(yvals) & !is_guard
    sum(is_stone_not_guard & (yvals < min_guard_y), na.rm = TRUE)
  })
}]

Stones[, Team1_Has_Closest := Team1_Closest < Team2_Closest]
Stones[, Current_Score_Team1 := ifelse(
  Team1_Has_Closest,
  rowSums(dist_matrix[, 1:6, drop=FALSE] < Team2_Closest, na.rm = TRUE),
  -1 * rowSums(dist_matrix[, 7:12, drop=FALSE] < Team1_Closest, na.rm = TRUE)
)]
Stones[, Stones_On_Ice := rowSums(!is.na(dist_matrix), na.rm = TRUE)]

calculate_cluster_score <- function(x_vals, y_vals) {
  coords <- cbind(x_vals, y_vals)
  # Remove stones that are not on the ice
  coords <- coords[complete.cases(coords), , drop = FALSE]
  
  if (nrow(coords) < 2) {
    return(2000)
  }
  return(mean(as.numeric(dist(coords))))
}

calculate_vertical_spread <- function(y_vals) {
  y_clean <- y_vals[!is.na(y_vals)]
  if (length(y_clean) < 2) return(0)
  return(sd(y_clean))
}

Stones[, Team1_Cluster_Score := apply(cbind(x_matrix[, 1:6], y_matrix[, 1:6]), 1, 
                                      function(r) calculate_cluster_score(r[1:6], r[7:12]))]

Stones[, Team2_Cluster_Score := apply(cbind(x_matrix[, 7:12], y_matrix[, 7:12]), 1, 
                                      function(r) calculate_cluster_score(r[1:6], r[7:12]))]

Stones[, Team1_Vertical_Spread := apply(y_matrix[, 1:6], 1, calculate_vertical_spread)]
Stones[, Team2_Vertical_Spread := apply(y_matrix[, 7:12], 1, calculate_vertical_spread)]

Stones[, Cluster_Diff := Team1_Cluster_Score - Team2_Cluster_Score]

# Begin working on cleaning the data for the model from here

model_data <- as.data.frame(Stones)

model_data <- model_data %>%
  mutate(
    Win = ifelse(Result_Team1 > 0, 1, 0),
    Hammer = Team1_Hammer,
    StoneAdv = Team1_In_House - Team2_In_House,
    ClosestAdv = Team2_Closest - Team1_Closest,
    GuardQualityAdv = Team1_Guard_Quality_Score - Team2_Guard_Quality_Score,
    LateralOffsetAdv = Team2_Guard_Lateral_Offset - Team1_Guard_Lateral_Offset,
    StonesHiddenAdv = Team1_Stones_Behind_Guards - Team2_Stones_Behind_Guards,
    ScoreDiff = ScoreDiffBeforeEnd,
    EndsRemaining = max(EndID) - EndID + 1,
    
    # This is useful for the steal probability we added later
    Steal_Happened = ifelse((Hammer == 1 & Result_Team1 < 0) | 
                              (Hammer == 0 & Result_Team1 > 0), 1, 0),
    
    # Interactions!
    Hammer_x_StoneAdv = Hammer * StoneAdv,
    ScoreDiff_x_Hammer = ScoreDiff * Hammer,
    ScoreDiff_x_EndsRemaining = ScoreDiff / (EndsRemaining + 1)
  ) %>%
  # Handle data issues here
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), 0, .))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))

saveRDS(model_data, "Data.rds")