# =============================================================================
# Second Spectrum Data Processing Functions
# NY Liberty Post-Game Report
# =============================================================================

library(jsonlite)
library(dplyr)
library(tidyr)

# =============================================================================
# LOAD DATA FUNCTIONS
# =============================================================================

load_game_data <- function(data_dir) {
  #' Load all Second Spectrum data files from a directory
  #' @param data_dir Path to directory containing JSON files
  #' @return List containing all game data
  
  list(
    metadata = fromJSON(file.path(data_dir, "metadata_json.json")),
    advanced_box = fromJSON(file.path(data_dir, "basketball-advanced-box.json")),
    possessions = fromJSON(file.path(data_dir, "basketball-possessions.json")),
    events = read_jsonl(file.path(data_dir, "basketball-events.jsonl"))
  )
}

read_jsonl <- function(filepath) {
  #' Read JSONL (JSON Lines) file
  lines <- readLines(filepath, warn = FALSE)
  lapply(lines, fromJSON) %>% bind_rows()
}

# =============================================================================
# EXTRACT GAME INFO
# =============================================================================

extract_game_info <- function(metadata) {
  #' Extract basic game information
  
  data <- metadata$data
  
  list(
    game_id = data$id,
    date = sprintf("%d/%d/%d", data$month, data$day, data$year),
    home_team = data$homeTeamName,
    away_team = data$awayTeamName,
    home_team_id = data$homeTeam$id,
    away_team_id = data$awayTeam$id,
    home_score = data$homeScore,
    away_score = data$awayScore,
    home_players = data$homeTeam$players %>% 
      select(id, name, number, position) %>%
      mutate(team = "home"),
    away_players = data$awayTeam$players %>% 
      select(id, name, number, position) %>%
      mutate(team = "away")
  )
}

# =============================================================================
# QUARTER-BY-QUARTER STATS
# =============================================================================

calc_quarter_stats <- function(possessions, game_info) {
  #' Calculate quarter-by-quarter statistics
  
  poss_df <- possessions$possessions %>%
    filter(period <= 4) %>%
    mutate(
      team = ifelse(teamId == game_info$home_team_id, "home", "away")
    )
  
  # Aggregate by quarter and team
  quarter_stats <- poss_df %>%
    group_by(team, period) %>%
    summarise(
      pts = sum(ptsScored),
      poss = n(),
      fga = sum(sapply(counts, function(x) x$FGA %||% 0)),
      fgm = sum(sapply(counts, function(x) x$FGM %||% 0)),
      tov = sum(sapply(counts, function(x) x$TO %||% 0)),
      orb = sum(sapply(counts, function(x) x$ORB %||% 0)),
      ast = sum(sapply(counts, function(x) x$AST %||% 0)),
      .groups = "drop"
    ) %>%
    mutate(
      ppp = pts / poss
    )
  
  # Add defensive rebounds (opponent's missed shots - opponent's ORB)
  quarter_stats <- quarter_stats %>%
    left_join(
      quarter_stats %>%
        mutate(opp_team = ifelse(team == "home", "away", "home")) %>%
        select(opp_team, period, opp_poss = poss, opp_orb = orb),
      by = c("team" = "opp_team", "period")
    ) %>%
    mutate(drb = opp_poss - opp_orb)
  
  quarter_stats
}

# =============================================================================
# PLAYER SHOOTING STATS
# =============================================================================

calc_player_shooting <- function(advanced_box, game_info) {
  #' Calculate individual player shooting statistics
  
  shots <- advanced_box$shots_agg %>%
    mutate(
      team = ifelse(team_id == game_info$home_team_id, "home", "away")
    )
  
  # Create player lookup
  all_players <- bind_rows(game_info$home_players, game_info$away_players)
  
  player_stats <- shots %>%
    left_join(all_players, by = c("player_id" = "id")) %>%
    mutate(
      # Shot locations
      rim_fga = dist_0_5_fga,
      rim_fgm = dist_0_5_fgm,
      paint_fga = dist_5_10_fga,
      paint_fgm = dist_5_10_fgm,
      mid_fga = dist_10_15_fga + dist_15p_fga,
      mid_fgm = dist_10_15_fgm + dist_15p_fgm,
      
      # Totals
      total_fga = rim_fga + paint_fga + mid_fga + tpa,
      total_fgm = rim_fgm + paint_fgm + mid_fgm + tpm,
      
      # Percentages
      fg_pct = ifelse(total_fga > 0, total_fgm / total_fga, NA),
      tp_pct = ifelse(tpa > 0, tpm / tpa, NA),
      
      # Points and PPS
      pts = (total_fgm - tpm) * 2 + tpm * 3,
      pps = ifelse(total_fga > 0, pts / total_fga, NA),
      
      # Shot clock intervals
      early_fga = shot_clock_18_24_att,
      early_fgm = shot_clock_18_24_made,
      mid_clock_fga = shot_clock_12_18_att,
      mid_clock_fgm = shot_clock_12_18_made,
      late_fga = shot_clock_6_12_att,
      late_fgm = shot_clock_6_12_made,
      butter_fga = shot_clock_0_6_att,
      butter_fgm = shot_clock_0_6_made
    ) %>%
    select(
      player_id, name, number, team = team.x,
      total_fga, total_fgm, fg_pct, pts, pps,
      tpa, tpm, tp_pct,
      rim_fga, rim_fgm, paint_fga, paint_fgm, mid_fga, mid_fgm,
      early_fga, early_fgm, mid_clock_fga, mid_clock_fgm,
      late_fga, late_fgm, butter_fga, butter_fgm
    ) %>%
    filter(total_fga > 0)
  
  # Add assist data
  if (!is.null(advanced_box$passes_agg)) {
    passes <- advanced_box$passes_agg %>%
      select(player_id, ast = num_assists, pot_ast = num_pot_assists, 
             pts_created = num_pts_created)
    
    player_stats <- player_stats %>%
      left_join(passes, by = "player_id") %>%
      mutate(across(c(ast, pot_ast, pts_created), ~replace_na(., 0)))
  }
  
  player_stats
}

# =============================================================================
# TEAM SHOOTING SUMMARY
# =============================================================================

calc_team_shooting <- function(player_stats) {
  #' Aggregate player stats to team level by location
  
  player_stats %>%
    group_by(team) %>%
    summarise(
      # Location totals
      rim_fga = sum(rim_fga),
      rim_fgm = sum(rim_fgm),
      paint_fga = sum(paint_fga),
      paint_fgm = sum(paint_fgm),
      mid_fga = sum(mid_fga),
      mid_fgm = sum(mid_fgm),
      tpa = sum(tpa),
      tpm = sum(tpm),
      total_fga = sum(total_fga),
      total_fgm = sum(total_fgm),
      
      # Shot clock totals
      early_fga = sum(early_fga),
      early_fgm = sum(early_fgm),
      mid_clock_fga = sum(mid_clock_fga),
      mid_clock_fgm = sum(mid_clock_fgm),
      late_fga = sum(late_fga),
      late_fgm = sum(late_fgm),
      butter_fga = sum(butter_fga),
      butter_fgm = sum(butter_fgm),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Percentages
      rim_pct = rim_fgm / rim_fga,
      paint_pct = paint_fgm / paint_fga,
      mid_pct = mid_fgm / mid_fga,
      tp_pct = tpm / tpa,
      total_fg_pct = total_fgm / total_fga,
      
      # Shot distribution
      rim_share = rim_fga / total_fga,
      paint_share = paint_fga / total_fga,
      mid_share = mid_fga / total_fga,
      three_share = tpa / total_fga,
      
      # PPS by location
      rim_pps = (rim_fgm * 2) / rim_fga,
      paint_pps = (paint_fgm * 2) / paint_fga,
      mid_pps = (mid_fgm * 2) / mid_fga,
      three_pps = (tpm * 3) / tpa
    )
}

# =============================================================================
# TRANSITION VS HALFCOURT
# =============================================================================

calc_pace_splits <- function(possessions, game_info) {
  #' Calculate transition vs halfcourt efficiency
  
  poss_df <- possessions$possessions %>%
    mutate(
      team = ifelse(teamId == game_info$home_team_id, "home", "away"),
      pace_type = ifelse(possessionLength < 7, "transition", "halfcourt")
    )
  
  poss_df %>%
    group_by(team, pace_type) %>%
    summarise(
      poss = n(),
      pts = sum(ptsScored),
      ppp = pts / poss,
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = pace_type,
      values_from = c(poss, pts, ppp),
      names_glue = "{pace_type}_{.value}"
    )
}

# =============================================================================
# FOUR FACTORS
# =============================================================================

calc_four_factors <- function(quarter_stats, team_shooting) {
  #' Calculate Dean Oliver's Four Factors
  
  # Aggregate quarter stats to game totals
  game_totals <- quarter_stats %>%
    group_by(team) %>%
    summarise(
      pts = sum(pts),
      poss = sum(poss),
      tov = sum(tov),
      orb = sum(orb),
      drb = sum(drb),
      .groups = "drop"
    )
  
  # Join with shooting stats
  four_factors <- game_totals %>%
    left_join(team_shooting %>% select(team, total_fga, total_fgm, tpa, tpm), by = "team") %>%
    mutate(
      # eFG% = (FGM + 0.5 * 3PM) / FGA
      efg_pct = (total_fgm + 0.5 * tpm) / total_fga,
      
      # TOV% = TOV / POSS
      tov_pct = tov / poss,
      
      # OREB% = ORB / (ORB + OPP_DRB)
      # Need opponent DRB
      opp_drb = lead(drb, default = first(drb)),
      orb_pct = orb / (orb + opp_drb),
      
      # PPP
      ppp = pts / poss
    )
  
  four_factors
}

# =============================================================================
# MAIN PROCESSING FUNCTION
# =============================================================================

process_game <- function(data_dir) {
  #' Main function to process all game data
  #' @param data_dir Directory containing Second Spectrum JSON files
  #' @return List containing all processed statistics
  
  cat("Loading data from:", data_dir, "\n")
  raw_data <- load_game_data(data_dir)
  
  cat("Extracting game info...\n")
  game_info <- extract_game_info(raw_data$metadata)
  
  cat("Calculating quarter stats...\n
")
  quarter_stats <- calc_quarter_stats(raw_data$possessions, game_info)
  
  cat("Calculating player shooting...\n")
  player_stats <- calc_player_shooting(raw_data$advanced_box, game_info)
  
  cat("Calculating team shooting...\n")
  team_shooting <- calc_team_shooting(player_stats)
  
  cat("Calculating pace splits...\n")
  pace_splits <- calc_pace_splits(raw_data$possessions, game_info)
  
  cat("Calculating four factors...\n")
  four_factors <- calc_four_factors(quarter_stats, team_shooting)
  
  cat("Done!\n")
  
  list(
    game_info = game_info,
    quarter_stats = quarter_stats,
    player_stats = player_stats,
    team_shooting = team_shooting,
    pace_splits = pace_splits,
    four_factors = four_factors
  )
}

# Helper for null coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
