# =============================================================================
# Analysis 2: Leaderboard Position Error
# =============================================================================
# QUESTION: How often does the live BassTrakk leaderboard show the wrong
# leader, and by how many positions? How much does correction fix this?
#
# This is the broadcast product implication — translates weight errors into
# the rank errors a viewer/analyst would actually observe.
#
# REQUIRES: master_summary_all, reconciled_ids
# ALSO NEEDS: analysis_01_results (for correction factors)
# =============================================================================

# Load correction factors from Analysis 1
a01 <- readRDS("data/analysis_01_predictive_validity.rds")

corrections     <- a01$corrections
field_mean_bias <- a01$field_mean_bias

# --- 1. Build per-day standings for all non-reconciled tournaments -----------
# rank anglers within each tournament-day by three weight estimates:
#   (a) raw BassTrakk cumulative total
#   (b) corrected BassTrakk cumulative total
#   (c) official certified cumulative weight (ground truth)

standings <- master_summary_all |>
  filter(
    full_coverage,
    !tournament_id %in% reconciled_ids,
    official_oz > 0,
    !is.na(bias_ratio),
    is.finite(bias_ratio)) |>
  left_join(corrections |> select(AnglerId, prior_mean_bias, cluster),
            by = "AnglerId") |>
  mutate(
    correction_factor = coalesce(prior_mean_bias, field_mean_bias),
    bt_corrected_oz   = bt_total / correction_factor) |>
  group_by(tournament_id, event_name, year) |>
  mutate(
    rank_official  = rank(-official_oz,  ties.method = "min"),
    rank_bt_raw    = rank(-bt_total,     ties.method = "min"),
    rank_bt_corr   = rank(-bt_corrected_oz, ties.method = "min"),
    # Rank errors vs. official ground truth
    rank_error_raw  = abs(rank_bt_raw  - rank_official),
    rank_error_corr = abs(rank_bt_corr - rank_official),
    # Was the overall leader (rank 1) correctly identified?
    true_leader      = rank_official == 1,
    bt_raw_says_lead = rank_bt_raw   == 1,
    bt_corr_says_lead= rank_bt_corr  == 1) |>
  ungroup()


# --- 2. Overall rank error summary -------------------------------------------

rank_overall <- standings |>
  summarise(
    n                       = n(),
    mean_rank_error_raw     = mean(rank_error_raw),
    mean_rank_error_corr    = mean(rank_error_corr),
    median_rank_error_raw   = median(rank_error_raw),
    median_rank_error_corr  = median(rank_error_corr),
    pct_exact_rank_raw      = mean(rank_error_raw  == 0) * 100,
    pct_exact_rank_corr     = mean(rank_error_corr == 0) * 100,
    pct_off_by_gt1_raw      = mean(rank_error_raw  >  1) * 100,
    pct_off_by_gt1_corr     = mean(rank_error_corr >  1) * 100,
    pct_off_by_gt3_raw      = mean(rank_error_raw  >  3) * 100,
    pct_off_by_gt3_corr     = mean(rank_error_corr >  3) * 100)

cat("=== Overall Leaderboard Position Error ===\n")
cat(sprintf("%-40s %10s %10s\n", "Metric", "Raw BT", "Corrected"))
cat(strrep("-", 62), "\n")
cat(sprintf("%-40s %10.2f %10.2f\n", "Mean rank error",       rank_overall$mean_rank_error_raw,   rank_overall$mean_rank_error_corr))
cat(sprintf("%-40s %10.2f %10.2f\n", "Median rank error",     rank_overall$median_rank_error_raw, rank_overall$median_rank_error_corr))
cat(sprintf("%-40s %10.1f %10.1f\n", "% in correct position", rank_overall$pct_exact_rank_raw,    rank_overall$pct_exact_rank_corr))
cat(sprintf("%-40s %10.1f %10.1f\n", "% off by >1 position",  rank_overall$pct_off_by_gt1_raw,    rank_overall$pct_off_by_gt1_corr))
cat(sprintf("%-40s %10.1f %10.1f\n", "% off by >3 positions", rank_overall$pct_off_by_gt3_raw,    rank_overall$pct_off_by_gt3_corr))


# --- 3. Leader identification: how often is the wrong angler shown in first? -

leader_analysis <- standings |>
  group_by(tournament_id, event_name, year) |>
  summarise(
    true_leader_id        = AnglerId[rank_official == 1][1],
    bt_raw_leader_id      = AnglerId[rank_bt_raw   == 1][1],
    bt_corr_leader_id     = AnglerId[rank_bt_corr  == 1][1],
    raw_leader_correct    = true_leader_id == bt_raw_leader_id,
    corr_leader_correct   = true_leader_id == bt_corr_leader_id,
    true_leader_bias      = bias_ratio[rank_official == 1][1],
    .groups = "drop")

cat("\n=== Live Leader Identification Accuracy ===\n")
cat(sprintf("Tournaments where raw BT correctly identified leader:       %d / %d (%.1f%%)\n",
            sum(leader_analysis$raw_leader_correct),
            nrow(leader_analysis),
            mean(leader_analysis$raw_leader_correct) * 100))
cat(sprintf("Tournaments where corrected BT correctly identified leader: %d / %d (%.1f%%)\n",
            sum(leader_analysis$corr_leader_correct),
            nrow(leader_analysis),
            mean(leader_analysis$corr_leader_correct) * 100))


# --- 4. By cluster: whose rank errors are largest? ---------------------------

cluster_rank <- standings |>
  filter(!is.na(cluster)) |>
  group_by(cluster) |>
  summarise(
    n                    = n(),
    mean_rank_error_raw  = mean(rank_error_raw),
    mean_rank_error_corr = mean(rank_error_corr),
    pct_off_by_gt1_raw   = mean(rank_error_raw  > 1) * 100,
    pct_off_by_gt1_corr  = mean(rank_error_corr > 1) * 100,
    .groups = "drop")

cat("\n=== Rank Error by Cluster ===\n")
print(cluster_rank |> mutate(across(where(is.double), \(x) round(x, 3))))



# --- 5. Distribution of raw rank errors (for reporting in paper) -------------

rank_dist <- standings |>
  count(rank_error_raw) |>
  mutate(pct = n / sum(n) * 100) |>
  filter(rank_error_raw <= 10)

cat("\n=== Distribution of Raw Rank Errors (top 10) ===\n")
print(rank_dist)


# --- 6. Save -----------------------------------------------------------------

analysis_02_results <- list(
  standings       = standings,
  rank_overall    = rank_overall,
  leader_analysis = leader_analysis,
  cluster_rank    = cluster_rank,
  rank_dist       = rank_dist)
saveRDS(analysis_02_results, "data/analysis_02_leaderboard_error.rds")
