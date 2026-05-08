# =============================================================================
# Analysis 3: Broadcast Error Rate by Cluster and Incentive Design (FINAL)
# =============================================================================
# REQUIRES: master_summary_all, reconciled_ids, per_angler_all,
#           rolling_profiles_fixed (for switcher robustness check only)
#
# PRIMARY cluster: four-season GMM (stable, matches Table 5 in paper)
# SECONDARY: rolling cluster used only for switcher robustness check
# =============================================================================

# =============================================================================
# Fix cluster label switching in rolling_profiles
# =============================================================================
# Rule: Cluster 1 = the group with LOWER within-angler sd_bias
#       (consistent reporters). If the GMM assigned it label 2, swap.
#
# This is applied as a post-hoc correction to the existing rolling_profiles
# object. Also add to compute_rolling_profile() going forward (see below).
# =============================================================================

# --- One-time fix for existing rolling_profiles object ----------------------

rolling_profiles_fixed <- rolling_profiles |>
  group_by(pred_season) |>
  mutate(
    # For each pred_season, find which cluster label has lower mean sd_bias
    cluster_1_sd = mean(sd_bias[cluster == 1], na.rm = TRUE),
    cluster_2_sd = mean(sd_bias[cluster == 2], na.rm = TRUE),
    # If cluster 1 has HIGHER sd than cluster 2, labels are flipped -- swap them
    labels_flipped = cluster_1_sd > cluster_2_sd,
    cluster_fixed = case_when(
      is.na(cluster)      ~ NA_real_,
      !labels_flipped     ~ cluster,           # already correct
      cluster == 1        ~ 2,                 # swap 1 -> 2
      cluster == 2        ~ 1                  # swap 2 -> 1
    )
  ) |>
  ungroup() |>
  select(-cluster_1_sd, -cluster_2_sd, -labels_flipped) |>
  rename(cluster_raw = cluster, cluster = cluster_fixed)

# --- Verify the fix ---------------------------------------------------------

rolling_profiles_fixed |>
  group_by(pred_season, cluster) |>
  summarise(
    n         = n(),
    mean_bias = mean(mean_bias),
    sd_bias   = mean(sd_bias),
    .groups   = "drop"
  ) |>
  arrange(pred_season, cluster)


# =============================================================================
# Revised cluster lookup for Analysis 3
# Uses four-season GMM cluster as primary grouping (stable, already reported
# in paper). Rolling cluster retained as robustness check only.
# =============================================================================

library(tidyverse)
library(mclust)

# --- 1. Re-derive four-season cluster from per_angler_all -------------------
# per_angler_all has mean_bias and sd_bias but no cluster column.
# Re-run the same GMM the paper used so we get consistent assignments.

clust_input <- per_angler_all |>
  filter(!is.na(sd_bias), is.finite(sd_bias)) |>
  select(AnglerId, angler_name, mean_bias, sd_bias)

set.seed(123)
mc_4season <- Mclust(
  clust_input |> select(mean_bias, sd_bias) |> as.matrix(),
  G = 2)

clust_input$cluster_raw <- mc_4season$classification

# Standardize: Cluster 1 = lower sd_bias group (consistent reporters)
sd_by_cluster <- tapply(clust_input$sd_bias,
                        clust_input$cluster_raw, mean, na.rm = TRUE)
if (sd_by_cluster["1"] > sd_by_cluster["2"]) {
  clust_input$cluster_4season <- ifelse(clust_input$cluster_raw == 1, 2, 1)
} else {
  clust_input$cluster_4season <- clust_input$cluster_raw
}


# Four-season cluster lookup: one row per AnglerId, no year dimension
cluster_4season_lookup <- clust_input |>
  select(AnglerId, cluster_4season)


# --- 2. Attach cluster to all qualifying observations -----------------------

obs_with_cluster <- master_summary_all |>
  filter(
    full_coverage,
    !tournament_id %in% reconciled_ids,
    official_oz > 0,
    !is.na(bias_ratio),
    is.finite(bias_ratio)
  ) |>
  left_join(cluster_4season_lookup |> select(AnglerId, cluster_4season),
            by = "AnglerId")

cat("=== Analysis 3: Broadcast Error Rate ===\n\n")
cat("Observations by cluster:\n")
print(obs_with_cluster |> count(cluster_4season))
cat("NA cluster (< 6 career tournaments, excluded from cluster summaries):",
    sum(is.na(obs_with_cluster$cluster_4season)), "\n\n")


# --- 3. Compute broadcast errors: BT rank vs official rank ------------------

broadcast_errors <- obs_with_cluster |>
  group_by(tournament_id, year) |>
  mutate(
    rank_official     = rank(-official_oz, ties.method = "min"),
    rank_bt           = rank(-bt_total,    ties.method = "min"),
    rank_error        = abs(rank_bt - rank_official),
    broadcast_error_1 = rank_error > 1,
    broadcast_error_3 = rank_error > 3,
    broadcast_error_5 = rank_error > 5,
    top10_official    = rank_official <= 10,
    top10_bt          = rank_bt       <= 10
  ) |>
  ungroup()


# --- 4. Overall broadcast error summary -------------------------------------

overall_broadcast <- broadcast_errors |>
  summarise(
    n                  = n(),
    mean_rank_error    = mean(rank_error),
    median_rank_error  = median(rank_error),
    pct_exact          = mean(rank_error == 0) * 100,
    pct_off_by_gt1     = mean(broadcast_error_1) * 100,
    pct_off_by_gt3     = mean(broadcast_error_3) * 100,
    pct_off_by_gt5     = mean(broadcast_error_5) * 100
  )

cat("=== Overall Broadcast Error (all anglers, all tournaments) ===\n")
print(overall_broadcast |> mutate(across(where(is.double), \(x) round(x, 3))))


# --- 5. Error rate by four-season cluster ------------------------------------

cluster_broadcast <- broadcast_errors |>
  filter(!is.na(cluster_4season)) |>
  group_by(cluster_4season) |>
  summarise(
    n                  = n(),
    mean_bias          = mean(bias_ratio),
    mean_rank_error    = mean(rank_error),
    median_rank_error  = median(rank_error),
    pct_exact          = mean(rank_error == 0) * 100,
    pct_off_by_gt1     = mean(broadcast_error_1) * 100,
    pct_off_by_gt3     = mean(broadcast_error_3) * 100,
    pct_off_by_gt5     = mean(broadcast_error_5) * 100,
    top10_capture_rate = mean(top10_official == top10_bt) * 100,
    .groups = "drop"
  )

cat("\n=== Broadcast Error by Four-Season Cluster ===\n")
print(cluster_broadcast |> mutate(across(where(is.double), \(x) round(x, 3))))

# Chi-square test: are rank errors distributed differently across clusters?
# Collapse to binary (off by > 1 or not) for the test
cluster_chisq <- broadcast_errors |>
  filter(!is.na(cluster_4season)) |>
  mutate(error_binary = if_else(broadcast_error_1, "error", "correct")) |>
  count(cluster_4season, error_binary) |>
  pivot_wider(names_from = error_binary, values_from = n, values_fill = 0) |>
  column_to_rownames("cluster_4season") |>
  chisq.test()

cat(sprintf("\nChi-square test (error >1 place by cluster):\n"))
cat(sprintf("X²(%d) = %.3f, p = %.4f\n",
            cluster_chisq$parameter,
            cluster_chisq$statistic,
            cluster_chisq$p.value))

# --- 6. Top-10 leaderboard integrity ----------------------------------------

top10_integrity <- broadcast_errors |>
  group_by(tournament_id, event_name, year) |>
  summarise(
    n_overlap   = length(intersect(
      AnglerId[rank_bt       <= 10],
      AnglerId[rank_official <= 10])),
    pct_overlap = n_overlap / 10 * 100,
    .groups = "drop"
  )

cat("\n=== Top-10 Leaderboard Integrity ===\n")
cat(sprintf("Mean %% of true top-10 correctly shown in BT top-10: %.1f%%\n",
            mean(top10_integrity$pct_overlap)))
cat(sprintf("Tournaments with perfect top-10 match: %d / %d (%.1f%%)\n",
            sum(top10_integrity$pct_overlap == 100),
            nrow(top10_integrity),
            mean(top10_integrity$pct_overlap == 100) * 100))
cat(sprintf("Worst tournament top-10 overlap: %.0f%%\n",
            min(top10_integrity$pct_overlap)))

# Top-10 integrity by year
top10_by_year <- top10_integrity |>
  group_by(year) |>
  summarise(
    mean_pct_overlap    = mean(pct_overlap),
    pct_perfect_match   = mean(pct_overlap == 100) * 100,
    .groups = "drop"
  )

cat("\nTop-10 integrity by year:\n")
print(top10_by_year |> mutate(across(where(is.double), \(x) round(x, 1))))


# --- 7. Contingency prize ROI -----------------------------------------------

prize_contenders <- master_summary_all |>
  filter(full_coverage, !tournament_id %in% reconciled_ids,
         official_oz > 0, !is.na(bias_ratio)) |>
  group_by(tournament_id) |>
  mutate(
    abs_dev     = abs(bias_ratio - 1),
    prize_top10 = rank(abs_dev, ties.method = "min") <= 10
  ) |>
  ungroup() |>
  group_by(AnglerId) |>
  summarise(
    prize_appearances = sum(prize_top10, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(prize_motivated = prize_appearances >= 3)

prize_vs_not <- broadcast_errors |>
  left_join(prize_contenders, by = "AnglerId") |>
  filter(!is.na(prize_motivated)) |>
  group_by(prize_motivated) |>
  summarise(
    n               = n(),
    mean_bias       = mean(bias_ratio),
    mean_rank_error = mean(rank_error),
    pct_off_by_gt1  = mean(broadcast_error_1) * 100,
    pct_off_by_gt3  = mean(broadcast_error_3) * 100,
    .groups = "drop"
  )

cat("\n=== Broadcast Error: Prize-Motivated vs. Others ===\n")
cat("(Prize-motivated = reconstructed prize top-10 >= 3 tournaments)\n")
print(prize_vs_not |> mutate(across(where(is.double), \(x) round(x, 3))))

# Prize motivation x cluster interaction
prize_cluster <- broadcast_errors |>
  left_join(prize_contenders, by = "AnglerId") |>
  filter(!is.na(prize_motivated), !is.na(cluster_4season)) |>
  group_by(cluster_4season, prize_motivated) |>
  summarise(
    n               = n(),
    mean_bias       = mean(bias_ratio),
    mean_rank_error = mean(rank_error),
    pct_off_by_gt1  = mean(broadcast_error_1) * 100,
    .groups = "drop"
  )

cat("\n=== Prize Motivation x Cluster Interaction ===\n")
print(prize_cluster |> mutate(across(where(is.double), \(x) round(x, 3))))


# --- 8. Year-over-year broadcast error trend --------------------------------

yearly_broadcast <- broadcast_errors |>
  group_by(year) |>
  summarise(
    n               = n(),
    mean_bias       = mean(bias_ratio),
    mean_rank_error = mean(rank_error),
    pct_off_by_gt1  = mean(broadcast_error_1) * 100,
    pct_off_by_gt3  = mean(broadcast_error_3) * 100,
    .groups = "drop"
  )

cat("\n=== Broadcast Error Rate by Year ===\n")
print(yearly_broadcast |> mutate(across(where(is.double), \(x) round(x, 3))))

# Spearman correlation: does broadcast error track the bias trend?
year_cor <- cor.test(yearly_broadcast$year,
                     yearly_broadcast$pct_off_by_gt1,
                     method = "spearman")
cat(sprintf("\nSpearman rho (year vs pct_off_by_gt1): %.3f, p = %.4f\n",
            year_cor$estimate, year_cor$p.value))


# --- 9. Robustness: cluster-switcher sensitivity check ----------------------
# Anglers whose rolling cluster assignment changed across pred_seasons.
# If switchers drive the cluster_broadcast result, the finding is fragile.

switchers <- rolling_profiles_fixed |>
  group_by(AnglerId) |>
  filter(!is.na(cluster)) |>
  summarise(
    ever_switched = n_distinct(cluster) > 1,
    .groups = "drop"
  )

cat("\n=== Robustness: Excluding Cluster-Switchers ===\n")
cat("Anglers who switched rolling clusters:",
    sum(switchers$ever_switched), "\n")

cluster_broadcast_stable <- broadcast_errors |>
  left_join(switchers, by = "AnglerId") |>
  filter(!is.na(cluster_4season),
         !is.na(ever_switched),
         !ever_switched) |>         # stable reporters only
  group_by(cluster_4season) |>
  summarise(
    n              = n(),
    mean_bias      = mean(bias_ratio),
    mean_rank_error= mean(rank_error),
    pct_off_by_gt1 = mean(broadcast_error_1) * 100,
    .groups = "drop"
  )

cat("Broadcast error by cluster (stable reporters only):\n")
print(cluster_broadcast_stable |>
        mutate(across(where(is.double), \(x) round(x, 3))))
cat("(If similar to full-sample result, cluster finding is robust)\n")


# --- 10. Save ----------------------------------------------------------------

analysis_03_results <- list(
  broadcast_errors         = broadcast_errors,
  overall_broadcast        = overall_broadcast,
  cluster_broadcast        = cluster_broadcast,
  cluster_chisq            = cluster_chisq,
  top10_integrity          = top10_integrity,
  top10_by_year            = top10_by_year,
  prize_vs_not             = prize_vs_not,
  prize_cluster            = prize_cluster,
  yearly_broadcast         = yearly_broadcast,
  year_cor                 = year_cor,
  cluster_broadcast_stable = cluster_broadcast_stable,
  cluster_4season_lookup   = cluster_4season_lookup
)

saveRDS(analysis_03_results, "data/analysis_03_broadcast_error.rds")
