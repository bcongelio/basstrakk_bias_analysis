# =============================================================================
# Analysis 4: Venue-Level Mixed-Effects Model
# =============================================================================
# QUESTION: Which specific venues produce the worst live accuracy, controlling
# for the fact that different anglers fish different events?
#
# MODEL: bias_ratio ~ venue_type + (1 | AnglerId)
# Random intercept for angler absorbs stable individual reporting style,
# isolating the venue-level contribution to bias.
#
# REQUIRES: master_summary_all, reconciled_ids
# INSTALLS: lme4 (if not already present)
# =============================================================================
install.packages("tidyverse")

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

# --- 1. Prepare modeling data ------------------------------------------------

model_data <- master_summary_all |>
  filter(
    full_coverage,
    !tournament_id %in% reconciled_ids,
    official_oz > 0,
    !is.na(bias_ratio),
    is.finite(bias_ratio)
  ) |>
  # Need at least a venue identifier. Use event_name if you have a
  # venue_type column from your existing pipeline, use that instead.
  # The script below builds venue_type from event_name patterns --
  # replace with your actual venue_type column if it already exists.
  mutate(
    venue_type = case_when(
      str_detect(event_name, regex("fork|sam rayburn|texoma|toledo|bull shoals|table rock|beaver|ouachita|eufaula|jordan|hartwell|lanier|allatoona|clarks hill|strom|guntersville|pickwick|wheeler|wilson|weiss|cherokee|oconee|murray|nottely",  ignore_case = TRUE)) ~ "Southern Reservoir",
      str_detect(event_name, regex("okeechobee|kissimmee|toho|harris|istokpoga|seminole|orange|griffin|dora|weir|panasoffkee|mayo|orange",  ignore_case = TRUE)) ~ "Florida Grass",
      str_detect(event_name, regex("potomac|james|st\\. johns|suwannee|winyah|sabine|atchafalaya|red river|tennessee river|cumberland|ohio|columbia|snake|columbia|willamette",  ignore_case = TRUE)) ~ "Tidal/River",
      str_detect(event_name, regex("mille lacs|st\\. clair|erie|champlain|ontario|superior|huron|michigan|oneida|cayuga|finger|saginaw|green bay",  ignore_case = TRUE)) ~ "Northern/Clear Water",
      TRUE ~ "Other Reservoir"
    ),
    # Center bias ratio for interpretability (deviation from 1.0)
    bias_deviation = bias_ratio - 1
  )

cat("Model data rows:", nrow(model_data), "\n")
cat("Venue type distribution:\n")
print(table(model_data$venue_type))


# --- 2. Fit mixed-effects model ----------------------------------------------
# Random intercept for AnglerId absorbs individual reporting style.
# venue_type fixed effect captures venue-level bias independent of who fishes there.

mod_venue <- lmer(
  bias_ratio ~ venue_type + (1 | AnglerId),
  data    = model_data,
  REML    = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)

cat("\n=== Mixed-Effects Model: bias_ratio ~ venue_type + (1|AnglerId) ===\n")
print(summary(mod_venue))


# --- 3. Extract venue fixed effects with confidence intervals ----------------

venue_effects <- broom.mixed::tidy(mod_venue, effects = "fixed", conf.int = TRUE) |>
  filter(str_detect(term, "venue_type")) |>
  mutate(
    venue = str_remove(term, "venue_type"),
    # Reference level is absorbed into intercept; add it back for display
    estimate_from_ref = estimate
  ) |>
  select(venue, estimate, std.error, conf.low, conf.high, p.value)

# Add the reference category (intercept represents it)
intercept_val <- fixef(mod_venue)["(Intercept)"]
ref_level     <- levels(factor(model_data$venue_type))[1]   # alphabetical default

venue_effects_full <- bind_rows(
  tibble(venue = paste0(ref_level, " (reference)"),
         estimate = intercept_val, std.error = NA,
         conf.low = NA, conf.high = NA, p.value = NA),
  venue_effects |>
    mutate(estimate = estimate + intercept_val)
) |>
  arrange(estimate)

cat("\n=== Venue Fixed Effects (absolute bias ratio scale) ===\n")
cat("Higher = less underreporting; 1.0 = perfect agreement with scale\n\n")
print(venue_effects_full |> mutate(across(where(is.double), \(x) round(x, 4))))


# --- 4. Random effects: ICC and individual angler variance -------------------
icc_val <- performance::icc(mod_venue)
var_components <- as.data.frame(VarCorr(mod_venue))

cat("\n=== Variance Components ===\n")
print(var_components)
cat(sprintf("\nIntraclass correlation (angler-level): %.4f\n", icc_val$ICC_adjusted))
cat("Interpretation: %.1f%% of bias variance is attributable to stable\n",
    icc_val$ICC_adjusted * 100)
cat("individual differences (vs. tournament/venue-level variation).\n")


# --- 5. Per-venue summary with model-adjusted means -------------------------
# Marginal means from the model, adjusted for angler composition
venue_marginal <- emmeans::emmeans(mod_venue, ~ venue_type) |>
  as.data.frame() |>
  arrange(emmean)

cat("\n=== Marginal Mean Bias Ratio by Venue Type (model-adjusted) ===\n")
print(venue_marginal |> mutate(across(where(is.double), \(x) round(x, 4))))


# --- 6. Per-tournament raw summary (for identifying specific problem events) -

tournament_summary <- model_data |>
  group_by(tournament_id, event_name, year, venue_type) |>
  summarise(
    n          = n(),
    mean_bias  = mean(bias_ratio),
    sd_bias    = sd(bias_ratio),
    pct_gt20pct_under = mean(bias_ratio < 0.80) * 100,
    .groups = "drop"
  ) |>
  arrange(mean_bias)

cat("\n=== 10 Worst Tournaments by Mean Bias Ratio ===\n")
print(head(tournament_summary, 10) |>
        mutate(across(where(is.double), \(x) round(x, 3))))

cat("\n=== 10 Best Tournaments by Mean Bias Ratio ===\n")
print(tail(tournament_summary, 10) |>
        mutate(across(where(is.double), \(x) round(x, 3))))

# --- 7. Save -----------------------------------------------------------------

analysis_04_results <- list(
  model_data          = model_data,
  model               = mod_venue,
  venue_effects_full  = venue_effects_full,
  venue_marginal      = venue_marginal,
  var_components      = var_components,
  icc                 = icc_val,
  tournament_summary  = tournament_summary
)
saveRDS(analysis_04_results, "data/analysis_04_venue_model.rds")
