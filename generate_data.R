#generate_data.R
# Script to generate simulated data for the ID ROLL OUT AMS Dashboard
# Replicates statistical findings from Giesen et al., 2025
set.seed(123) # Ensure reproducibility
# Number of hospitals
n_hospitals <- 10
hospital_ids <- 1:n_hospitals
# Define Staffing Status (4 Ongoing, 6 Stopped)
# Based on HTML: "Hospitals with ongoing full-time AMS/ID staffing (4/10)"
staffing_status <- c(rep("Ongoing", 4), rep("Stopped", 6))
hospital_meta <- data.frame(
  Hospital_ID = hospital_ids,
  Staffing_Status = sample(staffing_status) # Randomly assign status
)
# Function to generate scores bounded 0-100
gen_scores <- function(n, mean_val, sd_val, min_val, max_val) {
  scores <- rnorm(n, mean = mean_val, sd = sd_val)
  scores <- pmax(min_val, pmin(max_val, scores)) # Clamp to range
  return(round(scores))
}
# --- Baseline Data (2021) ---
# Median ~37%, Range 20-60%
baseline_total <- gen_scores(n_hospitals, mean_val = 37, sd_val = 10, min_val = 20, max_val = 60)
# --- Intervention Data (2022) ---
# Median ~76%, Range 62-86%
intervention_total <- gen_scores(n_hospitals, mean_val = 76, sd_val = 8, min_val = 62, max_val = 86)
# --- Follow-up Data (2023) ---
# Logic: 
# Ongoing staffing -> ~13% drop from Intervention
# Stopped staffing -> ~32% drop from Intervention
followup_total <- numeric(n_hospitals)
for (i in 1:n_hospitals) {
  status <- hospital_meta$Staffing_Status[i]
  prev_score <- intervention_total[i]
  
  if (status == "Ongoing") {
    drop <- rnorm(1, mean = 13, sd = 3)
  } else {
    drop <- rnorm(1, mean = 32, sd = 5)
  }
  
  new_score <- prev_score - drop
  # Clamp to range 48-80% (approximate based on text, though individual drops might vary)
  # We'll just clamp 0-100 to be safe, but try to respect the observed range if possible
  followup_total[i] <- round(max(48, min(80, new_score)))
}
# Combine Total Scores
df_total <- data.frame(
  Hospital_ID = rep(hospital_ids, 3),
  Phase = factor(rep(c("Baseline", "Intervention", "FollowUp"), each = n_hospitals), 
                 levels = c("Baseline", "Intervention", "FollowUp")),
  Total_AMS_Score = c(baseline_total, intervention_total, followup_total)
)
# Merge metadata
df_total <- merge(df_total, hospital_meta, by = "Hospital_ID")
# --- Generate Sub-scores ---
# Total Score is composed of 5 categories. We need to distribute the Total Score into these.
# Weights (approx max points): Framework(16), Resources(38), Prevention(18), Surveillance(10), Evaluation(18) -> Sum=100
# We will generate sub-scores that roughly sum up to the Total_AMS_Score.
generate_subscores <- function(total_score, phase) {
  # Base proportions for each category based on the phase trends described in HTML
  # Baseline: Low everywhere
  # Intervention: High everywhere
  # Follow-up: Evaluation drops the most
  
  if (phase == "Baseline") {
    weights <- c(Framework=0.4, Resources=0.3, Prevention=0.45, Surveillance=0.2, Evaluation=0.1)
  } else if (phase == "Intervention") {
    weights <- c(Framework=0.95, Resources=0.7, Prevention=0.75, Surveillance=0.7, Evaluation=0.8)
  } else { # FollowUp
    weights <- c(Framework=0.8, Resources=0.5, Prevention=0.75, Surveillance=0.5, Evaluation=0.3)
  }
  
  # Max points per category
  max_pts <- c(Framework=16, Resources=38, Prevention=18, Surveillance=10, Evaluation=18)
  
  # Calculate raw points based on weights * max_pts
  raw_pts <- weights * max_pts
  
  # Scale raw_pts so their sum equals the total_score (which is out of 100)
  # This is a bit of an approximation since Total is %, and Sum of Max Pts is 100.
  # So Total Score % is roughly equal to Total Points.
  
  current_sum <- sum(raw_pts)
  scale_factor <- total_score / current_sum
  
  final_pts <- raw_pts * scale_factor
  
  # Ensure no category exceeds its max points
  final_pts <- pmin(final_pts, max_pts)
  
  # If we capped some, we might be under the total. Distribute remainder?
  # For simplicity in this simulation, we'll just return the % of max for each category
  # to be consistent with the "Score" concept (0-100% per category).
  
  # Return as percentage 0-100 for each category
  pct_scores <- (final_pts / max_pts) * 100
  return(round(pct_scores))
}
# Apply subscore generation
subscores_list <- list()
for (i in 1:nrow(df_total)) {
  res <- generate_subscores(df_total$Total_AMS_Score[i], df_total$Phase[i])
  subscores_list[[i]] <- res
}
subscores_df <- do.call(rbind, subscores_list)
colnames(subscores_df) <- c("Framework_Score", "Resources_Score", "Prevention_Score", "Surveillance_Score", "Evaluation_Score")
final_df <- cbind(df_total, subscores_df)
# Reorder columns
final_df <- final_df[, c("Hospital_ID", "Phase", "Staffing_Status", 
                         "Framework_Score", "Resources_Score", "Prevention_Score", 
                         "Surveillance_Score", "Evaluation_Score", "Total_AMS_Score")]
# Save to CSV
write.csv(final_df, "simulated_ams_data.csv", row.names = FALSE)
cat("Data generation complete. 'simulated_ams_data.csv' created.\n")
print(head(final_df))