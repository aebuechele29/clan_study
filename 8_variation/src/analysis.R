
model_null <- lmer(inc_all ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = r_hh)

# Extract variance components
summary(model_null)

# Compute ICCs
icc_results <- performance::icc(model_null, by_group = TRUE)
icc_results