## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(seminr)
library(seminrExtras)

# set the theme for plotting output
seminr_theme_set(seminr_theme_academic())

# Create measurement model ----
corp_rep_mm_ext <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

alt_mm <- constructs(
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model ----
corp_rep_sm_ext <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)
alt_sm <- relationships(
  paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA")),
  paths(from = c("CUSA"),         to = c("CUSL"))
)


# Estimate the models ----
established_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm_ext,
  structural_model  = corp_rep_sm_ext,
  missing = mean_replacement,
  missing_value = "-99")

competing_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = alt_mm,
  structural_model  = alt_sm,
  missing = mean_replacement,
  missing_value = "-99")



## ----fig.show='hold'----------------------------------------------------------
plot(established_model)
plot(competing_model)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# 
# # Create measurement model ----
# corp_rep_mm_ext <- constructs(
#   composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
#   composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
#   composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
#   composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
#   composite("COMP", multi_items("comp_", 1:3)),
#   composite("LIKE", multi_items("like_", 1:3)),
#   composite("CUSA", single_item("cusa")),
#   composite("CUSL", multi_items("cusl_", 1:3))
# )
# 
# alt_mm <- constructs(
#   composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
#   composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
#   composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
#   composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
#   composite("COMP", multi_items("comp_", 1:3)),
#   composite("LIKE", multi_items("like_", 1:3)),
#   composite("CUSA", single_item("cusa")),
#   composite("CUSL", multi_items("cusl_", 1:3))
# )
# 
# # Create structural model ----
# corp_rep_sm_ext <- relationships(
#   paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
#   paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
#   paths(from = c("CUSA"),         to = c("CUSL"))
# )
# alt_sm <- relationships(
#   paths(from = c("QUAL", "PERF", "CSOR", "ATTR"), to = c("COMP", "LIKE")),
#   paths(from = c("COMP", "LIKE"), to = c("CUSA")),
#   paths(from = c("CUSA"),         to = c("CUSL"))
# )
# 
# 
# # Estimate the models ----
# established_model <- estimate_pls(
#   data = corp_rep_data,
#   measurement_model = corp_rep_mm_ext,
#   structural_model  = corp_rep_sm_ext,
#   missing = mean_replacement,
#   missing_value = "-99")
# 
# competing_model <- estimate_pls(
#   data = corp_rep_data,
#   measurement_model = alt_mm,
#   structural_model  = alt_sm,
#   missing = mean_replacement,
#   missing_value = "-99")
# 
# # Function to compare the Loss of two models
# compare_results <- assess_cvpat_compare(established_model = established_model,
#                                         alternative_model = competing_model,
#                                         testtype = "two.sided",
#                                         nboot = 2000,
#                                         technique = predict_DA,
#                                         seed = 123,
#                                         noFolds = 10,
#                                         reps = 10,
#                                         cores = 1)
# 
# 
# print(compare_results,
#       digits = 3)
# 
# # Assess the base model ----
# assess_results <- assess_cvpat(established_model,
#                                seed = 123,
#                                cores = 1)
# print(assess_results$CVPAT_compare_LM,
#       digits = 3)
# print(assess_results$CVPAT_compare_IA,
#       digits = 3)
# 

## ----echo=TRUE----------------------------------------------------------------
# Assess the base model ----
assess_results <- assess_cvpat(established_model,
                               nboot = 200,
                               noFolds = 5,
                               reps = 1,
                               seed = 123,
                               cores = 1)
print(assess_results$CVPAT_compare_LM,
      digits = 3)
print(assess_results$CVPAT_compare_IA,
      digits = 3)

## ----echo=TRUE----------------------------------------------------------------
# Function to compare the Loss of two models
compare_results <- assess_cvpat_compare(established_model = established_model,
                                        alternative_model = competing_model,
                                        testtype = "two.sided",
                                        nboot = 200,
                                        technique = predict_DA,
                                        seed = 123,
                                        noFolds = 5,
                                        reps = 1,
                                        cores = 1)

print(compare_results,
      digits = 3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(seminr)
# library(seminrExtras)
# 
# # Estimate a model
# mobi_mm <- constructs(
#   composite("Image",        multi_items("IMAG", 1:5)),
#   composite("Expectation",  multi_items("CUEX", 1:3)),
#   composite("Value",        multi_items("PERV", 1:2)),
#   composite("Satisfaction", multi_items("CUSA", 1:3)),
#   composite("Loyalty",      multi_items("CUSL", 1:3))
# )
# 
# mobi_sm <- relationships(
#   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#   paths(from = "Expectation",  to = c("Value", "Satisfaction")),
#   paths(from = "Value",        to = "Satisfaction"),
#   paths(from = "Satisfaction", to = "Loyalty")
# )
# 
# mobi_pls <- estimate_pls(data = mobi,
#                           measurement_model = mobi_mm,
#                           structural_model  = mobi_sm)
# 
# # Run cIPMA (IPMA + NCA)
# cipma_result <- assess_cipma(mobi_pls,
#                               target = "Loyalty",
#                               scale_min = 1,
#                               scale_max = 10,
#                               nca_test.rep = 1000,
#                               seed = 123)
# 
# # View results
# print(cipma_result)
# summary(cipma_result)
# 
# # cIPMA map (importance vs. performance, with NCA overlay)
# plot(cipma_result, type = "cipma")
# 
# # Standard IPMA map (without NCA distinction)
# plot(cipma_result, type = "ipma")
# 
# # Use standardized total effects for importance axis
# plot(cipma_result, importance_metric = "standardized")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(seminr)
# library(seminrExtras)
# 
# # Estimate a simple model
# mobi_mm <- constructs(
#   composite("Image",        multi_items("IMAG", 1:5)),
#   composite("Value",        multi_items("PERV", 1:2)),
#   composite("Satisfaction", multi_items("CUSA", 1:3)),
#   composite("Loyalty",      multi_items("CUSL", 1:3))
# )
# 
# mobi_sm <- relationships(
#   paths(from = c("Image", "Value"), to = "Satisfaction"),
#   paths(from = "Satisfaction", to = "Loyalty")
# )
# 
# mobi_pls <- estimate_pls(data = mobi,
#                           measurement_model = mobi_mm,
#                           structural_model  = mobi_sm)
# 
# # Run NCA -- predictors auto-detected from structural model
# nca_result <- assess_nca(mobi_pls,
#                           target = "Satisfaction",
#                           test.rep = 1000,
#                           seed = 123)
# 
# # Effect sizes and significance
# print(nca_result)
# 
# # Full summary with bottleneck tables
# summary(nca_result)
# 
# # Visualize
# plot(nca_result, type = "effects")   # Effect size bar chart
# plot(nca_result, type = "scatter")   # Ceiling line scatter plots

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# # Run NCA-ESSE on the same model
# esse_result <- assess_nca_esse(mobi_pls,
#                                 target = "Satisfaction",
#                                 thresholds = seq(0, 0.05, by = 0.005),
#                                 seed = 123)
# 
# # View results
# print(esse_result)
# 
# # Summary tables (Table A2 style)
# summary(esse_result)
# 
# # Sensitivity plot (Fig. 4 in Becker et al., 2026)
# plot(esse_result, type = "sensitivity")
# 
# # Difference plot (Fig. 6 in Becker et al., 2026)
# plot(esse_result, type = "difference")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(seminr)
# library(seminrExtras)
# 
# # Estimate a model
# corp_mm <- constructs(
#   composite("COMP", multi_items("comp_", 1:3)),
#   composite("LIKE", multi_items("like_", 1:3)),
#   composite("CUSA", single_item("cusa")),
#   composite("CUSL", multi_items("cusl_", 1:3))
# )
# 
# corp_sm <- relationships(
#   paths(from = c("COMP", "LIKE"), to = "CUSA"),
#   paths(from = "CUSA", to = "CUSL")
# )
# 
# corp_model <- estimate_pls(
#   data = corp_rep_data,
#   measurement_model = corp_mm,
#   structural_model  = corp_sm,
#   missing = mean_replacement,
#   missing_value = "-99")
# 
# # Run full COA pipeline
# coa_result <- assess_coa(corp_model,
#                           focal_construct = "CUSL",
#                           noFolds = 10, reps = 1, cores = 1,
#                           seed = 123)
# 
# # Print results
# print(coa_result)
# summary(coa_result)
# 
# # Visualize
# plot(coa_result, type = "pd")      # Predictive deviance distribution
# plot(coa_result, type = "tree")    # Decision tree
# plot(coa_result, type = "groups")  # Deviant group highlights

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# library(seminr)
# library(seminrExtras)
# 
# mobi_mm <- constructs(
#   composite("Image",        multi_items("IMAG", 1:5)),
#   composite("Expectation",  multi_items("CUEX", 1:3)),
#   composite("Value",        multi_items("PERV", 1:2)),
#   composite("Satisfaction", multi_items("CUSA", 1:3)),
#   composite("Loyalty",      multi_items("CUSL", 1:3))
# )
# 
# mobi_sm <- relationships(
#   paths(from = "Image",       to = c("Expectation", "Satisfaction", "Loyalty")),
#   paths(from = "Expectation", to = c("Value", "Satisfaction")),
#   paths(from = "Value",       to = "Satisfaction"),
#   paths(from = "Satisfaction", to = "Loyalty")
# )
# 
# mobi_pls <- estimate_pls(data = mobi,
#                           measurement_model = mobi_mm,
#                           structural_model  = mobi_sm)
# 
# # CTA-PLS with borrowing (default)
# cta_result <- assess_cta(mobi_pls, nboot = 5000, seed = 123)
# print(cta_result)
# summary(cta_result)
# 
# # Without borrowing — only Image (5 indicators) is testable
# cta_no_borrow <- assess_cta(mobi_pls, nboot = 5000, borrow = FALSE)
# print(cta_no_borrow)
# 
# # Visualize adjusted p-values per construct
# plot(cta_result)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# # FIMIX with K=2 segments (using the corporate reputation model from the setup)
# fimix_k2 <- assess_fimix(established_model, K = 2, nstart = 10, seed = 123)
# print(fimix_k2)
# summary(fimix_k2)
# plot(fimix_k2)
# 
# # Compare across K=2..4 using information criteria
# fimix_compare <- assess_fimix_compare(established_model,
#                                        K_range = 2:4,
#                                        nstart = 10,
#                                        seed = 123)
# print(fimix_compare)
# plot(fimix_compare)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# # PLS-POS with K=2 segments
# pos_k2 <- assess_pos(established_model, K = 2, nstart = 10, seed = 123)
# print(pos_k2)
# summary(pos_k2)
# plot(pos_k2, type = "rsquared")
# plot(pos_k2, type = "paths")
# 
# # Compare across K=2..4 to find optimal number of segments
# pos_compare <- assess_pos_compare(established_model,
#                                    K_range = 2:4,
#                                    nstart = 10,
#                                    seed = 123)
# print(pos_compare)
# plot(pos_compare)
# 
# # Extract segment-specific models for further analysis
# seg_models <- pos_segments(pos_k2)
# summary(seg_models[[1]])

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# # Specify a mediation model
# mobi_mm <- constructs(
#   composite("Image",        multi_items("IMAG", 1:5)),
#   composite("Expectation",  multi_items("CUEX", 1:3)),
#   composite("Value",        multi_items("PERV", 1:2)),
#   composite("Satisfaction", multi_items("CUSA", 1:3)),
#   composite("Loyalty",      multi_items("CUSL", 1:3))
# )
# mobi_sm <- relationships(
#   paths(from = "Image",       to = c("Expectation", "Satisfaction", "Loyalty")),
#   paths(from = "Expectation", to = c("Value", "Satisfaction")),
#   paths(from = "Value",       to = "Satisfaction"),
#   paths(from = "Satisfaction", to = "Loyalty")
# )
# pls_model <- estimate_pls(mobi, mobi_mm, mobi_sm)
# 
# # Compute PCM for all mediation paths to Loyalty
# pcm_result <- assess_pcm(pls_model,
#                          target  = "Loyalty",
#                          noFolds = 10,
#                          reps    = 10)
# 
# # Print concise overview
# pcm_result
# 
# # Detailed per-indicator results
# summary(pcm_result)
# 
# # Visual comparison of mediation paths
# plot(pcm_result)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# cong_result <- congruence_test(mobi_pls,
#                                 nboot = 2000,
#                                 seed = 123)
# print(cong_result)
# summary(cong_result)

