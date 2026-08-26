# ============================================================
# PAPER / DATA IN BRIEF - FINAL COMPACT ANALYSIS SCRIPT
# Full sample: N = 522
# major: 1=AI, 2=SE, 3=IC, 4=Business
# group: EarlyYear / FinalYear
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(lavaan)
library(semTools)
library(writexl)
library(ggplot2)

# ============================================================
# PART 1. MAIN MODEL AND ESTIMATION
# ============================================================

# ------------------------------------------------------------
# 1. Data


df <- read_excel("dataset.xlsx")

items <- c(
  "x1","x2","x3",
  "x4","x5","x6",
  "x7","x8","x9",
  "x10","x11","x12",
  "x13","x14","x15"
)

df <- df %>%
  mutate(
    major = factor(
      major,
      levels = 1:4,
      labels = c("AI","SE","IC","Business")
    ),
    group = factor(
      group,
      levels = c("EarlyYear","FinalYear")
    ),
    GPA = as.numeric(GPA)
  )

df[items] <- lapply(df[items], as.numeric)

cat("N =", nrow(df), "\n")
print(table(df$group))
print(table(df$major))


# ------------------------------------------------------------
# 2. Construct scores for descriptive figures only
# ------------------------------------------------------------

df <- df %>%
  mutate(
    Loyalty_score     = rowMeans(across(c(x1,x2,x3)), na.rm = TRUE),
    Trust_score       = rowMeans(across(c(x4,x5,x6)), na.rm = TRUE),
    Usefulness_score  = rowMeans(across(c(x7,x8,x9)), na.rm = TRUE),
    Willingness_score = rowMeans(across(c(x10,x11,x12)), na.rm = TRUE),
    Bias_score        = rowMeans(across(c(x13,x14,x15)), na.rm = TRUE)
  )


# ------------------------------------------------------------
# 3. Measurement model
# ------------------------------------------------------------

model_5f <- '
Loyalty =~ x1 + x2 + x3
Trust =~ x4 + x5 + x6
Usefulness =~ x7 + x8 + x9
Willingness =~ x10 + x11 + x12
Bias =~ x13 + x14 + x15
'

fit_cfa <- cfa(
  model_5f,
  data = df,
  estimator = "MLR",
  std.lv = TRUE,
  missing = "fiml"
)


# ------------------------------------------------------------
# 4. Reliability / validity
# ------------------------------------------------------------

constructs <- list(
  Loyalty = c("x1","x2","x3"),
  Trust = c("x4","x5","x6"),
  Usefulness = c("x7","x8","x9"),
  Willingness = c("x10","x11","x12"),
  Bias = c("x13","x14","x15")
)

alpha_table <- bind_rows(
  lapply(names(constructs), function(nm){
    
    a <- psych::alpha(
      df[constructs[[nm]]],
      warnings = FALSE
    )
    
    data.frame(
      Construct = nm,
      Alpha = a$total$raw_alpha
    )
  })
)

cr <- semTools::compRelSEM(fit_cfa)
ave <- semTools::AVE(fit_cfa)

cr_ave_table <- data.frame(
  Construct = names(cr),
  CR = as.numeric(cr),
  AVE = as.numeric(ave)
)

loading_table <- standardizedSolution(fit_cfa) %>%
  filter(op == "=~") %>%
  select(
    Construct = lhs,
    Item = rhs,
    Loading = est.std,
    pvalue
  )

htmt_table <- as.data.frame(
  semTools::htmt(
    model_5f,
    data = df[items]
  )
)

htmt_table$Construct <- rownames(htmt_table)
rownames(htmt_table) <- NULL


# ------------------------------------------------------------
# 5. CFA fit
# ------------------------------------------------------------

cfa_fit <- data.frame(
  Model = "Five-factor CFA",
  ChiSq = fitMeasures(fit_cfa, "chisq"),
  df = fitMeasures(fit_cfa, "df"),
  CFI = fitMeasures(fit_cfa, "cfi.robust"),
  TLI = fitMeasures(fit_cfa, "tli.robust"),
  RMSEA = fitMeasures(fit_cfa, "rmsea.robust"),
  SRMR = fitMeasures(fit_cfa, "srmr")
)


# ------------------------------------------------------------
# 6. Measurement invariance: Early vs Final
# ------------------------------------------------------------

fit_configural <- cfa(
  model_5f,
  data = df,
  group = "group",
  estimator = "MLR",
  std.lv = TRUE
)

fit_metric <- cfa(
  model_5f,
  data = df,
  group = "group",
  group.equal = "loadings",
  estimator = "MLR",
  std.lv = TRUE
)

fit_scalar <- cfa(
  model_5f,
  data = df,
  group = "group",
  group.equal = c("loadings","intercepts"),
  estimator = "MLR",
  std.lv = TRUE
)

invariance_table <- data.frame(
  Model = c("Configural","Metric","Scalar"),
  CFI = c(
    fitMeasures(fit_configural, "cfi.robust"),
    fitMeasures(fit_metric, "cfi.robust"),
    fitMeasures(fit_scalar, "cfi.robust")
  ),
  RMSEA = c(
    fitMeasures(fit_configural, "rmsea.robust"),
    fitMeasures(fit_metric, "rmsea.robust"),
    fitMeasures(fit_scalar, "rmsea.robust")
  ),
  SRMR = c(
    fitMeasures(fit_configural, "srmr"),
    fitMeasures(fit_metric, "srmr"),
    fitMeasures(fit_scalar, "srmr")
  )
) %>%
  mutate(
    Delta_CFI = c(NA, diff(CFI)),
    Delta_RMSEA = c(NA, diff(RMSEA)),
    Delta_SRMR = c(NA, diff(SRMR))
  )

metric_ok <- abs(invariance_table$Delta_CFI[2]) <= .01
scalar_ok <- abs(invariance_table$Delta_CFI[3]) <= .01


# ------------------------------------------------------------
# 7. Latent means
# only if scalar invariance is acceptable
# ------------------------------------------------------------

latent_means <- data.frame()

if(scalar_ok){
  
  latent_means <- parameterEstimates(
    fit_scalar,
    standardized = TRUE
  ) %>%
    filter(
      op == "~1",
      lhs %in% c(
        "Loyalty","Trust","Usefulness",
        "Willingness","Bias"
      )
    ) %>%
    mutate(
      Group = ifelse(
        group == 1,
        "EarlyYear",
        "FinalYear"
      )
    ) %>%
    select(
      Group,
      Construct = lhs,
      Estimate = est,
      SE = se,
      z,
      pvalue
    )
}


# ------------------------------------------------------------
# 8. Overall structural model
# ------------------------------------------------------------

model_sem <- '
Loyalty =~ x1 + x2 + x3
Trust =~ x4 + x5 + x6
Usefulness =~ x7 + x8 + x9
Willingness =~ x10 + x11 + x12
Bias =~ x13 + x14 + x15

Trust ~ a*Loyalty
Usefulness ~ b*Trust
Willingness ~ c*Usefulness + d*Bias

Loyalty ~~ Bias

indirect_Loyalty_Usefulness := a*b
indirect_Trust_Willingness := b*c
indirect_Loyalty_Willingness := a*b*c
'

fit_sem <- sem(
  model_sem,
  data = df,
  estimator = "MLR",
  std.lv = TRUE,
  missing = "fiml"
)

sem_fit <- data.frame(
  Model = "Structural model",
  ChiSq = fitMeasures(fit_sem, "chisq"),
  df = fitMeasures(fit_sem, "df"),
  CFI = fitMeasures(fit_sem, "cfi.robust"),
  TLI = fitMeasures(fit_sem, "tli.robust"),
  RMSEA = fitMeasures(fit_sem, "rmsea.robust"),
  SRMR = fitMeasures(fit_sem, "srmr")
)

sem_paths <- parameterEstimates(
  fit_sem,
  standardized = TRUE,
  ci = TRUE
) %>%
  filter(op %in% c("~",":=")) %>%
  select(
    Outcome = lhs,
    op,
    Predictor = rhs,
    Estimate = est,
    SE = se,
    z,
    pvalue,
    CI_Lower = ci.lower,
    CI_Upper = ci.upper,
    Std_Estimate = std.all
  )


# ------------------------------------------------------------
# 9. Multi-group structural model
# ------------------------------------------------------------

measurement_constraints <- character(0)

if(metric_ok)
  measurement_constraints <- "loadings"

if(scalar_ok)
  measurement_constraints <- c("loadings","intercepts")


model_sem_free <- '
Loyalty =~ x1 + x2 + x3
Trust =~ x4 + x5 + x6
Usefulness =~ x7 + x8 + x9
Willingness =~ x10 + x11 + x12
Bias =~ x13 + x14 + x15

Trust ~ c(a1,a2)*Loyalty
Usefulness ~ c(b1,b2)*Trust
Willingness ~ c(c1,c2)*Usefulness + c(d1,d2)*Bias

Loyalty ~~ Bias

indirect_Early := a1*b1*c1
indirect_Final := a2*b2*c2
'

fit_sem_free <- sem(
  model_sem_free,
  data = df,
  group = "group",
  group.equal = measurement_constraints,
  estimator = "MLR",
  std.lv = TRUE
)


model_sem_equal <- '
Loyalty =~ x1 + x2 + x3
Trust =~ x4 + x5 + x6
Usefulness =~ x7 + x8 + x9
Willingness =~ x10 + x11 + x12
Bias =~ x13 + x14 + x15

Trust ~ c(a,a)*Loyalty
Usefulness ~ c(b,b)*Trust
Willingness ~ c(c,c)*Usefulness + c(d,d)*Bias

Loyalty ~~ Bias
'

fit_sem_equal <- sem(
  model_sem_equal,
  data = df,
  group = "group",
  group.equal = measurement_constraints,
  estimator = "MLR",
  std.lv = TRUE
)


group_model_fit <- data.frame(
  Model = c(
    "Free structural paths",
    "Equal structural paths"
  ),
  CFI = c(
    fitMeasures(fit_sem_free, "cfi.robust"),
    fitMeasures(fit_sem_equal, "cfi.robust")
  ),
  RMSEA = c(
    fitMeasures(fit_sem_free, "rmsea.robust"),
    fitMeasures(fit_sem_equal, "rmsea.robust")
  ),
  SRMR = c(
    fitMeasures(fit_sem_free, "srmr"),
    fitMeasures(fit_sem_equal, "srmr")
  )
)

group_model_test <- lavTestLRT(
  fit_sem_free,
  fit_sem_equal
)


# ------------------------------------------------------------
# 10. Structural paths by group
# ------------------------------------------------------------

group_paths <- parameterEstimates(
  fit_sem_free,
  standardized = TRUE,
  ci = TRUE
) %>%
  filter(op == "~") %>%
  mutate(
    Group = case_when(
      group == 1 ~ "EarlyYear",
      group == 2 ~ "FinalYear"
    )
  ) %>%
  select(
    Group,
    Outcome = lhs,
    Predictor = rhs,
    Estimate = est,
    SE = se,
    pvalue,
    Std_Estimate = std.all
  )


# ------------------------------------------------------------
# 11. Wald tests for group differences
# ------------------------------------------------------------

wald_table <- bind_rows(
  
  data.frame(
    Path = "Loyalty -> Trust",
    test = I(list(
      lavTestWald(
        fit_sem_free,
        constraints = "a1 == a2"
      )
    ))
  ),
  
  data.frame(
    Path = "Trust -> Usefulness",
    test = I(list(
      lavTestWald(
        fit_sem_free,
        constraints = "b1 == b2"
      )
    ))
  ),
  
  data.frame(
    Path = "Usefulness -> Willingness",
    test = I(list(
      lavTestWald(
        fit_sem_free,
        constraints = "c1 == c2"
      )
    ))
  ),
  
  data.frame(
    Path = "Bias -> Willingness",
    test = I(list(
      lavTestWald(
        fit_sem_free,
        constraints = "d1 == d2"
      )
    ))
  )
  
) %>%
  rowwise() %>%
  mutate(
    ChiSq = test$stat,
    df = test$df,
    pvalue = test$p.value
  ) %>%
  select(-test) %>%
  ungroup()


# ------------------------------------------------------------
# 12. GPA associations
# ------------------------------------------------------------

model_gpa <- '
Loyalty =~ x1 + x2 + x3
Trust =~ x4 + x5 + x6
Usefulness =~ x7 + x8 + x9
Willingness =~ x10 + x11 + x12
Bias =~ x13 + x14 + x15

GPA ~~ Loyalty
GPA ~~ Trust
GPA ~~ Usefulness
GPA ~~ Willingness
GPA ~~ Bias
'

fit_gpa <- sem(
  model_gpa,
  data = df,
  estimator = "MLR",
  std.lv = TRUE,
  missing = "fiml"
)

gpa_latent <- standardizedSolution(fit_gpa) %>%
  filter(
    op == "~~",
    (
      lhs == "GPA" &
        rhs %in% c(
          "Loyalty","Trust","Usefulness",
          "Willingness","Bias"
        )
    ) |
      (
        rhs == "GPA" &
          lhs %in% c(
            "Loyalty","Trust","Usefulness",
            "Willingness","Bias"
          )
      )
  ) %>%
  mutate(
    Construct = ifelse(
      lhs == "GPA",
      rhs,
      lhs
    )
  ) %>%
  select(
    Construct,
    Correlation = est.std,
    SE = se,
    z,
    pvalue
  )


# ============================================================
# PART 2. ROBUSTNESS CHECKS
# ============================================================

# ------------------------------------------------------------
# 13. Straight-lining
# ------------------------------------------------------------

df$response_sd <- apply(
  df[items],
  1,
  sd,
  na.rm = TRUE
)

df$straightliner <- df$response_sd < .20

df_robust <- df %>%
  filter(!straightliner)


fit_cfa_robust <- cfa(
  model_5f,
  data = df_robust,
  estimator = "MLR",
  std.lv = TRUE
)

fit_sem_robust <- sem(
  model_sem,
  data = df_robust,
  estimator = "MLR",
  std.lv = TRUE
)


robustness_table <- data.frame(
  Model = c(
    "CFA full sample",
    "CFA no straight-liners",
    "SEM full sample",
    "SEM no straight-liners"
  ),
  
  CFI = c(
    fitMeasures(fit_cfa, "cfi.robust"),
    fitMeasures(fit_cfa_robust, "cfi.robust"),
    fitMeasures(fit_sem, "cfi.robust"),
    fitMeasures(fit_sem_robust, "cfi.robust")
  ),
  
  RMSEA = c(
    fitMeasures(fit_cfa, "rmsea.robust"),
    fitMeasures(fit_cfa_robust, "rmsea.robust"),
    fitMeasures(fit_sem, "rmsea.robust"),
    fitMeasures(fit_sem_robust, "rmsea.robust")
  ),
  
  SRMR = c(
    fitMeasures(fit_cfa, "srmr"),
    fitMeasures(fit_cfa_robust, "srmr"),
    fitMeasures(fit_sem, "srmr"),
    fitMeasures(fit_sem_robust, "srmr")
  )
)


# ------------------------------------------------------------
# 14. Measurement invariance across major
# secondary robustness only
# ------------------------------------------------------------

fit_major_config <- cfa(
  model_5f,
  data = df,
  group = "major",
  estimator = "MLR",
  std.lv = TRUE
)

fit_major_metric <- cfa(
  model_5f,
  data = df,
  group = "major",
  group.equal = "loadings",
  estimator = "MLR",
  std.lv = TRUE
)

major_invariance <- data.frame(
  Model = c("Major configural","Major metric"),
  CFI = c(
    fitMeasures(fit_major_config, "cfi.robust"),
    fitMeasures(fit_major_metric, "cfi.robust")
  ),
  RMSEA = c(
    fitMeasures(fit_major_config, "rmsea.robust"),
    fitMeasures(fit_major_metric, "rmsea.robust")
  ),
  SRMR = c(
    fitMeasures(fit_major_config, "srmr"),
    fitMeasures(fit_major_metric, "srmr")
  )
) %>%
  mutate(
    Delta_CFI = c(NA, diff(CFI))
  )


# ============================================================
# PART 3. MANUSCRIPT TABLES AND FIGURES
# ============================================================

# ------------------------------------------------------------
# 15. Table 1: sample and descriptive statistics
# ------------------------------------------------------------

table1 <- df %>%
  summarise(
    N = n(),
    
    GPA_Mean = mean(GPA, na.rm = TRUE),
    GPA_SD = sd(GPA, na.rm = TRUE),
    
    Loyalty_Mean = mean(Loyalty_score),
    Loyalty_SD = sd(Loyalty_score),
    
    Trust_Mean = mean(Trust_score),
    Trust_SD = sd(Trust_score),
    
    Usefulness_Mean = mean(Usefulness_score),
    Usefulness_SD = sd(Usefulness_score),
    
    Willingness_Mean = mean(Willingness_score),
    Willingness_SD = sd(Willingness_score),
    
    Bias_Mean = mean(Bias_score),
    Bias_SD = sd(Bias_score)
  )


# ------------------------------------------------------------
# 16. Table 2: measurement model
# ------------------------------------------------------------

table2 <- loading_table %>%
  left_join(
    cr_ave_table,
    by = "Construct"
  ) %>%
  left_join(
    alpha_table,
    by = "Construct"
  )


# ------------------------------------------------------------
# 17. Table 3: measurement invariance
# ------------------------------------------------------------

table3 <- invariance_table


# ------------------------------------------------------------
# 18. Table 4: structural model
# ------------------------------------------------------------

table4 <- sem_paths


# ------------------------------------------------------------
# 19. Table 5: academic-stage differences
# ------------------------------------------------------------

table5a <- group_paths
table5b <- wald_table
table5c <- latent_means


# ------------------------------------------------------------
# 20. Table 6: GPA associations
# ------------------------------------------------------------

table6 <- gpa_latent


# ------------------------------------------------------------
# 21. Export all tables
# ------------------------------------------------------------

write_xlsx(
  list(
    Table1_Descriptives = table1,
    Table2_Measurement = table2,
    Table2_HTMT = htmt_table,
    Table3_Invariance = table3,
    Table4_Structural = table4,
    Table5A_Group_Paths = table5a,
    Table5B_Group_Tests = table5b,
    Table5C_Latent_Means = table5c,
    Table6_GPA = table6,
    Robustness = robustness_table,
    Major_Invariance = major_invariance
  ),
  "Manuscript_Tables.xlsx"
)


# ------------------------------------------------------------
# 22. Figure 1: conceptual model
# use semPlot if desired
# ------------------------------------------------------------

# install.packages("semPlot")
# library(semPlot)

# semPaths(
#   fit_sem,
#   what = "std",
#   whatLabels = "std",
#   layout = "tree",
#   residuals = FALSE,
#   intercepts = FALSE,
#   edge.label.cex = .8
# )


# ------------------------------------------------------------
# 23. Figure 2: construct means by academic stage
# ------------------------------------------------------------

figure_data <- df %>%
  select(
    group,
    Loyalty_score,
    Trust_score,
    Usefulness_score,
    Willingness_score,
    Bias_score
  ) %>%
  pivot_longer(
    -group,
    names_to = "Construct",
    values_to = "Score"
  ) %>%
  mutate(
    Construct = gsub("_score", "", Construct)
  ) %>%
  group_by(group, Construct) %>%
  summarise(
    Mean = mean(Score),
    SE = sd(Score)/sqrt(n()),
    .groups = "drop"
  )


p1 <- ggplot(
  figure_data,
  aes(
    x = Construct,
    y = Mean,
    group = group,
    shape = group
  )
) +
  geom_point(
    position = position_dodge(.3),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = Mean - 1.96*SE,
      ymax = Mean + 1.96*SE
    ),
    position = position_dodge(.3),
    width = .15
  ) +
  labs(
    x = NULL,
    y = "Mean construct score",
    shape = "Academic stage"
  ) +
  theme_minimal()

ggsave(
  "Figure_constructs_by_group.png",
  p1,
  width = 8,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# 24. Figure 3: GPA associations
# ------------------------------------------------------------

p2 <- ggplot(
  gpa_latent,
  aes(
    x = reorder(
      Construct,
      Correlation
    ),
    y = Correlation
  )
) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Latent correlation with GPA"
  ) +
  theme_minimal()

ggsave(
  "Figure_GPA_latent_correlations.png",
  p2,
  width = 7,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# 25. Print key results
# ------------------------------------------------------------

cat("\n========== CFA FIT ==========\n")
print(cfa_fit)

cat("\n========== MEASUREMENT INVARIANCE ==========\n")
print(invariance_table)

cat("\n========== STRUCTURAL MODEL ==========\n")
print(sem_fit)

cat("\n========== STRUCTURAL PATHS ==========\n")
print(sem_paths)

cat("\n========== GROUP MODEL TEST ==========\n")
print(group_model_test)

cat("\n========== WALD TESTS ==========\n")
print(wald_table)

cat("\n========== LATENT MEANS ==========\n")
print(latent_means)

cat("\n========== GPA ASSOCIATIONS ==========\n")
print(gpa_latent)

cat("\n========== ROBUSTNESS ==========\n")
print(robustness_table)



# ============================================================
# PART 3. MANUSCRIPT TABLES AND FIGURES
# ============================================================


# ============================================================
# TABLE 1. SAMPLE CHARACTERISTICS AND DESCRIPTIVE STATISTICS
# ============================================================

# ---- Table 1A. Sample composition ----

table1a_group <- df %>%
  count(group, name = "N") %>%
  mutate(
    Variable = "Academic stage",
    Category = as.character(group),
    Percent = 100 * N / sum(N)
  ) %>%
  select(Variable, Category, N, Percent)


table1a_major <- df %>%
  count(major, name = "N") %>%
  mutate(
    Variable = "Academic major",
    Category = as.character(major),
    Percent = 100 * N / sum(N)
  ) %>%
  select(Variable, Category, N, Percent)


table1a <- bind_rows(
  table1a_group,
  table1a_major
) %>%
  mutate(
    Percent = round(Percent, 1)
  )


# ---- Table 1B. Construct and GPA descriptives ----

table1b <- data.frame(
  Variable = c(
    "Loyalty",
    "Trust",
    "Usefulness",
    "Willingness",
    "Bias",
    "GPA"
  ),
  Mean = c(
    mean(df$Loyalty_score, na.rm = TRUE),
    mean(df$Trust_score, na.rm = TRUE),
    mean(df$Usefulness_score, na.rm = TRUE),
    mean(df$Willingness_score, na.rm = TRUE),
    mean(df$Bias_score, na.rm = TRUE),
    mean(df$GPA, na.rm = TRUE)
  ),
  SD = c(
    sd(df$Loyalty_score, na.rm = TRUE),
    sd(df$Trust_score, na.rm = TRUE),
    sd(df$Usefulness_score, na.rm = TRUE),
    sd(df$Willingness_score, na.rm = TRUE),
    sd(df$Bias_score, na.rm = TRUE),
    sd(df$GPA, na.rm = TRUE)
  ),
  Min = c(
    min(df$Loyalty_score, na.rm = TRUE),
    min(df$Trust_score, na.rm = TRUE),
    min(df$Usefulness_score, na.rm = TRUE),
    min(df$Willingness_score, na.rm = TRUE),
    min(df$Bias_score, na.rm = TRUE),
    min(df$GPA, na.rm = TRUE)
  ),
  Max = c(
    max(df$Loyalty_score, na.rm = TRUE),
    max(df$Trust_score, na.rm = TRUE),
    max(df$Usefulness_score, na.rm = TRUE),
    max(df$Willingness_score, na.rm = TRUE),
    max(df$Bias_score, na.rm = TRUE),
    max(df$GPA, na.rm = TRUE)
  )
) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 1. SAMPLE CHARACTERISTICS AND DESCRIPTIVE STATISTICS\n")
cat("############################################################\n")

cat("\n--- Table 1A. Sample composition ---\n")
print(table1a)

cat("\n--- Table 1B. Construct scores and GPA ---\n")
print(table1b)



# ============================================================
# TABLE 2. MEASUREMENT MODEL
# Factor loadings, Alpha, CR, AVE, and HTMT
# ============================================================

table2 <- loading_table %>%
  left_join(
    alpha_table,
    by = "Construct"
  ) %>%
  left_join(
    cr_ave_table,
    by = "Construct"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


table2_htmt <- htmt_table %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 2. MEASUREMENT MODEL ASSESSMENT\n")
cat("############################################################\n")

cat("\n--- Table 2A. Factor loadings, Alpha, CR, and AVE ---\n")
print(table2)

cat("\n--- Table 2B. HTMT discriminant validity ---\n")
print(table2_htmt)



# ============================================================
# TABLE 3. MEASUREMENT INVARIANCE
# EarlyYear versus FinalYear
# ============================================================

table3 <- invariance_table %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 3. MEASUREMENT INVARIANCE ACROSS ACADEMIC STAGES\n")
cat("############################################################\n")

print(table3)



# ============================================================
# TABLE 4. OVERALL STRUCTURAL MODEL
# Direct and indirect effects
# ============================================================

table4_fit <- sem_fit %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


table4_paths <- sem_paths %>%
  mutate(
    Effect = ifelse(
      op == "~",
      paste(Predictor, "->", Outcome),
      Outcome
    ),
    Effect_Type = ifelse(
      op == "~",
      "Direct",
      "Indirect"
    )
  ) %>%
  select(
    Effect_Type,
    Effect,
    Estimate,
    SE,
    z,
    pvalue,
    CI_Lower,
    CI_Upper,
    Std_Estimate
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 4. OVERALL STRUCTURAL MODEL\n")
cat("############################################################\n")

cat("\n--- Table 4A. Model fit ---\n")
print(table4_fit)

cat("\n--- Table 4B. Direct and indirect effects ---\n")
print(table4_paths)



# ============================================================
# TABLE 5. ACADEMIC-STAGE COMPARISONS
# Latent means + group-specific paths + Wald tests
# ============================================================

table5a <- latent_means %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


table5b <- group_paths %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


table5c <- wald_table %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


# Overall structural equality test
table5d <- data.frame(
  Comparison = "Free vs. equal structural paths",
  ChiSq_Difference = group_model_test$`Chisq diff`[2],
  df_Difference = group_model_test$`Df diff`[2],
  pvalue = group_model_test$`Pr(>Chisq)`[2]
) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 5. ACADEMIC-STAGE COMPARISONS\n")
cat("############################################################\n")

cat("\n--- Table 5A. Latent mean differences ---\n")
print(table5a)

cat("\n--- Table 5B. Structural paths by academic stage ---\n")
print(table5b)

cat("\n--- Table 5C. Wald tests of individual path differences ---\n")
print(table5c)

cat("\n--- Table 5D. Overall structural invariance test ---\n")
print(table5d)



# ============================================================
# TABLE 6. LATENT ASSOCIATIONS WITH GPA
# ============================================================

table6 <- gpa_latent %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("TABLE 6. LATENT ASSOCIATIONS WITH GPA\n")
cat("############################################################\n")

print(table6)



# ============================================================
# SUPPLEMENTARY TABLE S1. ROBUSTNESS CHECK
# Excluding potential straight-liners
# ============================================================

tableS1 <- robustness_table %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("SUPPLEMENTARY TABLE S1. ROBUSTNESS CHECK\n")
cat("############################################################\n")

print(tableS1)



# ============================================================
# SUPPLEMENTARY TABLE S2. MAJOR INVARIANCE
# ============================================================

tableS2 <- major_invariance %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )


cat("\n")
cat("############################################################\n")
cat("SUPPLEMENTARY TABLE S2. MEASUREMENT INVARIANCE ACROSS MAJORS\n")
cat("############################################################\n")

print(tableS2)



# ============================================================
# EXPORT ALL MANUSCRIPT TABLES
# ============================================================

write_xlsx(
  list(
    Table1A_Sample = table1a,
    Table1B_Descriptives = table1b,
    
    Table2A_Measurement = table2,
    Table2B_HTMT = table2_htmt,
    
    Table3_Invariance = table3,
    
    Table4A_SEM_Fit = table4_fit,
    Table4B_SEM_Effects = table4_paths,
    
    Table5A_Latent_Means = table5a,
    Table5B_Group_Paths = table5b,
    Table5C_Wald_Tests = table5c,
    Table5D_Overall_Group_Test = table5d,
    
    Table6_GPA = table6,
    
    Supplement_S1_Robustness = tableS1,
    Supplement_S2_Major = tableS2
  ),
  
  "Manuscript_Tables.xlsx"
)



# ============================================================
# FIGURE 1. ESTIMATED CONCEPTUAL MODEL
# ============================================================

# Figure 1 is optional because it requires semPlot.
# Run once if semPlot is not installed:
#
# install.packages("semPlot")

if(requireNamespace("semPlot", quietly = TRUE)){
  
  png(
    "Figure1_SEM_Model.png",
    width = 2400,
    height = 1600,
    res = 300
  )
  
  semPlot::semPaths(
    fit_sem,
    what = "std",
    whatLabels = "std",
    layout = "tree2",
    residuals = FALSE,
    intercepts = FALSE,
    exoCov = TRUE,
    edge.label.cex = 1.0,
    sizeLat = 9,
    sizeMan = 6,
    nCharNodes = 0,
    title = FALSE
  )
  
  dev.off()
  
  cat("\nFigure 1 saved: Figure1_SEM_Model.png\n")
  
} else {
  
  cat(
    "\nFigure 1 not generated:",
    "install package 'semPlot' if needed.\n"
  )
}



# ============================================================
# FIGURE 2. CONSTRUCT SCORES BY ACADEMIC STAGE
# ============================================================

figure2_data <- df %>%
  
  select(
    group,
    Loyalty_score,
    Trust_score,
    Usefulness_score,
    Willingness_score,
    Bias_score
  ) %>%
  
  pivot_longer(
    cols = -group,
    names_to = "Construct",
    values_to = "Score"
  ) %>%
  
  mutate(
    Construct = gsub(
      "_score",
      "",
      Construct
    )
  ) %>%
  
  group_by(
    group,
    Construct
  ) %>%
  
  summarise(
    N = sum(!is.na(Score)),
    Mean = mean(Score, na.rm = TRUE),
    SE = sd(Score, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  )


figure2 <- ggplot(
  figure2_data,
  aes(
    x = Construct,
    y = Mean,
    group = group,
    shape = group
  )
) +
  
  geom_point(
    position = position_dodge(width = .35),
    size = 3
  ) +
  
  geom_errorbar(
    aes(
      ymin = Mean - 1.96 * SE,
      ymax = Mean + 1.96 * SE
    ),
    position = position_dodge(width = .35),
    width = .12
  ) +
  
  labs(
    x = NULL,
    y = "Mean construct score",
    shape = "Academic stage"
  ) +
  
  theme_minimal(
    base_size = 12
  )


ggsave(
  filename = "Figure2_Constructs_by_Stage.png",
  plot = figure2,
  width = 8,
  height = 5,
  dpi = 300
)


cat(
  "\nFigure 2 saved:",
  "Figure2_Constructs_by_Stage.png\n"
)

# ============================================================
# REPRODUCIBILITY INFORMATION
# ============================================================

writeLines(
  capture.output(sessionInfo()),
  "sessionInfo.txt"
)

cat("\nReproducibility file saved: sessionInfo.txt\n")

# ============================================================
# FINAL MANUSCRIPT OUTPUT SUMMARY
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat("             MANUSCRIPT OUTPUT COMPLETED\n")
cat("============================================================\n")

cat("\nMain tables:\n")
cat("Table 1. Sample characteristics and descriptive statistics\n")
cat("Table 2. Measurement model assessment\n")
cat("Table 3. Measurement invariance across academic stages\n")
cat("Table 4. Overall structural model and indirect effects\n")
cat("Table 5. Latent means and structural group comparisons\n")
cat("Table 6. Latent associations with GPA\n")

cat("\nSupplementary tables:\n")
cat("Table S1. Robustness excluding potential straight-liners\n")
cat("Table S2. Measurement invariance across majors\n")

cat("\nFigures:\n")
cat("Figure 1. Estimated structural equation model\n")
cat("Figure 2. Construct scores by academic stage\n")

cat("\nExcel output:\n")
cat("Manuscript_Tables.xlsx\n")

cat("\n============================================================\n")