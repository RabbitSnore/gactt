################################################################################

# GACTT - Exploration

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c(
  "tidyverse",
  "lme4",
  "lmerTest",
  "performance"
)

lapply(packages, library, character.only = TRUE)

# Load cleaned data ------------------------------------------------------------

gactt <- read_rds("data/gactt_data-cleaned.rds")

# How do ratings of acidity and bitterness relate to preference? ---------------

# Long format data for tasting notes

taste_long <- gactt %>% 
  pivot_longer(
    cols          = c(ends_with("bitter"), 
                      ends_with("acidity"), 
                      ends_with("preference")),
    names_pattern = "(.)_(.*)",
    names_to      = c("coffee", "measure"),
    values_to     = "rating"
  ) %>% 
  pivot_wider(
    names_from  = "measure",
    values_from = "rating", 
    id_cols     = c("id", "coffee")
  )

## Adding person mean-centered measures

taste_long <- taste_long %>%
  group_by(id) %>% 
  mutate(
    preference_pmc = as.numeric(scale(preference, scale = FALSE)),
    acidity_pmc    = as.numeric(scale(acidity, scale = FALSE)),
    bitter_pmc     = as.numeric(scale(bitter, scale = FALSE))
  ) %>% 
  ungroup()

## Add self-described expertise

taste_long <- taste_long %>% 
  left_join(select(gactt, id, expertise), by = "id")

# Simple bivariate correlations

taste_cor <- cor(select(taste_long,
                        expertise,
                        preference, acidity, bitter,
                        preference_pmc, acidity_pmc, bitter_pmc),
                 use = "pairwise.complete")

# visualizations

scatter_acid_pref <- 
ggplot(taste_long,
       aes(
         x = acidity,
         y = preference
       )) +
  geom_jitter(
    alpha  = .10,
    height = .20,
    width  = .20
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x"
  ) +
  theme_classic()

scatter_bitter_pref <- 
ggplot(taste_long,
       aes(
         x = bitter,
         y = preference
       )) +
  geom_jitter(
    alpha  = .10,
    height = .20,
    width  = .20
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x"
  ) +
  theme_classic()

scatter_acid_pref_pmc <- 
ggplot(taste_long,
       aes(
         x = acidity_pmc,
         y = preference_pmc
       )) +
  geom_jitter(
    alpha  = .10,
    height = .05,
    width  = .05
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x"
  ) +
  theme_classic()

scatter_bitter_pref_pmc <- 
ggplot(taste_long,
       aes(
         x = bitter_pmc,
         y = preference_pmc
       )) +
  geom_jitter(
    alpha  = .10,
    height = .05,
    width  = .05
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x"
  ) +
  theme_classic()

# Mixed-effects modeling

## Raw scores

lmm_taste_unc  <- lmer(preference
                       ~ 1
                       + (1|id)
                       + (1|coffee),
                       data = taste_long)

icc_taste <- icc(lmm_taste_unc, by_group = TRUE, tolerance = 0)

lmm_taste_base <- lmer(preference
                       ~ 1
                       + acidity
                       + bitter
                       + (1 + acidity + bitter|id)
                       + (1 + acidity + bitter|coffee),
                       data = taste_long)

## Person mean-centered

lmm_taste_pmc  <- lmer(preference
                       ~ 1
                       + acidity_pmc
                       + bitter_pmc
                       + (1 + acidity_pmc + bitter_pmc|id)
                       + (1 + acidity_pmc + bitter_pmc|coffee),
                       data = taste_long)

taste_pmc_coefs <- coef(lmm_taste_pmc)

lmm_taste_pmc_exp  <- lmer(preference
                           ~ 1
                           + acidity_pmc
                           + bitter_pmc
                           + expertise
                           + (1 + acidity_pmc + bitter_pmc|id)
                           + (1 + acidity_pmc + bitter_pmc|coffee),
                           data = taste_long)

### Visualizations of random effects

hist_acidity_taste <- 
ggplot(taste_pmc_coefs$id,
       aes(
         x = acidity_pmc
       )) +
  geom_histogram(
    binwidth = 0.05,
    color    = "black",
    fill     = "grey"
  ) +
  scale_x_continuous(
    breaks = seq(-1.0, 0.8, 0.1)
  ) +
  labs(
    x = "Random Slope Estimate - Acidity",
    y = "count"
  ) +
  theme_classic()

hist_bitter_taste <- 
ggplot(taste_pmc_coefs$id,
       aes(
         x = bitter_pmc
       )) +
  geom_histogram(
    binwidth = 0.05,
    color    = "black",
    fill     = "grey"
  ) +
  scale_x_continuous(
    breaks = seq(-1.0, 0.8, 0.1)
  ) +
  labs(
    x = "Random Slope Estimate - Bitterness",
    y = "Count"
  ) +
  theme_classic()
