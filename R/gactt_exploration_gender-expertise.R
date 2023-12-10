################################################################################

# GACTT - Exploration of Gender and Self-Described Expertise

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c(
  "tidyverse"
)

lapply(packages, library, character.only = TRUE)

# Load cleaned data ------------------------------------------------------------

gactt <- read_rds("data/gactt_data-cleaned.rds")

# Gender and self-described expertise ------------------------------------------

# Two interesting results that James reported in the video were (1) that women
# reported preferring Coffee D (the funky natural process one) less than men and
# (2) that people who described themselves as having greater coffee expertise
# preferred Coffee D to a greater extent. Is this gender difference explained by
# differences in self-described expertise?

# Subset data for gender analyses

data_gender_exp <- gactt %>% 
  filter(gender %in% c("Female", "Male", "Non-binary")) %>% 
  mutate(
    gender = as.factor(gender),
    exp_mc = as.numeric(scale(expertise, scale = FALSE))
  )

# Descriptive tables

table_gender_dpref <- data_gender_exp %>% 
  group_by(gender) %>% 
  summarise(
    mean_dpref   = mean(d_preference, na.rm = TRUE),
    sd_dpref     = sd(d_preference, na.rm = TRUE),
    median_dpref = median(d_preference, na.rm = TRUE),
    n            = n()
  )

table_gender_exp <- data_gender_exp %>% 
  group_by(gender) %>% 
  summarise(
    mean_exp   = mean(expertise),
    sd_exp     = sd(expertise),
    median_exp = median(expertise),
    n          = n()
  )

table_exp_dpref <- data_gender_exp %>% 
  group_by(expertise) %>% 
  summarise(
    mean_dpref   = mean(d_preference, na.rm = TRUE),
    sd_dpref     = sd(d_preference, na.rm = TRUE),
    median_dpref = median(d_preference, na.rm = TRUE),
    n            = n()
  )

table_gender_exp_dpref <- data_gender_exp %>% 
  group_by(gender, expertise) %>% 
  summarise(
    mean_dpref   = mean(d_preference, na.rm = TRUE),
    sd_dpref     = sd(d_preference, na.rm = TRUE),
    median_dpref = median(d_preference, na.rm = TRUE),
    n            = n()
  )

## Favorite in comparison

table_comparison <- data_gender_exp %>% 
  group_by(expertise) %>% 
  summarise(
    A = sum(compare_overall == "Coffee A")/n(),
    B = sum(compare_overall == "Coffee B")/n(),
    C = sum(compare_overall == "Coffee C")/n(),
    D = sum(compare_overall == "Coffee D")/n()
  )

table_comparison_long <- table_comparison %>% 
  pivot_longer(
    cols      = c("A", "B", "C", "D"),
    names_to  = "coffee",
    values_to = "proportion"
  )

table_comparison_gender <- data_gender_exp %>% 
  group_by(gender, expertise) %>% 
  summarise(
    A = sum(compare_overall == "Coffee A")/n(),
    B = sum(compare_overall == "Coffee B")/n(),
    C = sum(compare_overall == "Coffee C")/n(),
    D = sum(compare_overall == "Coffee D")/n()
  )

table_comparison_gender_long <- table_comparison_gender %>% 
  pivot_longer(
    cols      = c("A", "B", "C", "D"),
    names_to  = "coffee",
    values_to = "proportion"
  )

# Visualizations

hist_gender_exp <- 
  ggplot(data_gender_exp,
         aes(
           x = expertise
         )) +
  facet_wrap(~ gender) +
  geom_histogram(
    binwidth = 1,
    color    = "black",
    fill     = "grey"
  ) +
  geom_vline(
    data = table_gender_exp,
    aes(
      xintercept = mean_exp
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "Self-Described Expertise",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  theme_classic()

hist_gender_dpref <- 
  ggplot(data_gender_exp,
         aes(
           x = d_preference
         )) +
  facet_wrap(~ gender) +
  geom_histogram(
    binwidth = 1,
    color    = "black",
    fill     = "grey"
  ) +
  geom_vline(
    data = table_gender_dpref,
    aes(
      xintercept = mean_dpref
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "Preference for Coffee D",
    y = "Count"
  ) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  theme_classic()

scatter_gender_exp_dpref <- 
  ggplot(data_gender_exp,
         aes(
           x = expertise,
           y = d_preference
         )) +
  facet_wrap(~ gender) +
  geom_jitter(
    alpha  = .10,
    height = .20,
    width  = .20
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x"
  ) +
  labs(
    x = "Self-Described Expertise",
    y = "Preference for Coffee D"
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  scale_y_continuous(
    breaks = 1:5
  ) +
  theme_classic()

## Favorite in comparison

stacked_favorite <- 
ggplot(table_comparison_long,
       aes(
         y = proportion,
         x = expertise,
         fill = coffee
       )) +
  geom_col() +
  scale_fill_manual(
    values = c("#AABD8C", "#477998", "#291F1E", "#A3333D")
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, .05)
  ) +
  labs(
    y    = "Proportion Most Preferred Coffee",
    x    = "Self-Described Expertise",
    fill = "Coffee"
  ) +
  theme_classic()

stacked_favorite_gender <- 
ggplot(table_comparison_gender_long %>% filter(gender != "Non-binary"),
       aes(
         y = proportion,
         x = expertise,
         fill = coffee
       )) +
  facet_wrap(~ gender, nrow = 2) +
  geom_col() +
  scale_fill_manual(
    values = c("#AABD8C", "#477998", "#291F1E", "#A3333D")
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, .05)
  ) +
  labs(
    y    = "Proportion Most Preferred Coffee",
    x    = "Self-Described Expertise",
    fill = "Coffee"
  ) +
  theme_classic()

# Regression analysis

lm_gender          <- lm(d_preference 
                         ~ 1
                         + gender,
                         data = data_gender_exp)

lm_gender_exp_base <- lm(d_preference 
                         ~ 1
                         + gender
                         + exp_mc,
                         data = data_gender_exp)

lm_gender_exp_int  <- lm(d_preference 
                         ~ 1
                         + gender
                         * exp_mc,
                         data = data_gender_exp)

lrt_gender_exp <- anova(lm_gender, lm_gender_exp_base, lm_gender_exp_int)