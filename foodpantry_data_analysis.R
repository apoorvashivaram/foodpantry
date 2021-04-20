# food pantry analysis - data analysis - December 2020 & April 2021 -------

# loading packages --------------------------------------------------------
library(tidyverse)
library(lme4)
library(janitor)
library(huxtable)
library(gtsummary)
library(ggsignif)
library(scales)
library(lmerTest)

# reading in cleaned data -------------------------------------------------

foodpantry <- read_csv("data/processed/foodpantry_cleaned.csv")


# prepare data for analysis -----------------------------------------------

# convert necessary columns into factors
foodpantry <- foodpantry %>% 
  mutate_at(c("signs_condition", "signs_up", "type_signs_acad_rel", "signs", 
              "aisle", "conv_initiate", "turns", "valence",
              "adult1gender", "adult2gender", "adult3gender", "adult4gender",
              "child1gender", "child2gender", "child3gender", "child4gender",
              "re", "language_spoken"), factor)

# ordering the levels of factors 
foodpantry <- foodpantry %>% 
  mutate(
    # valence
    valence = factor(valence, levels = c("very negative", "negative", "neutral", "positive", "very positive", "other"), ordered = TRUE),
    valence = fct_lump(valence, 5),
    # turns
    turns = factor(turns, levels = c("0-5", "6-15", "16+")),
    # signs
    signs = factor(signs, levels = c("number", "colors & shapes", "one word answers", "pronouncements")),
    # r&e
    re = factor(re, levels = c("white", "asian", "black", "latinx", "multiracial")),
    # aisles
    aisle = factor(aisle, levels = c("dry goods", "freezer", "bread", "produce"))
  ) 

# demographics ------------------------------------------------------------

# **** demographics - sample sizes ---------------------------------------------
# count total number of families -- n = 212
foodpantry %>% 
  distinct(subject_number) %>% 
  count()

# counting sample size of no signs -- n = 80
# counting sample size of signs up -- n = 132
foodpantry %>% 
  group_by(signs_condition) %>% 
  distinct(subject_number) %>% 
  count()

# counting sample size on each day of observation
# no signs day 1 -- n = 45
# no signs day 2 -- n = 35
# signs up day 1 -- n = 44
# signs up day 2 -- n = 46
# signs up day 3 -- n = 42
foodpantry %>% 
  group_by(signs_up) %>% 
  distinct(subject_number) %>% 
  count()

# counting sample size of signs up
# greater than 105 because families were observed more than 1 time
# number = 77
# colors & shapes = 84
# one word answers = 86
# pronouncements = 91
# NA (no signs) = 80
foodpantry %>% 
  group_by(signs) %>% 
  distinct(subject_number) %>% 
  count()

# **** demographics - group structure ---------------------------------------------

# group structure -- overall
foodpantry %>% 
  select(subject_number,
         number_adults, number_children) %>% 
  group_by(subject_number) %>% 
  count(number_adults, number_children) %>%
  # create a rule to count values that have been agreed upon by independent coders
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(number_adults, number_children)


# group structure -- no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>%
  select(subject_number,
         number_adults, number_children) %>% 
  group_by(subject_number) %>% 
  count(number_adults, number_children) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(number_adults, number_children)

# group structure -- signs up -- prompts
foodpantry %>% 
  filter(signs_condition == "up") %>%
  # un-comment whichever sign if relevant
  # filter(signs == "number") %>%
  # filter(signs == "colors & shapes") %>%
  # filter(signs == "one word answers") %>%
  # filter(signs == "pronouncements") %>%
  select(subject_number,
         number_adults, number_children) %>% 
  group_by(subject_number) %>% 
  count(number_adults, number_children) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(number_adults, number_children)


# **** demographics - adult gender ---------------------------------------------

# counting adult1gender - overall
foodpantry %>% 
  group_by(subject_number) %>%
  count(adult1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(adult1gender)

# adult1gender for no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>% 
  group_by(subject_number) %>% 
  count(adult1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(adult1gender)

# adult1gender for signs up - prompts
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  # un-comment whichever sign is relevant
  # filter(signs == "number") %>%
  # filter(signs == "colors & shapes") %>%
  # filter(signs == "one word answers") %>%
  # filter(signs == "pronouncements") %>%
  group_by(subject_number) %>% 
  count(adult1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(adult1gender)

# **** demographics - child gender ---------------------------------------------

# counting child1gender - 91 male, 116 female, 5 NA -- overall
foodpantry %>% 
  group_by(subject_number) %>% 
  count(child1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(child1gender)

# child1gender for no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>% 
  group_by(subject_number) %>% 
  count(child1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(child1gender)

# child1gender for signs up - prompts
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  # un-comment whichever sign is relevant
  # filter(signs == "number") %>%
  # filter(signs == "colors & shapes") %>%
  # filter(signs == "one word answers") %>%
  # filter(signs == "pronouncements") %>%
  group_by(subject_number) %>% 
  count(child1gender) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(child1gender)


# **** demographics - child age ------------------------------------------------

# counting target_child_age -- overall
foodpantry %>% 
  group_by(subject_number) %>% 
  count(target_child_age) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(target_child_age)

# counting target_child_age -- no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>% 
  group_by(subject_number) %>% 
  count(target_child_age) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(target_child_age)

# counting target_child_age -- prompts
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  # un-comment whichever sign is relevant
  # filter(signs == "number") %>%
  # filter(signs == "colors & shapes") %>%
  # filter(signs == "one word answers") %>%
  # filter(signs == "pronouncements") %>%
  group_by(subject_number) %>% 
  count(target_child_age) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(target_child_age)


# **** demographics - race & ethnicity -----------------------------------------

# counting re -- overall
foodpantry %>% 
  group_by(subject_number) %>% 
  count(re) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(re)

# counting re -- no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>% 
  group_by(subject_number) %>% 
  count(re) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(re)

# counting re -- prompts
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  # un-comment whichever sign is relevant
  # filter(signs == "number") %>%
  # filter(signs == "colors & shapes") %>%
  # filter(signs == "one word answers") %>%
  # filter(signs == "pronouncements") %>%
  group_by(subject_number) %>% 
  count(re) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(re)


# **** demographics - language used ---------------------------------------
# counting language -- overall
foodpantry %>% 
  group_by(subject_number) %>% 
  count(language_spoken) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(language_spoken)

# counting language -- no signs
foodpantry %>% 
  filter(signs_condition == "no signs") %>% 
  group_by(subject_number) %>% 
  count(language_spoken) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(language_spoken)

# counting language -- signs up
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  group_by(subject_number) %>% 
  count(language_spoken) %>%
  mutate(max = max(n) == n) %>% 
  filter(max == TRUE) %>% 
  distinct(subject_number, .keep_all = TRUE) %>%
  ungroup() %>% 
  count(language_spoken)


# data visualization ------------------------------------------------------


# necessary functions for raincloud plots ---------------------------------

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <- ggproto("GeomFlatViolin", Geom,
                          setup_data = function(data, params) {
                            data$width <- data$width %||%
                              params$width %||% (resolution(data$x, FALSE) * 0.9)
                            
                            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                            data %>%
                              group_by(group) %>%
                              mutate(
                                ymin = min(y),
                                ymax = max(y),
                                xmin = x,
                                xmax = x + width / 2
                              )
                          },
                          
                          draw_group = function(data, panel_scales, coord) {
                            # Find the points for the line to go all the way around
                            data <- transform(data,
                                              xminv = x,
                                              xmaxv = x + violinwidth * (xmax - x)
                            )
                            
                            # Make sure it's sorted properly to draw the outline
                            newdata <- rbind(
                              plyr::arrange(transform(data, x = xminv), y),
                              plyr::arrange(transform(data, x = xmaxv), -y)
                            )
                            
                            # Close the polygon: set first and last point the same
                            # Needed for coord_polar and such
                            newdata <- rbind(newdata, newdata[1, ])
                            
                            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
                          },
                          
                          draw_key = draw_key_polygon,
                          
                          default_aes = aes(
                            weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"
                          ),
                          
                          required_aes = c("x", "y")
)


# **** hypothesis 1 -- stacked bar chart -- no signs vs. up ----------------------
foodpantry %>% 
  filter(!is.na(turns)) %>% 
  ggplot() +
  geom_bar(aes(signs_condition, fill = turns), color = "black", position = "fill", width = 0.6) +
  scale_x_discrete(labels = c("no-signs", "signs-up")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey(start = 0.95, end = 0.45) +
  labs(x = "Signs Condition",
       y = "Percentage of Conversational Turns",
       fill = "Conversational\nTurns") +
  theme_bw(18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
  )

## save bar chart
# ggsave("plots/figure_2.jpg", last_plot(), width = 7, height = 5, units = "in", dpi = 300)


# **** hypothesis 2 -- academically relevant vs. non-academically relevant --------------------

# raincloud plots 
foodpantry %>% 
  filter(!is.na(type_signs_acad_rel)) %>% 
  ggplot(
    aes(x = type_signs_acad_rel, y = num_col_shape_talk)
    ) +
  geom_flat_violin(
    fill = "grey80",
    position = position_nudge(x = .1, y = 0), 
    adjust = 1.5, 
    trim = FALSE, 
    alpha = .5
    ) +
  geom_point(
    position = position_jitter(width = 0.05, height = 0.3, seed = 1234),
    size = 1, 
    shape = 20
    ) +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "red",
    position = position_nudge(x = .13, y = 0)
    ) +
  stat_summary(
    fun.data = ~mean_se(., mult = 1.96),
    geom = "errorbar",
    width = 0.05,   
    color = "red",
    position = position_nudge(x = .13, y = 0)
    ) +
  geom_segment(aes(x = 1.13, xend = 2.13, y = 2, yend = 2)) +
  annotate(
    "text",
    x = 1.63, 
    y = 2.2,
    label = "***",
    size = 7
    ) +
  scale_y_continuous(labels = 0:6, breaks = seq(0, 6, 1), limits = c(0,6)) +
  labs(
    x = "Category of Signs",
    y = "Number, Color and Shape Talk"
    ) +
  theme_bw(20) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black")
    )

# # save raincloud plot
# ggsave("plots/figure_3.jpg", last_plot(), width = 8, height = 6, units = "in", dpi = 300)


# **** hypothesis 3 -- number, color and shape, one-word answers, pronouncements --------------------

# raincloud plot
foodpantry %>% 
  filter(!is.na(signs)) %>% 
  ggplot(
    aes(x = signs, y = num_col_shape_talk)
    ) +
  geom_flat_violin(
    fill = "grey20",
    position = position_nudge(x = .1, y = 0), 
    adjust = 1.5, 
    trim = FALSE, 
    alpha = .5
    ) +
  geom_point(
    position = position_jitter(width = .05, height = 0.3, seed = 123),
    size = 1, 
    shape = 20
    ) +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "red",
    position = position_nudge(x = .2, y = 0)
    ) +
  stat_summary(
    fun.data = ~mean_se(., mult = 1.96),
    geom = "errorbar",
    width = 0.05,
    color = "red",
    position = position_nudge(x = .2, y = 0)
    ) +
  geom_segment(
    aes(x = 3.2, xend = 4.2, y = 2, yend = 2)
    ) +
  annotate(
    "text",
    x = 3.63, 
    y = 2.2,
    label = "***",
    size = 7
    ) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(
    labels = c("Number", "Color-and-shape", 
               "One-word answers", "Pronouncements")
    ) +
  scale_y_continuous(labels = 0:6, breaks = seq(0, 6, 1), limits = c(0, 6)) +
  labs(
    x = "Type of Signs",
    y = "Number, Color and Shape Talk"
    ) +
  theme_bw(20) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black")
    )

# # save the plot
# ggsave("plots/figure_4.jpg", last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# data analysis -----------------------------------------------------------

# **** hypothesis 1 -- chi-sq test -- no signs vs. signs up ------------------
# quantity of conversation analysis with conversational turns
# signs (no signs vs up) and conversational turns
foodpantry %>%
  filter(!is.na(turns)) %>%
  select(signs_condition, turns) %>%
  table() %>%
  stats::chisq.test()

# for row proportions (margin = 1)
foodpantry %>%
  filter(!is.na(turns)) %>%
  select(signs_condition, turns) %>%
  table() %>%
  # convert to percentages and round to 0 decimal places
  prop.table(margin = 1) %>%
  `*`(100) %>%
  round(0) 

# chi sq freq table (formatted) for turns by signs condition (no signs vs up)
foodpantry %>% 
  filter(!is.na(turns)) %>%
  select(signs_condition, turns) %>%
  tbl_summary(by = signs_condition,
              label = list(turns ~ "Conversational Turns")) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  modify_header(label ~ "") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Signs Condition**") %>%
  bold_labels()

# **** hypothesis 2 -- poisson regression -- academically relevant vs. non-academically relevant ----
# mean and SD of num_col_shape_talk
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  group_by(type_signs_acad_rel) %>% 
  summarize(mean = mean(num_col_shape_talk, na.rm = TRUE),
            sd = sd(num_col_shape_talk, na.rm = TRUE),
            min = min(num_col_shape_talk),
            max = max(num_col_shape_talk))

# poisson regression of num_col_shape_talk on academically relevant vs non-academically relevant signs
# Fixed effects are variables that are constant across individuals; 
# these variables, like age, sex, or ethnicity, don’t change over observation period. They have fixed effects; 
# in other words, any change they cause to an individual is the same.
# For example, any effects from being a 8-year-old female won't change over time during observation period

# ********* mean and SD ----
# of num_col_shape_talk
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  rename(type_of_signs_up = type_signs_acad_rel) %>% 
  group_by(type_of_signs_up) %>%
  summarize(mean = mean(num_col_shape_talk, na.rm = TRUE),
            sd = sd(num_col_shape_talk, na.rm = TRUE),
            min = min(num_col_shape_talk),
            max = max(num_col_shape_talk)) 

# ********* mixed effects poisson regression ----
# poisson regression of num_col_shape_talk on academically relevant vs non-academically relevant signs
poisson_fit_acad_rel <- foodpantry %>% 
  filter(signs_condition == "up") %>% 
  mutate_at("subject_number", factor) %>% 
  mutate(type_signs_acad_rel = fct_relevel(type_signs_acad_rel, "non-academically relevant")) %>%
  glmer(num_col_shape_talk ~ type_signs_acad_rel +
          child1gender + target_child_age +
          (1|subject_number), 
        family = poisson(link = "log"), 
        data = .)

# create regression table 
huxreg("Number, Color & Shape Talk" = poisson_fit_acad_rel,
       error_pos = "same",
       coefs = c("Intercept" = "(Intercept)",
                 "academically relevant" = "type_signs_acad_relacademically relevant",
                 "Child's Gender - Male" = "child1genderm",
                 "Target Child's Age" = "target_child_age")
) %>% 
  set_italic(final(1), 1) %>% 
  set_caption("Poisson Regression Results Predicting Number, Color & Shape Talk Within the Signs-up Condition (academically relevant vs. non-academically relevant)")

# ********* break down % by condition ----
# percentage of sample that got a specific score on dependent variable (num_col_shape_talk)
# for each level of the independent variable (academically-relevant or non-academically-relevant signs)
foodpantry %>% 
  filter(!is.na(type_signs_acad_rel)) %>% 
  filter(!is.na(num_col_shape_talk)) %>% 
  select(type_signs_acad_rel, num_col_shape_talk) %>%
  table() %>%
  prop.table(margin = 1) %>%
  `*`(100) %>%
  round(0) 

# **** hypothesis 3 -- poisson regressions -- comparing each type of sign ----
# ********* mean and SD ----

# number vs. color-and-shape / one-word vs. pronouncements --
# mean and SD for num_col_shape_talk
foodpantry %>% 
  filter(signs_condition == "up") %>% 
  group_by(signs) %>% 
  summarize(mean = round(mean(num_col_shape_talk, na.rm = TRUE), 2),
            sd = sd(num_col_shape_talk, na.rm = TRUE),
            min = min(num_col_shape_talk),
            max = max(num_col_shape_talk))

# ********* mixed effects poisson regression ----

# ************* number as reference ----
# number vs. remaining signs (number as reference variable)
poisson_fit_number_ref <- foodpantry %>%
  filter(signs_condition == "up") %>% 
  mutate_at("subject_number", factor) %>% 
  mutate(type_signs_acad_rel = fct_relevel(type_signs_acad_rel, "non-academically relevant")) %>%
  mutate(signs = fct_relevel(signs, "colors & shapes")) %>%
  glmer(num_col_shape_talk ~ signs %in% type_signs_acad_rel +
          child1gender + target_child_age +
          (1|subject_number), 
        family = poisson(link = "log"), 
        data = .) 

# create regression table
huxreg("Number, Color & Shape Talk" = poisson_fit_number_ref,
       error_pos = "same",
       coefs = c("Intercept" = "(Intercept)",
                 "Colors & Shapes (academically relevant)" = "signscolors & shapes:type_signs_acad_relacademically relevant",
                 "One-word answers (non-academically relevant)" = "signsone word answers:type_signs_acad_relnon-academically relevant",
                 "Pronouncements (non-academically relevant)" = "signspronouncements:type_signs_acad_relnon-academically relevant",
                 "Child's Gender - Male" = "child1genderm",
                 "Target Child's Age" = "target_child_age"
       )
) %>% 
  set_italic(final(1), 1) %>% 
  set_caption("Poisson Regression Results Predicting Number, Color & Shape Talk Across Signs with Number Signs as the Reference Variable")

# ************* color & shape as reference ----
# color-and-shape vs. remaining signs (colors & shapes as reference variable)
poisson_fit_cs_ref <- foodpantry %>% 
  filter(signs_condition == "up") %>% 
  mutate_at("subject_number", factor) %>% 
  mutate(type_signs_acad_rel = fct_relevel(type_signs_acad_rel, "non-academically relevant"),
         signs = fct_relevel(signs, "number")) %>%
  glmer(num_col_shape_talk ~ signs %in% type_signs_acad_rel +
          child1gender + target_child_age +
          (1|subject_number), 
        family = poisson(link = "log"), 
        data = .) 

# create regression table
huxreg("Number, Color & Shape Talk" = poisson_fit_cs_ref,
       error_pos = "same",
       coefs = c("Intercept" = "(Intercept)",
                 "Number (academically relevant)" = "signsnumber:type_signs_acad_relacademically relevant",
                 "One-word answers (non-academically relevant)" = "signsone word answers:type_signs_acad_relnon-academically relevant",
                 "Pronouncements (non-academically relevant)" = "signspronouncements:type_signs_acad_relnon-academically relevant",
                 "Child's Gender - Male" = "child1genderm",
                 "Target Child's Age" = "target_child_age"
       )
) %>% 
  set_italic(final(1), 1) %>% 
  set_caption("Poisson Regression Results Predicting Number, Color & Shape Talk Across Signs with Color & Shape signs as the reference variable")

# ************* one word answers as reference ----
# one-word answers vs. remaining signs (one word answers as reference variable)
poisson_fit_ow_ref <- foodpantry %>% 
  filter(signs_condition == "up") %>% 
  mutate_at("subject_number", factor) %>% 
  mutate(type_signs_acad_rel = fct_relevel(type_signs_acad_rel, "academically relevant"),
         signs = fct_relevel(signs, "pronouncements")) %>% 
  glmer(num_col_shape_talk ~ signs %in% type_signs_acad_rel +
          child1gender + target_child_age +
          (1|subject_number), 
        family = poisson(link = "log"), 
        data = .)

# create regression table
huxreg("Number, Color & Shape Talk" = poisson_fit_ow_ref,
       error_pos = "same",
       coefs = c("Intercept" = "(Intercept)",
                 "Number (academically relevant)" = "signsnumber:type_signs_acad_relacademically relevant",
                 "Colors & Shapes (academically relevant)" = "signscolors & shapes:type_signs_acad_relacademically relevant",
                 "Pronouncements (non-academically relevant)" = "signspronouncements:type_signs_acad_relnon-academically relevant",
                 "Child's Gender - Male" = "child1genderm",
                 "Target Child's Age" = "target_child_age"
       )
) %>% 
  set_italic(final(1), 1) %>% 
  set_caption("Poisson Regression Results Predicting Number, Color & Shape Talk Across Signs with One word answers signs as the reference variable")
