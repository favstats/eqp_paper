<<<<<<< HEAD
## ---- echo=F-------------------------------------------------------------
knitr::opts_chunk$set(message = F, warning = F, fig.align = "center", fig.width = 10, fig.height = 6)

## ------------------------------------------------------------------------
pacman::p_load(tidyverse, haven, psych, sjPlot, ggpubr)

## ------------------------------------------------------------------------
trolley <- read_spss("data/TrolleyExperimentArgumentNew.sav") %>% 
  janitor::clean_names(.) %>% 
  filter(general_finisher == 1) %>% 
  drop_na(t1_eqp_eqp1, t1_eqp_eqp3, t1_eqp_eqp7, t1_eqp_eqp8, t1_eqp_eqp9,
         t1_eqp_eqp11, t1_eqp_eqp12 ,t1_eqp_eqp13, t1_eqp_eqp14, t1_eqp_eqp18,
         t1_eqp_eqp2, t1_eqp_eqp4, t1_eqp_eqp5, t1_eqp_eqp6, t1_eqp_eqp10,
         t1_eqp_eqp15, t1_eqp_eqp16 ,t1_eqp_eqp17, t1_eqp_eqp19, t1_eqp_eqp20,
         t1_szenario1q2, t2_szenario1q2, t1_szenario2q2, t2_szenario2q2) %>% 
  filter(t2_gender != 2) %>% 
  mutate(gender = ifelse(t2_gender == 1, "Men", "Women")) %>% 
  mutate(leftright = t2_pol_alignment) %>% 
  mutate(pol_interest = 5 - t2_pol_interest) %>% 
  mutate(church_attendance = 7 - t2_religion_church) %>% 
  mutate(age = 2018 - as.numeric(t2_year_of_birth)) %>% 
  mutate(university = ifelse(t2_university == 4, 0, 1)) %>% 
  mutate(groups = case_when(
    general_group_control == 1 ~ "Control Group",
    general_group_discussion == 1 ~ "Discussion Group",
    general_group_information == 1 ~ "Information Group",
  )) 

trolley

## ---- analysis-----------------------------------------------------------
trolley %>% 
  group_by(university) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  group_by(gender) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  group_by(groups) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  select(leftright, pol_interest, church_attendance, age) %>% 
  describe() %>% knitr::kable()

## ------------------------------------------------------------------------
eqp <- trolley %>% 
  select(contains("eqp")) %>% 
#  na.omit() %>% 
  select(t1_eqp_eqp1, t1_eqp_eqp3, t1_eqp_eqp7, t1_eqp_eqp8, t1_eqp_eqp9,
         t1_eqp_eqp11, t1_eqp_eqp12 ,t1_eqp_eqp13, t1_eqp_eqp14, t1_eqp_eqp18,
         t1_eqp_eqp2, t1_eqp_eqp4, t1_eqp_eqp5, t1_eqp_eqp6, t1_eqp_eqp10,
         t1_eqp_eqp15, t1_eqp_eqp16 ,t1_eqp_eqp17, t1_eqp_eqp19, t1_eqp_eqp20)

trolley$mean_eqp <- rowMeans(eqp)

trolley <- eqp %>% 
  psych::pca(2, rotate = "varimax") %>% 
  predict.psych(data = eqp) %>% 
  cbind(trolley, .)
  
trolley <- trolley %>% 
  rename(idealism_pca = RC1) %>% 
  rename(relativism_pca = RC2)   

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

age_compare <- trolley %>% 
  ggplot(aes(groups, age)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Age") +
  ggtitle("Age Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

age_compare

tidytemplate::ggsave_it(age_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
gender_compare <- sjp.xtab(trolley$groups, trolley$gender, 
         margin = "row", bar.pos = "stack",
         show.summary = TRUE, coord.flip = TRUE, 
         prnt.plot = F)$plot +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_gdocs("Gender") +
  xlab("") +
  ggtitle("Gender Comparison between Experimental Groups") +
  scale_alpha(range = c(0.4, 0.8))

gender_compare

tidytemplate::ggsave_it(gender_compare, width = 10, height = 6)

## ---- fig.width=14, fig.height=6-----------------------------------------
dem_compare <- cowplot::plot_grid(age_compare, gender_compare)

dem_compare

tidytemplate::ggsave_it(dem_compare, width = 14, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

idealism_pca_compare <- trolley %>% 
  ggplot(aes(groups, idealism_pca)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Idealism") +
  ggtitle("Idealism Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

idealism_pca_compare

tidytemplate::ggsave_it(idealism_pca_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

relativism_pca_compare <- trolley %>% 
  ggplot(aes(groups, relativism_pca)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Relativism") +
  ggtitle("Relativism Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

relativism_pca_compare

tidytemplate::ggsave_it(relativism_pca_compare, width = 10, height = 6)

## ---- fig.width=12, fig.height=6-----------------------------------------
uv_compare <- cowplot::plot_grid(relativism_pca_compare, idealism_pca_compare)

uv_compare

tidytemplate::ggsave_it(uv_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

t1_szenario1q2_compare <- trolley %>% 
  ggplot(aes(groups, t1_szenario1q2)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Morally justifiable: Switch Track") +
  ggtitle("'Switch Track' Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_compare

tidytemplate::ggsave_it(t1_szenario1q2_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

t1_szenario2q2_compare <- trolley %>% 
  ggplot(aes(groups, t1_szenario2q2)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Morally justifiable: Push Person") +
  ggtitle("'Push Person' Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_compare

tidytemplate::ggsave_it(t1_szenario2q2_compare, width = 10, height = 6)

## ---- fig.width=12, fig.height=6-----------------------------------------
av_compare <- cowplot::plot_grid(t1_szenario1q2_compare, t1_szenario2q2_compare)

av_compare

tidytemplate::ggsave_it(av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------

t1_szenario1q2_gender <- trolley %>% 
  ggplot(aes(gender, t1_szenario1q2)) +
  geom_violin(aes(fill = gender), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Gender") + ylab("Morally justifiable: Switch Track") +
  ggtitle("Morally Justifiable to Switch Track by Gender") +
  ggpubr::stat_compare_means(label.x.npc = .4) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_gender

tidytemplate::ggsave_it(t1_szenario1q2_gender, width = 10, height = 6)

t1_szenario2q2_gender <- trolley %>% 
  ggplot(aes(gender, t1_szenario2q2)) +
  geom_violin(aes(fill = gender), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Gender") + ylab("Morally justifiable: Push Person") +
  ggtitle("Morally Justifiable to Push Person by Gender") +
  ggpubr::stat_compare_means(label.x.npc = .5) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_gender

tidytemplate::ggsave_it(t1_szenario2q2_gender, width = 10, height = 6)

gender_av_compare <- cowplot::plot_grid(t1_szenario1q2_gender, t1_szenario2q2_gender)

gender_av_compare

tidytemplate::ggsave_it(gender_av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
t1_szenario1q2_gender <- trolley %>% 
  ggplot(aes(age, t1_szenario1q2)) +
  geom_jitter(aes(color = gender), alpha = 0.6) +
  geom_smooth(aes(color = gender),method = "lm") +
  xlab("Age") + ylab("Morally justifiable: Switch Track") +
  ggtitle("Morally Justifiable to Switch Track by Gender and Age") +
  ggpubr::stat_compare_means(label.x.npc = .4) +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_gender

tidytemplate::ggsave_it(t1_szenario1q2_gender, width = 10, height = 6)

t1_szenario2q2_gender <- trolley %>% 
  ggplot(aes(age, t1_szenario2q2)) +
  geom_jitter(aes(color = gender), alpha = 0.6) +
  geom_smooth(aes(color = gender),method = "lm") +
  xlab("Age") + ylab("Morally justifiable: Push Person") +
  ggtitle("Morally Justifiable to Push Person by Gender and Age") +
  ggpubr::stat_compare_means(label.x.npc = .5) +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_gender

tidytemplate::ggsave_it(t1_szenario2q2_gender, width = 10, height = 6)

gender_av_compare <- cowplot::plot_grid(t1_szenario1q2_gender, t1_szenario2q2_gender)

gender_av_compare

tidytemplate::ggsave_it(gender_av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
# eqp %>% 
#   psych::pca(2, rotate = "varimax") %>% 
#   .$loadings %>% unclass() %>% as.data.frame() %>% 
#   rownames_to_column("eqp_variable")

strip_away_stuff <- function(x) {
x <- str_remove(x, "t1_eqp_")
x <- parse_number(x) %>% 
  ifelse(. %in% 1:9, paste0("eqp0", .), .) %>% 
  ifelse(. %in% 10:20, paste0("eqp", .), .) 
}

eqp <- eqp %>% 
  set_names(eqp %>% names %>% strip_away_stuff) 

factor_names <- c(`1` = "Idealism",
                  `2` = "Relativism")


factor_analysis <- sjp.pca(eqp, rotation = "varimax", 
        nmbr.fctr = 2, prnt.plot = F, show.cronb = T, 
        show.values = T)$plot  +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  ggtitle("Ethical Positions Questionnaire - Factor Analysis") +
  facet_grid(~xpos, labeller = as_labeller(factor_names))

factor_analysis

tidytemplate::ggsave_it(factor_analysis, width = 10, height = 6)

## ------------------------------------------------------------------------
trolley %>% 
  select(t1_szenario1q2, t2_szenario1q2, t1_szenario2q2, t2_szenario2q2, idealism_pca, relativism_pca, gender, age, church_attendance, general_group_control, general_group_discussion, general_group_information) %>% 
  describe() %>% 
  select(-vars, -trimmed, -mad, -se) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
trolley %<>% 
  mutate(groups = factor(groups)) %>% 
  mutate(gender = factor(gender)) 

## ---- include = F--------------------------------------------------------
fit1_s1 <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender, 
           data = trolley)

reg1_s1 <- sjPlot::plot_model(fit1_s1, show.p = T, 
                              show.values = T, value.offset = 0.2) +
  ggtitle("Model 1a - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Relativism", "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight()

reg1_s1

# ggsave(filename = "text/images/reg1_s1.png", width = 8, height = 5)

broom::glance(fit1_s1) %>% knitr::kable()

## ---- include = F--------------------------------------------------------
fit1_s2 <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance, 
           data = trolley)

reg1_s2 <- sjPlot::plot_model(fit1_s2, show.p = T, 
                              show.values = T, value.offset = 0.2) +
  ggtitle("Model 1b - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Relativism", "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight()

reg1_s2

# ggsave(filename = "text/images/reg1_s2.png", width = 8, height = 5)

broom::glance(fit1_s2) %>% knitr::kable()



## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg1_s1, reg1_s2, ncol = 1)


ggsave(filename = "text/images/reg1_combined.png", width = 8, height = 9)

## ---- include = F--------------------------------------------------------
fit2_s1_int_idealism <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit2_s1_int_idealism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  ggtitle("Model 2a - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Relativism", 
                              "Idealism X Gender",
                              "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg2_s1_int_idealism

reg2_s1_int_idealism

# ggsave(filename = "text/images/reg2_s1_int_idealism.png", width = 8, height = 5)

broom::glance(fit2_s1_int_idealism) %>% knitr::kable()

get_model_data(fit2_s1_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2a - Switch Track - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Idealism") 


# ggsave(filename = "text/images/reg2_s1_int2_idealism.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit2_s2_int_idealism <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit2_s2_int_idealism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  ggtitle("Model 2b - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Relativism", 
                              "Idealism X Gender",
                              "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg2_s2_int_idealism

reg2_s2_int_idealism

# ggsave(filename = "text/images/reg2_s2_int_idealism.png", width = 8, height = 5)

broom::glance(fit2_s2_int_idealism) %>% knitr::kable()

get_model_data(fit2_s2_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2b - Push Person - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Push Person") +
  xlab("Idealism") 


# ggsave(filename = "text/images/reg2_s2_int2_idealism.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg2_s1_int_idealism, reg2_s2_int_idealism, ncol = 1)


ggsave(filename = "text/images/reg2_c1_idealism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit2_s1_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
            mutate(type = "Model 2a - Switch Track"),  
  get_model_data(fit2_s2_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
            mutate(type = "Model 2b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2 - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Idealism")  
  
ggsave(filename = "text/images/reg2_c2_idealism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit3_s1_int_relativism <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit3_s1_int_relativism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  scale_x_discrete(labels = c("Relativism X Gender", 
                              "Relativism",
                              "Idealism", 
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 3a - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg3_s1_int_relativism

reg3_s1_int_relativism

# ggsave(filename = "text/images/reg3_s1_int_relativism.png", width = 8, height = 5)

broom::glance(fit3_s1_int_relativism) %>% knitr::kable()

get_model_data(fit3_s1_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3a - Switch Track - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Relativism") 


# ggsave(filename = "text/images/reg3_s1_int2_relativism.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit3_s2_int_relativism <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit3_s2_int_relativism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  scale_x_discrete(labels = c("Relativism X Gender", 
                              "Relativism",
                              "Idealism", 
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 3b - Morally justifiable: Push Person") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg3_s2_int_relativism

reg3_s2_int_relativism

# ggsave(filename = "text/images/reg3_s2_int_relativism.png", width = 8, height = 5)

broom::glance(fit3_s2_int_relativism) %>% knitr::kable()

get_model_data(fit3_s2_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3b - Push Person - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Push Person") +
  xlab("Relativism") 


# ggsave(filename = "text/images/reg3_s2_int2_relativism.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg3_s1_int_relativism, reg3_s2_int_relativism, ncol = 1)


ggsave(filename = "text/images/reg3_c1_relativism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit3_s1_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
            mutate(type = "Model 3a - Switch Track"),  
  get_model_data(fit3_s2_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
            mutate(type = "Model 3b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3 - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Relativism")  
  
ggsave(filename = "text/images/reg3_c2_relativism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit4_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca +
             gender + age + church_attendance +
             groups, 
           data = trolley)

sjPlot::plot_model(fit4_s1, show.p = T, show.values = T, value.offset = 0.4) +
  ggtitle("Model 4a - Opinion Change - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg4_s1

reg4_s1

# ggsave(filename = "text/images/reg4_s1.png", width = 8, height = 5)

broom::glance(fit4_s1) %>% knitr::kable()

## ---- include = F--------------------------------------------------------
fit4_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups, 
           data = trolley)

sjPlot::plot_model(fit4_s2, show.p = T, show.values = T, value.offset = 0.4) +
  ggtitle("Model 4b - Opinion Change - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg4_s2

reg4_s2

# ggsave(filename = "text/images/reg4_s2.png", width = 8, height = 5)

broom::glance(fit4_s2) %>% knitr::kable()

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg4_s1, reg4_s2, ncol = 1)


ggsave(filename = "text/images/reg4_combined.png", width = 8, height = 9)

## ---- include = F--------------------------------------------------------
fit5_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit5_s1, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism X Information Group",
                              "Idealism X Discussion Group",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 5a - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg5_s1

reg5_s1

# ggsave(filename = "text/images/reg5_s1.png", width = 8, height = 5)

broom::glance(fit5_s1) %>% knitr::kable()

get_model_data(fit5_s1, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5a - Switch Track - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Idealism") 

# ggsave(filename = "text/images/reg5_s1_int.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit5_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit5_s2, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism X Information Group",
                              "Idealism X Discussion Group",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 5b - Opinion Change - Morally justifiable: Push Person") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg5_s2

reg5_s2

# ggsave(filename = "text/images/reg5_s2.png", width = 8, height = 5)

broom::glance(fit5_s2) %>% knitr::kable()

get_model_data(fit5_s2, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5b - Push Person - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Push Person") +
  xlab("Idealism") 

# ggsave(filename = "text/images/reg5_s2_int.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg5_s1, reg5_s2, ncol = 1)


ggsave(filename = "text/images/reg5_c1_idealism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit5_s1, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
            mutate(type = "Model 5a - Switch Track"),  
  get_model_data(fit5_s2, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
            mutate(type = "Model 5b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5 - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Idealism")  
  
ggsave(filename = "text/images/reg5_c2_idealism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit6_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit6_s1, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism X Information Group",
                              "Relativism X Discussion Group",
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 6a - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg6_s1

reg6_s1

# ggsave(filename = "text/images/reg6_s1.png", width = 8, height = 5)

broom::glance(fit6_s1) %>% knitr::kable()

get_model_data(fit6_s1, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6a - Switch Track - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Relativism") 

# ggsave(filename = "text/images/reg6_s1_int.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit6_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit6_s2, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism X Information Group",
                              "Relativism X Discussion Group",
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 6b - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg6_s2

reg6_s2

# ggsave(filename = "text/images/reg6_s2.png", width = 8, height = 5)

broom::glance(fit6_s2) %>% knitr::kable()

get_model_data(fit6_s2, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6b - Push Person - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Push Person") +
  xlab("Relativism") 

# ggsave(filename = "text/images/reg6_s2_int.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg6_s1, reg6_s2, ncol = 1)


ggsave(filename = "text/images/reg6_c1_relativism.png", width = 7, height = 8)

## ------------------------------------------------------------------------

bind_rows(
  get_model_data(fit6_s1, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
            mutate(type = "Model 6a - Switch Track"),  
  get_model_data(fit6_s2, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
            mutate(type = "Model 6b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6 - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Relativism")  

ggsave(filename = "text/images/reg6_c2_relativism.png", width = 8, height = 5)


=======
## ---- echo=F-------------------------------------------------------------
knitr::opts_chunk$set(message = F, warning = F, fig.align = "center", fig.width = 10, fig.height = 6)

## ------------------------------------------------------------------------
pacman::p_load(tidyverse, haven, psych, sjPlot, ggpubr)

## ------------------------------------------------------------------------
trolley <- read_spss("data/TrolleyExperimentArgumentNew.sav") %>% 
  janitor::clean_names(.) %>% 
  filter(general_finisher == 1) %>% 
  drop_na(t1_eqp_eqp1, t1_eqp_eqp3, t1_eqp_eqp7, t1_eqp_eqp8, t1_eqp_eqp9,
         t1_eqp_eqp11, t1_eqp_eqp12 ,t1_eqp_eqp13, t1_eqp_eqp14, t1_eqp_eqp18,
         t1_eqp_eqp2, t1_eqp_eqp4, t1_eqp_eqp5, t1_eqp_eqp6, t1_eqp_eqp10,
         t1_eqp_eqp15, t1_eqp_eqp16 ,t1_eqp_eqp17, t1_eqp_eqp19, t1_eqp_eqp20,
         t1_szenario1q2, t2_szenario1q2, t1_szenario2q2, t2_szenario2q2) %>% 
  filter(t2_gender != 2) %>% 
  mutate(gender = ifelse(t2_gender == 1, "Men", "Women")) %>% 
  mutate(leftright = t2_pol_alignment) %>% 
  mutate(pol_interest = 5 - t2_pol_interest) %>% 
  mutate(church_attendance = 7 - t2_religion_church) %>% 
  mutate(age = 2018 - as.numeric(t2_year_of_birth)) %>% 
  mutate(university = ifelse(t2_university == 4, 0, 1)) %>% 
  mutate(groups = case_when(
    general_group_control == 1 ~ "Control Group",
    general_group_discussion == 1 ~ "Discussion Group",
    general_group_information == 1 ~ "Information Group",
  )) 

trolley

## ---- analysis-----------------------------------------------------------
trolley %>% 
  group_by(university) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  group_by(gender) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  group_by(groups) %>% 
  tally() %>% knitr::kable()

trolley %>% 
  select(leftright, pol_interest, church_attendance, age) %>% 
  describe() %>% knitr::kable()

## ------------------------------------------------------------------------
eqp <- trolley %>% 
  select(contains("eqp")) %>% 
#  na.omit() %>% 
  select(t1_eqp_eqp1, t1_eqp_eqp3, t1_eqp_eqp7, t1_eqp_eqp8, t1_eqp_eqp9,
         t1_eqp_eqp11, t1_eqp_eqp12 ,t1_eqp_eqp13, t1_eqp_eqp14, t1_eqp_eqp18,
         t1_eqp_eqp2, t1_eqp_eqp4, t1_eqp_eqp5, t1_eqp_eqp6, t1_eqp_eqp10,
         t1_eqp_eqp15, t1_eqp_eqp16 ,t1_eqp_eqp17, t1_eqp_eqp19, t1_eqp_eqp20)

trolley$mean_eqp <- rowMeans(eqp)

trolley <- eqp %>% 
  psych::pca(2, rotate = "varimax") %>% 
  predict.psych(data = eqp) %>% 
  cbind(trolley, .)
  
trolley <- trolley %>% 
  rename(idealism_pca = RC1) %>% 
  rename(relativism_pca = RC2)   

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

age_compare <- trolley %>% 
  ggplot(aes(groups, age)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Age") +
  ggtitle("Age Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

age_compare

tidytemplate::ggsave_it(age_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
gender_compare <- sjp.xtab(trolley$groups, trolley$gender, 
         margin = "row", bar.pos = "stack",
         show.summary = TRUE, coord.flip = TRUE, 
         prnt.plot = F)$plot +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_gdocs("Gender") +
  xlab("") +
  ggtitle("Gender Comparison between Experimental Groups") +
  scale_alpha(range = c(0.4, 0.8))

gender_compare

tidytemplate::ggsave_it(gender_compare, width = 10, height = 6)

## ---- fig.width=14, fig.height=6-----------------------------------------
dem_compare <- cowplot::plot_grid(age_compare, gender_compare)

dem_compare

tidytemplate::ggsave_it(dem_compare, width = 14, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

idealism_pca_compare <- trolley %>% 
  ggplot(aes(groups, idealism_pca)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Idealism") +
  ggtitle("Idealism Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

idealism_pca_compare

tidytemplate::ggsave_it(idealism_pca_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

relativism_pca_compare <- trolley %>% 
  ggplot(aes(groups, relativism_pca)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Relativism") +
  ggtitle("Relativism Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

relativism_pca_compare

tidytemplate::ggsave_it(relativism_pca_compare, width = 10, height = 6)

## ---- fig.width=12, fig.height=6-----------------------------------------
uv_compare <- cowplot::plot_grid(relativism_pca_compare, idealism_pca_compare)

uv_compare

tidytemplate::ggsave_it(uv_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

t1_szenario1q2_compare <- trolley %>% 
  ggplot(aes(groups, t1_szenario1q2)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Morally justifiable: Switch Track") +
  ggtitle("'Switch Track' Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_compare

tidytemplate::ggsave_it(t1_szenario1q2_compare, width = 10, height = 6)

## ------------------------------------------------------------------------
my_comparisons <- list( c("Control Group", "Discussion Group"), 
                        c("Discussion Group", "Information Group"), 
                        c("Control Group", "Information Group") )

t1_szenario2q2_compare <- trolley %>% 
  ggplot(aes(groups, t1_szenario2q2)) +
  geom_violin(aes(fill = groups), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Experimental Groups") + ylab("Morally justifiable: Push Person") +
  ggtitle("'Push Person' Comparison between Experimental Groups") +
  ggpubr::stat_compare_means(comparisons = my_comparisons) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_compare

tidytemplate::ggsave_it(t1_szenario2q2_compare, width = 10, height = 6)

## ---- fig.width=12, fig.height=6-----------------------------------------
av_compare <- cowplot::plot_grid(t1_szenario1q2_compare, t1_szenario2q2_compare)

av_compare

tidytemplate::ggsave_it(av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------

t1_szenario1q2_gender <- trolley %>% 
  ggplot(aes(gender, t1_szenario1q2)) +
  geom_violin(aes(fill = gender), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Gender") + ylab("Morally justifiable: Switch Track") +
  ggtitle("Morally Justifiable to Switch Track by Gender") +
  ggpubr::stat_compare_means(label.x.npc = .4) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_gender

tidytemplate::ggsave_it(t1_szenario1q2_gender, width = 10, height = 6)

t1_szenario2q2_gender <- trolley %>% 
  ggplot(aes(gender, t1_szenario2q2)) +
  geom_violin(aes(fill = gender), alpha = 0.6) +
  geom_boxplot(width = 0.2) +
  xlab("Gender") + ylab("Morally justifiable: Push Person") +
  ggtitle("Morally Justifiable to Push Person by Gender") +
  ggpubr::stat_compare_means(label.x.npc = .5) +
  ggthemes::scale_fill_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_gender

tidytemplate::ggsave_it(t1_szenario2q2_gender, width = 10, height = 6)

gender_av_compare <- cowplot::plot_grid(t1_szenario1q2_gender, t1_szenario2q2_gender)

gender_av_compare

tidytemplate::ggsave_it(gender_av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
t1_szenario1q2_gender <- trolley %>% 
  ggplot(aes(age, t1_szenario1q2)) +
  geom_jitter(aes(color = gender), alpha = 0.6) +
  geom_smooth(aes(color = gender),method = "lm") +
  xlab("Age") + ylab("Morally justifiable: Switch Track") +
  ggtitle("Morally Justifiable to Switch Track by Gender and Age") +
  ggpubr::stat_compare_means(label.x.npc = .4) +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario1q2_gender

tidytemplate::ggsave_it(t1_szenario1q2_gender, width = 10, height = 6)

t1_szenario2q2_gender <- trolley %>% 
  ggplot(aes(age, t1_szenario2q2)) +
  geom_jitter(aes(color = gender), alpha = 0.6) +
  geom_smooth(aes(color = gender),method = "lm") +
  xlab("Age") + ylab("Morally justifiable: Push Person") +
  ggtitle("Morally Justifiable to Push Person by Gender and Age") +
  ggpubr::stat_compare_means(label.x.npc = .5) +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  guides(fill = F)

t1_szenario2q2_gender

tidytemplate::ggsave_it(t1_szenario2q2_gender, width = 10, height = 6)

gender_av_compare <- cowplot::plot_grid(t1_szenario1q2_gender, t1_szenario2q2_gender)

gender_av_compare

tidytemplate::ggsave_it(gender_av_compare, width = 12, height = 6)

## ------------------------------------------------------------------------
# eqp %>% 
#   psych::pca(2, rotate = "varimax") %>% 
#   .$loadings %>% unclass() %>% as.data.frame() %>% 
#   rownames_to_column("eqp_variable")

strip_away_stuff <- function(x) {
x <- str_remove(x, "t1_eqp_")
x <- parse_number(x) %>% 
  ifelse(. %in% 1:9, paste0("eqp0", .), .) %>% 
  ifelse(. %in% 10:20, paste0("eqp", .), .) 
}

eqp <- eqp %>% 
  set_names(eqp %>% names %>% strip_away_stuff) 

factor_names <- c(`1` = "Idealism",
                  `2` = "Relativism")


factor_analysis <- sjp.pca(eqp, rotation = "varimax", 
        nmbr.fctr = 2, prnt.plot = F, show.cronb = T, 
        show.values = T)$plot  +
  ggthemes::scale_color_gdocs("") +
  ggthemes::theme_hc() +
  ggtitle("Ethical Positions Questionnaire - Factor Analysis") +
  facet_grid(~xpos, labeller = as_labeller(factor_names))

factor_analysis

tidytemplate::ggsave_it(factor_analysis, width = 10, height = 6)

## ------------------------------------------------------------------------
trolley %>% 
  select(t1_szenario1q2, t2_szenario1q2, t1_szenario2q2, t2_szenario2q2, idealism_pca, relativism_pca, gender, age, church_attendance, general_group_control, general_group_discussion, general_group_information) %>% 
  describe() %>% 
  select(-vars, -trimmed, -mad, -se) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
trolley %<>% 
  mutate(groups = factor(groups)) %>% 
  mutate(gender = factor(gender)) 

## ---- include = F--------------------------------------------------------
fit1_s1 <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender, 
           data = trolley)

reg1_s1 <- sjPlot::plot_model(fit1_s1, show.p = T, 
                              show.values = T, value.offset = 0.2) +
  ggtitle("Model 1a - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Relativism", "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight()

reg1_s1

# ggsave(filename = "text/images/reg1_s1.png", width = 8, height = 5)

broom::glance(fit1_s1) %>% knitr::kable()

## ---- include = F--------------------------------------------------------
fit1_s2 <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance, 
           data = trolley)

reg1_s2 <- sjPlot::plot_model(fit1_s2, show.p = T, 
                              show.values = T, value.offset = 0.2) +
  ggtitle("Model 1b - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Relativism", "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight()

reg1_s2

# ggsave(filename = "text/images/reg1_s2.png", width = 8, height = 5)

broom::glance(fit1_s2) %>% knitr::kable()



## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg1_s1, reg1_s2, ncol = 1)


ggsave(filename = "text/images/reg1_combined.png", width = 8, height = 9)

## ---- include = F--------------------------------------------------------
fit2_s1_int_idealism <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit2_s1_int_idealism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  ggtitle("Model 2a - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Relativism", 
                              "Idealism X Gender",
                              "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg2_s1_int_idealism

reg2_s1_int_idealism

# ggsave(filename = "text/images/reg2_s1_int_idealism.png", width = 8, height = 5)

broom::glance(fit2_s1_int_idealism) %>% knitr::kable()

get_model_data(fit2_s1_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2a - Switch Track - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Idealism") 


# ggsave(filename = "text/images/reg2_s1_int2_idealism.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit2_s2_int_idealism <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit2_s2_int_idealism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  ggtitle("Model 2b - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Relativism", 
                              "Idealism X Gender",
                              "Idealism", 
                              "Gender (Men/Women)", "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg2_s2_int_idealism

reg2_s2_int_idealism

# ggsave(filename = "text/images/reg2_s2_int_idealism.png", width = 8, height = 5)

broom::glance(fit2_s2_int_idealism) %>% knitr::kable()

get_model_data(fit2_s2_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2b - Push Person - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Push Person") +
  xlab("Idealism") 


# ggsave(filename = "text/images/reg2_s2_int2_idealism.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg2_s1_int_idealism, reg2_s2_int_idealism, ncol = 1)


ggsave(filename = "text/images/reg2_c1_idealism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit2_s1_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
            mutate(type = "Model 2a - Switch Track"),  
  get_model_data(fit2_s2_int_idealism, type = "pred",
               terms = c("idealism_pca", "gender")) %>% 
            mutate(type = "Model 2b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 2 - Idealism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Idealism")  
  
ggsave(filename = "text/images/reg2_c2_idealism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit3_s1_int_relativism <- lm(t1_szenario1q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit3_s1_int_relativism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  scale_x_discrete(labels = c("Relativism X Gender", 
                              "Relativism",
                              "Idealism", 
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 3a - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg3_s1_int_relativism

reg3_s1_int_relativism

# ggsave(filename = "text/images/reg3_s1_int_relativism.png", width = 8, height = 5)

broom::glance(fit3_s1_int_relativism) %>% knitr::kable()

get_model_data(fit3_s1_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3a - Switch Track - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Relativism") 


# ggsave(filename = "text/images/reg3_s1_int2_relativism.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit3_s2_int_relativism <- lm(t1_szenario2q2 ~ idealism_pca + relativism_pca + 
             gender + age + church_attendance + gender +
               gender*relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit3_s2_int_relativism, show.p = T, show.values = T,
                   value.offset = 0.2) +
  scale_x_discrete(labels = c("Relativism X Gender", 
                              "Relativism",
                              "Idealism", 
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 3b - Morally justifiable: Push Person") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg3_s2_int_relativism

reg3_s2_int_relativism

# ggsave(filename = "text/images/reg3_s2_int_relativism.png", width = 8, height = 5)

broom::glance(fit3_s2_int_relativism) %>% knitr::kable()

get_model_data(fit3_s2_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3b - Push Person - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  ylab("Morally justifiable: Push Person") +
  xlab("Relativism") 


# ggsave(filename = "text/images/reg3_s2_int2_relativism.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg3_s1_int_relativism, reg3_s2_int_relativism, ncol = 1)


ggsave(filename = "text/images/reg3_c1_relativism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit3_s1_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
            mutate(type = "Model 3a - Switch Track"),  
  get_model_data(fit3_s2_int_relativism, type = "pred",
               terms = c("relativism_pca", "gender")) %>% 
            mutate(type = "Model 3b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 3 - Relativism X Gender") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Gender") +
  ggthemes::scale_color_fivethirtyeight("Gender") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Relativism")  
  
ggsave(filename = "text/images/reg3_c2_relativism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit4_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca +
             gender + age + church_attendance +
             groups, 
           data = trolley)

sjPlot::plot_model(fit4_s1, show.p = T, show.values = T, value.offset = 0.4) +
  ggtitle("Model 4a - Opinion Change - Morally justifiable: Switch Track") +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg4_s1

reg4_s1

# ggsave(filename = "text/images/reg4_s1.png", width = 8, height = 5)

broom::glance(fit4_s1) %>% knitr::kable()

## ---- include = F--------------------------------------------------------
fit4_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups, 
           data = trolley)

sjPlot::plot_model(fit4_s2, show.p = T, show.values = T, value.offset = 0.4) +
  ggtitle("Model 4b - Opinion Change - Morally justifiable: Push Person") +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg4_s2

reg4_s2

# ggsave(filename = "text/images/reg4_s2.png", width = 8, height = 5)

broom::glance(fit4_s2) %>% knitr::kable()

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg4_s1, reg4_s2, ncol = 1)


ggsave(filename = "text/images/reg4_combined.png", width = 8, height = 9)

## ---- include = F--------------------------------------------------------
fit5_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit5_s1, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism X Information Group",
                              "Idealism X Discussion Group",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 5a - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg5_s1

reg5_s1

# ggsave(filename = "text/images/reg5_s1.png", width = 8, height = 5)

broom::glance(fit5_s1) %>% knitr::kable()

get_model_data(fit5_s1, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5a - Switch Track - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Idealism") 

# ggsave(filename = "text/images/reg5_s1_int.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit5_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * idealism_pca, 
           data = trolley)

sjPlot::plot_model(fit5_s2, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism",
                              "Idealism X Information Group",
                              "Idealism X Discussion Group",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 5b - Opinion Change - Morally justifiable: Push Person") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg5_s2

reg5_s2

# ggsave(filename = "text/images/reg5_s2.png", width = 8, height = 5)

broom::glance(fit5_s2) %>% knitr::kable()

get_model_data(fit5_s2, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5b - Push Person - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Push Person") +
  xlab("Idealism") 

# ggsave(filename = "text/images/reg5_s2_int.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg5_s1, reg5_s2, ncol = 1)


ggsave(filename = "text/images/reg5_c1_idealism.png", width = 8, height = 9)

## ------------------------------------------------------------------------
bind_rows(
  get_model_data(fit5_s1, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
            mutate(type = "Model 5a - Switch Track"),  
  get_model_data(fit5_s2, type = "pred",
               terms = c("idealism_pca", "groups")) %>% 
            mutate(type = "Model 5b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 5 - Idealism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Idealism")  
  
ggsave(filename = "text/images/reg5_c2_idealism.png", width = 8, height = 5)


## ---- include = F--------------------------------------------------------
fit6_s1 <- lm(t2_szenario1q2 ~ t1_szenario1q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit6_s1, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism X Information Group",
                              "Relativism X Discussion Group",
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 6a - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg6_s1

reg6_s1

# ggsave(filename = "text/images/reg6_s1.png", width = 8, height = 5)

broom::glance(fit6_s1) %>% knitr::kable()

get_model_data(fit6_s1, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6a - Switch Track - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Switch Track") +
  xlab("Relativism") 

# ggsave(filename = "text/images/reg6_s1_int.png", width = 8, height = 5)

## ---- include = F--------------------------------------------------------
fit6_s2 <- lm(t2_szenario2q2 ~ t1_szenario2q2 + idealism_pca + relativism_pca + 
             gender + age + church_attendance +
             groups * relativism_pca, 
           data = trolley)

sjPlot::plot_model(fit6_s2, show.p = T, 
                              show.values = T, value.offset = 0.4) +
  scale_x_discrete(labels = c("Mor. Justifiable T1", 
                              "Relativism X Information Group",
                              "Relativism X Discussion Group",
                              "Relativism",
                              "Idealism", 
                              "Information Group",
                              "Discussion Group",
                              "Gender (Men/Women)", 
                              "Church Attendance",
                              "Age")) +
  ggtitle("Model 6b - Opinion Change - Morally justifiable: Switch Track") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight() -> reg6_s2

reg6_s2

# ggsave(filename = "text/images/reg6_s2.png", width = 8, height = 5)

broom::glance(fit6_s2) %>% knitr::kable()

get_model_data(fit6_s2, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6b - Push Person - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  ylab("Morally justifiable: Push Person") +
  xlab("Relativism") 

# ggsave(filename = "text/images/reg6_s2_int.png", width = 8, height = 5)

## ---- fig.width = 7, fig.height = 8--------------------------------------
cowplot::plot_grid(reg6_s1, reg6_s2, ncol = 1)


ggsave(filename = "text/images/reg6_c1_relativism.png", width = 7, height = 8)

## ------------------------------------------------------------------------

bind_rows(
  get_model_data(fit6_s1, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
            mutate(type = "Model 6a - Switch Track"),  
  get_model_data(fit6_s2, type = "pred",
               terms = c("relativism_pca", "groups")) %>% 
            mutate(type = "Model 6b - Push Person")
  ) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), alpha = 0.11) +
  geom_line(aes(color = group), size = 1.2) +
  ggtitle("Model 6 - Relativism X Experimental Groups") +
  ggthemes::theme_hc() +
  ggthemes::scale_fill_fivethirtyeight("Experimental Groups") +
  ggthemes::scale_color_fivethirtyeight("Experimental Groups") +
  facet_wrap(~type) +
  ylab("Morally justifiable 1 - 10") +
  xlab("Relativism")  

ggsave(filename = "text/images/reg6_c2_relativism.png", width = 8, height = 5)


>>>>>>> be74a443fc2c19d263592bbe77254754f8227b89
