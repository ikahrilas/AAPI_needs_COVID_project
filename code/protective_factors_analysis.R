# protective factors analysis
library(tidyverse)
library(psych) # for point biserial correlations
library(ellipse)
library(RColorBrewer)
library(GGally)

# load data
load("KeyDataset.4.21.21.RData")

dat <- key_dataset %>% filter(Race_Asian == "Yes")

# select variables of interest
vars <- c("Depression_or_Anxiety",
          "PHQ1",
          "PHQ2",
          "PHQ3",
          "PHQ4",
          "Coping_FriendsFamily",
          "Coping_Healthcare",
          "Coping_Meditation",
          "Coping_Religious",
          "Coping_ScreenTime",
          "Coping_Hobbies",
          "Coping_CivicEngagement",
          "Coping_Eating",
          "Coping_Sleeping",
          "Coping_Alcohol",
          "Coping_Tobacco",
          "Coping_MarijuanaCBD",
          "SocialSupport_During",
          "SocialSupport1_R",
          "SocialSupport2_R",
          "SocialSupport3_R",
          "SocialSupport4_R",
          "Exercising_Since",
          "Covid_Impact1",
          "Covid_Impact2",
          "Covid_Impact3",
          "Covid_Impact4",
          "Covid_Impact5")

dat <- 
  dat %>% 
  select(all_of(vars)) %>% 
  mutate(across(.cols = c(Coping_FriendsFamily:Coping_MarijuanaCBD), 
                .fns = ~ if_else(.x == "Yes", 1, 0)
                ),
         across(.cols = c(Covid_Impact1, Covid_Impact2, Covid_Impact3, Covid_Impact4, Covid_Impact5),
                .fns = ~ 4 - .x, 
                .names = "{.col}"),
         phq_total = PHQ1 + PHQ2 + PHQ3 + PHQ4,
         mh_protection = 1 - Depression_or_Anxiety)

# derive tetrachoric correlations for dichotomous variables
## define vector of dichotomous variables for correlations
di_vars <- c("Coping_FriendsFamily",
             "Coping_Healthcare",
             "Coping_Meditation",
             "Coping_Religious",
             "Coping_ScreenTime",
             "Coping_Hobbies",
             "Coping_CivicEngagement",
             "Coping_Eating",
             "Coping_Sleeping",
             "Coping_Alcohol",
             "Coping_Tobacco",
             "Coping_MarijuanaCBD")

## function to derive the tetrachoric coefficients and enter into tibble
tet_coef_dat <- 
  map_df(di_vars, ~ {
    coef <- 
      tetrachoric(
      as.matrix(
        dat[ ,c("mh_protection", .x)]))$rho[1, 2] # derive and extract tetrachoric correlation
# merge into tibble
  tibble(var = .x,
       coef = coef)
})


# derive point biserial correlations for dichotomous/continuous pairs
## define vector of continuous variables
cont_vars <- c("SocialSupport_During",
               "SocialSupport1_R",
               "SocialSupport2_R",
               "SocialSupport3_R",
               "SocialSupport4_R",
               "Exercising_Since",
               "Covid_Impact1",
               "Covid_Impact2",
               "Covid_Impact3",
               "Covid_Impact4",
               "Covid_Impact5")

## function to derive the point biserial coefficients and enter into tibble
bi_coef_dat <- 
  map_df(cont_vars, ~ {
    coef <- 
      biserial(dat[.x], dat["mh_protection"]) %>% as.numeric() # derive point biserial correlations
    # merge into tibble
    tibble(var = .x,
           coef = coef)
  })

## merge polychoric and point biserial coefficient data into single tibble
coef_dat <- 
  bind_rows(tet_coef_dat, bi_coef_dat) %>% 
  arrange(desc(coef)) %>% 
  mutate(var = factor(var, levels = var))

highlight_dat <- 
  coef_dat %>% 
    filter(coef > .15,
           var != "SocialSupport_During")
  
highlight_risk_dat <- 
  coef_dat %>% 
  arrange(desc(coef)) %>% 
  filter(var %in% c("Coping_MarijuanaCBD",
                    "Coping_Sleeping",
                    "Coping_Tobacco",
                    "Coping_Eating",
                    "Coping_Alcohol")) %>% 
  mutate(var = factor(var, levels = var))

highlight_dat$var <- droplevels(highlight_dat$var)
highlight_risk_dat$var <- droplevels(highlight_risk_dat$var)

levels(highlight_dat$var) <- c("Exercising",
                               "Connecting with Family/Friends",
                               "Appreciating Life",
                               "Acceptance",
                               "Emotional Support from Family/Friends",
                               "Gratitude for Each Day")

levels(highlight_risk_dat$var) <- c("Coping with Alcohol",
                                    "Coping with Eating",
                                    "Coping with Tobacco",
                                    "Coping with Sleeping",
                                    "Coping with Marijuana/CBD"
                                    )

# for annotations
library(grid)
text_high <- textGrob("Protective", gp=gpar(fontsize=13, col = "green"))
text_low <- textGrob("Not Protective", gp=gpar(fontsize=13, col = "red"))

# plotting
coef_dat %>% 
ggplot(aes(var, coef)) +
  geom_segment(aes(x = var, xend = var, y=0, yend = coef) , size=0.5, color = "skyblue", alpha = 0.5) +
  geom_point(size = 2, color = "blue") + 
  #geom_segment(data = highlight_dat, aes(x = var, xend = var, y=0, yend = coef) , size=1, color = "orange", alpha = 0.8) +
  #geom_point(data = highlight_dat, aes(x = var, y = coef), size = 3, color = "forestgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_discrete(limits = rev(levels(var))) +
  ylab("Protection Against Depression and Anxiety") +
  xlab("Factor") + 
  geom_rect(xmin = 0, xmax = Inf,
            ymin = 0, ymax = Inf,
            fill = "green",
            alpha = 0.003) +
  geom_rect(xmin = 0, xmax = Inf,
            ymin = -Inf, ymax = 0,
            fill = "red",
            alpha = 0.003) +
  annotate(geom = "text", 
           x = 14.75, 
           y = 0.17,
           label = "Protective Factors",
           color = "forestgreen") +
  annotate(geom = "text",
           x = 10.7,
           y = -0.2,
           label = "Non Protective Factors",
           color = "red") +
  annotate(geom = "segment",
           x = c(12.75, 13.2),
           xend = c(12.75, 13.2),
           y = c(0.02, -0.05),
           yend = c(0.3, -0.38)) +
  annotate(geom = "segment",
           x = c(12.5, 12.5, 13.2, 13.2),
           xend = c(12.75, 12.75, 13.45, 13.45),
           y = c(0.02, 0.3, -0.05, -0.38),
           yend = c(0.02, 0.3, -0.05, -0.38)) +
  annotate(geom = "segment",
           x = 0.9,
           xend = 6.1,
           y = .38,
           yend = .38) +
  annotate(geom = "segment",
           x = c(0.9, 6.1),
           xend = c(0.9, 6.1),
           y = c(.38, .38),
           yend = c(.36, .36)) +
  annotate(geom = "text",
           x = 3.5,
           y = .425,
           label = "Top Protective Factors",
           fontface = "bold")

ggsave("all_factors_plot.png", device = "png",  dpi = "retina", height = 5, width = 8)

# smaller ones that becky wants
## from Anne:
# brown #765F56	
# beige #EBDDC4
# blue #95B6D1
# orange #DB804D	
# green #989C78	
# mustard #D6B062
## protective
ggplot(highlight_dat, aes(var, coef)) +
  geom_segment(aes(x = var, xend = var, y=0, yend = coef) , size=1, color = "#95B6D1", alpha = 1) +
  geom_point(size = 3, color = "#989C78") + 
  theme_classic() + 
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Factors Supporting Mental Health") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14))
ggsave("protect_plot.png", device = "png",  dpi = "retina", height = 4, width = 8)
## risk
ggplot(highlight_risk_dat, aes(var, abs(coef))) +
  geom_segment(aes(x = var, xend = var, y=0, yend = abs(coef)), size=1, color = "#765F56", alpha = 1) +
  geom_point(size = 3, color = "#D6B062") + 
  theme_classic() + 
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Risk Factors for Mental Health") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14))

ggsave("risk_plot.png", device = "png",  dpi = "retina", height = 4, width = 8)

# use phq9 as the outcome
# social support one, exercising since, social support two, reverse score covid impact questions
vars <- c("Depression_or_Anxiety",
          "PHQ1",
          "PHQ2",
          "PHQ3",
          "PHQ4",
          "SocialSupport2_R",
          "Exercising_Since",
          "Covid_Impact1",
          "Covid_Impact2",
          "Covid_Impact3",
          "Covid_Impact4",
          "Covid_Impact5")

dat <- 
  key_dataset %>% 
    select(vars) %>% 
    mutate(across(.cols = c(Covid_Impact1, Covid_Impact2, Covid_Impact3, Covid_Impact4, Covid_Impact5),
           .fns = ~ 4 - .x, 
           .names = "{.col}_r"),
           phq_total = PHQ1 + PHQ2 + PHQ3 + PHQ4)

cor_mat <- 
  dat %>% 
  select(phq_total, 
         Covid_Impact1_r,
         Covid_Impact2_r,
         Covid_Impact3_r,
         Covid_Impact4_r,
         Covid_Impact5_r,
         SocialSupport2_R,
         Exercising_Since) %>% 
  relocate(phq_total, 
           Covid_Impact3_r, 
           Covid_Impact2_r, 
           Covid_Impact4_r, 
           Exercising_Since,
           Covid_Impact5_r,
           SocialSupport2_R,
           Covid_Impact1_r) %>% 
  cor(use = "complete.obs") %>% 
  round(digits = 2)
  

# Build a Panel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)

plotcorr(cor_mat, 
         col = my_colors[cor_mat * 50+50], 
         mar=c(1,1,1,1),
         type = "upper",
         diag = FALSE) 

cor_mat[ord, ord]

