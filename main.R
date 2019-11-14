# Francesco Cabiddu, CabidduF@cardiff.ac.uk
# Ganglmayer, Attig, Daum and Paulus (2019): https://doi.org/10.1016/j.infbeh.2019.101340

# load libraries ----------------------------------------------------------
lib <- c("magrittr", "tidyverse","readxl")
lapply(lib, require, character.only = TRUE)
rm(lib)


# import dataset ----------------------------------------------------------
# download dataset in your working directory from https://osf.io/qdx4n/?view_only=fa9e929fe4524755b38383fd223378f5
goal_2 <- read_excel("Data_Goal.xlsx", sheet = "Study2")

# new plot ----------------------------------------------------------------
goal_2_prop <- goal_2 %>%
  select(Vpn, `Age group`, `FirstFixationT1 Path`:`FirstFixationT3 Hand`) %>% 
  rowwise() %>%
  filter(sum(abs(`FirstFixationT1 Path`), 
             abs(`FirstFixationT2 Path`), 
             abs(`FirstFixationT3 Path`), na.rm = TRUE) >= 2) %>%
  filter(sum(abs(`FirstFixationT1 Hand`), 
             abs(`FirstFixationT2 Hand`), 
             abs(`FirstFixationT3 Hand`), na.rm = TRUE) >= 2) %>%
  ungroup %>%
  gather("Paradigm", "First_Fixation", -Vpn, 
         -`Age group`) %>%
  separate(Paradigm, c("Trial", "Paradigm"), sep = " ") %>%
  na.omit %>%
  filter(First_Fixation %in% c(1, -1)) %>%
  mutate(First_Fixation = First_Fixation %>%
           factor(levels = c(-1, 1), labels = c("Position", "Goal")),
         `Age group` = `Age group` %>%
           factor(levels = 1:3, labels = c("11-month-olds", "32-month-olds", "Adults")),
         Paradigm = factor(Paradigm))

# sample size table
sample_sizes <- goal_2_prop %>%
  mutate(First_Fixation = ifelse(First_Fixation == "Goal", TRUE, FALSE)) %>%
  group_by(Vpn, `Age group`, Paradigm) %>%
  summarise(prop_correct = sum(First_Fixation) / n()) %>%
  ungroup %>%
  mutate(Paradigm = ifelse(Paradigm == "Hand", "Human", "Non-human") %>%
           factor) %>%
  rename(Age_group = `Age group`) %>%
  group_by(Age_group, Paradigm) %>%
  summarise(N = n()) %>%
  ungroup %>%
  {.$N}

goal_2_prop %>%
  mutate(First_Fixation = ifelse(First_Fixation == "Goal", TRUE, FALSE)) %>%
  group_by(Vpn, `Age group`, Paradigm) %>%
  summarise(prop_correct = sum(First_Fixation) / n()) %>%
  ungroup %>%
  rename(Age_group = `Age group`) %>%
  group_by(Paradigm, Age_group) %>%
  summarise(mean_prop = mean(prop_correct),
            conf_low = t.test(prop_correct, mu = 0.5)$conf.int[1],
            conf_high = t.test(prop_correct, mu = 0.5)$conf.int[2],
            p_value = t.test(prop_correct, mu = 0.5)$p.value %>% round(5) %>%
              {ifelse(. < 0.05, "*", "")}) %>%
  ungroup %>%
  rowwise %>%
  mutate(conf = mean_prop-conf_low) %>%
  ungroup %>%
  mutate(Age_group = factor(Age_group, levels = c("11-month-olds", "32-month-olds", "Adults"))) %>%
  arrange(Age_group) %>%
  mutate(Paradigm = ifelse(Paradigm == "Hand", "Human", "Non-human") %>%
           factor) %>% 
  ggplot(aes(Age_group, `mean_prop`, fill = Paradigm)) +  
  geom_bar(alpha = 0.8, width = 0.4, stat = "identity", position =position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=`mean_prop`-conf, ymax=`mean_prop`+conf),
                width=.2,                    # Width of the error bars
                position=position_dodge(0.5)) +
  geom_text(aes(x = c(0.300, 1.125, 1.875, 2.125, 2.875, 3.125), 
                y = `mean_prop` + conf + 0.03, label = p_value), size = 7) +
  geom_text(aes(x = c(0.815, 1.065, 1.815, 2.065, 2.815, 3.065), 
                y =  0.03, label = "N", fontface = 3), size = 3.8) +
  geom_text(aes(x = c( 0.895, 1.145, 1.895, 2.145, 2.895, 3.145), 
                y =  0.03, label = paste(" =", sample_sizes)), size = 3.8) +
  scale_fill_grey(start = 0.5, end= 0.85) +
  labs(x = "Age Group", y = "Goal Anticipations (Mean Proportion)", fill = "Agent type") +
  ggtitle("") +
  theme(axis.text=element_text(size=15),  
        axis.title = element_text(size=25),
        legend.text = element_text(size = 25),         
        legend.title = element_text(size = 25),  
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.4, 'cm'),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept=0.5, linetype="dashed", size=0.5, alpha = 0.8) 

# converage t CI --------------------------------------------------------
coverage_simulation_t_CI <- function(DF, cond1, cond2, THETA) {
  loops <- 10^5
  coverage <- rep(NA,loops)
  
  # distribution of individual sample sizes
  NsDist <- DF %>%
    filter(`Age group` == cond1, Paradigm == cond2) %>%
    group_by(Vpn) %>%
    summarise(N = n()) %>%
    {.$N}
  
  # true probability of a correct "look"
  theta <- THETA
  
  # number of subjects
  nsubs <- DF %>%
    filter(`Age group` == cond1, Paradigm == cond2) %>%
    {.$Vpn} %>%
    unique %>%
    length
  
  # estimate coverage
  for(i in 1:loops){
    # generate one sample of individual proportions
    props <- rep(NA,nsubs)
    for(j in 1:nsubs){
      n <- sample(NsDist,1)
      props[j] <- rbinom(1,n,theta)/n
    }
    # calculate CI; if no variation set CI bounds to mean
    if(min(props)==max(props)){ ci <- c(mean(props),mean(props)) 
    } else { ci <- t.test(props)$conf.int }
    # check coverage
    coverage[i] <- (ci[1] < theta & theta < ci[2])
  }
  # coverage estimate (as %)
  tibble(`Age group` = DF$`Age group`[1],
         Paradigm = DF$Paradigm[1],
         Coverage = round( 100*mean(coverage), 1 ))
}

set.seed(1255)
goal_2_prop %>%
  coverage_simulation_t_CI(cond1 = "11-month-olds", 
                           cond2 = "Hand", 
                           THETA = 0.6) %>%
  rbind(
    goal_2_prop %>%
      coverage_simulation_t_CI(cond1 = "11-month-olds", 
                               cond2 = "Path", 
                               THETA = 0.3)
  ) %>%
  rbind(
    goal_2_prop %>%
      coverage_simulation_t_CI(cond1 = "32-month-olds", 
                               cond2 = "Hand", 
                               THETA = 0.7)
  ) %>%
  rbind(
    goal_2_prop %>%
      coverage_simulation_t_CI(cond1 = "32-month-olds", 
                               cond2 = "Path", 
                               THETA = 0.4)
  ) %>%
  rbind(
    goal_2_prop %>%
      coverage_simulation_t_CI(cond1 = "Adults", 
                               cond2 = "Hand", 
                               THETA = 0.8)
  ) %>%
  rbind(
    goal_2_prop %>%
      coverage_simulation_t_CI(cond1 = "Adults", 
                               cond2 = "Path", 
                               THETA = 0.8)
  )
