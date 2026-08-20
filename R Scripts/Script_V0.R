##-------------------------------------------##
## Perceived Discrimination in Latin America ##
## Written by: Santiago Gómez-Echeverry      ##
## Last update: 20/04/2026                   ##
##-------------------------------------------##

#### - (I) Working space and packages - ####

rm(list = ls())
packs <- c('mvord', 'haven', 'ggplot2', 'ggpubr', 'sjPlot', 'sjlabelled', 'dplyr', 'tidyr', 'gridExtra', 'forcats', 'stringr', 'MASS')
ipacks <- packs %in% rownames(installed.packages())
if(any(ipacks == F)){install.packages(packs[!ipacks])}
invisible(lapply(packs, library, character.only = T))

sys <- Sys.info()
fold_graphs <- paste0("C:/Users/", sys[7], "/Dropbox/Papers/Reinforced differential treatment/4 - Graphs and Tables")
dat_lapop <- read_dta(paste0("C:/Users/", sys[7],"/Dropbox/Papers/Reinforced differential treatment/3 - Data/2 - Raw/Merge_2023_LAPOP_AmericasBarometer_v1.0_w.dta"))
lapop_colors <- c("#FCF2EA", "#FBE5D6", "#FAD9C2", "#EBDAAF", "#E4CD94","#D3BD7F", "#9E6934", "#82562A", "#5C3D1E", "#422C16", "#362412")


#### - (II) Data Arrangement and Descriptive statistics - ####

### - 1. Data arrangement - ###

fac_cols <- c("dis11", "dis12")
dat_lapop[fac_cols] <- lapply(dat_lapop[fac_cols], factor,labels = c("Many times", "Some times", "A few times", "Never"))
dat_lapop$colorr[dat_lapop$colorr==97] <- NA
dat_lapop$dis11 <- fct_rev(dat_lapop$dis11)
dat_lapop$dis12 <- fct_rev(dat_lapop$dis12)
dat_lapop$q1tc_r[dat_lapop$q1tc_r==3] <- NA
dat_lapop$sex <- factor(dat_lapop$q1tc_r, levels = c(1,2), labels = c("Male", "Female"))
dat_lapop$ur <- factor(dat_lapop$ur, levels = c(1,2), labels = c("Rural", "Urban"))
dat_lapop$edre <- dat_lapop$edre + 1
dat_lapop$edre <- factor(dat_lapop$edre, levels = 1:7, labels = c("None", "Primary incomplete", "Primary complete",
                                                                  "Secondary incomplete", "Secondary incomplete", 
                                                                  "Tertiary incomplete", "Tertiary complete")) 
dat_lapop <- dat_lapop %>% filter(!is.na(dis11))
dat_lapop <- dat_lapop %>% filter(!is.na(dis12))
dat_lapop <- dat_lapop[dat_lapop$pais!=40 & dat_lapop$pais!=41,]
dat_lapop$pais <- as.factor(dat_lapop$pais)
# Arranging of the ethnicity variable

table(dat_lapop$etid)
dat_lapop$etid <- factor(as.character(as_factor(dat_lapop$etid)))
table(dat_lapop$etid)

dat_lapop <- dat_lapop %>% 
  mutate(etid = recode(etid,
                       "Mestizo" = "Mestiza",
                       "Maya Ketchi" = "Indígena",
                       "Maya Mopan" = "Indígena", 
                       "Maya Yucatec" = "Indígena", 
                       "Indio" = "Indígena", 
                       "Aymara" = "Indígena", 
                       "Garifuna" = "Otra", 
                       "De la Amazonia" = "Otra",
                       "Quechua" = "Indígena", 
                       "Sirio/libanés" = "Otra", 
                       "Zamba" = "Otra", 
                       "Criollo" = "Otra", 
                       "Hindú" = "Otra", 
                       `Hindustani ("indios orientales")`= "Otra",
                       "Indio del este" = "Otra", 
                       "China" = "Otra",
                       "Chino" = "Otra",
                       "Oriental" = "Otra",
                       "Granates" = "Otra",
                       "Javanés" = "Otra",
                       "Asiático" = "Otra",
                       "Español" = "Otra"))

country_labels <- c(
  "1" = "Mexico", "2" = "Guatemala","3" = "El Salvador", "4" = "Honduras", "5" = "Nicaragua", "6" = "Costa Rica", "7" = "Panama", "8" = "Colombia",
  "9" = "Ecuador", "10" = "Bolivia", "11" = "Peru", "12" = "Paraguay", "13" = "Chile", "14" = "Uruguay", "15" = "Brazil", "17" = "Argentina", 
  "21" = "Dominican Republic", "22" = "Haiti", "23" = "Jamaica", "25" = "Trinidad and Tobago", "26" = "Belize", "27" = "Suriname", "28" = "Bahamas",
  "30" = "Grenada", "40" = "United States", "41" = "Canada")

dat_lapop <- dat_lapop %>%
  mutate(pais_label = recode(as.character(pais), !!!country_labels), pais_label = factor(pais_label))

# Create the collapsed skin color variable
dat_lapop <- dat_lapop %>%
  mutate(colorr_collapsed = case_when(colorr %in% 1:4 ~ "Light", colorr %in% 5:6 ~ "Medium", colorr >= 7 ~ "Dark"),
         colorr_collapsed = factor(colorr_collapsed, levels = c("Dark", "Medium", "Light")))

# Check the distribution
table(dat_reg$colorr_collapsed, useNA = "ifany")




### - 2. Descriptive statistics - ###

##  Figure 1  ##

correct_etid_order <- dat_lapop %>% 
  pull(etid) %>% 
  unique()

# Option A: Stacked bar
mako_colors <- rev(viridis::mako(4))                                            # Get the mako palette for 4 values
custom_colors <- c(mako_colors, "gray87")                                       # Add gray as the last color
valid_etid <- setdiff(levels(dat_lapop$etid), c("No sabe", "No responde"))      # Store the order of the etid levels

p1a <- dat_lapop %>%
  dplyr::select(etid, colorr, dis11) %>% 
  filter(!is.na(etid), !is.na(colorr), !is.na(dis11)) %>%
  filter(etid != "No sabe", etid != "No responde") %>%
  count(etid, colorr, dis11) %>%
  tidyr::complete(etid = valid_etid, colorr, dis11, fill = list(n = 0)) %>%
  mutate(etid = factor(etid, levels = correct_etid_order)) %>%
  group_by(etid, colorr) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>% 
  mutate(dis11_plot = ifelse(total == 0, "No cases", as.character(dis11)), 
         dis11_plot = factor(dis11_plot, levels = c("Never", "A few times", "Some times", "Many times", "No cases"))) %>% 
  group_by(etid, colorr) %>%
  mutate(p = ifelse(total > 0, n / total, ifelse(dis11_plot == "No cases", 1, 0))) %>%
  ungroup() %>% 
  ggplot(aes(x = factor(colorr), y = p, fill = dis11_plot)) + geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + scale_fill_manual(values = custom_colors, drop = FALSE) +
  facet_wrap(~ etid, ncol = 3) + labs(x = "Skin color", y = "Proportion",fill = "") +
  theme_minimal(base_size = 12) + theme(strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom")
p1a
ggsave("Fig1a.png", plot = p1a, path = fold_graphs, width = 20, height = 15, units = "cm")

# Option B: Balloon plot
p1b <- dat_lapop %>%
  dplyr::select(etid, colorr, dis11) %>% 
  filter(!is.na(etid), !is.na(colorr), !is.na(dis11)) %>%
  filter(!etid %in% c("No sabe", "No responde")) %>%
  mutate(etid = factor(etid, levels = correct_etid_order)) %>%
  count(etid, colorr, dis11) %>%
  group_by(etid, colorr) %>%
  mutate(total = sum(n),p = n / total) %>%
  ungroup() %>%
  ggplot(aes(x = factor(colorr), y = dis11)) +
  geom_point(aes(size = total, color = p), alpha = 0.9) +
  scale_color_viridis_c(option = "mako", direction = -1) +
  scale_size(range = c(1, 8)) +
  facet_wrap(~ etid, ncol = 3) +
  labs(x = "Skin color", y = "Perceived Disc. by skin color", color = "Proportion", size = "Sample size") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold"))
p1b
ggsave("Fig1b.png", plot = p1b, path = fold_graphs, width = 20, height = 15, units = "cm")


##  Figure 2  ##

# Option A: Simple
p2a <- dat_lapop %>%
  filter(!is.na(dis12), !is.na(colorr), !is.na(sex)) %>%
  count(sex, colorr, dis12) %>%
  group_by(sex, colorr) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(colorr), y = p, fill = dis12)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "rocket", direction = -1, name = "") +
  labs(x = "Skin Color",y = "Proportion") +
  facet_wrap(~ sex, ncol = 1) +
  theme_minimal(base_size = 12) + theme(strip.text = element_text(face = "bold"),
                                        axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor = element_blank(), legend.position = "bottom")
p2a
ggsave("Fig2a.png", plot = p2a, path = fold_graphs, width = 15, height = 16.5, units = "cm")

# Option B: Cross with ethnicity

rocket_colors <- rev(viridis::rocket(4))    

p2b <- dat_lapop %>%
  group_by(colorr_collapsed, etid, sex, dis12) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!etid %in% c("No sabe", "No responde")) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(colorr_collapsed)) %>%
  mutate(etid = factor(etid, levels = correct_etid_order)) %>% 
  group_by(colorr_collapsed, etid, sex) %>%
  mutate(proportion = count / sum(count)) %>% 
  ggplot(aes(x = colorr_collapsed, y = proportion, fill = dis12)) +
  geom_col(position = "fill", width = 0.7) +
  facet_grid(sex ~ etid, labeller = labeller(sex = c(`0` = "Female", `1` = "Male"))) +
  scale_fill_manual(values = rocket_colors, drop = F) +
  labs(x = "Skin color", y = "Proportion") + theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold"), legend.position = "bottom")
p2b
ggsave("Fig2b.png", plot = p2b, path = fold_graphs, width = 15, height = 16.5, units = "cm")

##  Figure 3: Plot Dep variables  ##

p3 <- dat_lapop %>% 
  dplyr::filter(!is.na(dis11), !is.na(dis12), !is.na(pais_label)) %>% 
  mutate(dis11 = factor(dis11, level = c("Never", "A few times", "Some times", "Many times")),
         dis12 = factor(dis12, level = c("Never", "A few times", "Some times", "Many times")),) %>% 
  count(pais_label, dis11, dis12, name = "n") %>% 
  group_by(pais_label, dis11) %>% 
  mutate(p_row = n/sum(n), lab_row = scales::percent(p_row, accuracy = 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = dis12, y = dis11, fill = p_row)) + 
    geom_tile(color = "white", linewidth = 0.2) +  geom_text(aes(label = lab_row, color = p_row > 0.5), size = 2.8) +
    scale_color_manual(values = c("TRUE" = "gray70", "FALSE" = "gray30", guide = "none")) +
    scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0,1), breaks = seq(0, 1, 0.25)) +
    facet_wrap(~ pais_label, ncol = 5) + labs(x = "By sex/gender", y = "By skin color", fill = "Percentage") + 
    theme_minimal(base_size = 11) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
                                          strip.text = element_text(face = "bold", size = 9))
p3  
ggsave("Fig3.png", plot = p3,  path = fold_graphs, width = 30, height = 20, units = "cm")  

##  Table 1: Descriptive stats.  ##


#### - (III) Regression Analyses - ####

# Reg_0: Age
dat_reg <- dat_lapop[(!is.na(dat_lapop$colorr) & !is.na(dat_lapop$sex) & !is.na(dat_lapop$etid) & !is.na(dat_lapop$q2) & !is.na(dat_lapop$ur) &
                        !is.na(dat_lapop$edre)) & !is.na(dat_lapop$etid),]

res_by_con <- vector(mode = "list", length = 10)
conts <- unique(dat_reg$pais)
for (i in 1:length(conts)){
  cty <- conts[[i]]
  temp_dat <- dat_reg[dat_reg$pais==cty,]
  res_by_con[[i]] <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr + sex + colorr:sex, data = temp_dat, link = mvlogit())
  cat("Estimation", i, "done \n")
}

summary(res_by_con[[1]])

fit11 <- polr(dis11 ~ 1 + colorr + sex + etid + colorr:sex + colorr:etid + etid:sex + q2 + ur + edre, data = dat_reg)
summary(fit11)
fit12 <- polr(dis12 ~ 1 + colorr + sex + etid + colorr:sex + colorr:etid + etid:sex + q2 + ur + edre, data = dat_reg)
summary(fit12)

beta_start <- c(coef(fit11), coef(fit12))
theta_start <- c(fit11$zeta, fit12$zeta)


res0 <- local(mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr_collapsed + sex + etid, 
                    error.structure = cor_equi(~ pais),
                    data = dat_reg, weights.name = "weight1500" , link = mvprobit(), control = mvord.control(solver = "BFGS")))
res1 <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr_collapsed + sex + etid + colorr_collapsed:sex + colorr_collapsed:etid + etid:sex, 
              error.structure = cor_equi(~ pais), 
              data = dat_reg, weights.name = "weight1500", link = mvprobit(), control = mvord.control(solver = "BFGS")) 
resF <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr_collapsed + sex + etid + colorr_collapsed:sex + colorr_collapsed:etid + etid:sex + q2 + I(q2^2) + ur + edre, 
              error.structure = cor_equi(~ pais), 
              data = dat_reg, weights.name = "weight1500", link = mvprobit(), control = mvord.control(solver = "BFGS")) 




summary(res_het)
margins <- marginal_predict(resF, type = "all.prob") %>% 
  as.data.frame()
dat_reg <- cbind(dat_reg, margins)






# Calculate with standard errors
color_effects <- dat_reg %>%
  group_by(colorr_collapsed) %>%
  summarise(
    mean_11 = mean(prob_dis11, na.rm = TRUE),
    se_11 = sd(prob_dis11, na.rm = TRUE) / sqrt(n()),
    mean_12 = mean(prob_dis12, na.rm = TRUE),
    se_12 = sd(prob_dis12, na.rm = TRUE) / sqrt(n())
  )

# Reshape for plotting
color_long <- pivot_longer(color_effects, 
                           cols = c(mean_11, mean_12),
                           names_to = "outcome", 
                           values_to = "probability")
color_long$se <- ifelse(color_long$outcome == "mean_11", color_long$se_11, color_long$se_12)
color_long$outcome <- factor(color_long$outcome, labels = c("dis11", "dis13"))

ggplot(color_long, aes(x = colorr_collapsed, y = probability, fill = outcome)) +
  geom_col(position = position_dodge(0.9), width = 0.8) +
  geom_errorbar(aes(ymin = probability - se, ymax = probability + se),
                position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  labs(x = "Skin color", y = "Predicted probability", fill = "Outcome") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")









cor_cont <- summary(res_het)$error.structure
rownames(cor_cont) <- str_extract(rownames(cor_cont), "(?<=pais)\\d+")

cor_cont$country <- c("Mexico", "Guatemala", "El Salvador", "Honduras", "Costa Rica", "Panama", "Colombia",
                 "Ecuador", "Bolivia", "Peru", "Paraguay", "Chile", "Uruguay", "Brazil", "Argentina", 
                 "Dominican Republic", "Jamaica", "Trinidad & Tobago", "Belize", "Suriname", 
                 "Bahamas", "Grenada")

ggplot(cor_cont, aes(x = country, y = Estimate)) +
  geom_point(size = 4, color = "darkseagreen4") +  # Plot coefficients
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, 
                    ymax = Estimate + 1.96 * `Std. Error`), 
                width = 0.2, color = "darkseagreen") +  # Error bars
  theme_minimal() +
  labs(x = "Country", y = "Estimate") +
  coord_flip()  # Flipping for better readability
ggsave("Fig2_Cor.png", path = fold_graphs, width = 12, height = 15, units = "cm")


# If we use a logit, we can interpret the odd-ratio directly.
# What should we do with the rho coefficients? A cool idea would be to present this by country!
marginal_predict(res_het, type = "all.prob") # This gives all the probabilities. If not specified, it only provides the largest prob.

compute_marginal_effects <- function(model, var, data, step = 1, n_boot = 50) {
  boot_results <- replicate(n_boot, {
    sample_data <- data[sample(nrow(data), replace = TRUE), ]
    
    data_high <- sample_data
    data_low <- sample_data
    
    data_high[[var]] <- data_high[[var]] + step
    data_low[[var]] <- data_low[[var]] - step
    
    probs_high <- predict(model, newdata = data_high, type = "prob")
    probs_low <- predict(model, newdata = data_low, type = "prob")
    
    (probs_high - probs_low) / (2 * step)
  }, simplify = FALSE)
  
  mean_effects <- apply(simplify2array(boot_results), c(1, 2), mean)
  se_effects <- apply(simplify2array(boot_results), c(1, 2), sd)
  
  results <- as.data.frame(mean_effects)
  results$se <- as.data.frame(se_effects)
  results$colorr <- data$colorr
  return(results)
}

marginal_effects <- compute_marginal_effects(res_het, "colorr", dat_reg)

# Reshape for plotting
marginal_long <- marginal_effects %>%
  pivot_longer(cols = -c(colorr, se), names_to = "Category", values_to = "MarginalEffect")

# Plot marginal effects with standard errors
ggplot(marginal_long, aes(x = colorr, y = MarginalEffect, color = Category)) +
  geom_line() +
  geom_ribbon(aes(ymin = MarginalEffect - 1.96 * se, ymax = MarginalEffect + 1.96 * se, fill = Category), alpha = 0.2) +
  labs(title = "Marginal Effects of colorr on Predicted Probabilities",
       x = "colorr", y = "Marginal Effect") +
  theme_minimal()


# Reg_1

# Reg_2

