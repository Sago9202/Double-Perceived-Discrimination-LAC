##-------------------------------------------##
## Perceived Discrimination in Latin America ##
## Written by: Santiago Gómez-Echeverry      ##
## Last update: 20/05/2026                   ##
##-------------------------------------------##

#### - (I) Working space and packages - ####

rm(list = ls())
packs <- c('mvord', 'haven', 'ggplot2', 'ggpubr', 'sjPlot', 'sjlabelled', 'dplyr', 'tidyr', 'gridExtra', 'forcats', 
           'stringr', 'MASS', 'purrr', 'xtable', 'knitr','kableExtra','ggh4x', 'calecopal','emmeans', 'polycor')
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
                                                                  "Secondary incomplete", "Secondary complete", 
                                                                  "Tertiary incomplete", "Tertiary complete")) 
dat_lapop$edre <- fct_relevel(dat_lapop$edre, "Secondary complete")
dat_lapop <- dat_lapop %>% filter(!is.na(dis11))
dat_lapop <- dat_lapop %>% filter(!is.na(dis12))
dat_lapop <- dat_lapop[dat_lapop$pais!=6 & dat_lapop$pais!=40 & dat_lapop$pais!=41,]
dat_lapop$pais <- as.factor(dat_lapop$pais)
# Arranging of the ethnicity variable
table(dat_lapop$etid)
dat_lapop$etid <- factor(as.character(as_factor(dat_lapop$etid)))
table(dat_lapop$etid)

dat_lapop <- dat_lapop %>% 
  mutate(etid = dplyr::recode(etid,
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
                       "Español" = "Otra"),
         etid = as.character(etid),
         etid = na_if(etid, "No sabe"),
         etid = na_if(etid, "No responde"), 
         etid = factor(etid) %>%  fct_relevel("Mestiza"))
table(dat_lapop$etid)

country_labels <- c(
  "1" = "Mexico", "2" = "Guatemala","3" = "El Salvador", "4" = "Honduras", "5" = "Nicaragua", "6" = "Costa Rica", "7" = "Panama", "8" = "Colombia",
  "9" = "Ecuador", "10" = "Bolivia", "11" = "Peru", "12" = "Paraguay", "13" = "Chile", "14" = "Uruguay", "15" = "Brazil", "17" = "Argentina", 
  "21" = "Dominican Republic", "22" = "Haiti", "23" = "Jamaica", "25" = "Trinidad and Tobago", "26" = "Belize", "27" = "Suriname", "28" = "Bahamas",
  "30" = "Grenada", "40" = "United States", "41" = "Canada")

dat_lapop <- dat_lapop %>%
  mutate(pais_label = dplyr::recode(as.character(pais), !!!country_labels), pais_label = factor(pais_label))

# Create the collapsed skin color variable
dat_lapop <- dat_lapop %>%
  mutate(colorr_collapsed = case_when(colorr %in% 1:4 ~ "Light", colorr %in% 5:6 ~ "Medium", colorr >= 7 ~ "Dark"),
         colorr_collapsed = factor(colorr_collapsed, levels = c("Dark", "Medium", "Light")))

# Check the distribution
table(dat_lapop$colorr_collapsed, useNA = "ifany")

# Income ranges
dat_lapop$inc_ran <- dat_lapop$q10inc %>% 
  as.character() %>% 
  str_sub(-2) %>% 
  as.factor()
table(dat_lapop$inc_ran)

# Occupation
dat_lapop <- dat_lapop %>% 
  mutate(ocup = factor(ocup4a, levels = 1:7,  labels = c("Employed, Working", "Employed, Not working", "Actively looking for a job",
                                          "Student", "Dedicated to Household", "Retired or unable to work", "Not working and not looking for a job")))

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
  ggplot(aes(x = factor(colorr), y = p, fill = dis11_plot)) + 
  geom_col(position = "fill") +
  geom_text(data = . %>% distinct(etid, colorr, total), aes(x = factor(colorr), y = 1.02, label = total), 
            size = 2.5, angle = 45, inherit.aes = F, color = "gray40") +
  scale_y_continuous(labels = scales::percent_format(), 
                     expand = expansion(mult = c(0, 0.08))) + 
  scale_fill_manual(values = custom_colors, drop = FALSE) +
  facet_wrap(~ etid, ncol = 3) + 
  labs(x = "Skin color", y = "Proportion", fill = "") +
  theme_minimal(base_size = 12) + 
  theme(strip.text = element_text(face = "bold"),  axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
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

p2a <- dat_lapop %>%
  dplyr::select(etid, sex, dis12) %>% 
  filter(!is.na(dis12), !is.na(etid), !is.na(sex)) %>%
  count(sex, etid, dis12) %>%
  group_by(sex, etid) %>%
  mutate(total = sum(n), p = n/total) %>% 
  ungroup() %>%
  ggplot(aes(x = etid, y = p, fill = dis12)) +
  geom_col(position = "fill") +
  geom_text(data = . %>% distinct(sex, etid, total), aes(x = etid, y = 1.02, label = total), 
            size = 4, angle = 45, inherit.aes = F, color = "gray40") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "rocket", direction = -1, name = "") +
  labs(x = "Ethnicity",y = "Proportion") +
  facet_wrap(~ sex) +
  theme_minimal(base_size = 18) + theme(strip.text = element_text(face = "bold"),
                                        axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor = element_blank(), legend.position = "bottom")

p2a
ggsave("Fig2a.png", plot = p2a, path = fold_graphs, width = 25, height = 15, units = "cm")

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
ggsave("Fig2b.png", plot = p2b, path = fold_graphs, width = 20, height = 15, units = "cm")

##  Figure 3: Plot Dep variables  ##

# Option A.
p3 <- dat_lapop %>% 
  dplyr::filter(!is.na(dis11), !is.na(dis12), !is.na(pais_label)) %>% 
  mutate(dis11 = factor(dis11, level = c("Never", "A few times", "Some times", "Many times")),
         dis12 = factor(dis12, level = c("Never", "A few times", "Some times", "Many times")),) %>% 
  count(pais_label, dis11, dis12, name = "n") %>% 
  group_by(pais_label) %>% 
  mutate(p_total = n/sum(n), lab_row = scales::percent(p_total, accuracy = 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = dis12, y = dis11, fill = p_total)) + 
  geom_tile(color = "white", linewidth = 0.2) +  geom_text(aes(label = lab_row, color = p_total > 0.5), size = 2.8) +
  scale_color_manual(values = c("TRUE" = "gray70", "FALSE" = "gray30", guide = "none")) +
  scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0,1), breaks = seq(0, 1, 0.25)) +
  facet_wrap(~ pais_label, ncol = 5) + labs(x = "By sex/gender", y = "By skin color", fill = "Percentage") + 
  theme_minimal(base_size = 11) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
                                        strip.text = element_text(face = "bold", size = 9))
p3 <- dat_lapop %>% 
  dplyr::filter(!is.na(dis11), !is.na(dis12), !is.na(pais_label)) %>% 
  mutate(dis11 = factor(dis11, level = c("Never", "A few times", "Some times", "Many times")),
         dis12 = factor(dis12, level = c("Never", "A few times", "Some times", "Many times")),) %>% 
  count(pais_label, dis11, dis12, name = "n") %>% 
  group_by(pais_label, dis11) %>% 
  mutate(p_row = n/sum(n), lab_row = scales::percent(p_row, accuracy = 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = dis12, y = dis11, fill = p_row)) + 
    geom_tile(color = "white", linewidth = 0.2) +  geom_text(aes(label = lab_row, color = p_row > 0.5), size = 2.8, show.legend = F) +
    scale_color_manual(values = c("TRUE" = "gray70", "FALSE" = "gray30", guide = "none")) +
    scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0,1), breaks = seq(0, 1, 0.25)) +
    facet_wrap(~ pais_label, ncol = 3) + labs(x = "By sex/gender", y = "By skin color", fill = "Percentage") + 
    theme_minimal(base_size = 11) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
                                          strip.text = element_text(face = "bold", size = 9)) 

p3  
ggsave("Fig3.png", plot = p3,  path = fold_graphs, width = 20, height = 35, units = "cm")  

##  Table 1: Descriptive stats.  ##

vars <- c("q2", "q12cn", "q12bn", "colorr", "colori", "sex", "etid", "ur", "edre", "inc_ran", "ocup")

make_entry <- function(var){
  var_lab <- sjlabelled::get_label(var)
  if (is.numeric(var)){
    return(tibble(Variable = var_lab, N = sum(!is.na(var)), Mean = mean(var, na.rm = T), SD = sd(var, na.rm = T), 
                   Min = min(var, na.rm = T), Max = max(var, na.rm = T)))
  } else if (is.factor(var)) {
    tab <- table(var) 
    prc <- prop.table(tab)
    tab_n <- tibble(Variable = "", N = sum(!is.na(var)), Mean = NA, SD = NA, Min = 1, Max = length(prc))
    tab_f <- tibble(Variable = names(tab), N = as.numeric(tab), Mean = as.numeric(prc), SD = NA, Min = NA, Max = NA)
    return(rbind(tab_n, tab_f))
  }
}


tab_final <- map_dfr(vars, ~ make_entry(dat_lapop[[.x]]))
print(xtable(tab_final, digits = c(0,0,0,2,2,2,2), caption = "Descriptive statistics"), include.rownames = F, sanitize.text.function = identity)

#### - (III) Regression Analyses - ####

# Reg_0: Age
dat_reg <- dat_lapop[(!is.na(dat_lapop$colorr) & !is.na(dat_lapop$sex) & !is.na(dat_lapop$etid) & !is.na(dat_lapop$q2) & !is.na(dat_lapop$ur) &
                      !is.na(dat_lapop$edre)) & !is.na(dat_lapop$etid) & !is.na(dat_lapop$inc_ran) & !is.na(dat_lapop$ocup) & !is.na(dat_lapop$q12cn) &
                      !is.na(dat_lapop$q12bn) & !is.na(dat_lapop$colori),]

# Full skin color palette
m0 <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr + sex + etid, error.structure = cor_general(~ pais),
                    data = dat_reg, weights.name = "weight1500", link = mvlogit())
m1 <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr + sex + etid + colorr:sex + colorr:etid + etid:sex, 
              error.structure = cor_equi(~ pais), data = dat_reg, weights.name = "weight1500", link = mvlogit()) 
mF <- mvord(formula = MMO2(dis11, dis12) ~ 0 + colorr + sex + etid + colorr:sex + colorr:etid + etid:sex + q12cn + q12bn + q2 + I(q2^2) + ur + edre + inc_ran + ocup + colori, 
              error.structure = cor_equi(~ pais), data = dat_reg, weights.name = "weight1500", link = mvlogit()) 
results <- list(m0, m1, mF)

## Saving results

setwd(fold_graphs)
saveRDS(results, file ="reg_results.RData")
results <- readRDS("reg_results.RData")
m1 <- results[[1]] 
m2 <- results[[2]]
mF <- results[[3]]
#pred_mF <- predictions(mF)

## Results table

outcome_tables <- list()

for (j in 1:2) {                        
  mod_tables <- list()
  for (i in 1:length(results)) {
    model <- results[[i]]
    coefs <- summary(model)$coefficients
    pattern <- paste0(" ", j, "$")
    rows <- grep(pattern, rownames(coefs))
    coefs_sub <- coefs[rows, , drop = FALSE]
    est <- coefs_sub[, "Estimate"]
    se  <- coefs_sub[, "Std. Error"]
    or  <- exp(est)
    ci_l <- exp(est - 1.96 * se)
    ci_u <- exp(est + 1.96 * se)
    # Compute p-values (two-tailed Wald test)
    z <- est / se
    p <- 2 * pnorm(-abs(z))
    stars <- ifelse(p < 0.001, "***",ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))) # Assign stars
    or_fmt <- sprintf("%.3f%s", or, stars)     # Format OR with stars
    cell <- sprintf("%s \\newline [%.3f, %.3f]", or_fmt, ci_l, ci_u)
    term <- gsub(paste0(" ", j), "", rownames(coefs_sub))
    mod_tables[[i]] <- data.frame(var = term, formatted = cell, stringsAsFactors = FALSE)
  }
  joined <- mod_tables[[1]]
  for (k in 2:length(mod_tables)) {
    joined <- full_join(joined, mod_tables[[k]], by = "var")
  }
  colnames(joined) <- c("var", paste0("m", c(0,1,"F"), "_dis", j))
  outcome_tables[[j]] <- joined
}

# Combine outcomes
final_table <- full_join(outcome_tables[[1]], outcome_tables[[2]], by = "var")

# Replace missing (NA) with em-dash
final_table[is.na(final_table)] <- "---"

# Create the LaTeX table
latex_table <- final_table %>%
  kable(format = "latex", booktabs = T, linesep = "", escape = F,  caption = "Odds Ratios and 95\\% Confidence Intervals for Disability Outcomes",
    label = "tab:2",align = "lcccccc", col.names = c("Variable", rep(c("Model 0", "Model 1", "Model F"), 2))) %>%
  add_header_above(c(" " = 1, "Discrimination based on Skin color" = 3, "Discrimination based on Sex/Gender" = 3), bold = T) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9)

# Print the LaTeX code to console or save to file
cat(latex_table)

## Interactions plot

summary(mF)
margins <- marginal_predict(mF, type = "all.prob") %>% 
  as.data.frame()
dat_reg <- cbind(dat_reg, margins)
dat_plot <- cbind(dat_reg, margins) %>%
  pivot_longer(
    cols = matches("^dis1[12]\\."), 
    names_to = c("outcome", "category"),
    names_pattern = "^(dis1[12])\\.(.*)",
    values_to = "predicted_prob"
  ) %>%
  mutate(
    category = str_replace_all(category, "\\.", " "), 
    category = factor(category, levels = c("Never", "A few times", "Some times", "Many times"))
  )


ggplot(filter(dat_plot, outcome == "dis11"), 
       aes(x = factor(colorr), y = predicted_prob, color = etid, group = etid)) +
  geom_smooth(method = "loess", formula = y ~ x, span = 1.0, se = TRUE, linewidth = 1) + 
  stat_summary(fun = mean, geom = "point", size = 1.5, alpha = 0.4) +     
  facet_wrap(. ~ category, scales = "free", ncol = 4) +                    
  facetted_pos_scales(y = list(category == "Never" ~ scale_y_continuous(limits = c(0.5, 1)),
                               category %in% c("A few times", "Some times", "Many times") ~ scale_y_continuous(limits = c(0, 0.2)))) +
  labs(x = "Respondent's Skin Color (1 = Lightest, 11 = Darkest)", y = "Predicted Probability", color = "Ethnicity") +
  theme_minimal() + scale_color_manual(values = cal_palette("kelp1")) +
  guides(colour = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 10),
        panel.spacing = unit(1, "lines"))
ggsave("Fig5a.png", path = fold_graphs, width = 15, height = 15, units = "cm")

diff_data <- dat_reg %>%
  group_by(colorr, sex, etid) %>%
  summarise(prob = mean(dis12.Many.times, na.rm = TRUE),
            se = sd(dis12.Many.times, na.rm = TRUE) / sqrt(n()),) %>%
  pivot_wider(names_from = sex, values_from = c(prob, se), names_glue = "{.value}_{sex}") %>%
  mutate(gender_gap = prob_Female - prob_Male, se_diff = sqrt(se_Female^2 + se_Male^2),
         lower_CI = gender_gap - 1.96 * se_diff, upper_CI = gender_gap + 1.96 * se_diff)

ggplot() +
  geom_line(data = prob_long, aes(x = colorr, y = prob, color = sex), linewidth = 1.2) +
  geom_ribbon(data = diff_data, aes(x = colorr, ymin = lower_CI, ymax = upper_CI),  fill = "black", alpha = 0.15) +
  geom_line(data = diff_data, aes(x = colorr, y = gender_gap, linetype = "Gender gap"), color = "black", linewidth = 1) +
  geom_errorbar(data = diff_data, aes(x = colorr, ymin = lower_CI, ymax = upper_CI), width = 0.2, color = "black", alpha = 0.5) +
  geom_point(data = diff_data, aes(x = colorr, y = gender_gap), color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40", linewidth = 0.6) +
  facet_wrap(~ etid, ncol = 3) + scale_x_continuous(breaks = 1:11) +
  scale_colour_manual(name = "Sex", values = c("Male" = "blue", "Female" = "red")) +
  scale_linetype_manual(name = "", values = c("Gender gap" = "dashed")) +
  labs(x = "Respondent's Skin Color (1 = Lightest, 11 = Darkest)", y = "Probability of reporting 'Many times'") +
  theme_minimal() + theme(strip.text = element_text(face = "bold", size = 11), panel.grid.minor = element_blank(), 
                          panel.spacing = unit(1.5, "lines"), legend.position = "bottom")

ggplot(diff_data, aes(x = colorr, y = gender_gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "maroon", linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "black", alpha = 0.15) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  facet_wrap(~ etid, ncol = 3) +
  scale_x_continuous(breaks = 1:11) +
  labs(x = "Respondent's Skin Color (1 = Lightest, 11 = Darkest)", y = "Pr(Many times = 1 | Female) - Pr(Many times = 1 | Male)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 11), 
        panel.grid.minor = element_blank(), panel.spacing = unit(1.5, "lines"))

ggplot() +
  geom_line(data = prob_long, aes(x = colorr, y = prob, color = sex), linewidth = 1.2) +
  geom_ribbon(data = diff_data, aes(x = colorr, ymin = lower_CI, ymax = upper_CI),  fill = "black", alpha = 0.15) +
  geom_line(data = diff_data, aes(x = colorr, y = gender_gap, linetype = "Diff"), color = "black", linewidth = 1) +
  geom_errorbar(data = diff_data, aes(x = colorr, ymin = lower_CI, ymax = upper_CI), width = 0.2, color = "black", alpha = 0.5) +
  geom_point(data = diff_data, aes(x = colorr, y = gender_gap), color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40", linewidth = 0.6) +
  facet_wrap(~ etid, ncol = 3) + scale_x_continuous(breaks = 1:11) +
  scale_colour_manual(name = "Sex", values = c("Male" = "#F69C73FF", "Female" = "#A11A5BFF")) +
  scale_linetype_manual(name = "", values = c("Diff" = "dashed")) +
  labs(x = "Respondent's Skin Color (1 = Lightest, 11 = Darkest)",  y = "Probability of reporting 'Many times'") +
  theme_minimal() +   theme(strip.text = element_text(face = "bold", size = 11), panel.grid.minor = element_blank(),
                            panel.spacing = unit(1.5, "lines"), legend.position = "bottom")
ggsave("Fig5b.png", path = fold_graphs, width = 25, height = 15, units = "cm")

## Plotting the correlations across countries

dat_reg$cor_pais <- error_structure(mF, type = "corr")
avg_cor <- mean(dat_reg$cor_pais)

cors_coef <- dat_reg[, c("pais", "cor_pais")] %>% 
  as.matrix() %>% 
  unique() %>% 
  as.data.frame()

sum_mF <- summary(mF)
cors_coef$sd <- sum_mF$error.structure[, "Std. Error"]

cors_coef <- cors_coef %>%
  mutate(pais_label = dplyr::recode(as.character(pais), !!!country_labels), pais_label = factor(pais_label),
         cor_pais = as.numeric(cor_pais), 
         sd = as.numeric(sd))

cors_coef <- cors_coef %>%
  mutate(theta    = atanh(cor_pais),lower = tanh(theta - 1.96 * sd), upper    = tanh(theta + 1.96 * sd))

poly_cors <- dat_reg %>%
  group_by(pais) %>%
  summarise(
    cor_poly = tryCatch(
      polychor(dis11, dis12, ML = TRUE),   # returns a numeric
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

cors_coef <- cors_coef %>%
  left_join(poly_cors, by = "pais")


ggplot(cors_coef, aes(x = fct_rev(pais_label))) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "coral3", show.legend = F) +
  geom_point(aes(y = cor_pais), size = 4, color = "coral4") +
  geom_hline(yintercept = avg_cor, linetype = "dashed", color = "darkslategray4", linewidth = 1) +
  theme_minimal() + labs(x = "Country", y = "Estimate") + coord_flip() + ylim(0, 1)
ggsave("Fig5a_Cor.png", path = fold_graphs, width = 12, height = 15, units = "cm")

### Correlation decomposition

b <- coef(mF)
b1 <- b[grepl(" 1$", names(b))]; names(b1) <- sub(" 1$", "", names(b1))
b2 <- b[grepl(" 2$", names(b))]; names(b2) <- sub(" 2$", "", names(b2))

X0 <- model.matrix(~ 0 + colorr + sex + etid, data = dat_reg)
common_names <- intersect(names(b1), colnames(X0))
b1 <- b1[common_names]; b2 <- b2[common_names]; X0 <- X0[, common_names]

compute_country_stats <- function(pais_code) {
  idx <- dat_reg$pais == pais_code
  Sigma <- cov(X0[idx, , drop = FALSE])
  c(V1 = as.numeric(t(b1) %*% Sigma %*% b1),V2 = as.numeric(t(b2) %*% Sigma %*% b2), crosscov = as.numeric(t(b1) %*% Sigma %*% b2))
}

pais_codes <- as.character(unique(dat_reg$pais))
stats_df <- do.call(rbind, lapply(pais_codes, compute_country_stats)) %>%
  as.data.frame() %>%
  mutate(pais = pais_codes)

dat_reg$cor_pais <- error_structure(mF, type = "corr")
cor_m0 <- dat_reg[, c("pais", "cor_pais")] %>%
  as.matrix() %>%
  unique() %>% 
  as.data.frame() %>%
  mutate(cor_pais = as.numeric(cor_pais), pais = as.character(pais))

poly_cors <- dat_reg %>%
  group_by(pais) %>%
  summarise(cor_poly = polychor(dis11, dis12, ML = TRUE), .groups = "drop") %>%
  mutate(pais = as.character(pais))

full <- stats_df %>%
  left_join(cor_m0, by = "pais") %>%
  left_join(poly_cors, by = "pais") %>%
  mutate(pais_label = dplyr::recode(pais, !!!country_labels), predicted_raw  = (crosscov + cor_pais) / sqrt((V1 + 1) * (V2 + 1)),
    actual_raw  = cor_poly, diff  = predicted_raw - actual_raw, suppression = cor_pais > cor_poly) %>%
  dplyr::select(pais_label, V1, V2, crosscov, cor_pais, cor_poly, predicted_raw, diff, suppression) %>%
  arrange(suppression, V1)

df <- as.data.frame(full) %>%
  mutate(denom = sqrt((V1 + 1) * (V2 + 1)), rho = predicted_raw * denom - crosscov, num = crosscov + rho)

ggplot(df, aes(x = V1, y = crosscov)) +
  geom_point(aes(fill = predicted_raw, size = V2), shape = 21, color = "black", alpha = 0.9) +
  geom_text_repel(aes(label = pais_label), size = 3.8, max.overlaps = 20) +
  scale_fill_viridis_c(option = "mako",  name = "Predicted\nCorrelation") +
  scale_size_continuous(range = c(3, 7), name = expression("Gender Var (" * V[g] * ")")) +
  labs(x = expression("Skin color Var("*V[s]*")"),y = expression("Explained correlation( "* C[p]*")")) +
  theme_minimal() +
  theme(legend.position = "right",panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 10))
ggsave("Fig5b_Cor.png", path = fold_graphs, width = 17, height = 15, units = "cm")
