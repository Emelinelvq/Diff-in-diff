# Packages ----

library(dplyr)
library(multiwayvcov)
library(tidyr)
library(lmtest)
library(sandwich)


# Load data ----

load("df_group3.Rda")
df <- df_group3

#Early stage analysis ----
## Question 1: Given the description of the policy, build a variable Gi equal to 1 if municipality i belongs to a treated province and 0 otherwise. Run a descriptive statistic analysis to compare the groups {G = 0} and {G = 1}. Are these groups very different in terms of outcomes? If so, is it a problem? Are these groups very different in terms of the other variables? If so, is it a problem? ----

### création de G ----

df <- df %>%
  mutate(G = ifelse(province_index %in% c(2, 3, 6), 1, 0))

### Comparaison des groupes - avant traitement - 2016 -----

analyse_2016 <- df %>%
  group_by(G) %>%
  summarise(
    nb_communes = n(),
    moyenne_conso_2016 = mean(ln_avg_household_organic_2016, na.rm = TRUE),
    part_urbain = mean(urban, na.rm = TRUE),
    part_cantine_2016 = mean(organic_school_canteen, na.rm = TRUE)
  )

print(analyse_2016)


# pas de grosses différences au niveau du nombre de municipalités par groupe, ni de la consommation de bio moyenne des ménages. Par contre une différence un peu plus marquée au niveau de la présence de bio dans les cantines en 2016 (davantage de bio dans les muninicpalités traitées). La plus grande différence entre les deux groupes est la proportion de municipalités urbaines dans chaque groupe (le groupe traitement est beaucoup plus urbain avec 58% contre 25% dans le groupe contrôle). Ayant besoin de deux groupes similaires pour évaluer les effets de la politique, cette différence peut être un problème pour comparer les deux groupes. Il faudra probablement utiliser une CATT . 


# Analysis without covariates ----

## Pseudo-parallel trend tests ----

### Question 3: Give the definition of the ATT in 2018 and 2019. Suppose we want to use a DID approach without covariates for the moment. How and under which assumption can these ATTs be identified from the observed data? ----

#Pour les années post-traitement t appartient à {2018, 2019}, l'ATT se définit formellement par :ATT_t = E[Y_t(1) - Y_t(0) | G = 1]. Où :Y_t(1) est la consommation  de bio (en log) à l'année t avec la campagne publicitaire. Y_t(0) est la consommation  à l'année t en l'absence de campagne publicitaire. Dans les deux cas nous sommes dans le cas G = 1 qui correspond au groupe traité. Y_t(0) n'est donc pas observable dans ce cas.

# Y_t(0) n'étant pas observable, pour identifier l'ATT il faut poser l'hypothèse des unconditional parallel trends (UPT), selon laquelle les deux groupes (traité et contrôle) auraient suivi la même trajectoire. C'est-à-dire que l'évolution moyenne de l'outcome entre la période pré-traitement (2017) et la période post-traitement (2018 ou 2019) aurait été la même dans les deux groupes. 
#Formellement, pour chaque année t appartient à {2018, 2019\} : E[Y_t(0) - Y_{2017}(0) | G = 1] = E[Y_t(0) - Y_{2017}(0) | G = 0]

#Grâce à l'hypothèse UPT, on peut remplacer l'évolution contrefactuelle inobservable du groupe traité par l'évolution observée du groupe de contrôle. Ainsi, l'ATT devient identifiable à partir des données observées (les variables Y).
#l'ATT est identifié par la "différence des différences" :ATT_t = E(Y_t - Y_{2017} | G = 1) - E(Y_t - Y_{2017} | G = 0)

### Question 4: Explain how you can perform a pseudo test of the parallel trend assumptions between groups {G = 0} and {G = 1} (caution: for each ATT, there is a corresponding UPT assumption to pseudo-test). Write down the tests you want to implement, implement them and comment. ----

# L'hypothèse UPT ne peut pas être testée directement sur les années post-traitement (2018 et 2019) car nous n'observons pas l'outcome sans traitement Y_t(0) pour les municipalités traitées. Cependant, puisque nous disposons des données pré-traitement (2016-2017), nous pouvons effectuer un pseudo-test de l'UPT. De manière générale, il est préférable de réaliser plusieurs pseudo-tests de l'UPT sur les différentes périodes pré-traitement, afin d'être sûr de l'évolution similaire des deux groupes.
# Dans notre cas, nous n'avons accès aux données que de deux périodes pré-traitement (2016,2017) ce qui nous permet d'effectuer un seul pseudo-test sur l'évolution entre celles-ci. Si l'UPT est vérifiée sur la période étudiée, alors nous estimerons qu'il est possible que les deux groupes évoluent de manière similaire, et nous partirons de cette (forte) hypothèse pour développer notre analyse. 

#VErsion 1

# 1. Calcul de la différence de consommation avant le traitement
df$delta_Y_pre <- df$ln_avg_household_organic_2017 - df$ln_avg_household_organic_2016

# 2. Régression (Pseudo-test UPT)
# On régresse cette évolution sur l'appartenance au groupe traité (G)
pseudo_test_model <- lm(delta_Y_pre ~ G, data = df)

# 3. Affichage des résultats avec des erreurs standards robustes
coeftest(pseudo_test_model, vcov = vcovHC(pseudo_test_model, type = "HC1"))

#version 2

# On crée un ID allant de 1 à 5000 (le nombre de communes)
df$id <- 1:nrow(df)

# 1. Sélection des données pour le pseudo-test (2016 et 2017 sont pré-traitement)
df_long <- df %>% 
  select(id, G, ln_avg_household_organic_2016, ln_avg_household_organic_2017, ln_avg_household_organic_2018, ln_avg_household_organic_2019)

# 2. Passage au format LONG
panel_df <- df_long %>%
  pivot_longer(cols = c(ln_avg_household_organic_2016, ln_avg_household_organic_2017, ln_avg_household_organic_2018, ln_avg_household_organic_2019),
               names_to = "year", 
               values_to = "ln_organic")

# 3. Recodage de la variable temps (0 pour 2016, 1 pour 2017)
panel_df <- panel_df %>%
  mutate(time_pseudo = ifelse(year == "ln_avg_household_organic_2016", 0, 1),
         time_ATT_2018=ifelse(year == "ln_avg_household_organic_2018", 1, 0),
         time_ATT_2019=ifelse(year == "ln_avg_household_organic_2019", 1, 0)  )

# 4. Régression DID pour le pseudo-test

pseudo_reg <- lm(ln_organic ~ G * time_pseudo, data = panel_df)

# 5. Calcul des erreurs standards robustes (Clustered at municipality level)

cov_pseudo <- vcovCL(pseudo_reg, cluster = ~id)
res_pseudo <- coeftest(pseudo_reg, vcov = cov_pseudo)

print(res_pseudo)

# Nous obtenons un coefficient proche de 0 (-0,002), et une significativité assez élevée (p-value de 0,69). Nous ne sommes donc pas en mesure de refuser l’hypothèse des pentes parallèles. Concrètement, cela signifie que rien ne prouve que les deux groupes ne suivaient pas une évolution similaire entre 2016 et 2017. Nous pouvons donc faire une estimation sous l’hypothèse des pentes parallèles pour les années 2018 et 2019.
#

## Average treatment effect estimation ----

### Question 5: Estimate the ATTs and build a 95% confidence interval for each estimate. Detail carefully the estimation and inference method used. Interpret the results. ----

#### ATT 2017-2018 - version "manuelle" ----

df <- df %>%
  mutate(delta_organic_2018 = ln_avg_household_organic_2018 - ln_avg_household_organic_2017)


mean_delta_organic_treated_2018 <- 
  df |>
  subset( G == 1, select = delta_organic_2018, drop = TRUE) |>
  mean()

mean_delta_organic_control_2018 <- 
  df |>
  subset(G == 0, select = delta_organic_2018, drop = TRUE) |>
  mean()

# Computation of ATT 

ATT_UPT_2018 <- mean_delta_organic_treated_2018 - mean_delta_organic_control_2018
print(ATT_UPT_2018)


##### ATT 2018 avec régression trick ----

# Sélection des données et format long
data_18_select <- df %>% select(id, G, ln_avg_household_organic_2017, ln_avg_household_organic_2018)

panel_18 <- as.data.frame(pivot_longer(data_18_select, -c(id, G), 
                                       values_to = "ln_organic", names_to = "year"))

# Recodage du temps : 0 pour l'année de référence, 1 pour l'année traitée
panel_18$year[panel_18$year == "ln_avg_household_organic_2017"] <- 0
panel_18$year[panel_18$year == "ln_avg_household_organic_2018"] <- 1
panel_18$year <- as.numeric(panel_18$year)

# Régression DID
reg_18 <- lm(ln_organic ~ G * year, data = panel_18)

# Inférence (Clustered SEs)
cov_18 <- cluster.vcov(reg_18, panel_18$id, df_correction = FALSE)
se_18 <- sqrt(diag(cov_18))[4]
att_18 <- coef(reg_18)[4]

# mêmes résultats qu'avec la version manuelle = OK

##### ATT 2019 (Comparaison 2017 vs 2019) avec régression trick ----

# Sélection des données et format long
data_19_select <- df %>% select(id, G, ln_avg_household_organic_2017, ln_avg_household_organic_2019)

panel_19 <- as.data.frame(pivot_longer(data_19_select, -c(id, G), 
                                       values_to = "ln_organic", names_to = "year"))

# Recodage
panel_19$year[panel_19$year == "ln_avg_household_organic_2017"] <- 0
panel_19$year[panel_19$year == "ln_avg_household_organic_2019"] <- 1
panel_19$year <- as.numeric(panel_19$year)

# Régression
reg_19 <- lm(ln_organic ~ G * year, data = panel_19)

# Inférence
cov_19 <- cluster.vcov(reg_19, panel_19$id, df_correction = FALSE)
se_19 <- sqrt(diag(cov_19))[4]
att_19 <- coef(reg_19)[4]

##### Calcul des Intervalles de Confiance à 95% ----
alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)

ic_18 <- c(att_18 - z_alpha * se_18, att_18 + z_alpha * se_18)
ic_19 <- c(att_19 - z_alpha * se_19, att_19 + z_alpha * se_19)

# Affichage des résultats
cat("ATT 2018 :", round(att_18, 4), "SE :", round(se_18, 4), "IC : [", round(ic_18[1], 4), ";", round(ic_18[2], 4), "]\n")
cat("ATT 2019 :", round(att_19, 4), "SE :", round(se_19, 4), "IC : [", round(ic_19[1], 4), ";", round(ic_19[2], 4), "]\n")


### Question 6 : Given the results of Questions 4 and 5, explain whether a DID approach without covariates seems relevant here. ----

# Bien que le pseudo test semble valider l'hypothèse des pentes parallèles, le fait que le groupe traitement soit plus urbain que le groupe contrôle pourrait jouer un rôle sur la différence observée de consommation de bio, et donc sur les résultats. 
# Utiliser des covariables permettraient d'avoir des résultats plus fiables en éliminant l'effet des facteurs socio-démographiques. 


# Analysis with covariates ----


## Question 7: Write down the parallel trend assumptions required to identify and estimate the ATTs when one uses a DID approach with covariates. What is the other assumption needed to run a DID analysis with covariates? Verify this second assumption in the data. ----

# vérification de la distribution des covariables 

check_distribution <- df %>%
  group_by(G, urban, organic_school_canteen) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = G, values_from = n, names_prefix = "G_")

print(check_distribution)

# pour chaque profil de municipalités du groupe traité il y a bien un profil similaire dans le groupe controle. On peut donc procéder à l'ATT sous CPT


