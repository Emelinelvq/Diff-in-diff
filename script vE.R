# Packages ----

library(dplyr)
library(multiwayvcov)
library(tidyr)


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


# pas de grosses différences au niveau du nombre de municipalités par groupe, ni de la consommation de bio moyenne des ménages. Par contre une différence un peu plus marquée au niveau de la présence de bio dans les cantines en 2016 (davantage de bio dnas les muniicpalités traitées). La plus grande différence entre les deux groupes est la proportion de municipalités urbaines dans chaque groupe (le groupe 1 est beaucoup plus urbain avec 58% contre 25% dans le groupe 2). Ayant besoin de deux groupes similaires pour évaluer les effets de la politique, cette différence peut être un problème pour comparer les deux groupes. Il faudra probablement utiliser une CATT . 

# on peut aussi regarder les différences d'évolution entre 2016 et 2017

analyse_tendances <- df %>%
  mutate(evol_16_17 = ln_avg_household_organic_2017 - ln_avg_household_organic_2016) %>%
  group_by(G) %>%
  summarise(
    nb_communes = n(),
    tendance_moyenne = mean(evol_16_17, na.rm = TRUE)
  )

print(analyse_tendances)


# Analysis without covariates ----

## Pseudo-parallel trend tests ----

### Question 3: Give the definition of the ATT in 2018 and 2019. Suppose we want to use a DID approach without covariates for the moment. How and under which assumption can these ATTs be identified from the observed data? ----

#Pour les années post-traitement t appartient à {2018, 2019}, l'ATT se définit formellement par :ATT_t = E[Y_t(1) - Y_t(0) | G = 1]. Où :Y_t(1) est la consommation  de bio (en log) à l'année t avec la campagne publicitaire. Y_t(0) est la consommation  à l'année t en l'absence de campagne publicitaire. Dans les deux cas nous sommes dans le cas G = 1 qui correspond au groupe traité. Y_t(0) n'est donc pas observable dans ce cas.

# Y_t(0) n'étant pas observable, pour identifier l'ATT il faut poser l'hypothèse des unconditional parallel trends (UPT), selon laquelle les deux groupes (traité et contrôle) auraient suivi la même trajectoire. C'est-à-dire que l'évolution moyenne de l'outcome entre la période pré-traitement (2017) et la période post-traitement (2018 ou 2019) aurait été la même dans les deux groupes. 
#Formellement, pour chaque année t appartient à {2018, 2019\} : E[Y_t(0) - Y_{2017}(0) | G = 1] = E[Y_t(0) - Y_{2017}(0) | G = 0]

#Grâce à l'hypothèse UPT, on peut remplacer l'évolution contrefactuelle inobservable du groupe traité par l'évolution observée du groupe de contrôle. Ainsi, l'ATT devient identifiable à partir des données observées (les variables Y).
#l'ATT est identifié par la "différence des différences" :ATT_t = E(Y_t - Y_{2017} | G = 1) - E(Y_t - Y_{2017} | G = 0)

### Question 4: Explain how you can perform a pseudo test of the parallel trend assumptions between groups {G = 0} and {G = 1} (caution: for each ATT, there is a corresponding UPT assumption to pseudo-test). Write down the tests you want to implement, implement them and comment. ----

# L'hypothèse UPT ne peut pas être testée directement sur les années post-traitement (2018 et 2019) car nous n'observons pas l'outcome sans traitement Y_t(0) pour les municipalités traitées.Cependant, puisque nous disposons des données pour l'année 2016, nous pouvons effectuer un pseudo-test de l'UPT. Si l'UPT est vérifiée dans la période pré-traitement (2016-2017), alors cela est un bon signe et donne de la crédibilité à l'hypothèse d'UPT pour les périodes suivantes pour. 

#jsp trop ce qu'il veut dire avec "for each ATT, there is a corresponding UPT assumption to pseudo-test". peut etre que tester 2016-2017 permet de vérifier l'att entre 2017 et 2018 mais pas entre 2017 et 2019 ? mais jsp ce qu'on peut faire autre que 2016-2017 vu ce qu'on a 








