# ============================================================
# ERFI 2005 - Méga-base Genre & relations familiales (TD L2)
# Script unique et autonome à exécuter dans RStudio
# ============================================================

# 0. Packages -------------------------------------------------
# (si besoin : install.packages("tidyverse"); install.packages("scales"))
library(tidyverse)
library(scales)

# 1. Import de la base brute ----------------------------------
# Le fichier ERFI_data.csv doit être dans le dossier de travail.
erfi <- read_csv("ERFI_data.csv", show_col_types = FALSE) %>%
  # ID unique par répondant pour relier toutes les tables
  mutate(id = row_number())

message("\nAperçu d'ERFI :")
glimpse(erfi)
message("Dimensions : ", paste(dim(erfi), collapse = " x "))

# 2. Sous-base : personnes en couple --------------------------
# - conjoint dans le ménage
# - 18-79 ans
# - sexe renseigné

erfi_couple <- erfi %>%
  filter(
    EA_VERIFC == 1,
    between(MA_AGEM_rec, 18, 79),
    !is.na(MA_SEXE)
  ) %>%
  mutate(
    # Recodages lisibles pour l'analyse
    sexe = recode_factor(MA_SEXE, `1` = "Homme", `2` = "Femme"),
    age = MA_AGEM_rec,
    age_cat = case_when(
      between(age, 18, 30) ~ "Jeunes (18-30)",
      between(age, 31, 55) ~ "Adultes (31-55)",
      age >= 56            ~ "Aînés (56+)",
      TRUE                 ~ NA_character_
    ),
    # Division des tâches domestiques (exemple : repas)
    division_repas = case_when(
      OA_REPAS %in% 1:2 ~ "Plutôt moi",
      OA_REPAS == 3     ~ "Égalitaire",
      OA_REPAS %in% 4:5 ~ "Plutôt conjoint",
      TRUE ~ NA_character_
    ),
    # Gestion des revenus
    gestion_revenus = case_when(
      OB_GESTION %in% 1:2 ~ "Un seul gère",
      OB_GESTION %in% 3:4 ~ "Gestion commune",
      OB_GESTION == 5     ~ "Séparée",
      TRUE ~ NA_character_
    ),
    # Habillage des enfants (si concerné·es)
    division_habillage = case_when(
      EA_HAB %in% 1:2 ~ "Plutôt moi",
      EA_HAB == 3     ~ "Égalitaire",
      EA_HAB %in% 4:5 ~ "Plutôt conjoint",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    age_cat            = factor(age_cat, levels = c("Jeunes (18-30)", "Adultes (31-55)", "Aînés (56+)")),
    division_repas     = factor(division_repas, levels = c("Plutôt moi", "Égalitaire", "Plutôt conjoint")),
    gestion_revenus    = factor(gestion_revenus, levels = c("Un seul gère", "Gestion commune", "Séparée")),
    division_habillage = factor(division_habillage, levels = c("Plutôt moi", "Égalitaire", "Plutôt conjoint"))
  )

message("\nSous-base erfi_couple :")
glimpse(erfi_couple)

# 3. Méga-base OA_* (tâches domestiques) en format long -------

# 3.1. Sélection des OA_* pour les personnes en couple
erfi_couple_oa <- erfi_couple %>%
  select(id, sexe, age, age_cat, starts_with("OA_"))

oa_vars <- erfi_couple_oa %>%
  select(starts_with("OA_")) %>%
  names()

message("\nVariables OA_* détectées :")
print(oa_vars)

# 3.2. Recode générique des tâches OA_ (même logique que OA_REPAS)
recode_tache <- function(x) {
  case_when(
    x %in% 1:2 ~ "Plutôt moi",
    x == 3     ~ "Égalitaire",
    x %in% 4:5 ~ "Plutôt conjoint",
    TRUE ~ NA_character_
  )
}

erfi_oa_long <- erfi_couple_oa %>%
  pivot_longer(
    cols = all_of(oa_vars),
    names_to = "tache",
    values_to = "code_tache"
  ) %>%
  mutate(
    repartition = recode_tache(code_tache),
    repartition = factor(repartition, levels = c("Plutôt moi", "Égalitaire", "Plutôt conjoint")),
    tache       = factor(tache, levels = oa_vars)
  )

message("\nMéga-base erfi_oa_long (1 ligne = 1 personne x 1 tâche) :")
glimpse(erfi_oa_long)

# 3.3. Score d'égalité sur toutes les tâches OA_* par personne
score_egal_OA_personne <- erfi_oa_long %>%
  mutate(
    egal_tache = case_when(
      repartition == "Égalitaire" ~ 1,
      repartition %in% c("Plutôt moi", "Plutôt conjoint") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(egal_tache) %>%
  group_by(id, sexe, age_cat) %>%
  summarise(
    nb_taches    = n(),
    score_egal_OA = mean(egal_tache),
    .groups = "drop"
  )

# 3.4. On rattache ce score global à la base couple
erfi_couple <- erfi_couple %>%
  left_join(score_egal_OA_personne %>% select(id, score_egal_OA), by = "id")

# 4. Scores d'égalité repas + revenus --------------------------
erfi_couple <- erfi_couple %>%
  mutate(
    egal_repas = case_when(
      division_repas == "Égalitaire" ~ 1,
      division_repas %in% c("Plutôt moi", "Plutôt conjoint") ~ 0,
      TRUE ~ NA_real_
    ),
    egal_gestion = case_when(
      gestion_revenus == "Gestion commune" ~ 1,
      gestion_revenus %in% c("Un seul gère", "Séparée") ~ 0,
      TRUE ~ NA_real_
    ),
    score_egal_2d = egal_repas + egal_gestion    # 0 à 2
  )

# 5. TABLEAUX DE RÉFÉRENCE ------------------------------------

# 5.1. Gestion des revenus par âge
resume_gestion_age <- erfi_couple %>%
  drop_na(age_cat, gestion_revenus) %>%
  count(age_cat, gestion_revenus, name = "effectif") %>%
  group_by(age_cat) %>%
  mutate(pct = round(effectif / sum(effectif) * 100, 1)) %>%
  arrange(age_cat, gestion_revenus)

message("\nGestion des revenus par âge :")
print(resume_gestion_age)

# 5.2. Repas par âge et sexe
table_repas_age_sexe <- erfi_couple %>%
  drop_na(age_cat, sexe, division_repas) %>%
  count(age_cat, sexe, division_repas, name = "effectif") %>%
  group_by(age_cat, sexe) %>%
  mutate(pct = round(effectif / sum(effectif) * 100, 1)) %>%
  arrange(age_cat, sexe, division_repas)

message("\nPréparation des repas par âge et sexe :")
print(table_repas_age_sexe)

# 5.3. Score égalité 2D (repas + revenus) par âge et sexe
score_egal_2d_age_sexe <- erfi_couple %>%
  drop_na(age_cat, sexe, score_egal_2d) %>%
  group_by(age_cat, sexe) %>%
  summarise(
    n = n(),
    score_moyen_2d = round(mean(score_egal_2d), 2),
    .groups = "drop"
  )

message("\nScore égalité (repas+revenus) par âge et sexe :")
print(score_egal_2d_age_sexe)

# 5.4. Score égalité OA_* global par âge et sexe
score_egal_OA_age_sexe <- score_egal_OA_personne %>%
  group_by(age_cat, sexe) %>%
  summarise(
    n = n(),
    score_moyen_OA = round(mean(score_egal_OA), 3),
    .groups = "drop"
  )

message("\nScore égalité OA_* par âge et sexe :")
print(score_egal_OA_age_sexe)

# 5.5. Part de réponses égalitaires par tâche et génération
resume_OA_egal_age <- erfi_oa_long %>%
  drop_na(age_cat, repartition) %>%
  group_by(tache, age_cat) %>%
  summarise(
    pct_egal = round(mean(repartition == "Égalitaire") * 100, 1),
    .groups = "drop"
  )

message("\nPart de réponses égalitaires par tâche et génération :")
print(resume_OA_egal_age)

# 6. >= 10 GRAPHIQUES -----------------------------------------

# G1 : distribution de l'âge par sexe
G1 <- erfi_couple %>%
  ggplot(aes(x = age, fill = sexe)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(
    title = "Distribution de l'âge des personnes en couple",
    x = "Âge",
    y = "Effectif",
    fill = "Sexe"
  ) +
  theme_minimal()

# G2 : préparation des repas selon l'âge
G2 <- erfi_couple %>%
  drop_na(age_cat, division_repas) %>%
  ggplot(aes(x = age_cat, fill = division_repas)) +
  geom_bar(position = "fill") +
  labs(
    title = "Préparation des repas selon l'âge",
    x = "Classe d'âge",
    y = "Proportion de répondant·es",
    fill = "Préparation des repas"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

# G3 : gestion des revenus selon l'âge
G3 <- erfi_couple %>%
  drop_na(age_cat, gestion_revenus) %>%
  ggplot(aes(x = age_cat, fill = gestion_revenus)) +
  geom_bar(position = "fill") +
  labs(
    title = "Gestion des revenus selon l'âge",
    x = "Classe d'âge",
    y = "Proportion de répondant·es",
    fill = "Gestion des revenus"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

# G4 : préparation des repas selon sexe, facetté par âge
G4 <- erfi_couple %>%
  drop_na(sexe, age_cat, division_repas) %>%
  ggplot(aes(x = sexe, fill = division_repas)) +
  geom_bar(position = "fill") +
  facet_wrap(~ age_cat) +
  labs(
    title = "Préparation des repas selon le sexe et la génération",
    x = "Sexe", y = "Proportion", fill = "Préparation des repas"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

# G5 : gestion des revenus selon sexe, facetté par âge
G5 <- erfi_couple %>%
  drop_na(sexe, age_cat, gestion_revenus) %>%
  ggplot(aes(x = sexe, fill = gestion_revenus)) +
  geom_bar(position = "fill") +
  facet_wrap(~ age_cat) +
  labs(
    title = "Gestion des revenus selon le sexe et la génération",
    x = "Sexe", y = "Proportion", fill = "Gestion des revenus"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

# G6 : score égalité 2D (repas+revenus) par âge & sexe
G6 <- score_egal_2d_age_sexe %>%
  ggplot(aes(x = age_cat, y = score_moyen_2d, color = sexe, group = sexe)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Score d'égalité (repas + revenus) par génération et sexe",
    x = "Classe d'âge",
    y = "Score moyen (0-2)",
    color = "Sexe"
  ) +
  theme_minimal()

# G7 : score égalité OA_* global par âge & sexe
G7 <- score_egal_OA_age_sexe %>%
  ggplot(aes(x = age_cat, y = score_moyen_OA, color = sexe, group = sexe)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Score moyen d'égalité (toutes tâches OA_)",
    x = "Classe d'âge",
    y = "Score moyen (0 = très inégalitaire, 1 = très égalitaire)",
    color = "Sexe"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()

# G8 : heatmap part d'égalité par tâche et génération
G8 <- resume_OA_egal_age %>%
  ggplot(aes(x = age_cat, y = tache, fill = pct_egal)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Part de répartition égalitaire par tâche domestique et génération",
    x = "Classe d'âge", y = "Tâche OA_*", fill = "% égalitaire"
  ) +
  theme_minimal()

# G9 : distribution du score égalité OA_* par génération
G9 <- score_egal_OA_personne %>%
  ggplot(aes(x = age_cat, y = score_egal_OA)) +
  geom_boxplot() +
  labs(
    title = "Distribution du score d'égalité OA_* par génération",
    x = "Classe d'âge",
    y = "Score d'égalité (0-1)"
  ) +
  theme_minimal()

# G10 : nombre de tâches renseignées par âge et sexe
G10 <- score_egal_OA_personne %>%
  ggplot(aes(x = age_cat, y = nb_taches, fill = sexe)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Nombre de tâches domestiques renseignées par génération et sexe",
    x = "Classe d'âge",
    y = "Nombre de tâches OA_*",
    fill = "Sexe"
  ) +
  theme_minimal()

# Affichage des graphiques (RStudio les montrera dans l'onglet Plots)
print(G1); print(G2); print(G3); print(G4); print(G5)
print(G6); print(G7); print(G8); print(G9); print(G10)

# 7. Tests de chi² (optionnels mais utiles pour l'écrit) ------
chisq_repas_age <- erfi_couple %>%
  drop_na(age_cat, division_repas) %>%
  xtabs(~ age_cat + division_repas) %>%
  chisq.test()

chisq_gestion_age <- erfi_couple %>%
  drop_na(age_cat, gestion_revenus) %>%
  xtabs(~ age_cat + gestion_revenus) %>%
  chisq.test()

chisq_repas_age
chisq_gestion_age

# 8. Comment exécuter ce script dans RStudio -------------------
# 1) File -> New File -> R Script
# 2) Coller l'intégralité de ce script (par ex. erfi_mega.R) dans le même dossier
#    que ERFI_data.csv
# 3) Cliquer sur "Source" (en haut à gauche)
# 4) Dans l'onglet Environment : erfi, erfi_couple, erfi_oa_long, etc.
# 5) Dans Plots : les graphiques G1 à G10 s'affichent automatiquement
#
# Note : geom_bar(position = "fill") calcule les effectifs puis les transforme
# en proportions (0-1) sans aes(y =) explicite, d'où son intérêt pour les barres
# empilées en pourcentage.
