## =============================================================
## ERFI 2005 – Blocs modulaires pour construire une méga-base
## Objectif : partir d'un data.frame erfi_raw et produire des
##            sous-blocs cohérents (OA_ / OB_ / EA_ / VA_ / OC_)
##            avec scores simples, prêts pour tableaux/graphes.
## =============================================================
## Le script est volontairement paramétrable : adapte la section
## 1. CONFIG si tes noms de variables diffèrent.
## =============================================================

## ================================
## 0. PACKAGES & OPTIONS
## ================================
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(cli)

## ================================
## 1. CONFIG : OBJET & VARIABLES DE BASE
## ================================
# 👉 ADAPTER ICI SI BESOIN
# erfi_raw <- readr::read_csv("ERFI_data.csv")
if (!exists("erfi_raw")) {
  cli::cli_abort(c(
    "x" = "L'objet `erfi_raw` n'est pas chargé dans l'environnement.",
    "i" = "Lis ton fichier (ex. readr::read_csv('ERFI_data.csv')) avant de sourcer ce script."))
}
erfi <- erfi_raw          # nom de ton fichier de base
var_id  <- "id"          # identifiant individu
var_age <- "age"         # variable d'âge (numérique)
var_sex <- "sexe"        # sexe (ex : 1 = H, 2 = F)

# Codes à considérer comme manquants (adapter si besoin)
codes_na <- c(8, 9, 97, 98, 99)

## ================================
## 2. FONCTIONS GÉNÉRIQUES
## ================================

# 2.1. Nettoyer un vecteur : remplace chaque code spécial par NA
default_clean <- function(x, na_codes = codes_na) {
  reduce(na_codes, ~ dplyr::na_if(.x, .y), .init = x)
}

# 2.2. Nettoyer une sélection de colonnes d'un data frame
clean_block <- function(data, cols, na_codes = codes_na) {
  data |>
    mutate(across(all_of(cols), default_clean, na_codes = na_codes))
}

# 2.3. Calculer un score moyen sur plusieurs items numériques
#      (utilise na.rm = TRUE pour ignorer les items manquants)
score_mean <- function(data, cols, new_name) {
  data |>
    mutate("{new_name}" := rowMeans(across(all_of(cols)), na.rm = TRUE))
}

# 2.4. Vérifier que toutes les colonnes existent avant de continuer
require_columns <- function(data, cols, block_name = "") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      c("x" = paste0("Colonnes manquantes pour ", block_name, ": ",
                      paste(missing, collapse = ", ")),
        "i" = "Vérifie la config ou les noms dans le fichier source."))
  }
  data
}

## ================================
## 3. VARIABLES SOCIODÉMO DE RÉFÉRENCE
## ================================

erfi_base <- erfi |>
  rename(id = {{ var_id }}, age = {{ var_age }}, sexe = {{ var_sex }}) |>
  mutate(
    age_cat = case_when(
      age >= 18 & age <= 30 ~ "Jeunes (18-30)",
      age >= 31 & age <= 55 ~ "Adultes (31-55)",
      age >= 56             ~ "Aînés (56+)",
      TRUE                  ~ NA_character_
    ),
    sexe_lib = case_when(
      sexe == 1 ~ "Homme",
      sexe == 2 ~ "Femme",
      TRUE      ~ NA_character_
    )
  )

## ================================
## 4. BLOC TÂCHES DOMESTIQUES (OA_)
## ================================
# Variables visibles dans ton extrait ; ajoute/enlève si besoin
oa_vars <- c(
  "OA_VAISS", "OA_REPAS", "OA_ALIME", "OA_LINGE",
  "OA_ASPIR", "OA_BRICO", "OA_COMPT", "OA_INVIT", "OA_SATREP"
)

erfi_oa <- erfi_base |>
  require_columns(oa_vars, block_name = "OA_*") |>
  select(id, age, age_cat, sexe, sexe_lib, all_of(oa_vars)) |>
  clean_block(oa_vars)

# Recodage simple : 1 = plutôt moi ; 2 = égalitaire ; 3 = plutôt conjoint
# Score 0 = conjoint, 0.5 = égalitaire, 1 = moi
recode_oa <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0.5,
    x == 3 ~ 0,
    TRUE   ~ NA_real_
  )
}

erfi_oa <- erfi_oa |>
  mutate(across(all_of(oa_vars), recode_oa, .names = "{.col}_sc")) |>
  score_mean(cols = paste0(oa_vars, "_sc"), new_name = "score_domestique")

## ================================
## 5. BLOC FINANCES / OBLIGATIONS (OB_)
## ================================
ob_vars <- c("OB_DACHQUO", "OB_DACHEX", "OB_DEDUC", "OB_DLOISIR", "OB_GESTION")

erfi_ob <- erfi_base |>
  require_columns(ob_vars, block_name = "OB_*") |>
  select(id, age, age_cat, sexe, sexe_lib, all_of(ob_vars)) |>
  clean_block(ob_vars) |>
  score_mean(ob_vars, "score_oblig_financieres")

## ================================
## 6. BLOC ENFANTS / AUTORITÉ PARENTALE (EA_)
## ================================
ea_vars <- c("EA_HAB", "EA_LIT", "EA_MAL", "EA_JOUE",
             "EA_AID", "EA_EMM", "EA_SATTACHE")

erfi_ea <- erfi_base |>
  require_columns(ea_vars, block_name = "EA_*") |>
  select(id, age, age_cat, sexe, sexe_lib, all_of(ea_vars)) |>
  clean_block(ea_vars) |>
  score_mean(ea_vars, "score_engagement_parental")

## ================================
## 7. BLOC VALEURS (VA_)
## ================================
va_vars <- names(erfi_base)[str_detect(names(erfi_base), "^VA_")]

erfi_va <- erfi_base |>
  select(id, age, age_cat, sexe, sexe_lib, all_of(va_vars)) |>
  clean_block(va_vars)

va_parite  <- c("VA_FEMENF", "VA_HOMENF", "VA_DEUXPAR",
               "VA_MERSEUL", "VA_ENRESPAR", "VA_ENFAIDPAR")
va_famille <- c("VA_MARIDEP", "VA_MARITJS", "VA_DIVORC", "VA_ENFCH")

erfi_va <- erfi_va |>
  score_mean(intersect(va_parite, va_vars),  "score_valeurs_genre") |>
  score_mean(intersect(va_famille, va_vars), "score_valeurs_famille")

## ================================
## 8. (OPTION) CONFLITS / RELATION (OC_)
## ================================
oc_vars <- names(erfi_base)[str_detect(names(erfi_base), "^OC_")]

erfi_oc <- erfi_base |>
  select(id, age, age_cat, sexe, sexe_lib, all_of(oc_vars)) |>
  clean_block(oc_vars) |>
  score_mean(oc_vars, "score_relation_couple")

## ================================
## 9. FUSION : MÉGA-BASE INDIVIDUELLE
## ================================
mega_base <- erfi_base |>
  select(id, age, age_cat, sexe, sexe_lib) |>
  left_join(erfi_oa |> select(-age, -sexe, -sexe_lib), by = "id") |>
  left_join(erfi_ob |> select(-age, -sexe, -sexe_lib), by = "id") |>
  left_join(erfi_ea |> select(-age, -sexe, -sexe_lib), by = "id") |>
  left_join(erfi_va |> select(-age, -sexe, -sexe_lib), by = "id") |>
  left_join(erfi_oc |> select(-age, -sexe, -sexe_lib), by = "id")

## ================================
## 10. TABLEAUX D'EXEMPLE PAR ÂGE & SEXE
## ================================

# Score domestique moyen par âge & sexe
# (utile pour regarder l'orientation globale de la division du travail)
tab_domestique <- mega_base |>
  group_by(age_cat, sexe_lib) |>
  summarise(
    n = n(),
    score_domestique_moy = mean(score_domestique, na.rm = TRUE),
    .groups = "drop"
  )

# Score valeurs de genre par âge & sexe
tab_valeurs_genre <- mega_base |>
  group_by(age_cat, sexe_lib) |>
  summarise(
    n = n(),
    score_valeurs_genre_moy = mean(score_valeurs_genre, na.rm = TRUE),
    .groups = "drop"
  )

# Score obligations financières par âge & sexe
tab_finances <- mega_base |>
  group_by(age_cat, sexe_lib) |>
  summarise(
    n = n(),
    score_oblig_financieres_moy = mean(score_oblig_financieres, na.rm = TRUE),
    .groups = "drop"
  )

## ======================================================================
## NOTES D'USAGE
## ======================================================================
# - Si tu as d'autres blocs (p. ex. variables professionnelles), duplique
#   le schéma : définir le vecteur de variables, clean_block, score_mean.
# - Les scores créés ici sont des moyennes simples pour garder l'échelle
#   originale ; adapte-les si tu as besoin de pondérations.
# - La fonction require_columns() lève une erreur claire si une variable
#   est absente : pratique pour éviter des NA inattendus en silence.
# - Les tableaux tab_* sont des exemples : recycle le même pattern pour
#   d'autres scores (count(), summarize(), group_by()).
# - Tous les noms sont modifiables en haut de fichier pour être compatibles
#   avec d'autres bases qu'ERFI.
