# ============================================================
# ERFI 2005 – Refactored megascript with modular pipeline
# Run in RStudio with run_erfi_pipeline()
# ============================================================

# Packages ----------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(scales)
})

# Default configuration --------------------------------------
default_config <- list(
  var_age = "MA_AGEM_rec",
  var_sex = "MA_SEXE",
  var_couple_flag = "EA_VERIFC",
  codes_na = c(8, 9, 97, 98, 99)
)

# Utility helpers --------------------------------------------
# Clean special missing codes for a vector
clean_na <- function(x, na_codes) {
  reduce(na_codes, ~ dplyr::na_if(.x, .y), .init = x)
}

# Clean a set of columns in a data.frame
clean_block <- function(data, cols, na_codes) {
  mutate(data, across(all_of(cols), clean_na, na_codes = na_codes))
}

# Compute a row-wise mean score across selected columns
score_mean <- function(data, cols, new_name) {
  mutate(data, "{new_name}" := rowMeans(across(all_of(cols)), na.rm = TRUE))
}

# Check that required columns exist; warn and return NULL otherwise
require_columns <- function(data, cols, block_name) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    warning(paste0("Missing columns for ", block_name, ": ", paste(missing_cols, collapse = ", ")))
    return(NULL)
  }
  data
}

# Build age category and labelled sex
add_age_sex_labels <- function(data, config) {
  data |>
    mutate(
      age = .data[[config$var_age]],
      sexe = .data[[config$var_sex]],
      age_cat = case_when(
        age >= 18 & age <= 30 ~ "Jeunes (18-30)",
        age >= 31 & age <= 55 ~ "Adultes (31-55)",
        age >= 56 ~ "Aînés (56+)",
        TRUE ~ NA_character_
      ),
      sexe_lib = case_when(
        sexe == 1 ~ "Homme",
        sexe == 2 ~ "Femme",
        TRUE ~ NA_character_
      )
    )
}

# File loader with guardrails
load_data <- function(input_file, config) {
  if (!file.exists(input_file)) {
    warning(paste0("File not found: ", input_file))
    return(NULL)
  }
  readr::read_csv(input_file, show_col_types = FALSE) |>
    mutate(id = row_number())
}

# Build base with age/sex, handling missing codes
build_base <- function(erfi_raw, config) {
  if (is.null(erfi_raw)) return(NULL)
  needed <- c(config$var_age, config$var_sex, config$var_couple_flag)
  raw_checked <- require_columns(erfi_raw, needed, "base variables")
  if (is.null(raw_checked)) return(NULL)

  erfi_raw |>
    add_age_sex_labels(config) |>
    mutate(across(c(age, sexe), clean_na, na_codes = config$codes_na))
}

# Couple sub-sample
build_couple_subset <- function(erfi_base, config) {
  if (is.null(erfi_base)) return(NULL)
  erfi_base |>
    filter(
      .data[[config$var_couple_flag]] == 1,
      between(age, 18, 79),
      !is.na(sexe)
    )
}

# Detect variable blocks by prefix
scan_blocks <- function(data) {
  list(
    oa = names(data)[str_detect(names(data), "^OA_")],
    ob = names(data)[str_detect(names(data), "^OB_")],
    ea = names(data)[str_detect(names(data), "^EA_")],
    va = names(data)[str_detect(names(data), "^VA_")],
    oc = names(data)[str_detect(names(data), "^OC_")]
  )
}

# OA block: domestic tasks recode and scores
build_block_OA <- function(erfi_couple, oa_vars, config) {
  if (length(oa_vars) == 0) {
    message("No OA_* variables detected.")
    return(list(erfi_oa = NULL, erfi_oa_long = NULL, score_person = NULL, resume_egal = NULL))
  }

  base_cols <- c("id", "age", "age_cat", "sexe", "sexe_lib")
  checked <- require_columns(erfi_couple, c(base_cols, oa_vars), "OA_*")
  if (is.null(checked)) {
    return(list(erfi_oa = NULL, erfi_oa_long = NULL, score_person = NULL, resume_egal = NULL))
  }

  erfi_oa <- erfi_couple |>
    select(all_of(base_cols), all_of(oa_vars)) |>
    clean_block(oa_vars, na_codes = config$codes_na)

  recode_egal <- function(x) {
    case_when(
      x %in% 1:2 ~ "Plutôt moi",
      x == 3 ~ "Égalitaire",
      x %in% 4:5 ~ "Plutôt conjoint",
      TRUE ~ NA_character_
    )
  }

  erfi_oa_long <- erfi_oa |>
    pivot_longer(cols = all_of(oa_vars), names_to = "tache", values_to = "code_tache") |>
    mutate(
      repartition = recode_egal(code_tache),
      repartition = factor(repartition, levels = c("Plutôt moi", "Égalitaire", "Plutôt conjoint")),
      tache = factor(tache, levels = oa_vars)
    )

  score_person <- erfi_oa_long |>
    mutate(
      egal_tache = case_when(
        repartition == "Égalitaire" ~ 1,
        repartition %in% c("Plutôt moi", "Plutôt conjoint") ~ 0,
        TRUE ~ NA_real_
      )
    ) |>
    drop_na(egal_tache) |>
    group_by(id, sexe_lib, age_cat) |>
    summarise(nb_taches = n(), score_egal_OA = mean(egal_tache), .groups = "drop")

  resume_egal <- erfi_oa_long |>
    drop_na(age_cat, repartition) |>
    group_by(tache, age_cat) |>
    summarise(pct_egal = mean(repartition == "Égalitaire") * 100, .groups = "drop")

  list(erfi_oa = erfi_oa, erfi_oa_long = erfi_oa_long, score_person = score_person, resume_egal = resume_egal)
}

# OB block: finances/obligations
build_block_OB <- function(erfi_couple, ob_vars, config) {
  if (length(ob_vars) == 0) {
    message("No OB_* variables detected.")
    return(NULL)
  }
  base_cols <- c("id", "age", "age_cat", "sexe", "sexe_lib")
  checked <- require_columns(erfi_couple, c(base_cols, ob_vars), "OB_*")
  if (is.null(checked)) return(NULL)

  erfi_couple |>
    select(all_of(base_cols), all_of(ob_vars)) |>
    clean_block(ob_vars, na_codes = config$codes_na) |>
    score_mean(ob_vars, "score_oblig_financieres")
}

# EA block: children/parenting
build_block_EA <- function(erfi_couple, ea_vars, config) {
  if (length(ea_vars) == 0) {
    message("No EA_* variables detected.")
    return(NULL)
  }
  base_cols <- c("id", "age", "age_cat", "sexe", "sexe_lib")
  checked <- require_columns(erfi_couple, c(base_cols, ea_vars), "EA_*")
  if (is.null(checked)) return(NULL)

  erfi_couple |>
    select(all_of(base_cols), all_of(ea_vars)) |>
    clean_block(ea_vars, na_codes = config$codes_na) |>
    score_mean(ea_vars, "score_engagement_parental")
}

# VA block: values
build_block_VA <- function(erfi_couple, va_vars, config) {
  if (length(va_vars) == 0) {
    message("No VA_* variables detected.")
    return(NULL)
  }
  base_cols <- c("id", "age", "age_cat", "sexe", "sexe_lib")
  checked <- require_columns(erfi_couple, c(base_cols, va_vars), "VA_*")
  if (is.null(checked)) return(NULL)

  va_parite <- c("VA_FEMENF", "VA_HOMENF", "VA_DEUXPAR", "VA_MERSEUL", "VA_ENRESPAR", "VA_ENFAIDPAR")
  va_famille <- c("VA_MARIDEP", "VA_MARITJS", "VA_DIVORC", "VA_ENFCH")

  erfi_couple |>
    select(all_of(base_cols), all_of(va_vars)) |>
    clean_block(va_vars, na_codes = config$codes_na) |>
    score_mean(intersect(va_parite, va_vars), "score_valeurs_genre") |>
    score_mean(intersect(va_famille, va_vars), "score_valeurs_famille")
}

# OC block: couple relationship/conflict
build_block_OC <- function(erfi_couple, oc_vars, config) {
  if (length(oc_vars) == 0) {
    message("No OC_* variables detected.")
    return(NULL)
  }
  base_cols <- c("id", "age", "age_cat", "sexe", "sexe_lib")
  checked <- require_columns(erfi_couple, c(base_cols, oc_vars), "OC_*")
  if (is.null(checked)) return(NULL)

  erfi_couple |>
    select(all_of(base_cols), all_of(oc_vars)) |>
    clean_block(oc_vars, na_codes = config$codes_na) |>
    score_mean(oc_vars, "score_relation_couple")
}

# Merge block scores into mega_base
build_mega_base <- function(erfi_couple, blocks) {
  if (is.null(erfi_couple)) return(NULL)
  mega <- erfi_couple |>
    select(id, age, age_cat, sexe, sexe_lib)

  if (!is.null(blocks$oa$score_person)) {
    mega <- left_join(mega, blocks$oa$score_person |> select(id, score_egal_OA), by = "id")
  }
  if (!is.null(blocks$ob)) {
    mega <- left_join(mega, blocks$ob |> select(id, score_oblig_financieres), by = "id")
  }
  if (!is.null(blocks$ea)) {
    mega <- left_join(mega, blocks$ea |> select(id, score_engagement_parental), by = "id")
  }
  if (!is.null(blocks$va)) {
    mega <- left_join(mega, blocks$va |> select(id, score_valeurs_genre, score_valeurs_famille), by = "id")
  }
  if (!is.null(blocks$oc)) {
    mega <- left_join(mega, blocks$oc |> select(id, score_relation_couple), by = "id")
  }

  mega
}

# Descriptive tables
build_tables <- function(mega_base, blocks) {
  if (is.null(mega_base)) return(NULL)
  tab_domestique <- mega_base |>
    group_by(age_cat, sexe_lib) |>
    summarise(n = n(), score_egal_OA_moy = mean(score_egal_OA, na.rm = TRUE), .groups = "drop")

  tab_finances <- mega_base |>
    group_by(age_cat, sexe_lib) |>
    summarise(n = n(), score_oblig_financieres_moy = mean(score_oblig_financieres, na.rm = TRUE), .groups = "drop")

  tab_valeurs_genre <- mega_base |>
    group_by(age_cat, sexe_lib) |>
    summarise(n = n(), score_valeurs_genre_moy = mean(score_valeurs_genre, na.rm = TRUE), .groups = "drop")

  list(
    tab_domestique = tab_domestique,
    tab_finances = tab_finances,
    tab_valeurs_genre = tab_valeurs_genre,
    resume_OA_egal_age = blocks$oa$resume_egal
  )
}

# Graphs builder; saves PNGs
build_graphs <- function(mega_base, tables, blocks, output_dir) {
  graphs <- list()
  if (is.null(mega_base)) return(graphs)

  if (nrow(mega_base) > 0) {
    graphs$G1 <- mega_base |>
      ggplot(aes(x = age, fill = sexe_lib)) +
      geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
      labs(title = "Age distribution of respondents in a couple", x = "Age", y = "Count", fill = "Sex") +
      theme_minimal()
    ggsave(file.path(output_dir, "G1_age_sex.png"), graphs$G1, width = 7, height = 4)
  }

  if (!is.null(tables$tab_domestique) && nrow(tables$tab_domestique) > 0) {
    graphs$G2 <- tables$tab_domestique |>
      ggplot(aes(x = age_cat, y = score_egal_OA_moy, color = sexe_lib, group = sexe_lib)) +
      geom_line() + geom_point(size = 2) +
      labs(title = "Average OA_* equality score by age group and sex", x = "Age group", y = "Mean equality score (0-1)", color = "Sex") +
      theme_minimal()
    ggsave(file.path(output_dir, "G2_score_OA_age_sex.png"), graphs$G2, width = 7, height = 4)
  }

  if (!is.null(blocks$oa$score_person) && nrow(blocks$oa$score_person) > 0) {
    graphs$G3 <- blocks$oa$score_person |>
      ggplot(aes(x = age_cat, y = score_egal_OA, color = sexe_lib, group = sexe_lib)) +
      geom_line(stat = "summary", fun = mean) +
      geom_point(alpha = 0.3) +
      labs(title = "Individual OA_* equality scores", x = "Age group", y = "Equality score (0-1)", color = "Sex") +
      theme_minimal()
    ggsave(file.path(output_dir, "G3_score_OA_individuals.png"), graphs$G3, width = 7, height = 4)
  }

  if (!is.null(blocks$oa$resume_egal) && nrow(blocks$oa$resume_egal) > 0) {
    graphs$G4 <- blocks$oa$resume_egal |>
      ggplot(aes(x = age_cat, y = tache, fill = pct_egal)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Share of equal split by task and age", x = "Age group", y = "Task", fill = "% egalitarian") +
      theme_minimal()
    ggsave(file.path(output_dir, "G4_heatmap_OA.png"), graphs$G4, width = 8, height = 6)
  }

  graphs
}

# Export tables as CSV
export_tables <- function(report, path = "tables_erfi") {
  if (is.null(report$tables)) return(invisible(NULL))
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  walk(names(report$tables), function(nm) {
    tbl <- report$tables[[nm]]
    if (is.null(tbl)) return(NULL)
    readr::write_csv(tbl, file.path(path, paste0(nm, ".csv")))
  })
  invisible(path)
}

# Highlights and summary text
build_highlights <- function(blocks, tables) {
  highlights <- list()
  if (!is.null(blocks$oa$resume_egal) && nrow(blocks$oa$resume_egal) > 0) {
    diff_gen <- blocks$oa$resume_egal |>
      pivot_wider(names_from = age_cat, values_from = pct_egal) |>
      mutate(diff_jeunes_aines = `Jeunes (18-30)` - `Aînés (56+)`) |>
      arrange(desc(abs(diff_jeunes_aines)))
    highlights$top_diff_tache <- diff_gen
  }

  if (!is.null(tables$tab_domestique) && nrow(tables$tab_domestique) > 0) {
    highlights$top_scores <- list(
      domestique = tables$tab_domestique |>
        arrange(desc(score_egal_OA_moy)) |>
        head(3),
      finances = tables$tab_finances |>
        arrange(desc(score_oblig_financieres_moy)) |>
        head(3),
      valeurs_genre = tables$tab_valeurs_genre |>
        arrange(desc(score_valeurs_genre_moy)) |>
        head(3)
    )
  }
  highlights
}

build_summary_text <- function(tables, highlights) {
  if (is.null(tables) || is.null(highlights$top_scores$domestique)) {
    return("Summary unavailable due to missing inputs.")
  }

  best_domestic <- highlights$top_scores$domestique |> slice_head(n = 1)
  group_desc <- paste0(best_domestic$age_cat, " - ", best_domestic$sexe_lib)
  best_score <- round(best_domestic$score_egal_OA_moy, 3)

  if (!is.null(highlights$top_diff_tache) && nrow(highlights$top_diff_tache) > 0) {
    top_task <- highlights$top_diff_tache |> slice_head(n = 1)
    task_name <- top_task$tache
    diff_value <- round(top_task$diff_jeunes_aines, 1)
    direction <- ifelse(diff_value > 0, "higher among younger respondents", "higher among older respondents")
    paste0(
      "Highest domestic equality score: ", group_desc, " (", best_score, "). ",
      "Largest OA_* gap between young and older groups for ", task_name, ": ",
      abs(diff_value), " percentage points (", direction, ")."
    )
  } else {
    paste0("Highest domestic equality score: ", group_desc, " (", best_score, ").")
  }
}

# Assemble full report object
build_report <- function(erfi_raw, erfi_base, erfi_couple, blocks, mega_base, tables, graphs, highlights, summary_text) {
  list(
    base = list(erfi_raw = erfi_raw, erfi_base = erfi_base, erfi_couple = erfi_couple, mega_base = mega_base),
    blocks = blocks,
    tables = tables,
    graphs = graphs,
    highlights = highlights,
    summary_text = summary_text
  )
}

# Main pipeline ------------------------------------------------
run_erfi_pipeline <- function(input_file = "ERFI_data.csv", output_dir = "plots_erfi", config = default_config) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  erfi_raw <- load_data(input_file, config)
  if (is.null(erfi_raw)) return(NULL)

  erfi_base <- build_base(erfi_raw, config)
  erfi_couple <- build_couple_subset(erfi_base, config)
  if (is.null(erfi_couple) || nrow(erfi_couple) == 0) {
    warning("Couple subset is empty; aborting pipeline.")
    return(NULL)
  }

  blocks <- list()
  blocks$vars <- scan_blocks(erfi_couple)
  blocks$oa <- build_block_OA(erfi_couple, blocks$vars$oa, config)
  blocks$ob <- build_block_OB(erfi_couple, blocks$vars$ob, config)
  blocks$ea <- build_block_EA(erfi_couple, blocks$vars$ea, config)
  blocks$va <- build_block_VA(erfi_couple, blocks$vars$va, config)
  blocks$oc <- build_block_OC(erfi_couple, blocks$vars$oc, config)

  mega_base <- build_mega_base(erfi_couple, blocks)
  tables <- build_tables(mega_base, blocks)
  graphs <- build_graphs(mega_base, tables, blocks, output_dir)
  highlights <- build_highlights(blocks, tables)
  summary_text <- build_summary_text(tables, highlights)

  report <- build_report(erfi_raw, erfi_base, erfi_couple, blocks, mega_base, tables, graphs, highlights, summary_text)
  message("Pipeline finished. Explore the 'report' object for outputs.")
  report
}

