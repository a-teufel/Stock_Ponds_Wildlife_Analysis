#!/usr/bin/env Rscript

# Stock-pond wildlife analysis used in the manuscript
#
# This is the paper-only version of the original reanalysis script. It creates:
#   - manuscript Tables 1-3 and the descriptive summaries quoted in the text;
#   - Figure 4: relative taxonomic composition;
#   - Figure 5: Bray-Curtis dissimilarity and PCoA;
#   - Figure 6: species-by-pond clustered heat map;
#   - Figure 7: bird Hill diversity and ecological-group richness; and
#   - Figure 8: classified bat-call totals.
#
# It intentionally does not run rarefaction, chi-square/Fisher tests,
# Friedman/Wilcoxon tests, Kruskal-Wallis/Dunn tests, or create exploratory
# figures that are not reported in the paper.
#
# From a terminal:
#   Rscript Stock_Pond_Manuscript_Analysis.R \
#     "Periubran-Rural pond wildlife manuscript data.xlsx"
#
# Optional second argument: output directory
#   Rscript Stock_Pond_Manuscript_Analysis.R data.xlsx manuscript_output
#
# In RStudio, put this script and the workbook in the same folder and click
# Source. A normalized CSV with the columns described below is also accepted.

required_packages <- c(
  "readr", "readxl", "dplyr", "tidyr", "tibble", "purrr",
  "ggplot2", "cowplot", "ggrepel", "scales", "vegan", "pheatmap"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install the missing packages, then rerun:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(cowplot)
  library(ggrepel)
  library(scales)
  library(vegan)
  library(pheatmap)
})

options(dplyr.summarise.inform = FALSE, scipen = 999)
set.seed(20260724)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

input_path <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  "Periubran-Rural pond wildlife manuscript data.xlsx"
}

if (!file.exists(input_path)) {
  stop(
    paste0(
      "Input file not found: ", input_path, "\n",
      "Pass the Excel workbook or normalized CSV as the first argument."
    ),
    call. = FALSE
  )
}

input_path <- normalizePath(input_path, mustWork = TRUE)

output_dir <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(dirname(input_path), "stock_pond_manuscript_output")
}

figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Study metadata and plot style
# ---------------------------------------------------------------------------

pond_levels <- c("PUSP", "PUMP", "PULP", "RSP", "RMP", "RLP")
periurban_ponds <- c("PUSP", "PUMP", "PULP")
rural_ponds <- c("RSP", "RMP", "RLP")

pond_labels <- c(
  PUSP = "Peri-urban S",
  PUMP = "Peri-urban M",
  PULP = "Peri-urban L",
  RSP = "Rural S",
  RMP = "Rural M",
  RLP = "Rural L"
)

pond_metadata <- tibble(
  pond = pond_levels,
  pond_label = unname(pond_labels[pond_levels]),
  site = factor(
    c(rep("Peri-urban", 3L), rep("Rural", 3L)),
    levels = c("Peri-urban", "Rural")
  ),
  pond_size = factor(
    rep(c("Small", "Medium", "Large"), 2L),
    levels = c("Small", "Medium", "Large")
  )
)

site_colors <- c("Peri-urban" = "#0072B2", "Rural" = "#D55E00")

theme_wildlife <- function(base_size = 11) {
  cowplot::theme_minimal_hgrid(
    font_size = base_size,
    rel_small = 0.9,
    color = "grey88"
  ) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey25"),
      strip.text = element_text(face = "bold", color = "grey20"),
      strip.background = element_blank(),
      legend.title = element_text(face = "bold"),
      panel.spacing = grid::unit(1.1, "lines")
    )
}

theme_set(theme_wildlife())

save_figure <- function(plot_object, file_stem, width, height) {
  ggsave(
    filename = file.path(figure_dir, paste0(file_stem, ".png")),
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )

  ggsave(
    filename = file.path(figure_dir, paste0(file_stem, ".pdf")),
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    bg = "white",
    limitsize = FALSE
  )
}

# ---------------------------------------------------------------------------
# Import and validate manuscript Table 1
# ---------------------------------------------------------------------------

required_columns <- c("code", "common_name", "group", pond_levels, "total")

read_wildlife_table <- function(path, pond_columns) {
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    workbook_sheets <- readxl::excel_sheets(path)
    summary_sheet_index <- which(
      tolower(trimws(workbook_sheets)) == "summary table"
    )

    if (length(summary_sheet_index) == 0L) {
      stop(
        paste0(
          "The workbook must contain a sheet named 'summary table'. ",
          "Available sheets: ", paste(workbook_sheets, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    workbook_table <- readxl::read_excel(
      path,
      sheet = workbook_sheets[[summary_sheet_index[[1L]]]],
      skip = 2L,
      col_names = c("common_name_full", "group", "code", pond_columns),
      col_types = c("text", "text", "text", rep("numeric", 6L)),
      na = c("", "NA", "-", "--"),
      trim_ws = TRUE,
      .name_repair = "minimal"
    ) |>
      filter(!if_all(everything(), is.na)) |>
      mutate(
        across(all_of(pond_columns), ~ replace_na(.x, 0)),
        common_name_full = trimws(common_name_full),
        common_name = trimws(
          sub(
            "[[:space:]]*\\([^()]+\\)[[:space:]]*$",
            "",
            common_name_full
          )
        ),
        group = trimws(group),
        code = trimws(code)
      ) |>
      filter(!is.na(code), nzchar(code))

    workbook_table$total <- rowSums(
      as.data.frame(workbook_table[pond_columns])
    )

    return(
      workbook_table |>
        select(code, common_name, group, all_of(pond_columns), total)
    )
  }

  if (extension == "csv") {
    return(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      )
    )
  }

  stop(
    paste0(
      "Unsupported input type '.", extension,
      "'. Use the supplied workbook or a normalized CSV."
    ),
    call. = FALSE
  )
}

table1_raw <- read_wildlife_table(input_path, pond_levels)

missing_columns <- setdiff(required_columns, names(table1_raw))
if (length(missing_columns) > 0L) {
  stop(
    paste("Missing required columns:", paste(missing_columns, collapse = ", ")),
    call. = FALSE
  )
}

if (anyDuplicated(table1_raw$code)) {
  duplicated_codes <- unique(table1_raw$code[duplicated(table1_raw$code)])
  stop(
    paste("Duplicate taxon codes:", paste(duplicated_codes, collapse = ", ")),
    call. = FALSE
  )
}

count_columns <- c(pond_levels, "total")

table1 <- table1_raw |>
  mutate(
    across(all_of(count_columns), ~ suppressWarnings(as.numeric(.x))),
    group = trimws(group),
    group = case_when(
      tolower(group) %in% c("raptors and vultures", "raptors & vultures") ~
        "Raptors and vultures",
      tolower(group) %in% c(
        "wading birds and waterfowl",
        "wading birds & waterfowl"
      ) ~ "Wading birds and waterfowl",
      tolower(group) %in% c(
        "ground and open-country",
        "ground/open-country",
        "ground and open country"
      ) ~ "Ground/open-country birds",
      tolower(group) == "perching birds" ~ "Perching birds",
      TRUE ~ group
    )
  )

if (anyNA(table1[count_columns])) {
  stop("At least one pond count or row total is missing or non-numeric.", call. = FALSE)
}

if (any(as.matrix(table1[count_columns]) < 0)) {
  stop("Counts must be non-negative.", call. = FALSE)
}

if (any(as.matrix(table1[count_columns]) %% 1 != 0)) {
  stop("Counts must be whole numbers.", call. = FALSE)
}

calculated_row_totals <- rowSums(as.data.frame(table1[pond_levels]))
bad_total_rows <- which(calculated_row_totals != table1$total)

if (length(bad_total_rows) > 0L) {
  stop(
    paste0(
      "The printed row total does not equal the six pond cells for: ",
      paste(table1$code[bad_total_rows], collapse = ", ")
    ),
    call. = FALSE
  )
}

observed_taxa <- table1 |>
  mutate(calculated_total = calculated_row_totals) |>
  filter(calculated_total > 0)

if (nrow(observed_taxa) == 0L) {
  stop("No taxon has a positive count.", call. = FALSE)
}

# Ponds are rows and taxa are columns for community analyses.
community <- t(as.matrix(observed_taxa[pond_levels]))
storage.mode(community) <- "numeric"
rownames(community) <- pond_levels
colnames(community) <- observed_taxa$code

# Taxa are rows and ponds are columns for the clustered heat map.
taxon_pond_table <- as.matrix(observed_taxa[pond_levels])
storage.mode(taxon_pond_table) <- "numeric"
rownames(taxon_pond_table) <- observed_taxa$code

counts_long <- observed_taxa |>
  select(code, common_name, group, all_of(pond_levels)) |>
  pivot_longer(
    cols = all_of(pond_levels),
    names_to = "pond",
    values_to = "table_records"
  ) |>
  mutate(pond = factor(pond, levels = pond_levels)) |>
  left_join(pond_metadata, by = "pond")

# ---------------------------------------------------------------------------
# Descriptive summaries used in Tables 1-2 and the Results text
# ---------------------------------------------------------------------------

diversity_metrics <- function(x) {
  positive <- as.numeric(x[x > 0])
  total_records <- sum(positive)
  richness <- length(positive)

  if (total_records == 0) {
    return(tibble(
      table_records = 0,
      hill_q0 = 0,
      hill_q1 = 0,
      hill_q2 = 0
    ))
  }

  probabilities <- positive / total_records
  shannon_entropy <- -sum(probabilities * log(probabilities))
  simpson_concentration <- sum(probabilities^2)

  tibble(
    table_records = total_records,
    hill_q0 = richness,
    hill_q1 = exp(shannon_entropy),
    hill_q2 = 1 / simpson_concentration
  )
}

alpha_by_pond <- purrr::map_dfr(pond_levels, function(current_pond) {
  bind_cols(
    tibble(pond = current_pond),
    diversity_metrics(community[current_pond, ])
  )
}) |>
  left_join(pond_metadata, by = "pond") |>
  select(
    pond, pond_label, site, pond_size,
    table_records, hill_q0, hill_q1, hill_q2
  )

site_summary <- tibble(
  site = c("Peri-urban", "Rural"),
  table_records = c(
    sum(community[periurban_ponds, , drop = FALSE]),
    sum(community[rural_ponds, , drop = FALSE])
  ),
  observed_richness = c(
    sum(colSums(community[periurban_ponds, , drop = FALSE]) > 0),
    sum(colSums(community[rural_ponds, , drop = FALSE]) > 0)
  )
)

species_occurrence <- observed_taxa |>
  transmute(
    code,
    common_name,
    group,
    total_records = calculated_total
  )

species_occurrence$periurban_records <- rowSums(
  as.data.frame(observed_taxa[periurban_ponds])
)
species_occurrence$rural_records <- rowSums(
  as.data.frame(observed_taxa[rural_ponds])
)

species_occurrence <- species_occurrence |>
  mutate(
    occurrence_status = case_when(
      periurban_records > 0 & rural_records > 0 ~ "Both properties",
      periurban_records > 0 ~ "Peri-urban only",
      rural_records > 0 ~ "Rural only",
      TRUE ~ "Unobserved"
    )
  )

bird_group_levels <- c(
  "Perching birds",
  "Raptors and vultures",
  "Wading birds and waterfowl",
  "Ground/open-country birds"
)

bird_taxa <- observed_taxa |>
  filter(group %in% bird_group_levels)

if (nrow(bird_taxa) == 0L) {
  stop(
    "No bird taxa were recognized. Check the ecological-group labels.",
    call. = FALSE
  )
}

taxonomic_class_by_site <- counts_long |>
  mutate(
    broad_group = if_else(
      group %in% bird_group_levels,
      "Birds",
      "Mammals and reptiles"
    ),
    site = as.character(site)
  ) |>
  group_by(site, broad_group, code) |>
  summarise(table_records = sum(table_records), .groups = "drop") |>
  group_by(site, broad_group) |>
  summarise(
    table_records = sum(table_records),
    observed_richness = sum(table_records > 0),
    .groups = "drop"
  )

bird_community <- t(as.matrix(bird_taxa[pond_levels]))
storage.mode(bird_community) <- "numeric"
rownames(bird_community) <- pond_levels
colnames(bird_community) <- bird_taxa$code

bird_alpha_by_pond <- purrr::map_dfr(pond_levels, function(current_pond) {
  bind_cols(
    tibble(pond = current_pond),
    diversity_metrics(bird_community[current_pond, ])
  )
}) |>
  left_join(pond_metadata, by = "pond") |>
  select(
    pond, pond_label, site, pond_size,
    table_records, hill_q0, hill_q1, hill_q2
  )

bird_group_richness <- tidyr::expand_grid(
  pond = pond_levels,
  ecological_group = bird_group_levels
) |>
  left_join(
    counts_long |>
      mutate(pond = as.character(pond)) |>
      filter(group %in% bird_group_levels) |>
      group_by(pond, group) |>
      summarise(
        observed_richness = sum(table_records > 0),
        .groups = "drop"
      ) |>
      rename(ecological_group = group),
    by = c("pond", "ecological_group")
  ) |>
  mutate(observed_richness = coalesce(observed_richness, 0L)) |>
  left_join(pond_metadata, by = "pond")

# ---------------------------------------------------------------------------
# Bray-Curtis dissimilarity, PCoA, and descriptive clustering
# ---------------------------------------------------------------------------

bray_distance <- vegan::vegdist(community, method = "bray", binary = FALSE)
bray_matrix <- as.matrix(bray_distance)

bray_pair_indices <- which(upper.tri(bray_matrix), arr.ind = TRUE)
bray_pairs <- tibble(
  pond_1 = rownames(bray_matrix)[bray_pair_indices[, "row"]],
  pond_2 = colnames(bray_matrix)[bray_pair_indices[, "col"]],
  bray_curtis = bray_matrix[bray_pair_indices]
) |>
  left_join(
    pond_metadata |>
      transmute(pond_1 = pond, site_1 = as.character(site)),
    by = "pond_1"
  ) |>
  left_join(
    pond_metadata |>
      transmute(pond_2 = pond, site_2 = as.character(site)),
    by = "pond_2"
  ) |>
  mutate(
    property_comparison = if_else(
      site_1 == site_2,
      "Within property",
      "Between properties"
    )
  )

bray_summary <- bind_rows(
  bray_pairs |>
    summarise(
      comparison = "All pond pairs",
      n_pairs = n(),
      mean_dissimilarity = mean(bray_curtis),
      minimum_dissimilarity = min(bray_curtis),
      maximum_dissimilarity = max(bray_curtis)
    ),
  bray_pairs |>
    group_by(property_comparison) |>
    summarise(
      comparison = first(property_comparison),
      n_pairs = n(),
      mean_dissimilarity = mean(bray_curtis),
      minimum_dissimilarity = min(bray_curtis),
      maximum_dissimilarity = max(bray_curtis),
      .groups = "drop"
    ) |>
    select(-property_comparison)
)

# Lingoes correction is applied because Bray-Curtis is not necessarily
# Euclidean. The ordination remains descriptive for these six ponds.
pcoa_result <- vegan::wcmdscale(
  bray_distance,
  k = 2,
  eig = TRUE,
  add = "lingoes"
)

pcoa_scores <- as.data.frame(pcoa_result$points)
names(pcoa_scores)[1:2] <- c("PCoA1", "PCoA2")

pcoa_scores <- pcoa_scores |>
  rownames_to_column("pond") |>
  as_tibble() |>
  left_join(pond_metadata, by = "pond")

pcoa_positive_eigenvalues <- pcoa_result$eig[pcoa_result$eig > 0]
pcoa_axis_variance_percent <- 100 * pcoa_positive_eigenvalues /
  sum(pcoa_positive_eigenvalues)

pcoa_summary <- tibble(
  correction = "Lingoes",
  additive_constant = pcoa_result$ac,
  pcoa_1_percent = pcoa_axis_variance_percent[[1L]],
  pcoa_2_percent = pcoa_axis_variance_percent[[2L]],
  first_two_axes_percent = sum(pcoa_axis_variance_percent[1:2])
)

species_profiles <- sweep(
  taxon_pond_table,
  MARGIN = 1L,
  STATS = rowSums(taxon_pond_table),
  FUN = "/"
)

species_cluster <- stats::hclust(
  stats::dist(sqrt(species_profiles), method = "euclidean"),
  method = "average"
)

pond_cluster <- stats::hclust(
  stats::as.dist(bray_matrix),
  method = "average"
)

# ---------------------------------------------------------------------------
# Bat-call summary used in manuscript Table 3 and Figure 8
# ---------------------------------------------------------------------------

bat_calls <- tribble(
  ~code,  ~common_name,                  ~rural_l, ~periurban_l,
  "LABO", "Eastern red bat",                 28,          112,
  "TABR", "Mexican free-tailed bat",          7,           41,
  "NYHU", "Evening bat",                     11,           21,
  "MYVE", "Cave myotis",                     17,           22,
  "PESU", "Tricolored bat",                   3,            3,
  "LANO", "Silver-haired bat",                1,            7,
  "LACI", "Hoary bat",                        0,            2
) |>
  mutate(species_label = paste0(common_name, " (", code, ")"))

# ---------------------------------------------------------------------------
# Figure 4: relative composition of Table 1 records
# ---------------------------------------------------------------------------

# The current manuscript figure displays eight named taxa plus "Other taxa."
# Keep this at 8 to reproduce that figure. The Methods and caption should also
# say eight, not seven.
top_n_taxa <- 8L

top_taxon_codes <- observed_taxa |>
  arrange(desc(calculated_total), common_name) |>
  slice_head(n = top_n_taxa) |>
  pull(code)

composition_data <- counts_long |>
  mutate(
    taxon = if_else(
      code %in% top_taxon_codes,
      paste0(common_name, " (", code, ")"),
      "Other taxa"
    )
  ) |>
  group_by(pond, site, taxon) |>
  summarise(table_records = sum(table_records), .groups = "drop") |>
  group_by(pond) |>
  mutate(proportion = table_records / sum(table_records)) |>
  ungroup()

taxon_order <- observed_taxa |>
  filter(code %in% top_taxon_codes) |>
  arrange(desc(calculated_total), common_name) |>
  transmute(taxon = paste0(common_name, " (", code, ")")) |>
  pull(taxon)

composition_data <- composition_data |>
  mutate(
    taxon = factor(taxon, levels = c(taxon_order, "Other taxa")),
    pond = factor(pond, levels = pond_levels)
  )

composition_colors <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7",
  "#E69F00", "#56B4E9", "#F0E442", "#000000", "grey78"
)
names(composition_colors) <- levels(composition_data$taxon)

figure_4 <- ggplot(
  composition_data,
  aes(x = pond, y = proportion, fill = taxon)
) +
  geom_col(width = 0.76, color = "white", linewidth = 0.25) +
  facet_grid(cols = vars(site), scales = "free_x", space = "free_x") +
  scale_fill_manual(values = composition_colors, name = NULL, drop = FALSE) +
  scale_x_discrete(labels = pond_labels) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(x = NULL, y = "Share of Table 1 records") +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 8.5)
  )

save_figure(figure_4, "figure_4_relative_composition", 10.5, 5.6)

# ---------------------------------------------------------------------------
# Figure 5: Bray-Curtis dissimilarity matrix and PCoA
# ---------------------------------------------------------------------------

bray_plot_data <- as.data.frame(
  as.table(bray_matrix),
  responseName = "dissimilarity",
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  rename(pond_row = Var1, pond_column = Var2) |>
  mutate(
    pond_row = factor(pond_row, levels = rev(pond_levels)),
    pond_column = factor(pond_column, levels = pond_levels)
  )

bray_heatmap <- ggplot(
  bray_plot_data,
  aes(x = pond_column, y = pond_row, fill = dissimilarity)
) +
  geom_tile(color = "white", linewidth = 0.55) +
  geom_text(
    aes(label = scales::number(dissimilarity, accuracy = 0.01)),
    size = 3.1,
    color = ifelse(bray_plot_data$dissimilarity > 0.62, "white", "grey15")
  ) +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#6BAED6", "#08306B"),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    name = "Bray-Curtis",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      barwidth = grid::unit(4.2, "cm"),
      barheight = grid::unit(0.30, "cm"),
      ticks = FALSE
    )
  ) +
  scale_x_discrete(labels = pond_labels) +
  scale_y_discrete(labels = pond_labels) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

pcoa_limit <- max(abs(c(pcoa_scores$PCoA1, pcoa_scores$PCoA2)), na.rm = TRUE)
if (!is.finite(pcoa_limit) || pcoa_limit == 0) {
  pcoa_limit <- 1
}
pcoa_limit <- pcoa_limit * 1.20

pcoa_plot <- ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2)) +
  geom_hline(yintercept = 0, color = "grey90", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5) +
  geom_point(
    aes(fill = site),
    shape = 21,
    size = 4.2,
    stroke = 0.6,
    color = "white"
  ) +
  ggrepel::geom_text_repel(
    aes(label = pond_label, color = site),
    seed = 20260724,
    min.segment.length = 0,
    size = 3.3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = site_colors, name = NULL) +
  scale_color_manual(values = site_colors, guide = "none") +
  coord_equal(
    xlim = c(-pcoa_limit, pcoa_limit),
    ylim = c(-pcoa_limit, pcoa_limit),
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    x = paste0(
      "PCoA 1 (",
      scales::number(pcoa_axis_variance_percent[[1L]], accuracy = 0.1),
      "%)"
    ),
    y = paste0(
      "PCoA 2 (",
      scales::number(pcoa_axis_variance_percent[[2L]], accuracy = 0.1),
      "%)"
    )
  ) +
  theme(panel.grid.minor = element_blank())

bray_legend <- cowplot::get_legend(
  bray_heatmap +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center"
    )
)

pcoa_legend <- cowplot::get_legend(
  pcoa_plot +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center"
    )
)

figure_5_panels <- cowplot::plot_grid(
  bray_heatmap + theme(legend.position = "none", aspect.ratio = 1),
  pcoa_plot + theme(legend.position = "none", aspect.ratio = 1),
  labels = c("A", "B"),
  label_fontface = "bold",
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

figure_5_legends <- cowplot::plot_grid(
  bray_legend,
  pcoa_legend,
  ncol = 2,
  rel_widths = c(1, 1)
)

figure_5 <- cowplot::plot_grid(
  figure_5_panels,
  figure_5_legends,
  ncol = 1,
  rel_heights = c(1, 0.14)
)

save_figure(figure_5, "figure_5_bray_curtis_and_pcoa", 11.8, 6.2)

# ---------------------------------------------------------------------------
# Figure 6: species-by-pond heat map and hierarchical clustering
# ---------------------------------------------------------------------------

pond_site_annotation <- data.frame(
  Site = factor(pond_metadata$site, levels = c("Peri-urban", "Rural")),
  row.names = pond_metadata$pond
)

maximum_heatmap_count <- max(taxon_pond_table)
heatmap_count_breaks <- unique(c(0, 1, 3, 10, maximum_heatmap_count))
heatmap_count_breaks <- heatmap_count_breaks[
  heatmap_count_breaks <= maximum_heatmap_count
]

species_heatmap_colors <- grDevices::colorRampPalette(
  c("#F7F7F7", "#D8DAEB", "#998EC3", "#542788")
)(100)

species_heatmap_scale_breaks <- seq(
  0,
  log2(maximum_heatmap_count + 1),
  length.out = length(species_heatmap_colors) + 1L
)

species_heatmap <- pheatmap::pheatmap(
  mat = log2(taxon_pond_table + 1),
  color = species_heatmap_colors,
  breaks = species_heatmap_scale_breaks,
  scale = "none",
  cluster_rows = species_cluster,
  cluster_cols = pond_cluster,
  labels_row = observed_taxa$common_name[
    match(rownames(taxon_pond_table), observed_taxa$code)
  ],
  labels_col = unname(pond_labels[colnames(taxon_pond_table)]),
  annotation_col = pond_site_annotation[
    colnames(taxon_pond_table),
    ,
    drop = FALSE
  ],
  annotation_colors = list(Site = site_colors),
  annotation_names_col = FALSE,
  show_rownames = TRUE,
  show_colnames = TRUE,
  border_color = "white",
  legend_breaks = log2(heatmap_count_breaks + 1),
  legend_labels = as.character(heatmap_count_breaks),
  treeheight_row = 70,
  treeheight_col = 45,
  fontsize = 9,
  fontsize_row = 6.4,
  fontsize_col = 8.5,
  angle_col = "45",
  silent = TRUE
)

figure_6 <- cowplot::ggdraw() +
  cowplot::draw_grob(species_heatmap$gtable)

save_figure(figure_6, "figure_6_species_by_pond_heatmap", 9.0, 10.5)

# ---------------------------------------------------------------------------
# Figure 7: bird Hill diversity and ecological-group richness
# ---------------------------------------------------------------------------

bird_hill_plot_data <- bird_alpha_by_pond |>
  select(pond, site, hill_q0, hill_q1, hill_q2) |>
  pivot_longer(
    cols = starts_with("hill_q"),
    names_to = "order",
    values_to = "effective_taxa"
  ) |>
  mutate(
    pond = factor(pond, levels = rev(pond_levels)),
    order = factor(
      order,
      levels = c("hill_q0", "hill_q1", "hill_q2"),
      labels = c(
        "Richness (q = 0)",
        "exp(Shannon) (q = 1)",
        "Inverse Simpson (q = 2)"
      )
    )
  )

bird_diversity_panel <- ggplot(
  bird_hill_plot_data,
  aes(x = effective_taxa, y = pond, group = order)
) +
  geom_col(
    aes(fill = site, alpha = order),
    position = position_dodge2(
      width = 0.82,
      preserve = "single",
      padding = 0.08
    ),
    width = 0.72,
    orientation = "y",
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = scales::number(effective_taxa, accuracy = 0.1)),
    position = position_dodge2(
      width = 0.82,
      preserve = "single",
      padding = 0.08
    ),
    hjust = -0.22,
    size = 3.0,
    color = "grey20",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = site_colors, guide = "none") +
  scale_alpha_manual(
    values = c(
      "Richness (q = 0)" = 0.38,
      "exp(Shannon) (q = 1)" = 0.68,
      "Inverse Simpson (q = 2)" = 1
    ),
    name = NULL,
    guide = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(fill = "grey30", color = NA)
    )
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0.14))
  ) +
  scale_y_discrete(labels = pond_labels) +
  labs(x = "Effective number of bird taxa", y = NULL) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

bird_group_plot_data <- bird_group_richness |>
  mutate(
    pond = factor(pond, levels = rev(pond_levels)),
    ecological_group = factor(
      ecological_group,
      levels = bird_group_levels,
      labels = c(
        "Perching birds",
        "Raptors & vultures",
        "Wading birds & waterfowl",
        "Ground/open-country"
      )
    )
  )

bird_richness_totals <- bird_group_plot_data |>
  group_by(pond) |>
  summarise(total_richness = sum(observed_richness), .groups = "drop")

bird_group_colors <- c(
  "Perching birds" = "#56B4E9",
  "Raptors & vultures" = "#CC79A7",
  "Wading birds & waterfowl" = "#009E73",
  "Ground/open-country" = "#E69F00"
)

bird_group_panel <- ggplot(
  bird_group_plot_data,
  aes(x = observed_richness, y = pond, fill = ecological_group)
) +
  geom_col(width = 0.72, color = "white", linewidth = 0.25) +
  geom_text(
    data = bird_richness_totals,
    aes(x = total_richness, y = pond, label = total_richness),
    inherit.aes = FALSE,
    hjust = -0.35,
    size = 3.0,
    color = "grey25"
  ) +
  scale_fill_manual(values = bird_group_colors, name = NULL, drop = FALSE) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_y_discrete(labels = pond_labels) +
  labs(x = "Observed bird richness", y = NULL) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8.5)
  )

figure_7 <- cowplot::plot_grid(
  bird_diversity_panel,
  bird_group_panel,
  labels = c("A", "B"),
  label_fontface = "bold",
  ncol = 2,
  rel_widths = c(1.05, 1),
  align = "hv",
  axis = "tblr"
)

save_figure(figure_7, "figure_7_bird_diversity_and_groups", 11.8, 6.0)

# ---------------------------------------------------------------------------
# Figure 8: classified bat-call totals for the two L ponds
# ---------------------------------------------------------------------------

bat_order <- bat_calls |>
  arrange(periurban_l) |>
  pull(species_label)

bat_plot_wide <- bat_calls |>
  mutate(species_label = factor(species_label, levels = bat_order))

bat_plot_long <- bat_plot_wide |>
  select(species_label, rural_l, periurban_l) |>
  pivot_longer(
    cols = c(rural_l, periurban_l),
    names_to = "site",
    values_to = "classified_calls"
  ) |>
  mutate(
    site = factor(
      site,
      levels = c("periurban_l", "rural_l"),
      labels = c("Peri-urban", "Rural")
    )
  )

figure_8 <- ggplot() +
  geom_segment(
    data = bat_plot_wide,
    aes(
      x = rural_l,
      xend = periurban_l,
      y = species_label,
      yend = species_label
    ),
    linewidth = 1.2,
    color = "grey80"
  ) +
  geom_point(
    data = bat_plot_long,
    aes(x = classified_calls, y = species_label, color = site),
    size = 3.3
  ) +
  scale_color_manual(values = site_colors, name = NULL) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  labs(x = "Classified bat calls", y = NULL) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

save_figure(figure_8, "figure_8_bat_calls", 8.8, 5.0)

# ---------------------------------------------------------------------------
# Export only manuscript-relevant tables and source data
# ---------------------------------------------------------------------------

write_csv(
  observed_taxa |>
    select(code, common_name, group, all_of(pond_levels), total),
  file.path(table_dir, "table_1_taxon_counts_by_pond.csv")
)

write_csv(
  alpha_by_pond |>
    transmute(
      pond = pond_label,
      table_records,
      q0_observed_richness = hill_q0,
      q1_exp_shannon = round(hill_q1, 1),
      q2_inverse_simpson = round(hill_q2, 1)
    ),
  file.path(table_dir, "table_2_pond_hill_diversity.csv")
)

write_csv(
  bat_calls |>
    select(code, common_name, rural_l, periurban_l),
  file.path(table_dir, "table_3_bat_calls.csv")
)

write_csv(site_summary, file.path(table_dir, "site_summary.csv"))
write_csv(
  taxonomic_class_by_site,
  file.path(table_dir, "birds_vs_mammals_reptiles_by_site.csv")
)
write_csv(
  species_occurrence,
  file.path(table_dir, "taxon_occurrence_by_site.csv")
)
write_csv(
  composition_data |>
    mutate(pond = as.character(pond), taxon = as.character(taxon)),
  file.path(table_dir, "figure_4_relative_composition_source.csv")
)
write_csv(
  as_tibble(bray_matrix, rownames = "pond"),
  file.path(table_dir, "figure_5_bray_curtis_matrix.csv")
)
write_csv(bray_pairs, file.path(table_dir, "bray_curtis_pairwise_summary.csv"))
write_csv(bray_summary, file.path(table_dir, "bray_curtis_results_summary.csv"))
write_csv(pcoa_scores, file.path(table_dir, "figure_5_pcoa_scores.csv"))
write_csv(pcoa_summary, file.path(table_dir, "figure_5_pcoa_summary.csv"))
write_csv(
  counts_long |>
    select(code, common_name, group, pond, pond_label, site, table_records),
  file.path(table_dir, "figure_6_heatmap_source.csv")
)
write_csv(
  bird_alpha_by_pond,
  file.path(table_dir, "figure_7_bird_diversity_source.csv")
)
write_csv(
  bird_group_richness,
  file.path(table_dir, "figure_7_bird_group_richness_source.csv")
)

shared_taxa <- sum(species_occurrence$occurrence_status == "Both properties")
periurban_only_taxa <- sum(
  species_occurrence$occurrence_status == "Peri-urban only"
)
rural_only_taxa <- sum(species_occurrence$occurrence_status == "Rural only")

analysis_notes <- c(
  "STOCK-POND MANUSCRIPT ANALYSIS",
  "",
  paste("Input:", input_path),
  paste("Observed taxa:", nrow(observed_taxa)),
  paste("Grand total of table records:", sum(community)),
  paste(
    "Shared / peri-urban-only / rural-only taxa:",
    shared_taxa, "/", periurban_only_taxa, "/", rural_only_taxa
  ),
  paste(
    "PCoA axes 1 and 2 (%):",
    round(pcoa_axis_variance_percent[[1L]], 1),
    "and",
    round(pcoa_axis_variance_percent[[2L]], 1)
  ),
  "",
  "INTERPRETIVE LIMITS",
  paste(
    "- The input is an aggregate taxon-by-pond matrix, not repeated sampling",
    "occasions. Results are descriptive."
  ),
  paste(
    "- There is one peri-urban property and one rural property, so the script",
    "does not run a replicated test of an urbanization or property effect."
  ),
  paste(
    "- Counts are table records or classified bat calls, not necessarily",
    "independent animal visits or individual bats."
  ),
  paste(
    "- Hill numbers, Bray-Curtis dissimilarity, PCoA, and clustering describe",
    "the surviving records and do not correct unknown effort or detectability."
  ),
  "",
  "MANUSCRIPT CONSISTENCY NOTE",
  paste(
    "- Figure 4 displays eight named taxa plus Other. The Methods and figure",
    "caption should therefore say eight rather than seven."
  ),
  paste(
    "- This script uses Mexican free-tailed bat consistently with Table 3 and",
    "the Results text; the current Figure 8 says Brazilian free-tailed bat."
  )
)

writeLines(
  analysis_notes,
  con = file.path(output_dir, "READ_ME_FIRST.txt")
)

capture.output(sessionInfo(), file = file.path(output_dir, "R_session_info.txt"))

message("")
message("Manuscript analysis complete.")
message("Observed taxa: ", nrow(observed_taxa))
message("Grand total of table records: ", sum(community))
message("Figures: ", normalizePath(figure_dir, mustWork = TRUE))
message("Tables:  ", normalizePath(table_dir, mustWork = TRUE))
