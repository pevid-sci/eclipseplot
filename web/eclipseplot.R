#' Create an eclipseplot for a risk of bias assessment with the ROBUST-RCT tool
#'
#' @description
#' A visual representation of risk-of-bias assessments of randomized controlled trials using the ROBUST-RCT framework.
#' It can display either the standard two-step assessment or a simplified judgment-only version.
#'
#'
#' @param robust_data A data frame containing the data from the risk-of-bias assessment with ROBUST-RCT.
#' @param standard Logical. If \code{TRUE} (default), displays the two-step eclipseplot. If \code{TRUE} (default), displays only the step 2 (judgment).
#' @param optionals Logical. If \code{TRUE}, includes columns \code{opt1} to \code{opt9}.
#' @param title Character. Custom title for the eclipse plot.
#' @param plot Character. Parts to display: \code{"full"} (default), \code{"eclipses"}, or \code{"legend"}.
#' @param design Character. Orientation of the assembly: \code{"horizontal"} (default) or \code{"vertical"}.
#' @param proportions Numeric vector of length 2. Relative widths/heights for plot and legend.
#'
#' @return A \code{ggplot} object assembled via \code{cowplot}.
#'
#' @details
#' The input data frame must include the required columns for the ROBUST-RCT domains.
#' The following columns are expected:
#' \itemize{
#'   \item \strong{pmid:} PubMed ID or unique identifier.
#'   \item \strong{study:} Study name (e.g., "Author, 2026").
#'   \item \strong{Core Items (1-6):} Columns named 'itemX_step1' and 'itemX_step2' (where X is 1 to 6). As standard in the ROBUST-RCT tool, item 6 does not have a multiple-choice step 1 evaluation, so it is automatically marked as NA.
#'   \item \strong{Optional Items:} If optionals = TRUE, columns named 'optX_step1' and 'optX_step2' (where X is 1 to 9).
#' }
#'
#' \strong{Allowed values for Step 1 (Evaluation):} "Definitely Yes", "Probably Yes", "Probably No", "Definitely No", "Not applicable".
#'
#' \strong{Allowed values for Step 2 (Judgment):} "Definitely Low", "Probably Low", "Probably High", "Definitely High", "Not applicable".
#'
#' @section Dataset Structure:
#' Your CSV should look like this (Top 2 rows):
#' \preformatted{
#' "pmid","study","item1_step1","item1_step2","item2_step1"..."item6_step2"
#' "40001","Albert, 2024","Definitely Yes","Definitely Low",..."Probably Low"
#' }
#'
#'
#' \strong{Color Mapping:}
#' \itemize{
#'   \item Definitely Low / Definitely Yes: Blue (#4C72B0)
#'   \item Probably Low / Probably Yes: Light Blue (#c3d0e4)
#'   \item Probably High / Probably No: Light Red (#f5c7c7)
#'   \item Definitely High / Definitely No: Red (#E15759)
#'   \item Not applicable: Grey (#D3D3D3)
#' }
#'
#'
#' @examples
#' # Load example datasets from the package
#' data(sample_brief)
#' data(sample_long)
#'
#' # 1. Standard plot (Core items, Steps 1 & 2)
#' data(sample_brief)
#' eclipseplot_plot <- eclipseplot(sample_brief)
#' print(eclipseplot_plot)
#'
#' # 2. Judgment only (Core items, Step 2)
#' eclipseplot(sample_long, standard = FALSE)
#'
#' # 3. Plot with optional items (Core and optional items, Steps 1 & 2)
#' eclipseplot(sample_long, optionals = TRUE)
#'
#' # 4. Judgment only with optional items (Core and optional items, Step 2)
#' eclipseplot(sample_long, standard = FALSE, optionals = TRUE)
#'
#' # 5. Vertical full-page plot
#' eclipseplot(sample_long, design = "vertical", proportions = c(0.7, 0.3))
#'
#' # 6. Plotting only the dots
#' eclipseplot(sample_brief, plot = "eclipses")
#'
#' # 7. Plotting only the legend
#' eclipseplot(sample_brief, plot = "legend")
#'
#' @author Pedro Rodrigues Vidor \email{pedro.vidor@@ufrgs.br}
#' @author Yohan Casiraghi (Contributor)
#' @author Sofia Simoni Rossi Fermo (Contributor)
#' @author Adolfo Moraes de Souza (Contributor)
#' @author Patricia Klarmann Ziegelmann (Contributor)
#' @author Maria Inês Schmidt (Contributor)
#' @author Maicon Falavigna (Contributor)
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#' @import ggforce
#' @import cowplot
#' @import tidyr


eclipseplot <- function(robust_data, standard = TRUE, optionals = FALSE,
                        title = NULL, plot = "full", design = "horizontal",
                        proportions = NULL) {


  # Null global variables to start
  study <- full_item <- item <- step <- value <- judgment_group <- x_coord <- y_coord <- NULL

  # --- 1. Parametric Definitions ---
  r_value <- 0.25
  y_separation <- 3 * r_value
  judgment_levels_ordered <- c("Definitely Low", "Probably Low", "Probably High", "Definitely High", "Not applicable")

  color_palette_ordered <- c(
    "Definitely Low"  = "#4C72B0",
    "Probably Low"    = "#c3d0e4",
    "Probably High"   = "#f5c7c7",
    "Definitely High" = "#E15759",
    "Not applicable"  = "#D3D3D3"
  )

  # --- 2. Data Cleaning ---
  if (!"study" %in% colnames(robust_data)) {
    robust_data$study <- paste0("PMID: ", robust_data$pmid)
  }
  study_labels <- rev(unique(robust_data$study))

  # --- 3. Internal Transformation Logic ---
  map_judg <- function(x) {
    dplyr::case_when(
      x %in% c("Definitely Yes", "Definitely Low") ~ "Definitely Low",
      x %in% c("Probably Yes", "Probably Low")   ~ "Probably Low",
      x %in% c("Probably No", "Probably High")  ~ "Probably High",
      x %in% c("Definitely No", "Definitely High") ~ "Definitely High",
      TRUE ~ "Not applicable"
    )
  }

  long_core <- robust_data |>
    dplyr::select(study, dplyr::matches("item[1-5]_step[1-2]"), dplyr::any_of("item6_step2")) |>
    tidyr::pivot_longer(cols = -study, names_to = "full_item", values_to = "value") |>
    dplyr::mutate(
      item = gsub("item(.)_step.", "\\1", full_item),
      step = as.numeric(gsub(".*step", "", full_item))
    )

  item6_step1 <- robust_data |>
    dplyr::select(study) |>
    dplyr::mutate(item = "6", step = 1, value = "Not applicable")

  long_final <- dplyr::bind_rows(long_core, item6_step1)

  if (optionals) {
    opt_cols <- colnames(robust_data)[grepl("opt[1-9]_step[1-2]", colnames(robust_data))]
    if(length(opt_cols) > 0) {
      long_opts <- robust_data |>
        dplyr::select(study, dplyr::all_of(opt_cols)) |>
        tidyr::pivot_longer(cols = -study, names_to = "full_item", values_to = "value") |>
        dplyr::mutate(
          item = gsub("opt", "o", full_item, ignore.case = TRUE),
          item = gsub("_step.", "", item),
          step = as.numeric(gsub(".*step", "", full_item))
        )
      long_final <- dplyr::bind_rows(long_final, long_opts)
    }
  }

  if (!standard) {
    long_final <- dplyr::filter(long_final, step == 2)
  }

  plot_df <- long_final |>
    dplyr::mutate(
      judgment_group = factor(map_judg(value), levels = judgment_levels_ordered),
      y_coord = as.numeric(factor(study, levels = study_labels)) * y_separation,
      x_coord = as.numeric(factor(item, levels = unique(item)))
    )

  # --- 5. Main Plot Object ---
  g_main <- ggplot2::ggplot(plot_df, ggplot2::aes(fill = judgment_group))

  if (standard) {
    g_main <- g_main +
      ggforce::geom_circle(ggplot2::aes(x0 = x_coord + ifelse(step == 1, -(r_value/2), (r_value/2)),
                                        y0 = y_coord, r = r_value), color = NA)
  } else {
    g_main <- g_main +
      # CORREÇÃO AQUI: Mudado de size para linewidth
      ggforce::geom_circle(ggplot2::aes(x0 = x_coord, y0 = y_coord, r = r_value),
                           color = "white", linewidth = 0.2)
  }

  g_main <- g_main +
    ggplot2::scale_fill_manual(values = color_palette_ordered) +
    ggplot2::scale_x_continuous(name = NULL, breaks = 1:length(unique(plot_df$item)), labels = unique(plot_df$item)) +
    ggplot2::scale_y_continuous(name = "", breaks = (1:length(study_labels)) * y_separation, labels = study_labels) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(face = "bold"),
                   legend.position = "none")

  # --- 6. Legend Construction (Geometry-based for CRAN Compatibility) ---
  leg_title <- ifelse(standard, "Two Steps for Assessing Risk of Bias", "Risk of Bias Judgment (Step 2)")
  leg_desc <- ifelse(standard, "Left: Evaluation (Step 1)\nRight: Judgment (Step 2)", "Displaying the final judgment of risk.")


  draw_bullet <- function(canvas, x_pos, y_pos, color_val, dot_size = 0.2) {
    canvas + cowplot::draw_plot(
      ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x=0, y=0), color=color_val, size=dot_size) +
        ggplot2::theme_void(),
      x = x_pos, y = y_pos, width = 0.1, height = 0.1,
      vjust = 0.52, hjust = 0.5
    )
  }

  # Classic vertical legend for horizontal design
  g_legend_classic <- cowplot::ggdraw() +
    cowplot::draw_text(leg_title, x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 10) +
    cowplot::draw_text(leg_desc,  x = 0.05, y = 0.80, hjust = 0, size = 9) +
    cowplot::draw_text("Core Items", x = 0.05, y = 0.72, hjust = 0, fontface = "bold", size = 10) +
    cowplot::draw_text("1: Random sequence generation\n2: Allocation concealment\n3: Blinding of participants\n4: Blinding of providers\n5: Blinding of assessors\n6: Outcome data missing",
                       x = 0.05, y = 0.60, hjust = 0, size = 9)

  for(i in 1:length(judgment_levels_ordered)) {
    pos_y <- 0.45 - (i * 0.04)
    # Draw the bullet and then the text
    g_legend_classic <- draw_bullet(g_legend_classic, 0.05, pos_y, color_palette_ordered[i], dot_size = 5)
    g_legend_classic <- g_legend_classic +
      cowplot::draw_text(judgment_levels_ordered[i], x = 0.12, y = pos_y, hjust = 0, size = 9)
  }

  # Modular blocks for vertical design (horizontal footer)
  g_leg_a <- cowplot::ggdraw() +
    cowplot::draw_text(leg_title, x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 9) +
    cowplot::draw_text(leg_desc,  x = 0.05, y = 0.65, hjust = 0, size = 8)

  g_leg_b <- cowplot::ggdraw() +
    cowplot::draw_text("Core Items", x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 9) +
    cowplot::draw_text("1: Random sequence generation\n2: Allocation concealment\n3: Blinding of participants\n4: Blinding of providers\n5: Blinding of assessors\n6: Outcome data missing",
                       x = 0.05, y = 0.45, hjust = 0, size = 8)

  g_leg_c <- cowplot::ggdraw()
  for(i in 1:length(judgment_levels_ordered)) {
    pos_y <- 0.85 - (i * 0.12)
    g_leg_c <- draw_bullet(g_leg_c, 0.05, pos_y, color_palette_ordered[i], dot_size = 4)
    g_leg_c <- g_leg_c +
      cowplot::draw_text(judgment_levels_ordered[i], x = 0.15, y = pos_y, hjust = 0, size = 8)
  }

  # --- 7. Final Assembly ---
  if (plot == "eclipses") return(g_main)
  if (plot == "legend") return(g_legend_classic)

  if (is.null(title)) {
    title <- ifelse(standard, "Risk of Bias (ROBUST-RCT)", "Risk of Bias (ROBUST-RCT) - Judgment Only")
  }

  plot_title <- cowplot::ggdraw() + cowplot::draw_text(title, x = 0.5, y = 0.5, size = 14, fontface = "bold")

  if (design == "horizontal") {
    if (is.null(proportions)) proportions <- c(0.75, 0.25)
    final_panel <- cowplot::plot_grid(
      g_main + ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 0, b = 10, l = 10)),
      g_legend_classic,
      ncol = 2, rel_widths = proportions, align = 'h', axis = 't'
    )
  } else {
    if (is.null(proportions)) proportions <- c(0.80, 0.20)
    legend_row <- cowplot::plot_grid(g_leg_a, g_leg_b, g_leg_c, ncol = 3, rel_widths = c(0.3, 0.35, 0.35))
    final_panel <- cowplot::plot_grid(g_main, legend_row, ncol = 1, rel_heights = proportions)
  }

  return(cowplot::plot_grid(plot_title, final_panel, ncol = 1, rel_heights = c(0.08, 0.92)))
}






#' Get path to example files (.csv)
#'
#' @param name Name of the file ("brief" or "long").
#' @export
get_eclipse_data <- function(name = "brief") {
  file_name <- ifelse(name == "brief", "sample_brief.csv", "sample_long.csv")
  path <- system.file("extdata", file_name, package = "eclipseplot")

  if (path == "") {
    stop("File not found. Make sure the package is installed correctly.")
  }

  return(path)
}


#' Export the eclipseplot to a file
#' @param plot_object The ggplot object returned by eclipseplot().
#' @param width Width in inches (Default is 14).
#' @param height Height in inches (Default is 9.8).
#' @param dpi Resolution (Default is 72).
#' @export

save_eclipseplot <- function(plot_object, width = 14, height = 9.8, dpi = 72) {
  # Note: The save function now receives the pre-built plot_object
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  file_name <- paste0("eclipseplot_robust_", timestamp, ".png")

  ggplot2::ggsave(
    filename = file_name,
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  message(paste0("Plot saved: ", file_name))
}

