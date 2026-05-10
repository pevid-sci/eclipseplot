#' Create a plot for a risk of bias assessment with the ROBUST-RCT tool
#'
#' @description
#' A visual representation of risk-of-bias assessments of randomized controlled trials using the ROBUST-RCT framework, as described in Wang et al. (2025) <doi:10.1136/bmj-2024-081199>.
#' It can display either the standard two-step assessment or a simplified judgment-only version.
#'
#'
#' @param robust_data A data frame containing the data from the risk-of-bias assessment with the ROBUST-RCT tool.
#' @param standard Logical. If \code{TRUE} (default), displays the two-step eclipseplot. If \code{FALSE}, displays only the step 2 (judgment).
#' @param optionals Logical. If \code{TRUE}, includes columns \code{opt1} to \code{opt9}.
#' @param title Character. Custom title for the eclipse plot.
#' @param plot Character. Parts to display: \code{"full"} (default), \code{"eclipses"}, or \code{"legend"}.
#' @param design Character. Orientation of the assembly: \code{"horizontal"} (default) or \code{"vertical"}.
#' @param proportions Numeric vector of length 2. Relative widths/heights for plot and legend.
#' @param opt_prefix Character. Text for optional items in the plot. Default is "o".
#'
#' @return A \code{ggplot} object (specifically a \code{ggdraw} object from the \code{cowplot} package)
#' representing the visualization of the risk of bias. The output can be further customized
#' using standard \code{ggplot2} functions or saved via \code{save_eclipseplot}.
#'
#' @details
#' The input data frame must include the required columns for the ROBUST-RCT domains.
#' The following columns are expected:
#' \itemize{
#'   \item \strong{pmid:} PubMed ID or unique identifier.
#'   \item \strong{study:} Study name (e.g., "Author, 2026").
#'   \item \strong{Core Items (1-6):} Columns named 'itemX_step1' and 'itemX_step2' (where X is 1 to 6). As standard in the ROBUST-RCT tool, item 6 does not have a multiple-choice step 1 evaluation, so it is automatically marked as NA.
#'   \item \strong{Optional Items:} If optionals = TRUE, columns named 'optX' (where X is 1 to 9). Legacy formats ('optX_step1' and 'optX_step2') are supported, but only Step 2 will be plotted.
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
#' # 1. Standard plot (Core items, Steps 1 & 2)
#' data(sample_brief)
#' eclipseplot_plot <- eclipseplot(sample_brief)
#' print(eclipseplot_plot)
#'
#' \donttest{
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
#' }
#'
#' @author Pedro Rodrigues Vidor \email{pedro.vidor@@ufrgs.br}
#' @author Yohan Casiraghi (Contributor)
#' @author Sofia Simoni Rossi Fermo (Contributor)
#' @author Adolfo Moraes de Souza (Contributor)
#' @author Patricia Klarmann Ziegelmann (Contributor)
#' @author Maria Inês Schmidt (Contributor)
#' @author Maicon Falavigna (Contributor)
#'
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_x_continuous scale_y_continuous coord_fixed theme_minimal theme element_blank element_text margin ggsave geom_point theme_void
#' @importFrom dplyr select mutate bind_rows filter case_when matches any_of all_of
#' @importFrom ggforce geom_circle
#' @importFrom cowplot ggdraw draw_text draw_plot plot_grid
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel


eclipseplot <- function(robust_data, standard = TRUE, optionals = FALSE,
                        title = NULL, plot = "full", design = "horizontal",
                        proportions = NULL, opt_prefix = "o") {
  
  
  # Null global variables to start
  study <- full_item <- item <- step <- value <- judgment_group <- x_coord <- y_coord <- is_optional <- NULL
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
      step = as.numeric(gsub(".*step", "", full_item)),
      is_optional = FALSE # NOVO: Flag para itens principais
    )
  
  item6_step1 <- robust_data |>
    dplyr::select(study) |>
    dplyr::mutate(item = "6", step = 1, value = "Not applicable", is_optional = FALSE)
  
  long_final <- dplyr::bind_rows(long_core, item6_step1)
  
  if (optionals) {
    # Includes _step2 for compatibility with legacy datasets
    opt_cols <- colnames(robust_data)[grepl("opt[1-9]$|opt[1-9]_step2", colnames(robust_data), ignore.case = TRUE)]
    
    if(length(opt_cols) > 0) {
      long_opts <- robust_data |>
        dplyr::select(study, dplyr::all_of(opt_cols)) |>
        tidyr::pivot_longer(cols = -study, names_to = "full_item", values_to = "value") |>
        dplyr::mutate(
          item = gsub("opt", opt_prefix, full_item, ignore.case = TRUE),
          item = gsub("_step2", "", item),
          step = 2, # Treated as judgment because there is no steps for optional items
          is_optional = TRUE # Highlights that optional items will be needed in the plot
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
  
  # Garante que o Step 2 seja desenhado por cima do Step 1 (Efeito eclipse perfeito)
  plot_df <- plot_df |> dplyr::arrange(step)
  
  g_main <- ggplot2::ggplot(plot_df, ggplot2::aes(fill = judgment_group))
  
  if (standard) {
    # Camada 1: Itens principais (meia-luas)
    g_main <- g_main +
      ggforce::geom_circle(
        data = plot_df |> dplyr::filter(!is_optional),
        ggplot2::aes(x0 = x_coord + ifelse(step == 1, -(r_value/2), (r_value/2)),
                     y0 = y_coord, r = r_value), color = NA)
    
    # Camada 2: Itens opcionais (círculo inteiro, centralizado, com borda)
    if (optionals) {
      g_main <- g_main +
        ggforce::geom_circle(
          data = plot_df |> dplyr::filter(is_optional),
          ggplot2::aes(x0 = x_coord, y0 = y_coord, r = r_value),
          color = "white", linewidth = 0.2)
    }
  } else {
    # Modo judgment-only: Tudo é centralizado
    g_main <- g_main +
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
                   legend.position = "none") # Hides the standard R legend
  
  # --- 6. Legend Construction (Geometry-based for CRAN Compatibility) ---
  
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
  y_offset <- ifelse(standard, 0, 0.13) # Closes the empty space if there is no first part of the legend
  g_legend_classic <- cowplot::ggdraw()
  
  #  The left/right only appears in the legend if the plot has the two steps (standard plot)
  if (standard) {
    g_legend_classic <- g_legend_classic +
      cowplot::draw_text("Two Steps for Assessing Risk of Bias", x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 10) +
      cowplot::draw_text("Left: Evaluation (Step 1)\nRight: Judgment (Step 2)",  x = 0.05, y = 0.80, hjust = 0, size = 9)
  }
  
  # Rest of the text using y_offset for dynamic alignment
  g_legend_classic <- g_legend_classic +
    cowplot::draw_text("Core Items", x = 0.05, y = 0.72 + y_offset, hjust = 0, fontface = "bold", size = 10) +
    cowplot::draw_text("1: Random sequence generation\n2: Allocation concealment\n3: Blinding of participants\n4: Blinding of providers\n5: Blinding of assessors\n6: Outcome data missing",
                       x = 0.05, y = 0.60 + y_offset, hjust = 0, size = 9)
  
  for(i in 1:length(judgment_levels_ordered)) {
    pos_y <- (0.50 + y_offset) - (i * 0.04)
    g_legend_classic <- draw_bullet(g_legend_classic, 0.05, pos_y, color_palette_ordered[i], dot_size = 5)
    g_legend_classic <- g_legend_classic +
      cowplot::draw_text(judgment_levels_ordered[i], x = 0.12, y = pos_y, hjust = 0, size = 9)
  }
  
  # Modular blocks for vertical design (horizontal footer)
  core_items_lines <- c("1: Random sequence generation", "2: Allocation concealment", "3: Blinding of participants", "4: Blinding of providers", "5: Blinding of assessors", "6: Outcome data missing")
  
  g_leg_a <- cowplot::ggdraw()
  
  # In the vertical plot, the legend also have the left/right legend only if the plot has the two steps
  if (standard) {
    leg_desc_lines <- c("Left: Evaluation (Step 1)", "Right: Judgment (Step 2)")
    g_leg_a <- g_leg_a + cowplot::draw_text("Two Steps for Assessing Risk of Bias", x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 9)
    for(i in 1:length(leg_desc_lines)) {
      pos_y <- 0.85 - (i * 0.12)
      g_leg_a <- g_leg_a + cowplot::draw_text(leg_desc_lines[i], x = 0.05, y = pos_y, hjust = 0, size = 8)
    }
  }
  
  g_leg_b <- cowplot::ggdraw() +
    cowplot::draw_text("Core Items", x = 0.05, y = 0.85, hjust = 0, fontface = "bold", size = 9)
  for(i in 1:length(core_items_lines)) {
    pos_y <- 0.85 - (i * 0.12)
    g_leg_b <- g_leg_b + cowplot::draw_text(core_items_lines[i], x = 0.05, y = pos_y, hjust = 0, size = 8)
  }
  
  g_leg_c <- cowplot::ggdraw()
  for(i in 1:length(judgment_levels_ordered)) {
    pos_y <- 0.85 - (i * 0.12)
    g_leg_c <- draw_bullet(g_leg_c, 0.05, pos_y, color_palette_ordered[i], dot_size = 4)
    g_leg_c <- g_leg_c + cowplot::draw_text(judgment_levels_ordered[i], x = 0.15, y = pos_y, hjust = 0, size = 8)
  }
  
  # --- 7. Final Assembly ---
  
  # The following line corrects a small issue where parts of the graphic were transparent
  bg_white <- ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))
  
  if (plot == "eclipses") return(g_main)
  if (plot == "legend") {
    if (design == "vertical") {
      # If the whole plot is in vertical, returns the legend in 3 columns
      # (which makes the legend more horizontal, ideal for being used with a vertical figure)
      legend_row <- cowplot::plot_grid(g_leg_a, g_leg_b, g_leg_c, ncol = 3, rel_widths = c(0.3, 0.35, 0.35))
      return(legend_row + bg_white)
    } else {
      # If horizontal, returns the classic version (3 rows of content, as standard)
      return(g_legend_classic + bg_white)
    }
  }
  
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
  
  final_plot <- cowplot::plot_grid(plot_title, final_panel, ncol = 1, rel_heights = c(0.08, 0.92))
  return(final_plot + bg_white)
}






#' Get path to example files (.csv)
#'
#' @param name Name of the file ("brief" or "long").
#' @return A character string containing the absolute file path to the requested
#' sample CSV file within the package installation directory.
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
#'
#' @param plot_object The ggplot object returned by eclipseplot().
#' @param filename Character. The name of the file to save. Must include extension (e.g., "plot.png").
#' @param width Width in inches (Default is 14).
#' @param height Height in inches (Default is 9.8).
#' @param dpi Resolution (Default is 72).
#'
#' @return No return value, called for side effects (saves a file to the specified location).
#'
#' @examples
#' \donttest{
#'   # Using a temporary directory
#'   data(sample_brief)
#'   p <- eclipseplot(sample_brief)
#'   tmp_file <- file.path(tempdir(), "test_plot.png")
#'   save_eclipseplot(p, filename = tmp_file)
#' }
#' @export
save_eclipseplot <- function(plot_object, filename, width = 14, height = 9.8, dpi = 72) {
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  message(paste0("Plot saved to: ", filename))
}









#' Convert ROBUST Standard Spreadsheet to eclipseplot format
#'
#' @description
#' This function facilitates the conversion of data extracted using the standard
#' ROBUST spreadsheet into the format required by 'eclipseplot'. It automatically
#' identifies header locations, maps Step 1 and Step 2 columns for each domain,
#' and filters out common spreadsheet artifacts (e.g., merged cells or labels like
#' "Intervention Group").
#'
#' @param path Path to the '.xlsx' file.
#' @param sheet Sheet name or index (defaults to 1).
#'
#' @return A standardized data frame formatted for 'eclipseplot' with specific
#' column names (itemX_stepY) and categorical levels for bias assessment.
#'
#' @export
#' @import readxl
#' @importFrom utils tail
from_xlsx <- function(path, sheet = 1) {
  
  # 1. Load data as text
  raw_data <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
  
  # 2. Locate the row containing "Step 1" or "Step 2"
  # We convert to character and remove NAs to check the row content
  header_row_idx <- which(apply(raw_data, 1, function(x) {
    any(grepl("Step 1|Step 2", as.character(x), ignore.case = TRUE))
  }))[1]
  
  if (is.na(header_row_idx)) {
    stop("Could not find ROBUST-RCT headers (Step 1/Step 2) in the spreadsheet. Please check if the file follows the standard format.")
  }
  
  # 3. Extract headers and data body
  headers <- as.character(raw_data[header_row_idx, ])
  data_body <- raw_data[(header_row_idx + 1):nrow(raw_data), ]
  colnames(data_body) <- headers
  
  # 4. Map columns dynamically with fallbacks
  step1_ind <- which(grepl("Step 1", headers, ignore.case = TRUE))
  step2_ind <- which(grepl("Step 2", headers, ignore.case = TRUE))
  
  # Improved Study Index Search: look for 'Study', 'Article', 'Author', or just use column 1
  study_idx <- which(grepl("Study|Article|Author", headers, ignore.case = TRUE))[1]
  if (is.na(study_idx)) study_idx <- 1
  
  # Item 6 Step 2 is the last Step 2 column
  last_step2_idx <- utils::tail(step2_ind, 1)
  
  # Check if we found enough columns to proceed
  if (length(step1_ind) < 5 || length(step2_ind) < 6) {
    stop("The spreadsheet does not contain enough 'Step 1' or 'Step 2' columns for the 6 standard items.")
  }
  
  clean_df <- data.frame(
    pmid = 0,
    study = data_body[[study_idx]],
    
    item1_step1 = data_body[[step1_ind[1]]],
    item1_step2 = data_body[[step2_ind[1]]],
    
    item2_step1 = data_body[[step1_ind[2]]],
    item2_step2 = data_body[[step2_ind[2]]],
    
    item3_step1 = data_body[[step1_ind[3]]],
    item3_step2 = data_body[[step2_ind[3]]],
    
    item4_step1 = data_body[[step1_ind[4]]],
    item4_step2 = data_body[[step2_ind[4]]],
    
    item5_step1 = data_body[[step1_ind[5]]],
    item5_step2 = data_body[[step2_ind[5]]],
    
    item6_step2 = data_body[[last_step2_idx]]
  )
  
  # 5. Filter out noise
  noise_labels <- c("Intervention", "Control", "Overall", "Study", "Article", "Author", NA)
  clean_df <- clean_df[!clean_df$study %in% noise_labels, ]
  clean_df <- clean_df[!is.na(clean_df$study), ]
  
  return(clean_df)
}





#' Data Cleanning Helper for Eclipse Plot
#'
#' @description
#' Standardizes both column names and cell values to ensure compatibility with
#' the 'eclipseplot' engine. It uses fuzzy matching via regular expressions to
#' identify headers and a robust dictionary to map various shorthand judgments
#' into the four standard risk-of-bias levels.
#'
#' @param df A data frame to be cleaned.
#'
#' @return A standardized data frame with specific categorical levels ("Definitely Low", "Probably Low",
#' "Probably High", and "Definitely High") and column names (itemX_stepY) for use in the eclipse plot.
#'
#' @details
#' \strong{Column Name Standardization:}
#' The function identifies columns regardless of case and replaces them with:
#' \itemize{
#'   \item \code{study}: Matches "study", "article", "label", "paper", or "rct".
#'   \item \code{pmid}: Matches "pmid", "pm_id", "pm id", "article id", "article_id", or "id".
#'   \item \code{itemX_stepY}: Matches any string containing "item" followed by a number
#'   and "step" followed by a number (e.g., "Item 1, Step 2" becomes "item1_step2").
#' }
#'
#' \strong{Value Standardization (Fuzzy Mapping):}
#' Judgment inputs are converted to lowercase and trimmed of whitespace.
#' The mapping logic is as follows:
#' \itemize{
#'   \item \strong{"Definitely Low"}: Result for "definitely yes", "definitely low", "dy", "dl", "yes", or "y".
#'   \item \strong{"Probably Low"}: Result for "probably yes", "probably low", "py", or "pl".
#'   \item \strong{"Probably High"}: Result for "probably no", "probably high", "pn", "ph", or "probably-no".
#'   \item \strong{"Definitely High"}: Result for "definitely no", "definitely high", "dn", "dh", "no", or "n".
#'   \item \strong{"Not applicable"}: Assigned to any other value, including empty cells (NA) or unrecognized text.
#' }
#'
#' @note
#' For the purposes of the 'eclipseplot' function, there is no distinction between
#' the internal mapping of Step 1 and Step 2 values. Both are standardized using
#' the Step 2 visual scheme (color palette) to ensure a cohesive graphical
#' representation, as the underlying directional logic remains the same.
#'
#' @example
#' data(messy)
#' messy
#' organized <- eclipsedf(messy)
#' organized
#'
#' @export


eclipsedf <- function(df) {
  
  # 1. Standardize Values Function (Internal)
  standardize_values <- function(x) {
    if (is.na(x)) return("Not applicable")
    val <- tolower(trimws(as.character(x)))
    
    if (val %in% c("definitely yes", "definitely low", "dy", "dl", "yes", "y")) return("Definitely Low")
    if (val %in% c("probably yes", "probably low", "py", "pl")) return("Probably Low")
    if (val %in% c("probably no", "probably high", "pn", "ph", "probably-no")) return("Probably High")
    if (val %in% c("definitely no", "definitely high", "dn", "dh", "no", "n")) return("Definitely High")
    
    return("Not applicable")
  }
  
  # 2. Rename Columns using Flexible Regex
  current_names <- colnames(df)
  new_names <- current_names
  
  for (i in seq_along(current_names)) {
    name_clean <- tolower(current_names[i])
    
    # Identify Study (Study, Article, label, paper, rct)
    if (grepl("study|article|label|paper|rct", name_clean)) {
      new_names[i] <- "study"
    }
    # Identify PMID (pmid, pm id, article id, id - accepting dots or spaces)
    else if (grepl("pmid|pm[^a-z0-9]*id|article[^a-z0-9]*id|^id$", name_clean)) {
      new_names[i] <- "pmid"
    }
    # Identify Item X Step Y (Captures Item.1.Step.2, item 1, step 2, etc.)
    # The [^0-9]* means "anything that is not a number"
    else if (grepl("item[^0-9]*(\\d)[^0-9]*step[^0-9]*(\\d)", name_clean)) {
      item_num <- gsub(".*item[^0-9]*(\\d).*", "\\1", name_clean)
      step_num <- gsub(".*step[^0-9]*(\\d).*", "\\1", name_clean)
      new_names[i] <- paste0("item", item_num, "_step", step_num)
    }
  }
  colnames(df) <- new_names
  
  # 3. Apply Value Cleaning to Item columns
  target_cols <- grep("item\\d_step\\d", colnames(df))
  
  if (length(target_cols) > 0) {
    for (col_idx in target_cols) {
      df[[col_idx]] <- sapply(df[[col_idx]], standardize_values)
    }
  }
  
  # 4. Final Structure Fixes
  if (!"study" %in% colnames(df)) {
    colnames(df)[1] <- "study"
  }
  if (!"pmid" %in% colnames(df)) {
    df$pmid <- 0
  }
  
  # 5. Reorder to EclipsePlot Standard
  expected_order <- c("pmid", "study",
                      "item1_step1", "item1_step2",
                      "item2_step1", "item2_step2",
                      "item3_step1", "item3_step2",
                      "item4_step1", "item4_step2",
                      "item5_step1", "item5_step2",
                      "item6_step2")
  
  opt_cols <- colnames(df)[grepl("opt", colnames(df), ignore.case = TRUE)]
  if(length(opt_cols) > 0) {
    expected_order <- c(expected_order, opt_cols)
  }
  
  existing_cols <- expected_order[expected_order %in% colnames(df)]
  df_final <- df[, existing_cols, drop = FALSE]
  
  return(df_final)
}
