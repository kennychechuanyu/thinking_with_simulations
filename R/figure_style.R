# Style shared by the figures: colors, annotation text size, number format, the
# ggplot2 theme and the function that writes each figure as a PDF and a PNG.
library(ggplot2)
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("Run R in a UTF-8 locale; the Greek letters in the figures need it.")
}

# Red marks B, the light that appeared with the trained tone, throughout. Blue
# marks the tone A in Figure 2 and the comparison light D from Figure 3 on.
# carry marks the comparisons that favor associability carryover in Figures 8
# and 9.
paper_colours <- c(ink = "#252525", grey = "#696969", light = "#BFBFBF", tint = "#EDEDED",
                   red = "#B64B41", blue = "#35658D", carry = "#AA3377")

# Size of annotate() and geom_text() labels: 9 pt, in ggplot2's millimeter units.
text_size <- 9 / .pt

# Quantities that cannot exceed 1, such as probabilities and learned values, as
# the paper prints them, without a leading zero: paper_number(.1303, 3) gives
# ".130". axis_number(), for axis labels, also drops trailing zeros:
# axis_number(c(0, .5, 1)) gives "0", ".5" and "1".
paper_number <- function(x, digits) {
  sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
}
axis_number <- function(x) {
  sub("^(-?)0\\.", "\\1.", format(x, trim = TRUE, drop0trailing = TRUE))
}

# Theme of the figures. title_gap is the space below each panel title, in
# points; a figure adds its own settings with + theme().
theme_paper <- function(title_gap = 4) {
  ink <- paper_colours[["ink"]]
  theme_classic(base_size = 10, base_family = "Helvetica") +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(size = 9, colour = ink),
      axis.title = element_text(size = 10),
      axis.title.x = element_text(margin = margin(t = 7)),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.line = element_line(linewidth = .3, colour = ink),
      axis.ticks = element_line(linewidth = .3, colour = ink),
      axis.ticks.length = unit(2.5, "pt"),
      plot.title = element_text(size = 10, face = "bold", margin = margin(b = title_gap)),
      plot.subtitle = element_text(size = 9, colour = paper_colours[["grey"]],
                                   margin = margin(b = 8)),
      plot.title.position = "plot",
      plot.margin = margin(6, 9, 6, 6),
      legend.position = "none"
    )
}

# Writes Figures/<name>.png at the given resolution and Figures/<name>.pdf. The
# PDF comes from the Quartz device on macOS, which spaces letters as the system
# does, and from cairo_pdf elsewhere; both keep Unicode labels such as the en
# dash and the Greek letters. Where neither device opens, the PDF is skipped.
save_figure <- function(plot, name, width, height, dpi = 600) {
  pdf_file <- file.path("Figures", paste0(name, ".pdf"))
  png_file <- file.path("Figures", paste0(name, ".png"))
  ggsave(png_file, plot, width = width, height = height, dpi = dpi, bg = "white")
  device <- pdf_device()
  if (is.null(device)) {
    cat(sprintf("Saved %s; %s not written because no PDF device opens\n", png_file, pdf_file))
  } else {
    ggsave(pdf_file, plot, width = width, height = height, device = device, bg = "white")
    cat(sprintf("Saved %s and %s\n", pdf_file, png_file))
  }
}

# The first of Quartz (macOS only) and cairo_pdf that writes a test PDF without
# a warning or an error, or NULL if neither does.
pdf_device <- function() {
  quartz_pdf <- function(filename, ...) grDevices::quartz(file = filename, type = "pdf", ...)
  devices <- if (capabilities("aqua")) list(quartz_pdf, grDevices::cairo_pdf) else
    list(grDevices::cairo_pdf)
  for (device in devices) if (device_opens(device)) return(device)
  NULL
}
device_opens <- function(device) {
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file))
  tryCatch({
    device(file)
    grDevices::dev.off()
    file.exists(file)
  }, warning = function(w) FALSE, error = function(e) FALSE)
}
