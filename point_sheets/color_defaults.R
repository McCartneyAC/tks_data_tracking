inova<-c("#004b8d", "#d52b1e", "#6caddf", "#4d4f53", "#a5a5a9")
pal_inova <- function(palette = c("inova"), alpha = 1) {
  palette <- match.arg(palette)
  if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")
  raw_cols <- inova
  raw_cols_rgb <- col2rgb(raw_cols)
  alpha_cols <- rgb(
    raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
    alpha = alpha * 255L, names = names(raw_cols),
    maxColorValue = 255L
  )
  scales::manual_pal(unname(alpha_cols))
}
#' Scale Colors as inova healthsystems
#'
#' @export scale_color_inova
scale_color_inova <- function(palette = c("inova"), alpha = 1, ...) {
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", "inova", pal_inova(palette, alpha), ...)
}
scale_colour_inova<-scale_color_inova
