library(ggplot2)
library(patchwork)

out <- "P:/KDIS/teaching/IDP/_materials/Fall 2026/_slides/figures"
z <- 0
col_inc <- "#2166ac"   # classified poor -> included
col_exc <- "#6b6b6b"   # classified non-poor -> excluded
red     <- "#d7301f"   # exclusion error
amber   <- "#f2a900"   # inclusion error

curves <- function(shift = 0) {
  x <- seq(-4.6, 4.6, length.out = 1000)
  data.frame(x = x, inc = dnorm(x, -1.3 + shift), exc = dnorm(x, 1.3 + shift))
}

panel <- function(shift = 0, excl = FALSE, incl = FALSE, ghost = FALSE,
                  curve_labels = TRUE, err_labels = c("Exclusion error", "Inclusion error"),
                  title = NULL, base = 17, axis_labels = TRUE, bottom_err = NULL) {
  d <- curves(shift)
  p <- ggplot(d, aes(x))
  if (ghost) {
    g <- curves(0)
    p <- p +
      geom_line(data = g, aes(y = inc), colour = col_inc, linetype = "dashed", alpha = .45, linewidth = .8) +
      geom_line(data = g, aes(y = exc), colour = col_exc, linetype = "dashed", alpha = .45, linewidth = .8)
  }
  if (excl) p <- p + geom_area(data = subset(d, x <= z), aes(y = exc), fill = red, alpha = .6)
  if (incl) p <- p + geom_area(data = subset(d, x >= z), aes(y = inc), fill = amber, alpha = .7)
  p <- p +
    geom_line(aes(y = inc), colour = col_inc, linewidth = 1.4) +
    geom_line(aes(y = exc), colour = col_exc, linewidth = 1.4) +
    annotate("segment", x = -4.6, xend = 4.7, y = 0, yend = 0, linewidth = .6,
             arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
    annotate("segment", x = z, xend = z, y = 0, yend = .5, linewidth = 1) +
    annotate("text", x = z, y = .53, label = "Poverty line z", size = base / 3.2, fontface = "bold")
  if (axis_labels) p <- p +
    annotate("text", x = z - .15, y = -.035, label = "\u2190 truly poor", hjust = 1, size = base / 3.4) +
    annotate("text", x = z + .15, y = -.035, label = "truly non-poor \u2192", hjust = 0, size = base / 3.4) +
    annotate("text", x = 4.7, y = -.035, label = "True income", hjust = 1, size = base / 3.4, colour = "grey30")
  if (!is.null(bottom_err)) p <- p +
    annotate("text", x = -.3, y = -.045, label = bottom_err[1], hjust = 1, colour = red,
             fontface = "bold", size = base / 3) +
    annotate("text", x = .3, y = -.045, label = bottom_err[2], hjust = 0, colour = "#b07a00",
             fontface = "bold", size = base / 3)
  if (curve_labels) {
    p <- p +
      annotate("text", x = -1.3 + shift, y = .445, label = "Classified poor\n\u2192 included",
               colour = col_inc, fontface = "bold", size = base / 3.2, lineheight = .9) +
      annotate("text", x = 1.3 + shift, y = .445, label = "Classified non-poor\n\u2192 excluded",
               colour = col_exc, fontface = "bold", size = base / 3.2, lineheight = .9)
  }
  if (excl && !is.na(err_labels[1])) {
    p <- p +
      annotate("segment", x = -3.3, xend = -.35, y = .075, yend = .03, colour = red, linewidth = .8,
               arrow = arrow(length = unit(.2, "cm"))) +
      annotate("text", x = -3.4, y = .075, label = err_labels[1], hjust = 1, colour = red,
               fontface = "bold", size = base / 3.2)
  }
  if (incl && !is.na(err_labels[2])) {
    p <- p +
      annotate("segment", x = 3.3, xend = .35, y = .075, yend = .03, colour = "#b07a00", linewidth = .8,
               arrow = arrow(length = unit(.2, "cm"))) +
      annotate("text", x = 3.4, y = .075, label = err_labels[2], hjust = 0, colour = "#b07a00",
               fontface = "bold", size = base / 3.2)
  }
  p +
    coord_cartesian(xlim = c(-5.6, 5.6), ylim = c(-.07, .56), clip = "off") +
    labs(title = title) +
    theme_void(base_size = base) +
    theme(plot.title = element_text(hjust = .5, face = "bold", margin = margin(b = 6)),
          plot.margin = margin(8, 8, 14, 8),
          plot.background = element_rect(fill = "white", colour = NA))
}

save <- function(p, f, w = 10, h = 5.4) ggsave(file.path(out, f), p, width = w, height = h, dpi = 150)

save(panel(),                          "targeting_step1.png")
save(panel(excl = TRUE),               "targeting_step2.png")
save(panel(excl = TRUE, incl = TRUE),  "targeting_step3.png")

strict <- panel(-0.55, TRUE, TRUE, ghost = TRUE, curve_labels = FALSE,
                err_labels = c(NA, NA), axis_labels = FALSE,
                bottom_err = c("More exclusion errors \u2191", "Fewer inclusion errors \u2193"),
                title = "Stricter criteria: fewer qualify", base = 15) +
  annotate("segment", x = -.6, xend = -1.5, y = .46, yend = .46, linewidth = 1,
           arrow = arrow(length = unit(.22, "cm"), type = "closed"))
loose  <- panel(+0.55, TRUE, TRUE, ghost = TRUE, curve_labels = FALSE,
                err_labels = c(NA, NA), axis_labels = FALSE,
                bottom_err = c("Fewer exclusion errors \u2193", "More inclusion errors \u2191"),
                title = "Looser criteria: more qualify", base = 15) +
  annotate("segment", x = .6, xend = 1.5, y = .46, yend = .46, linewidth = 1,
           arrow = arrow(length = unit(.22, "cm"), type = "closed"))
save(strict + loose, "targeting_step4.png", w = 14, h = 5.4)
