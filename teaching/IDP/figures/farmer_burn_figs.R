# Lecture 10: the farmer's burning decision as an externality (illustrative curves)
# Step 1: her benefit vs. her own cost -> she burns Q_F
# Step 2: add damage to others and the cost to society
# Step 3: Q* vs. Q_F and the excess burning
library(ggplot2)

out  <- "P:/KDIS/teaching/IDP/_materials/Fall 2026/_slides/figures"
blue <- "#2166ac"; grey <- "#6b6b6b"; red <- "#d7301f"

xmax <- 10.5
mb   <- function(q) 10 - q            # her benefit from burning one more acre
mpc  <- function(q) 1 + 0 * q         # her own cost of burning one more acre
md   <- function(q) 0.8 * q           # damage to people downwind
smc  <- function(q) mpc(q) + md(q)    # cost to society
qf   <- 9                             # mb = mpc
qs   <- 5                             # mb = smc
q    <- seq(0, 10, length.out = 200)

fig <- function(step) {
  p <- ggplot() +
    annotate("segment", x = 0, xend = xmax, y = 0, yend = 0, linewidth = .6,
             arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 11, linewidth = .6,
             arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
    annotate("text", x = xmax + .2, y = 0, label = "Acres burned", hjust = 0, size = 5) +
    annotate("text", x = .15, y = 11.2, label = "$ per acre", hjust = 0, size = 5)
  if (step == 3) {
    tri <- data.frame(x = c(qs, qf, qf), y = c(mb(qs), mb(qf), smc(qf)))
    p <- p + geom_polygon(data = tri, aes(x, y), fill = red, alpha = .18)
  }
  if (step >= 2) {
    p <- p +
      geom_line(aes(q, md(q)), colour = red, linetype = "dashed", linewidth = 1) +
      geom_line(aes(q[q <= 10], smc(q[q <= 10])), colour = red, linewidth = 1.6) +
      annotate("text", x = 10.1, y = md(10), label = "Damage to others", hjust = 0, colour = red, size = 5) +
      annotate("text", x = 10.1, y = smc(10), label = "Cost to society", hjust = 0, colour = red,
               fontface = "bold", size = 5)
  }
  p <- p +
    geom_line(aes(q, mb(q)), colour = blue, linewidth = 1.4) +
    geom_line(aes(q, mpc(q)), colour = grey, linewidth = 1.2) +
    annotate("text", x = 1.2, y = 10, label = "Her benefit", hjust = 0, colour = blue,
             fontface = "bold", size = 5) +
    annotate("text", x = 10.1, y = mpc(10), label = "Her cost", hjust = 0, colour = grey,
             fontface = "bold", size = 5) +
    annotate("segment", x = qf, xend = qf, y = 0, yend = mb(qf), linetype = "dotted") +
    annotate("text", x = qf, y = -.6, label = "Q[F]", parse = TRUE, size = 6) +
    annotate("text", x = qf, y = -1.3, label = "she burns", size = 4.2, colour = "grey30")
  if (step == 3) {
    p <- p +
      annotate("segment", x = qs, xend = qs, y = 0, yend = mb(qs), linetype = "dotted") +
      annotate("text", x = qs, y = -.6, label = "Q*", size = 5.5) +
      annotate("text", x = qs, y = -1.3, label = "best for society", size = 4.2, colour = "grey30") +
      annotate("point", x = qs, y = mb(qs), size = 2.5)
  }
  p + coord_cartesian(xlim = c(0, 13.2), ylim = c(-1.5, 11.5), clip = "off") +
    theme_void() + theme(plot.margin = margin(10, 10, 10, 10),
                         plot.background = element_rect(fill = "white", colour = NA))
}

for (s in 1:3) {
  ggsave(file.path(out, sprintf("farmer_burn_step%d.png", s)), fig(s),
         width = 8, height = 5, dpi = 150, bg = "white")
}
cat("ok\n")
