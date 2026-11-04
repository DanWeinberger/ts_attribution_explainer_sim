# Age Group Proportional Waffle Grids — RSV Incidence
# Uses grid graphics for precise physical layout
# Output: 8 x 3 inches at 200 dpi

library(grid)

# ── Data ──────────────────────────────────────────────────────────
#These numbers are extracted from Zheng Z, Warren JL, Shapiro ED, Pitzer VE, Weinberger DM. Estimated incidence of respiratory hospitalizations attributable to RSV infections across age and socioeconomic groups. Pneumonia (Nathan). 2022;14: 6.
age_labels    <- c("<1y","1y","2-4y","5-9y","10-19y","20-44y","45-65y","65-84y","85+y")
shaded_frac   <- c(0.72, 0.46, 0.32, 0.12, 0.08, 0.06, 0.16, 0.04, 0.03)
rsv_incidence <- c(1500, 500, 180, 40, 12, 9, 20, 130, 500)

highlight_color <- "#D94F3D"
base_color      <- "#909090"
grid_lwd        <- 0.4
COLS            <- 10L

# ── Grid row counts: rows ∝ incidence (physical height) ───────────
# ref_rows sets the maximum; rows[i] = round(ref_rows * inc[i] / max_inc)
# ref=30 gives good resolution while keeping all groups with >= 1 red cell
ref_rows  <- 30L
max_inc   <- max(rsv_incidence)
grid_rows <- pmax(round(ref_rows * rsv_incidence / max_inc), 1L)
n_red     <- round(shaded_frac * COLS * grid_rows)
max_rows  <- max(grid_rows)   # = ref_rows = 30
n_groups  <- length(age_labels)

# ── Physical layout (inches) ──────────────────────────────────────
fig_w   <- 8.0
fig_h   <- 3.0
dpi     <- 200

mar_l   <- 0.10;  mar_r   <- 0.10
mar_top <- 0.38;  mar_bot <- 0.28

usable_w <- fig_w - mar_l - mar_r
usable_h <- fig_h - mar_top - mar_bot

gap_frac <- 0.15   # gap = gap_frac * panel_width
cell_w   <- usable_w / (n_groups * COLS + (n_groups - 1L) * gap_frac * COLS)
cell_h   <- usable_h / max_rows   # physical row height; rows * cell_h ∝ incidence
gap_w    <- cell_w * gap_frac * COLS

# ── Draw ──────────────────────────────────────────────────────────
png("./figures/age_waffle_proportional.png",
    width = fig_w * dpi, height = fig_h * dpi, res = dpi, bg = "white")
grid.newpage()

for (i in seq_along(age_labels)) {
  rows <- grid_rows[i]
  nr   <- n_red[i]
  x0   <- mar_l + (i - 1L) * (COLS * cell_w + gap_w)   # left edge of panel
  y0   <- mar_bot                                         # bottom of every panel

  cell <- 0L
  for (r in seq(rows - 1L, 0L)) {        # r = rows-1 is top row → red fills from top
    for (c in seq(0L, COLS - 1L)) {
      cell <- cell + 1L
      fill <- if (cell <= nr) highlight_color else base_color
      grid.rect(
        x      = unit(x0 + (c + 0.5) * cell_w, "inches"),
        y      = unit(y0 + (r + 0.5) * cell_h, "inches"),
        width  = unit(cell_w, "inches"),
        height = unit(cell_h, "inches"),
        gp = gpar(fill = fill, col = "white", lwd = grid_lwd)
      )
    }
  }

  cx <- x0 + COLS * cell_w / 2

  # % label: consistent line above max_rows
  grid.text(
    label = paste0(round(nr / (COLS * rows) * 100), "%"),
    x = unit(cx, "inches"),
    y = unit(mar_bot + max_rows * cell_h + 0.04, "inches"),
    just = c("centre", "bottom"),
    gp = gpar(fontface = "bold", fontsize = 7.5, col = highlight_color)
  )

  # Age label below baseline
  grid.text(
    label = age_labels[i],
    x = unit(cx, "inches"),
    y = unit(mar_bot - 0.04, "inches"),
    just = c("centre", "top"),
    gp = gpar(fontface = "bold", fontsize = 7.5, col = "#222222")
  )
}

# Title
grid.text(
  label = "Proportion affected by age group  \u2022  height proportional to RSV incidence",
  x  = unit(fig_w / 2, "inches"),
  y  = unit(fig_h - 0.10, "inches"),
  just = c("centre", "top"),
  gp = gpar(fontface = "bold", fontsize = 8.5, col = "#222222")
)

dev.off()
message("Saved: age_waffle_proportional.png")