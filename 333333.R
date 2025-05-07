# Install the remotes package if you haven't already
install.packages("remotes")

# Install the aRtsy package from GitHub
remotes::install_github("koenderks/aRtsy")

# Load the aRtsy package
library(aRtsy)

# Now you can use the canvas_stripes function
set.seed(1)
canvas_stripes(colors = colorPalette("random", n = 10))
#===============================================================
library(ggplot2)
library(dplyr)

set.seed(123)
n <- 300

make_patch <- function(xmin, xmax, ymin, ymax, color) {
  data.frame(
    x = runif(n, xmin, xmax),
    y = runif(n, ymin, ymax),
    group = paste(xmin, ymin, sep = "-"),
    color = color
  )
}

# Define patch positions and colors
patches <- expand.grid(x = seq(0, 10, by = 2), y = seq(0, 10, by = 2))
colors <- sample(c("#F4A261", "#2A9D8F", "#E76F51", "#264653", "#E9C46A", "#8AB6D6", "#F28DB2"), nrow(patches), replace = TRUE)

data <- do.call(rbind, Map(
  make_patch,
  patches$x, patches$x + 2,
  patches$y, patches$y + 2,
  colors
))

ggplot(data, aes(x = x, y = y, group = group)) +
  geom_line(aes(color = color), alpha = 0.7, size = 0.3) +
  geom_line(aes(x = y, y = x, color = color), alpha = 0.7, size = 0.3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void()
#====================================
library(ggplot2)
library(dplyr)

set.seed(456)
n_lines <- 300
n_points <- 100

# For each vertical line, we generate 100 y-values
x_vals <- seq(0, 10, length.out = n_lines)

line_data <- lapply(seq_along(x_vals), function(i) {
  tibble(
    x = x_vals[i] + rnorm(n_points, 0, 0.01),  # x "jitter"
    y = seq(0, 10, length.out = n_points) + rnorm(n_points, 0, 0.1),
    line_id = i
  )
}) %>% bind_rows()

# Add random color per line
line_data <- line_data %>%
  group_by(line_id) %>%
  mutate(color = sample(c("#6a0dad", "#e0b0ff", "#1a1a1a", "#0f0", "#ffa07a"), 1))

# Plot
ggplot(line_data, aes(x = x, y = y, group = line_id, color = color)) +
  geom_line(alpha = 0.6, size = 0.3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))
#====================================================
library(ggplot2)
library(dplyr)

set.seed(999)

# Params
n_layers <- 3       # Number of overlapping line layers
n_lines <- 150      # Lines per layer
n_points <- 100     # Points per line

# Color palettes for each layer
palette_list <- list(
  c("#e63946", "#f1faee", "#a8dadc", "#457b9d", "#1d3557"),
  c("#ffb703", "#fb8500", "#8ecae6", "#219ebc", "#023047"),
  c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff")
)

all_layers <- lapply(1:n_layers, function(layer) {
  x_vals <- seq(0, 10, length.out = n_lines)
  layer_data <- lapply(seq_along(x_vals), function(i) {
    tibble(
      x = x_vals[i] + rnorm(n_points, 0, 0.01),
      y = seq(0, 10, length.out = n_points) + rnorm(n_points, 0, 0.2),
      line_id = paste(layer, i, sep = "-")
    )
  }) %>% bind_rows()
  
  layer_data %>%
    group_by(line_id) %>%
    mutate(color = sample(palette_list[[layer]], 1))
})

final_data <- bind_rows(all_layers)

# Plot
ggplot(final_data, aes(x = x, y = y, group = line_id, color = color)) +
  geom_line(alpha = 0.3, size = 0.4) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))
#==================================================
library(ggplot2)
library(dplyr)

set.seed(1234)

n_lines <- 150
n_points <- 100

make_lines <- function(direction = "vertical", palette) {
  axis_vals <- seq(0, 10, length.out = n_lines)
  lines <- lapply(seq_along(axis_vals), function(i) {
    if (direction == "vertical") {
      tibble(
        x = axis_vals[i] + rnorm(n_points, 0, 0.01),
        y = seq(0, 10, length.out = n_points) + rnorm(n_points, 0, 0.1),
        line_id = paste("v", i, sep = "-")
      )
    } else {
      tibble(
        y = axis_vals[i] + rnorm(n_points, 0, 0.01),
        x = seq(0, 10, length.out = n_points) + rnorm(n_points, 0, 0.1),
        line_id = paste("h", i, sep = "-")
      )
    }
  }) %>% bind_rows()
  
  lines %>%
    group_by(line_id) %>%
    mutate(color = sample(palette, 1))
}

# Palettes
palette_v <- c("#0077b6", "#00b4d8", "#90e0ef", "#caf0f8")
palette_h <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2")

v_lines <- make_lines("vertical", palette_v)
h_lines <- make_lines("horizontal", palette_h)

woven <- bind_rows(v_lines, h_lines)

# Plot
ggplot(woven, aes(x = x, y = y, group = line_id, color = color)) +
  geom_line(alpha = 0.25, size = 0.3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))
#==========================================================================
make_spiral <- function(turns = 5, n = 1000, color = "#ff6b6b") {
  t <- seq(0, 2 * pi * turns, length.out = n)
  tibble(
    x = (t / max(t)) * cos(t) + 5,  # Offset to center (x: ~0–10)
    y = (t / max(t)) * sin(t) + 5,
    group = 1,
    color = color
  )
}

spiral <- make_spiral(turns = 10, color = "#ff6b6b")

# Overlay spiral on top of previous woven plot
ggplot() +
  geom_line(data = woven, aes(x = x, y = y, group = line_id, color = color), alpha = 0.25, size = 0.3) +
  geom_path(data = spiral, aes(x = x, y = y), color = "#ff6b6b", size = 0.5, alpha = 0.4) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))
#======================================================
library(ggplot2)
library(dplyr)

set.seed(123)

n_lines <- 20   # Number of stripe blocks across x/y
n_points <- 100

# Color palette
colors <- c("#F4A261", "#2A9D8F", "#E76F51", "#264653", "#E9C46A", "#8AB6D6", "#F28DB2")

# Function to make vertical scribble patches
make_patch <- function(xmin, xmax, ymin, ymax, color, dir = "vertical") {
  tibble(
    id = paste(xmin, ymin, dir, sep = "-"),
    color = color,
    group = id,
    x = if (dir == "vertical") runif(n_points, xmin, xmax) else seq(xmin, xmax, length.out = n_points) + rnorm(n_points, 0, 0.05),
    y = if (dir == "vertical") seq(ymin, ymax, length.out = n_points) + rnorm(n_points, 0, 0.05) else runif(n_points, ymin, ymax)
  )
}

# Grid patches
patches <- expand.grid(
  x = seq(0, 10, length.out = n_lines),
  y = seq(0, 10, length.out = n_lines)
)

# Random color for each patch
patch_colors <- sample(colors, nrow(patches), replace = TRUE)

# Create both vertical and horizontal scribbles in each block
vertical_lines <- Map(function(x, y, col) {
  make_patch(x, x + 0.5, y, y + 0.5, col, dir = "vertical")
}, patches$x, patches$y, patch_colors) %>% bind_rows()

horizontal_lines <- Map(function(x, y, col) {
  make_patch(x, x + 0.5, y, y + 0.5, col, dir = "horizontal")
}, patches$x, patches$y, patch_colors) %>% bind_rows()

# Combine both
all_lines <- bind_rows(vertical_lines, horizontal_lines)

# Plot
ggplot(all_lines, aes(x = x, y = y, group = group, color = color)) +
  geom_line(alpha = 0.6, size = 0.3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))
#=========================================================
n_lines <- 30
n_dots <- 100
max_size <- 2
main_col <- "#326273"
bg_col <- "#BDD8DE"
s <- 2024
set.seed(s)
plot_data <- data.frame(
  x = stats::runif(n_lines * n_dots, 0, 10),
  y = rep(1:n_lines, n_dots),
  size = stats::rexp(n_lines * n_dots)
)
library(ggplot2)
g <- ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  geom_line(
    mapping = aes(group = y),
    alpha = 0.1, colour = main_col,
    linewidth = 0.3
  ) +
  geom_point(
    mapping = aes(size = size),
    pch = 21,
    fill = main_col, colour = main_col,
    alpha = 0.3
  )
g
g +
  scale_size(range = c(0.3, max_size)) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
#===============================================
library(ggplot2)
library(dplyr)

set.seed(456)

n_blocks <- 100  # Number of scribble blocks
n_points <- 100  # Points per line

# Color palette
colors <- c("#F4A261", "#2A9D8F", "#E76F51", "#264653", "#E9C46A", "#8AB6D6", "#F28DB2")

# Generate randomized block specs
blocks <- tibble(
  xmin = runif(n_blocks, 0, 10),
  ymin = runif(n_blocks, 0, 10),
  width = runif(n_blocks, 0.3, 1.2),
  height = runif(n_blocks, 0.3, 1.2),
  color = sample(colors, n_blocks, replace = TRUE)
) %>%
  mutate(
    xmax = xmin + width,
    ymax = ymin + height
  )

# Function to generate scribbles in either direction
make_patch <- function(xmin, xmax, ymin, ymax, color, dir = "vertical") {
  tibble(
    id = paste(xmin, ymin, dir, sep = "-"),
    color = color,
    group = id,
    x = if (dir == "vertical") runif(n_points, xmin, xmax) else seq(xmin, xmax, length.out = n_points) + rnorm(n_points, 0, 0.03),
    y = if (dir == "vertical") seq(ymin, ymax, length.out = n_points) + rnorm(n_points, 0, 0.03) else runif(n_points, ymin, ymax)
  )
}

# Create vertical and horizontal layers
vertical_lines <- Map(function(xmin, xmax, ymin, ymax, col) {
  make_patch(xmin, xmax, ymin, ymax, col, dir = "vertical")
}, blocks$xmin, blocks$xmax, blocks$ymin, blocks$ymax, blocks$color) %>% bind_rows()

horizontal_lines <- Map(function(xmin, xmax, ymin, ymax, col) {
  make_patch(xmin, xmax, ymin, ymax, col, dir = "horizontal")
}, blocks$xmin, blocks$xmax, blocks$ymin, blocks$ymax, blocks$color) %>% bind_rows()

# Combine both layers
all_lines <- bind_rows(vertical_lines, horizontal_lines)

# Plot
ggplot(all_lines, aes(x = x, y = y, group = group, color = color)) +
  geom_line(alpha = 0.5, size = 0.3) +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA))
