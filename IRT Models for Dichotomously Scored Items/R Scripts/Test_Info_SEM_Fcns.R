## Plotting Test Information and SEM Function from known item parameters 
## Derek Briggs & ChatGPT 5.2

# --- Packages ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- File path (as provided) ---
xlsx_path <- "/Data/three_item_test.xlsx"

# --- Read + clean parameters from sheet "threeItems" ---
raw <- read_excel(xlsx_path, sheet = "threeItems")

# Coerce a,b,c to numeric (sheet has extra non-numeric rows)
pars <- raw %>%
  transmute(
    item = suppressWarnings(as.numeric(item)),
    a    = suppressWarnings(as.numeric(a)),
    b    = suppressWarnings(as.numeric(b)),
    c    = suppressWarnings(as.numeric(c))
  ) %>%
  filter(!is.na(a) & !is.na(b) & !is.na(c)) %>%
  slice(1:3)  # the three items

# --- 3PL functions (logit metric by default: D = 1) ---
D <- 1  # set to 1.7 if you want the "3PL logistic with D=1.7" convention

p_3pl <- function(theta, a, b, c, D = 1) {
  L <- 1 / (1 + exp(-D * a * (theta - b)))
  c + (1 - c) * L
}

info_3pl <- function(theta, a, b, c, D = 1) {
  L  <- 1 / (1 + exp(-D * a * (theta - b)))
  P  <- c + (1 - c) * L
  dP <- (1 - c) * (D * a) * L * (1 - L)
  (dP^2) / (P * (1 - P))
}

# --- Theta grid ---
theta <- seq(-4, 4, by = 0.01)

# --- Compute item info + test info ---
item_infos <- sapply(1:nrow(pars), function(i) {
  info_3pl(theta, pars$a[i], pars$b[i], pars$c[i], D = D)
})

test_info <- rowSums(item_infos)
sem <- 1 / sqrt(test_info)

plot_df <- data.frame(theta = theta, info = test_info, sem = sem)

# --- Dual-axis scaling (ggplot2 requires linear transform) ---
scale_factor <- max(plot_df$info, na.rm = TRUE) / max(plot_df$sem, na.rm = TRUE)
plot_df$sem_scaled <- plot_df$sem * scale_factor

# --- Plot: left axis = Information, right axis = SEM (logits) ---
ggplot(plot_df, aes(x = theta)) +
  geom_line(aes(y = info), linewidth = 1) +
  geom_line(aes(y = sem_scaled), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Test Information",
    sec.axis = sec_axis(~ . / scale_factor, name = "SEM (logits)")
  ) +
  labs(
    x = expression(theta),
    title = "Three-Item 3PL Test: Information and SEM") +
  theme_minimal(base_size = 13)