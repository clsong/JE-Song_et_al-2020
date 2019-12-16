# Load necessary packages -------------------------------------------------
library(tidyverse)
library(latex2exp)
library(viridis)

# functions ---------------------------------------------------------------

# function that size of the feasibility domain
# input: a = relative reduction in growth rates (a.k.a Q1); b = relative reduction in growth rates (a.k.a Q2)
# output: the normalized angle
Omega <- function(a, b) {
  acos((a + b) / (sqrt(1 + a^2) * sqrt(1 + b^2))) * 2 / pi * 90
}


# data --------------------------------------------------------------------
# generate the data for coexistence
data_coexistence <- 10^(seq(from = 0, to = 2, by = .01)) %>%
  map_dfr(~ tibble(
    ratio_r = .,
    niche = .1,
    Q_1 = 10^(seq(from = -3, to = 3, by = .01)),
    Q_2 = niche^2 / Q_1,
    omega = Omega(Q_1, Q_2),
    fitness = ratio_r * sqrt(Q_2 / Q_1)
  )) %>%
  mutate(omega_range = cut(omega, 8)) %>%
  select(ratio_r, fitness, omega_range, omega) %>% 
  mutate(case = 'Coexistence')

# generate the data for priority effects
data_priority <- 10^(seq(from = 0, to = 2, by = .01)) %>%
  map_dfr(~ tibble(
    ratio_r = .,
    niche = 10,
    Q_1 = 10^(seq(from = -4, to = 4, by = .01)),
    Q_2 = niche^2 / Q_1,
    omega = Omega(Q_1, Q_2),
    fitness = ratio_r * sqrt(Q_2 / Q_1)
  )) %>%
  mutate(omega_range = cut(omega, 8)) %>%
  select(ratio_r, fitness, omega_range, omega) %>% 
  mutate(case = 'Priority effect')

# plotting --------------------------------------------------------------------
bind_rows(
  data_coexistence,
  data_priority
) %>% 
  filter(fitness > 0.1 & fitness < 100) %>%
  ggplot(aes(round(fitness, 8),  round(ratio_r, 8), fill = omega)) +
  geom_raster(interpolate = TRUE)+
  scale_fill_viridis(option='viridis')+
  facet_grid(~case)+
  geom_vline(xintercept = 1, size = 1, color =rgb(.8, .15, .15)) +
  geom_abline(slope = 1, intercept = 0, size = 1, color ='gray3') +
  annotate("text", x = 10^(-.5), y = 10^1.4, label = "Equalizing", color = 'white')+
  annotate("text", x = 10^(-.5), y = 10^1.3, label = "mechanism", parse = TRUE, color = 'white')+
  geom_curve(aes(x = 10^.4, y = 10^1.2, xend = 10^.04, yend = 10^1.2), 
             color = 'white',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  geom_curve(aes(x = 10^-.4, y = 10^1.2, xend = 10^-.04, yend = 10^1.2), 
             color = 'white',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  annotate("text", x = 10^1, y = 10^1.6,
           label = "towards",
           parse = TRUE)+
  annotate("text", x = 10^1, y = 10^1.5,
           label = expression(paste("increasing ", Omega)),
           parse = TRUE)+
  geom_curve(aes(x = 10^.96, y = 10^1.43, xend = 10^1.26, yend = 10^1.31), 
             color = 'gray3',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  geom_curve(aes(x = 10^1.63, y = 10^1.16, xend = 10^1.33, yend = 10^1.28), 
             color = 'gray3',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks() +
  labs(y = TeX('Ratio of intrinsic growth rates $\\frac{r_2}{r_1}$'),
       x = TeX('Fitness ratio $\\frac{r_2}{r_1}\\sqrt{\\frac{Q_2}{Q_1}}$'),
       fill= TeX('Normalized angle $\\Omega$')
  )+
  theme_bw() +
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    strip.text = element_text(size = 20),
    panel.spacing = unit(2, "lines"),
    axis.title=element_text(size=14,face="bold"),
    legend.title=element_text(size=12),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.background=element_blank(),
    legend.text=element_text(size=11),
    plot.title = element_text(hjust = 0.5, size = 20)
  )

