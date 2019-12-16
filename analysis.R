# Load necessary packages -------------------------------------------------
library(tidyverse)
library(latex2exp)
library(ggalt)
library(cowplot)

# functions ---------------------------------------------------------------

# function that size of the feasibility domain
# input: a = relative reduction in growth rates (a.k.a Q1); b = relative reduction in growth rates (a.k.a Q2)
# output: the normalized angle
Omega <- function(a, b) {
  acos((a + b) / (sqrt(1 + a^2) * sqrt(1 + b^2))) * 2 / pi * 90
}

# function that makes a factored vector into numeric
# input: x = a factored vector 
# output: the numeric version of x
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}

# data preparation --------------------------------------------------------

# data is available on https://figshare.com/articles/Dataset_Godoy_et_al_2014_Ecology_Letters/4793488
r <- read_csv("vita_rates.csv") %>%
  mutate(r = g * lambda / (1 - (1 - g) * s) - 1) %>%
  pull(r)
g <- read_csv("vita_rates.csv") %>%
  pull(g)
alpha <- read_csv("alpha.csv") %>%
  select(-X1) %>%
  as.matrix()
for (k in 1:nrow(alpha)) {
  alpha[, k] <- alpha[, k] * g[k]
}
rownames(alpha) <- colnames(alpha)


# analysis ------------------------------------------------------------------
Q1 <- c()
Q2 <- c()
ratio <- c()
for (i in 1:nrow(alpha)) {
  for (j in i:ncol(alpha)) {
    A <- alpha[c(i, j), c(i, j)]
    Q1 <- c(Q1, A[2, 1] / A[1, 1])
    Q2 <- c(Q2, A[1, 2] / A[2, 2])
    ratio <- c(ratio, r[j] / r[i])
  }
}

df <- tibble(Q1, Q2, ratio) %>%
  filter(!is.na(Q1) & !is.na(Q2)) %>%
  mutate(
    omega = Omega(Q1, Q2),
    niche = sqrt(Q1 * Q2),
    competitive = sqrt(Q2 / Q1),
    fitness = ratio * competitive
  ) %>%
  filter(is.finite(competitive) & competitive != 0) %>%
  filter(ratio != 1) %>% 
  mutate(case = if_else(niche <1, "Coexistence", "Priority effects")) %>% 
  mutate(
    competitive = if_else(ratio > 1, competitive, 1 / competitive),
    fitness = if_else(ratio > 1, fitness, 1 / fitness)
  ) %>% 
  mutate(
    ratio = if_else(ratio > 1, ratio, 1 / ratio)
  )


# Plot --------------------------------------------------------------------
df %>%
  ggplot(aes(fitness, ratio, color = case)) +
  geom_point(aes(size = omega)) +
  facet_grid(~case) +
  geom_vline(xintercept = 1, size = 1, color =rgb(.8, .15, .15)) +
  geom_abline(slope = 1, intercept = 0, size = 1, color ='gray3') +
  scale_colour_manual(values = c("dodgerblue","#FCBF4A"))+
  annotate("text", x = 7, y = 10^3, label = "Equalizing")+
  annotate("text", x = 7, y = 10^2.8, label = "mechanism",
           parse = TRUE)+
  annotate("text", x = 10^2.8, y = 10^3.9,
           label = "towards",
           parse = TRUE)+
  annotate("text", x = 10^2.8, y = 10^3.7,
           label = expression(paste("increasing ", Omega)),
           parse = TRUE)+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(1,17100)
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
  ) +
  annotation_logticks() +
  geom_curve(aes(x = 10^.7, y = 10^2.6, xend = 10^.08, yend = 10^2.6), 
             color = 'gray3',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  geom_curve(aes(x = 10^-.7, y = 10^2.6, xend = 10^-.08, yend = 10^2.6), 
               color = 'gray3',
               curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  geom_curve(aes(x = 10^2.85, y = 10^3.55, xend = 10^(3.3-.05), yend = 10^(3.3+.03)), 
             color = 'gray3',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  geom_curve(aes(x = 10^(2*3.3-2.85), y = 10^(2*3.3-3.55), xend = 10^(3.3+.04), yend = 10^(3.3-0.03)), 
             color = 'gray3',
             curvature=0, arrow = arrow(length = unit(0.2,"cm")))+
  labs(y = TeX('Ratio of intrinsic growth rates $\\frac{r_2}{r_1}$'),
       x = TeX('Fitness ratio $\\frac{r_2}{r_1}\\sqrt{\\frac{Q_2}{Q_1}}$'),
       size= TeX('Normalized angle $\\Omega$') 
       )+
  scale_size_continuous(breaks = c(20, 70),
                        labels = c('small', 'large'))+
  guides(color = FALSE)+
  theme_bw() +
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    strip.text = element_text(size = 20),
    legend.position=c(.88,.15),
    panel.spacing = unit(2, "lines"),
    axis.title=element_text(size=14,face="bold"),
    legend.title=element_text(size=12),
    legend.key = element_rect(colour = "transparent", fill = "white"),
    legend.background=element_blank(),
    legend.text=element_text(size=11)
  )
