library(tidyverse)
library(palmerpenguins)

penguins |> na.omit() |> group_by(sex, species) |> summarise_if(is.numeric, mean, na.rm = TRUE)

penguins |>
  na.omit() |>
  filter(species == "Chinstrap" | species == "Gentoo") |>
  mutate(species = factor(species, levels = c("Chinstrap", "Gentoo")),
         sex = factor(sex, levels = c("male", "female"))) |>
  ggplot( aes(x = species, y = bill_length_mm, colour = sex, fill = sex, shape = sex)) +
  geom_jitter(size = 1, alpha = 0.3, width = 0.23, stroke = 1.5) +
  scale_fill_manual(name = "Gender", values=c("male"="#4E8BC4", 
                                           "female"="#FF99BE")) +
  scale_color_manual(name = "Gender", values=c("male"="#4E8BC4", 
                                           "female"="#FF99BE")) +
  scale_shape_manual(name = "Gender", values=c("male"=24, 
                                            "female"=25)) +
  labs(title = "The mean Chinstrap bill is longer than the Gentoo...",
       subtitle = "Is this significant?",
       caption = "Caption 1. Male: μChn = 51.1mm & μGen = 49.5mm, Female: : μChn = 46.6mm & μGen = 45.6mm") +
  xlab("Species") +
  ylab("Bill Length (mm)") +
  theme_minimal() +
  theme(legend.position = c(0.93, 0.9),
        legend.background = element_rect(colour = "grey80", fill = "white"),
        panel.background = element_rect(color = NA, fill = "transparent"),
        panel.border = element_rect(colour = "grey60", fill = "transparent"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey95", size = 0.1),
        text = element_text(family = "Tahoma", colour = "grey40"),
        plot.title = element_text(size = 16, margin = margin(t = 4, r = 0, b = 4, l = 0)),
        plot.subtitle = element_text(size = 18, face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
        plot.caption = element_text(colour = "grey50", face = "italic", hjust = 0, vjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(colour = "grey60"),
        axis.text.y = element_text(colour = "grey60")
  ) +
  geom_point(data = penguins |> 
               filter(species == "Chinstrap" | species == "Gentoo") |> 
               filter(sex == "male"),
             stat = "summary", fun = "mean", 
             shape = 24, size = 3, fill = "#4E8BC4", color = "#4E8BC4", stroke = 2, alpha = 0.8
             ) +
  geom_point(data = penguins |> 
               filter(species == "Chinstrap" | species == "Gentoo") |> 
               filter(sex == "female"),
             stat = "summary", fun = "mean", 
             shape = 25, size = 3, fill = "#FF99BE", color = "#FF99BE", stroke = 2, alpha = 0.8
  )

new <- subset(penguins, sex == "female" | sex == "male")
Chinstrap_data <- subset(new, species == "Chinstrap")
Gentoo_data <- subset(new, species == "Gentoo")
x1 <- mean(Chinstrap_data$bill_length_mm)
x2 <- mean(Gentoo_data$bill_length_mm)
s1 <- sd(Chinstrap_data$bill_length_mm)
s2 <- sd(Gentoo_data$bill_length_mm)
n1 <- length(Chinstrap_data$bill_length_mm)
n2 <- length(Gentoo_data$bill_length_mm)

Sp <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)  # calculate the pooled variance
t <- (x1 - x2) / sqrt(Sp * (1/n1 + 1/n2))
t  # calculate the t-value

alpha = 0.01
dfr = n1 + n2 - 2
tcritical = qt(1 - alpha/2, dfr)
tcritical

# tmale = 3.186863,  tfemale = 1.87622,  tcritical = 2.629732

ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dnorm, n = 101, colour = "#888888", args = list(mean = 0, sd = 1.3)) +
  geom_hline(yintercept = 0, colour = "#333333", size = 0.1) +
  geom_segment(aes(x = 1.87622, xend = 1.87622, y = -0.01, yend = 0.107),
               size = 1.2, colour = "#FF99BE") +
  geom_text(aes(x = 1.87622, label = "1.88", y = -0.02),
            size = 4, colour = "#666666", vjust = 0.5) +
  geom_area(data = data.frame(x = seq(2.610402, 4, by = 0.01)), 
            aes(x, dnorm(x, mean = 0, sd = 1.3)), fill = "#a9c5a0", alpha = 0.65) +
  geom_vline(xintercept = 2.610402, colour = "#CACACA", linetype = "dotted", size = 1) +
  geom_segment(aes(x = 2.610402, xend = 2.610402, y = 0, yend = 0.041),
               size = 1.3, colour = "#a9c5a0") +
  geom_text(aes(x = 2.610402, label = "2.61 (critical value)", y = 0.2), 
            colour="#BABABA", angle = 90, size = 4, vjust = -0.5) +
  geom_segment(aes(x = 3.186863, xend = 3.186863, y = -0.01, yend = 0.016),
               size = 1.2, colour = "#4E8BC4") +
  geom_text(aes(x = 3.186863, label = "3.19", y = -0.02),
            size = 4, colour = "#666666", vjust = 0.5) +
  geom_segment(aes(x = -4, xend = 2.629732, y = 0.375, yend = 0.375),
               linetype = "dashed", colour = "#999999",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed", ends = "both")) +
  geom_text(aes(x = -0.85, label = "Fail to Reject", y = 0.375), 
            colour = "#999999", angle = 0, vjust = -0.5, size = 4) +
  geom_segment(aes(x = 2.629732, xend = 4, y = 0.375, yend = 0.375),
               linetype = "dashed", colour = "#999999",
               arrow = arrow(length = unit(0.20, "cm"), type = "closed", ends = "both")) +
  geom_text(aes(x = 3.3, label = "Reject", y = 0.375), 
            colour = "#999999", angle = 0, vjust = -0.5, size = 4) +
  labs(title = "Title",
       subtitle = "Subtitle") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(-0.03, 0.45), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-4, 4)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(color = NA, fill = "transparent"),
        panel.border = element_rect(colour = "grey90", fill = "transparent"),
        panel.grid.major.x = element_line(color = "grey97", size = 0.1),
        panel.grid.major.y = element_line(color = "grey97", size = 0.1),
        panel.grid.minor.x = element_line(color = "grey97", size = 0.1),
        panel.grid.minor.y = element_line(color = "grey97", size = 0.1),
        text = element_text(family = "Tahoma", colour = "grey40"),
        plot.title = element_text(size = 16, margin = margin(t = 4, r = 0, b = 4, l = 0)),
        plot.subtitle = element_text(size = 10, margin = margin(t = 0, r = 0, b = 10, l = 0)),
        plot.caption = element_text(colour = "grey50", face = "italic", hjust = 0, vjust = 2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
)
