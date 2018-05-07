#######################
# Author - Nikhil Rao #
#######################

# Libraries ---------------------------------------------------------------

library(extrafont)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(cowplot)

# Import Fonts ------------------------------------------------------------

# font_import() # only load this once
loadfonts(device = "win")
windowsFonts() # check what fonts your system has

# Load Data ---------------------------------------------------------------

load(url("http://www.stat.berkeley.edu/users/nolan/data/weather2011.rda"))

# Data Pre-Processing -----------------------------------------------------

sfoweatherUpdated <- sfoWeather
sfoweatherUpdated$newDay <- seq(from = 1, to = length(sfoweatherUpdated$Day), by = 1)

sfoweatherStats <- sfoweatherUpdated %>%
  group_by(Month) %>%
  summarise(recordhigh = max(High), recordlow = min(Low))

sfoweatherPrecip <- sfoweatherUpdated %>%
  group_by(Month) %>%
  mutate(cumsump = cumsum(Precip))

## - Customized Theme for Plot.  
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Cambria", face = "bold"), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## - Weather Chart.
labels <- parse(text = paste(seq(-20, 110, by = 10), "^o", sep = ""))
yint <- seq(-20, 3620, by = 10)

w <- ggplot(sfoweatherUpdated) +
  theme_map() +
  geom_linerange(aes(x = newDay, ymin = RecordLow, ymax = RecordHigh), color = "seashell2", size = 1.5) +
  geom_linerange(aes(x = newDay, ymin = NormalLow, ymax = NormalHigh), color = "seashell3", size = 1.5) +
  geom_linerange(aes(x = newDay, ymin = Low, ymax = High), color = "indianred4", size = 1.5) +
  geom_hline(aes(yintercept = yint), colour = "#f5f5f2", size = 1) +
  theme(axis.line.y = element_line(color = "antiquewhite3", size = 1.2),
        axis.text.x = element_text(family = "Cambria", color = "gray46", size = 11, face = "bold"),
        axis.title = element_blank(),
        plot.title = element_text(color = "#4e4d47",  
                                  face = "bold", 
                                  size = 20,
                                  margin = margin(r = -0.5, b = 0.5, unit = "cm"))) +
  labs(
    title = "San Francisco's Weather in 2011"
  ) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
                     labels = c("January", "February", "March", "April", "May",
                                "June", "July", "August", "September", "October",
                                "November", "December"),
                     position = "top") +
  scale_y_continuous(breaks = seq(-20, 110, by = 10),
                     sec.axis = dup_axis(),
                     labels = labels) +
  coord_cartesian(ylim = c(-20, 110))

w <- w +
    annotate("text", x = 5, y = 107, size = 5, fontface = "bold",
             hjust = 0, vjust = 0, label = "Temperature", family = "Cambria") +
    annotate("text", x = 5, y = 106, size = 4, color = "gray30",
             hjust = 0, vjust = 1, label = "Data represents average daily temperature.", family = "Cambria") +
    annotate("text", x = 5, y = 103, size = 4, color = "gray30",
             hjust = 0, vjust = 1, label = "Bars represent range between the daily high and low.",
             family = "Cambria")

w <- w +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "seashell2", size = 5) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "seashell3", size = 5) +
  annotate("segment", x = 181, xend = 181, y = 14, yend = 22, colour = "indianred4", size = 2) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "seashell3", size = .5) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "seashell3", size = .5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "seashell3", size = .5) +
  annotate("segment", x = 177, xend = 180, y = 14, yend = 14, colour = "indianred4", size = .5) +
  annotate("segment", x = 177, xend = 180, y = 22, yend = 22, colour = "indianred4", size = .5) +
  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size = 3, colour = "gray30") +
  annotate("text", x = 191, y = 25, label = "RECORD HIGH", size = 3, colour = "gray30") +
  annotate("text", x = 191, y = 5, label = "RECORD LOW", size = 3, colour = "gray30") +
  annotate("text", x = 169, y = 22, label = "ACTUAL HIGH", size = 3, colour = "gray30") +
  annotate("text", x = 169, y = 14, label = "ACTUAL LOW", size = 3, colour = "gray30")

w <- w +
  annotate("segment", x = 56.7, xend = 56.7, y = 34.5, yend = 28, colour = "blue3") +
  annotate("segment", x = 171, xend = 171, y = 91, yend = 98, colour = "firebrick3") +
  annotate("text", x = 55, xend = 55, y = 26, yend = 26, size = 3, colour = "gray30", label = "RECORD : 35 ^ o", 
           parse = TRUE) +
  annotate("text", x = 170, xend = 170, y = 101, yend = 101, size = 3, colour = "gray30", 
           label = "RECORD : 90 ^ o", parse = TRUE)

w <- w + 
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]),
                   y = -20, yend = 60), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]),
                   y = -20, yend = 60), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]),
                   y = 50, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]),
                   y = -20, yend = 110), linetype = "dashed", color = "azure4")

w

## - Cumulative Precipitation Chart.
p <- ggplot(sfoweatherPrecip) +
  theme_map() +
  geom_area(aes(x = newDay, y = cumsump), fill = "seashell2", size = 3) +
  geom_line(aes(x = newDay, y = cumsump), color = "dodgerblue4", size = 1) +
  geom_hline(aes(yintercept = 0), colour = "#f5f5f2", size = 1) +
  geom_hline(aes(yintercept = 2), colour = "#f5f5f2", size = 1) +
  geom_hline(aes(yintercept = 4), colour = "#f5f5f2", size = 1) +
  geom_hline(aes(yintercept = 6), colour = "#f5f5f2", size = 1) +
  geom_hline(aes(yintercept = 8), colour = "#f5f5f2", size = 1) +
  geom_hline(aes(yintercept = 10), colour = "#f5f5f2", size = 1) +
  theme(axis.line.y = element_line(color = "antiquewhite3", size = 1.2),
        axis.text.x = element_text(family = "Cambria", color = "gray46", size = 11, face = "bold"),
        axis.title = element_blank(),
        plot.title = element_text(color = "#4e4d47",  
                                  face = "bold", 
                                  size = 20,
                                  margin = margin(r = -0.5, b = 0.5, unit = "cm"))) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 10, by = 2),
                     sec.axis = dup_axis(),
                     labels = seq(0, 10, by = 2)) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
                     labels = c("January", "February", "March", "April", "May",
                                "June", "July", "August", "September", "October",
                                "November", "December"),
                     position = "bottom") +
  coord_cartesian(ylim = c(0, 12))

p <- p +
  annotate("text", x = 5, y = 11.5, size = 5, fontface = "bold",
           hjust = 0, vjust = 0, label = "Precipitation", family = "Cambria") +
  annotate("text", x = 30, y = 11.5, size = 4, 
           hjust = 0, vjust = 0, label = "Cumulative monthly precipitation in inches compared with normal monthly precipitation.", family = "Cambria")

p <- p +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]), 
           y = 4.19, yend = 4.19, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]), 
           y = 4.01, yend = 4.01, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]), 
           y = 3.26, yend = 3.26, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]), 
           y = 1.18, yend = 1.18, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]), 
           y = 0.38, yend = 0.38, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]), 
           y = 0.11, yend = 0.11, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]), 
           y = 0.03, yend = 0.03, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]), 
           y = 0.04, yend = 0.04, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]), 
           y = 0.17, yend = 0.17, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]), 
           y = 0.95, yend = 0.95, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]), 
           y = 2.38, yend = 2.38, color = "dodgerblue3") +
  annotate("segment", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]), 
           y = 4.03, yend = 4.03, color = "dodgerblue3")

p <- p +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 1]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 2]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 3]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 4]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 5]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 6]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 7]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 8]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 9]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 10]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 11]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4") +
  geom_segment(aes(x = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]), xend = max(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == 12]),
                   y = 0, yend = 10), linetype = "dashed", color = "azure4")

sfoMonthlyPrecip$Month <- seq(1, nrow(sfoMonthlyPrecip), by = 1)

preciplabels <- function(month, yrow, label) {
  annotate("text", x = min(sfoweatherPrecip$newDay[sfoweatherPrecip$Month == month]) + 6, y = sfoMonthlyPrecip[yrow, 2] + 0.5, 
           label = label, size = 3.5, colour = "gray16")
}

sfoMonthlyPrecip$normal = as.character(sfoMonthlyPrecip$normal)
sfoMonthlyPrecip$normal = as.numeric(sfoMonthlyPrecip$normal)

p <- p +
  preciplabels(1, 1, 4.19) +
  preciplabels(2, 2, 4.01) +
  preciplabels(3, 3, 3.26) +
  preciplabels(4, 4, 1.18) +
  preciplabels(5, 5, 0.38) +
  preciplabels(6, 6, 0.11) +
  preciplabels(7, 7, 0.03) +
  preciplabels(8, 8, 0.04) +
  preciplabels(9, 9, 0.17) +
  preciplabels(10, 10, 0.95) +
  preciplabels(11, 11, 2.38) +
  preciplabels(12, 12, 4.03)

p

plot_grid(w, p, align = "v", nrow = 2, rel_heights = c(1/2, 1/4))

### export: 1900 width vs 1500 height
