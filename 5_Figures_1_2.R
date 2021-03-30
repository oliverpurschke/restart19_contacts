data_path_p <- "..."
library("lubridate")
library("ggplot2")
library("ggsci")

# calculate custom boxplot stats to get rid of outliers

calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <-
    x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

Scenario_labs <- c("Scenario 1", "Scenario 2", "Scenario 3")
names(Scenario_labs) <- 1:3
threshold_labs <-
  c(">10 seconds",
    ">3 seconds",
    ">5 minutes",
    ">10 minutes",
    ">15 minutes")
names(threshold_labs) <-
  c(
    "n_kont_teil_10_mean",
    "n_kont_teil_3_mean",
    "n_kont_teil_300_mean",
    "n_kont_teil_600_mean",
    "n_kont_teil_900_mean"
  )



# Figure 1A ####

load(paste0(data_path_p, "data_for_n_kont_scen_15_10_5.Rdata"))

ggplot(data_for_n_kont_scen_15_10_5,
       aes(Scenario, n_kont, group = Scenario, fill = Scenario)) +
  stat_summary(fun.data = calc_boxplot_stat,
               geom = "boxplot",
               width = 0.5) +
  facet_wrap(~ Threshold, scales = "free_y") +
  theme_minimal() +
  labs(x = "Scenario",
       y = "Number of contacts") +
  scale_y_continuous(breaks = scales::pretty_breaks(4),
                     limits = c(0, NA)) +
  theme(legend.position = "none") +
  scale_fill_npg() +
  theme(strip.text = element_text(size = 8))

ggsave(
  paste0(data_path_p,
         "Figure_1A_",
         today(),
         ".jpg"),
  width = 8,
  height = 6,
  units = c("cm"),
  dpi = 300
)

ggsave(
  paste0(data_path_p,
         "Figure_1A_",
         today(),
         ".svg"),
  device = "svg",
  width = 8,
  height = 6,
  units = c("cm"),
  dpi = 300
)


# Figure 1B ####

load(paste0(data_path_p, "data_for_n_kont_scen_part_3sec.Rdata"))

ggplot(data_for_n_kont_scen_part_3sec,
       aes(Part, n_kont, group = Part, fill = Scenario)) +
  stat_summary(fun.data = calc_boxplot_stat,
               geom = "boxplot",
               width = 0.5) +
  facet_grid(
    Threshold ~ Scenario,
    scales = "free_y",
    labeller = labeller(Scenario = Scenario_labs)
  ) +
  theme_minimal() +
  labs(x = "Sections",
       y = "Number of contacts") +
  scale_y_continuous(breaks = scales::pretty_breaks(4),
                     limits = c(0, NA)) +
  theme(legend.position = "none") +
  scale_fill_npg()

ggsave(
  paste0(
    data_path_p,
    "n_kont_scen_section_10sek_5_10min_box_90_",
    today(),
    ".jpg"
  ),
  width = 14,
  height = 16,
  units = c("cm"),
  dpi = 300
)

ggsave(
  paste0(data_path_p,
         "Figure_1B_",
         today(),
         ".svg"),
  device = "svg",
  width = 14,
  height = 16,
  units = c("cm"),
  dpi = 300
)



# Figure 2 ####

load(paste0(data_path_p, "n_kont_agg_teil_cum.Rdata"))

part_labs = c("En", "1st", "HT", "2nd", "Ex")
n_kont_agg_teil_cum$Scenario <- factor(n_kont_agg_teil_cum$Scenario)
n_kont_agg_teil_cum$Threshold <- factor(n_kont_agg_teil_cum$Threshold)


ggplot(
  n_kont_agg_teil_cum[n_kont_agg_teil_cum$Threshold %in%
                        levels(n_kont_agg_teil_cum$Threshold)[c(1, 3, 5)], ],
  aes(
    x = Part,
    y = n_kont_teil_cum,
    color = Scenario,
    group = Scenario
  )
) +
  geom_step() +
  facet_wrap( ~ Threshold,
              scales = "free_y",
              labeller = labeller(Threshold = threshold_labs)) +
  theme_minimal() +
  
  labs(title = "",
       x = "Sections",
       y = "Number of contacts") +
  scale_x_discrete(labels = part_labs) +
  scale_color_npg() +
  theme(legend.position = "top")


ggsave(
  paste0(data_path_p,
         "Figure_2_",
         today(),
         ".jpg"),
  width = 10,
  height = 8,
  units = c("cm"),
  dpi = 300
)

ggsave(
  paste0(data_path_p,
         "Figure_2_",
         today(),
         ".svg"),
  device = "svg",
  width = 10,
  height = 8,
  units = c("cm"),
  dpi = 300
)
