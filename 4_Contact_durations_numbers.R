library(tidyverse)


data_path_p <- "..."


load(paste0(data_path_p, "t_kont_1_s1_3_w_par_gapfill_agg.Rdata"))


t_kont_1_s1_3_w_par_gapfill_agg <-
  t_kont_1_s1_3_w_par_gapfill_agg %>%
  filter(t_kont_all_non_w > 2) %>%
  mutate(t_kont_all_non_w = as.numeric(t_kont_all_non_w))


t_kont_1_s_dur_900_scen_w <- t_kont_1_s1_3_w_par_gapfill_agg %>%
  group_by(Scenario, TagID, uwb_targetTagID) %>%
  
  summarize(t_kont_all_w_sum = sum(t_kont_all_w, na.rm = T)) %>%
  group_by(Scenario, TagID) %>%
  summarize(
    n_kont_900 = sum(t_kont_all_w_sum > 900),
    n_kont_600 = sum(t_kont_all_w_sum > 600),
    n_kont_300 = sum(t_kont_all_w_sum > 300),
    n_kont_10 = sum(t_kont_all_w_sum > 9),
    n_kont_3 = sum(t_kont_all_w_sum > 2)
  )


t_kont_1_s_dur_900_scen_w_long <- t_kont_1_s_dur_900_scen_w %>%
  #select(-n_kont_3) %>%
  pivot_longer(
    cols = n_kont_900:n_kont_3,
    names_to = "Threshold",
    values_to = "n_kont",
    values_drop_na = TRUE
  ) %>%
  filter(Threshold %in% c("n_kont_10", "n_kont_300", "n_kont_900")) %>%
  mutate(
    Threshold = case_when(
      Threshold == "n_kont_10" ~ ">10 seconds",
      Threshold == "n_kont_300" ~ ">5 minutes",
      Threshold == "n_kont_900" ~ ">15 minutes"
    ),
    Threshold  = factor(
      Threshold ,
      levels = c(">10 seconds", ">5 minutes", ">15 minutes"),
      ordered = T
    ),
    Scenario  = factor(Scenario ,
                       levels = c("1", "2", "3"),
                       ordered = T)
  )



data_for_n_kont_scen_15_10_5 <- t_kont_1_s_dur_900_scen_w_long


save(
  data_for_n_kont_scen_15_10_5,
  file = paste0(data_path, "data_for_n_kont_scen_15_10_5.Rdata")
)
load(paste0(data_path_p, "data_for_n_kont_scen_15_10_5.Rdata"))



# Scenarion sections

# Number of contacts per scenario ####

t_kont_1_s_dur_900_scen_part_w <-
  t_kont_1_s1_3_w_par_gapfill_agg %>%
  ungroup() %>%
  group_by(Scenario, Part, TagID, uwb_targetTagID) %>%
  summarize(t_kont_all_w_sum = sum(t_kont_all_w, na.rm = T)) %>%
  group_by(Scenario, Part, TagID) %>%
  summarize(
    n_kont_900 = sum(t_kont_all_w_sum > 900),
    n_kont_600 = sum(t_kont_all_w_sum > 600),
    n_kont_300 = sum(t_kont_all_w_sum > 300),
    n_kont_10 = sum(t_kont_all_w_sum > 9),
    n_kont_3 = sum(t_kont_all_w_sum > 2)
  ) %>%
  mutate(Part = factor(
    Part,
    levels = c("Entry", "Half Time 1", "Break", "Half Time 2", "Exit")
  ))


data_for_n_kont_scen_part_3sec <- t_kont_1_s_dur_900_scen_part_w %>%
  pivot_longer(
    cols = n_kont_900:n_kont_3,
    names_to = "Threshold",
    values_to = "n_kont",
    values_drop_na = TRUE
  ) %>%
  filter(Threshold %in% c("n_kont_10", "n_kont_300", "n_kont_900")) %>%
  mutate(
    Threshold = case_when(
      Threshold == "n_kont_10" ~ ">10 seconds",
      Threshold == "n_kont_300" ~ ">5 minutes",
      Threshold == "n_kont_900" ~ ">15 minutes"
    ),
    Threshold  = factor(
      Threshold ,
      levels = c(">10 seconds", ">5 minutes", ">15 minutes"),
      ordered = T
    ),
    Scenario  = factor(Scenario ,
                       levels = c("1", "2", "3"),
                       ordered = T),
    Part = case_when(
      Part == "Entry" ~ "En",
      Part == "Half Time 1" ~ "1st",
      Part == "Break" ~ "HT",
      Part == "Half Time 2" ~ "2nd",
      Part == "Exit" ~ "Ex"
    ),
    Part  = factor(
      Part ,
      levels = c("En", "1st", "HT", "2nd", "Ex"),
      ordered = T
    )
  )



save(
  data_for_n_kont_scen_part_3sec,
  file = paste0(data_path_p, "data_for_n_kont_scen_part_3sec.Rdata")
)



# Cumulative contacts ####

load(paste0(data_path_p, "t_kont_1_s1_3_w_par_gapfill_agg.Rdata"))

tags_distinct <- unique(recorded_tags_sel$TagID)

t_kont_1_s1_3_w_par_gapfill_cum <-
  t_kont_1_s1_3_w_par_gapfill_agg %>%
  filter(TagID %in% tags_distinct) %>%
  mutate(Part = factor(
    Part,
    levels = c("Entry", "Half Time 1", "Break", "Half Time 2", "Exit"),
    ordered = T
  )) %>%
  arrange(Scenario, TagID, uwb_targetTagID, Part) %>%
  ungroup() %>%
  mutate(Part = factor(
    Part,
    levels = c("Entry", "Half Time 1", "Break", "Half Time 2", "Exit"),
    ordered = T
  )) %>%
  arrange(Scenario, TagID, uwb_targetTagID, Part) %>%
  group_by(Scenario, TagID, uwb_targetTagID) %>%
  summarize(t_kont_all_w_cum = cumsum(t_kont_all_w)) %>%
  bind_cols(t_kont_1_s1_3_w_par_gapfill_arr %>%
              select(t_kont_all_w, Part))



cl <- makeCluster(detectCores())
registerDoSNOW(cl)

t_kont_1_s1_3_w_par_gapfill_cum_first_parallel <-
  foreach(
    i = tags_distinct,
    .packages = c(
      "purrr",
      "dplyr",
      "doSNOW",
      "lubridate",
      "parallel",
      "tidyr",
      "tibble"
    ),
    .combine = bind_rows
  ) %dopar% {
    t_kont_1_s1_3_w_par_gapfill_cum %>%
      filter(TagID == i) %>%
      group_by(Scenario, TagID, uwb_targetTagID) %>%
      mutate(
        first_900 = as.numeric(row_number() == min(row_number()[t_kont_all_w_cum > 900])),
        first_600 = as.numeric(row_number() == min(row_number()[t_kont_all_w_cum > 600])),
        first_300 = as.numeric(row_number() == min(row_number()[t_kont_all_w_cum > 300])),
        first_10 = as.numeric(row_number() == min(row_number()[t_kont_all_w_cum > 9])),
        first_3 = as.numeric(row_number() == min(row_number()[t_kont_all_w_cum > 2]))
      ) %>%
      ungroup()
  }

stopCluster(cl)
gc()


t_kont_1_s1_3_w_par_gapfill_cum_first <-
  t_kont_1_s1_3_w_par_gapfill_cum_first_parallel

save(
  t_kont_1_s1_3_w_par_gapfill_cum_first,
  file = paste0(data_path, "t_kont_1_s1_3_w_par_gapfill_cum_first.Rdata")
)
load(paste0(data_path_p, "t_kont_1_s1_3_w_par_gapfill_cum_first.Rdata"))


# Average number of "new" contacts per participant ####

n_kont_agg_teil <- t_kont_1_s1_3_w_par_gapfill_cum_first %>%
  group_by(Scenario, Part, TagID) %>%
  summarize(
    n_kont_teil_900 = sum(first_900, na.rm = T),
    n_kont_teil_600 = sum(first_600, na.rm = T),
    n_kont_teil_300 = sum(first_300, na.rm = T),
    n_kont_teil_10 = sum(first_10, na.rm = T),
    n_kont_teil_3 = sum(first_3)
  ) %>%
  group_by(Scenario, Part) %>%
  summarize(
    n_kont_teil_900_mean = round(mean(n_kont_teil_900, na.rm = T), 3),
    n_kont_teil_600_mean = round(mean(n_kont_teil_600, na.rm = T), 3),
    n_kont_teil_300_mean = round(mean(n_kont_teil_300, na.rm = T), 3),
    n_kont_teil_10_mean = round(mean(n_kont_teil_10, na.rm = T), 3),
    n_kont_teil_3_mean = round(mean(n_kont_teil_3, na.rm = T), 3)
  ) %>%
  pivot_longer(
    cols = n_kont_teil_900_mean:n_kont_teil_3_mean,
    names_to = "Threshold",
    values_to = "n_kont_agg",
    values_drop_na = TRUE
  ) %>%
  arrange(Scenario, Threshold, Part)


n_kont_agg_teil_cum <- n_kont_agg_teil %>%
  group_by(Scenario, Threshold) %>%
  summarize(n_kont_teil_cum = cumsum(n_kont_agg)) %>%
  bind_cols(n_kont_agg_teil %>%
              ungroup() %>%
              select(Part, n_kont_agg)) %>%
  filter(
    Threshold %in% c(
      "n_kont_teil_10_mean",
      "n_kont_teil_300_mean",
      "n_kont_teil_900_mean"
    )
  ) %>%
  mutate(
    Scenario = factor(Scenario, levels = unique(n_kont_agg_teil_cum$Scenario)),
    Part = as.integer(Part),
    Threshold = case_when(
      Threshold == "n_kont_teil_10_mean" ~ ">10 seconds",
      Threshold == "n_kont_teil_300_mean" ~ ">5 minutes",
      Threshold == "n_kont_teil_900_mean" ~ ">15 minutes"
    ),
    Threshold  = factor(
      Threshold ,
      levels = c(">10 seconds", ">5 minutes", ">15 minutes"),
      ordered = T
    )
  )


save(n_kont_agg_teil_cum,
     file = paste0(data_path_p, "n_kont_agg_teil_cum.Rdata"))
