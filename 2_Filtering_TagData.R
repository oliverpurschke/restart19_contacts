# see restart_final.R
load(paste0(data_path, "recorded_tags_all.Rdata"))

dev_tracer <- read_csv2(".../device_tracer_id.csv")

recorded_tags_sel <- recorded_tags_all %>%
  filter(Type == "UWB", REU_Timestamp_Corr != 0,
         uwb_duration_sec < 3600) %>%
  
  left_join(serial_name, by = c("TagID" = "SN_Serial_Char")) %>%
  filter(SN_Typ %in% "Tag" == F, TagID %in% dev_tracer$name) %>%
  
  mutate(
    uwb_start_sec_corr_abs = as_datetime(uwb_start_sec_corr, tz = "Europe/Berlin"),
    REU_Timestamp_Corr_abs = as_datetime(REU_Timestamp_Corr, tz = "Europe/Berlin")
  ) %>%
  filter(
    uwb_start_sec_corr_abs > as_datetime("2020-08-22 07:45:00", tz = "Europe/Berlin"),
    REU_Timestamp_Corr_abs >= as_datetime("2020-08-22 10:00:00", tz = "Europe/Berlin")
  )
