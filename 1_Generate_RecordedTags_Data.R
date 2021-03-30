recorded_tags <- recorded_tags %>%
  select(
    T_ID = RT_ID,
    SnifferID = RT_SnifferID_Char,
    TagID = RT_TagID_Char,
    Packet_srcID = RT_SrcID_Char,
    Packet_role	= RT_Role,
    Packet_timestamp_sec	= RT_Timestamp,
    Packet_seqnr = RT_SeqNr
  )

recorded_tags_all <-
  recorded_tags %>%
  # reset
  inner_join(
    recorded_events_reset %>%
      select(RER_RT_ID,
             Event_counter = RER_EventNumber,
             # neu
             RER_Timestamp_Corr) %>%
      mutate(Type = "RESET"),
    by = c("T_ID" = "RER_RT_ID")
  ) %>%
  
  # uwb
  bind_rows(recorded_tags %>%
              inner_join(
                recorded_events_uwb %>% select(
                  REU_RT_ID,
                  Event_counter = REU_EventNumber,
                  uwb_targetTagID	= REU_TgtID_Char,
                  uwb_start_sec	= REU_StartSec,
                  uwb_duration_sec	= REU_DurationSec,
                  uwb_mindist_cm	= REU_MindistCm,
                  uwb_lut_entry_dist_cm = REU_LUT_EntryDist_cm,
                  # neu
                  uwb_start_sec_corr = REU_StartSec_Corr,
                  REU_Timestamp_Corr,
                  uwb_duration_sec_corr = REU_DurationSec_Corr
                  
                ) %>%
                  mutate(Type = "UWB"),
                by = c("T_ID" = "REU_RT_ID")
              )) %>%
  
  # ble
  bind_rows(recorded_tags %>%
              inner_join(
                recorded_events_ble %>% select(
                  REB_RT_ID,
                  Event_counter = REB_EventNumber,
                  ble_beaconID_hex	= REB_BeaconID_Char,
                  ble_start_sec	= REB_StartSec,
                  ble_rssi = REB_rssi,
                  # neu
                  ble_start_sec_corr = REB_StartSec_Corr ,
                  REB_Timestamp_Corr
                ) %>%
                  mutate(Type = "BLE"),
                by = c("T_ID" = "REB_RT_ID")
              )) %>%
  
  # time
  bind_rows(recorded_tags %>%
              inner_join(
                recorded_events_time %>% select(
                  RET_RT_ID,
                  Event_counter = RET_EventNumber,
                  time_snifferID	= RET_SrcID_Char,
                  time_timestamp_secSinceEpoch	= RET_SrcStamp,
                  time_start_sec = RET_StartSec,
                  # neu
                  time_start_sec_corr = RET_StartSec_Corr,
                  RET_Timestamp_Corr
                ) %>%
                  mutate(Type = "TIME"),
                by = c("T_ID" = "RET_RT_ID")
              )) %>%
  # sort event counter
  arrange(TagID, Event_counter)

save(recorded_tags_all,
     file = paste0(data_path, "recorded_tags_all.Rdata"))

load(paste0(data_path, "recorded_tags_all.Rdata"))