tags_distinct <- unique(recorded_tags_sel$TagID)


cl <- makeCluster(detectCores() - 1)
registerDoSNOW(cl)


t_kont_1_s1_3_w_par_gapfill <-
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
    recorded_tags_all_S3D9EF6C <- recorded_tags_sel %>%
      filter((TagID == i | uwb_targetTagID == i)) %>%
      arrange(REU_Timestamp_Corr)
    
    kont_t_target_S3D9EF6C_2 <- recorded_tags_all_S3D9EF6C %>%
      
      mutate(
        duration_kont = REU_Timestamp_Corr - uwb_start_sec_corr,
        swap_col = ifelse(TagID != i, 1, 0),
        TagID1 = ifelse(swap_col == 1, uwb_targetTagID, TagID),
        uwb_targetTagID = ifelse(swap_col == 1, TagID, uwb_targetTagID)
      ) %>%
      select(
        TagID = TagID1,
        Packet_timestamp_sec:Event_counter,
        uwb_targetTagID:uwb_duration_sec_corr,
        uwb_start_sec_corr_abs:duration_kont
      )
    
    kont_t_target_S3D9EF6C_3 <- kont_t_target_S3D9EF6C_2 %>%
      arrange(REU_Timestamp_Corr) %>%
      group_by(TagID, uwb_targetTagID) %>%
      arrange(REU_Timestamp_Corr, by_group = TRUE) %>%
      
      mutate(Inter_time = cumsum(
        uwb_start_sec_corr_abs > lag(REU_Timestamp_Corr_abs) &
          !is.na(lag(REU_Timestamp_Corr_abs))
      ))
    
    t_kont_1_S3D9EF6C <- kont_t_target_S3D9EF6C_3 %>%
      ungroup() %>%
      group_by(TagID, uwb_targetTagID, Inter_time) %>%
      summarize(
        uwb_start_1 = min(uwb_start_sec_corr),
        uwb_end_1 = max(REU_Timestamp_Corr)
      ) %>%
      mutate(
        duration_50_100_150_ges_sec_1 = uwb_end_1 - uwb_start_1,
        uwb_start_abs_1 = as_datetime(uwb_start_1, tz = "Europe/Berlin"),
        uwb_end_abs_1 = as_datetime(uwb_end_1, tz = "Europe/Berlin")
      )
    
    # Entry
    E1_ges_start <-
      (as_datetime("2020-08-22 10:00:00", tz = "Europe/Berlin"))
    E1_ges_end <-
      (as_datetime("2020-08-22 10:24:59", tz = "Europe/Berlin"))
    
    E1_ges_start_2 <-
      (as_datetime("2020-08-22 11:01:00", tz = "Europe/Berlin"))
    E1_ges_end_2 <-
      (as_datetime("2020-08-22 11:59:59", tz = "Europe/Berlin"))
    
    # 1st
    HZ1_1_ges_start <-
      (as_datetime("2020-08-22 12:00:00", tz = "Europe/Berlin"))
    HZ1_1_ges_end <-
      (as_datetime("2020-08-22 12:21:59", tz = "Europe/Berlin"))
    
    # HT
    P1_ges_start <-
      (as_datetime("2020-08-22 12:22:00", tz = "Europe/Berlin"))
    P1_ges_end <-
      (as_datetime("2020-08-22 12:44:59", tz = "Europe/Berlin"))
    
    # 2nd
    HZ2_1_ges_start <-
      (as_datetime("2020-08-22 12:45:00", tz = "Europe/Berlin"))
    HZ2_1_ges_end <-
      (as_datetime("2020-08-22 13:04:59", tz = "Europe/Berlin"))
    
    # Ex
    A1_ges_start <-
      (as_datetime("2020-08-22 13:05:00", tz = "Europe/Berlin"))
    A1_ges_end <-
      (as_datetime("2020-08-22 13:20:00", tz = "Europe/Berlin"))
    
    # scenario 2
    s2_ges_start <-
      (as_datetime("2020-08-22 13:50:00", tz = "Europe/Berlin"))
    s2_ges_end <-
      (as_datetime("2020-08-22 16:04:59", tz = "Europe/Berlin"))
    
    # En
    E2_ges_start <-
      (as_datetime("2020-08-22 13:50:00", tz = "Europe/Berlin"))
    E2_ges_end <-
      (as_datetime("2020-08-22 14:49:59", tz = "Europe/Berlin"))
    
    # 1st
    HZ1_2_ges_start <-
      (as_datetime("2020-08-22 14:50:00", tz = "Europe/Berlin"))
    HZ1_2_ges_end <-
      (as_datetime("2020-08-22 15:06:59", tz = "Europe/Berlin"))
    
    # HT
    P2_ges_start <-
      (as_datetime("2020-08-22 15:07:00", tz = "Europe/Berlin"))
    P2_ges_end <-
      (as_datetime("2020-08-22 15:29:59", tz = "Europe/Berlin"))
    
    # 2nd
    HZ2_2_ges_start <-
      (as_datetime("2020-08-22 15:30:00", tz = "Europe/Berlin"))
    HZ2_2_ges_end <-
      (as_datetime("2020-08-22 15:51:59", tz = "Europe/Berlin"))
    
    # Ex
    A2_ges_start <-
      (as_datetime("2020-08-22 15:52:00", tz = "Europe/Berlin"))
    A2_ges_end <-
      (as_datetime("2020-08-22 16:05:00", tz = "Europe/Berlin"))
    
    
    # scenario 3
    
    s3_ges_start <-
      (as_datetime("2020-08-22 16:30:00", tz = "Europe/Berlin"))
    s3_ges_end <-
      (as_datetime("2020-08-22 18:50:00", tz = "Europe/Berlin"))
    
    # En
    E3_ges_start <-
      (as_datetime("2020-08-22 16:30:00", tz = "Europe/Berlin"))
    E3_ges_end <-
      (as_datetime("2020-08-22 17:31:59", tz = "Europe/Berlin"))
    
    # 1st
    HZ1_3_ges_start <-
      (as_datetime("2020-08-22 17:32:00", tz = "Europe/Berlin"))
    HZ1_3_ges_end <-
      (as_datetime("2020-08-22 17:54:59", tz = "Europe/Berlin"))
    
    # HT
    P3_ges_start <-
      (as_datetime("2020-08-22 17:55:00", tz = "Europe/Berlin"))
    P3_ges_end <-
      (as_datetime("2020-08-22 18:15:59", tz = "Europe/Berlin"))
    
    # 2nd
    HZ2_3_ges_start <-
      (as_datetime("2020-08-22 18:16:00", tz = "Europe/Berlin"))
    HZ2_3_ges_end <-
      (as_datetime("2020-08-22 18:36:59", tz = "Europe/Berlin"))
    
    # Ex
    A3_ges_start <-
      (as_datetime("2020-08-22 18:37:00", tz = "Europe/Berlin"))
    A3_ges_end <-
      (as_datetime("2020-08-22 18:50:00", tz = "Europe/Berlin"))
    
    
    t_kont_1_dist_s1_3 <- t_kont_1_S3D9EF6C %>%
      ungroup() %>%
      mutate(
        # Scenario 1 ####
        
        # 1 Entry ####
        
        Int_dist_all = case_when(
          # falls start und end drin
          uwb_start_abs_1 >= E1_ges_start &
            uwb_end_abs_1 <= E1_ges_end
          ~ interval(uwb_start_abs_1, uwb_end_abs_1),
          
          between(uwb_start_abs_1, E1_ges_start, E1_ges_end) &
            uwb_end_abs_1 > E1_ges_end
          ~  interval(uwb_start_abs_1, E1_ges_end),
          
          between(uwb_end_abs_1, E1_ges_start, E1_ges_end) &
            uwb_start_abs_1 < E1_ges_start
          ~  interval(E1_ges_start, uwb_end_abs_1),
          
          uwb_start_abs_1 < E1_ges_start &
            uwb_end_abs_1 > E1_ges_end
          ~  interval(E1_ges_start, E1_ges_end)
        )
      ) %>%
      filter(!is.na(Int_dist_all)) %>%
      mutate(Scenario = 1,
             Part = "Entry 1") %>%
      
      # 2nd Entry ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= E1_ges_start_2 &
              uwb_end_abs_1 <= E1_ges_end_2
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, E1_ges_start_2, E1_ges_end_2) &
              uwb_end_abs_1 > E1_ges_end_2
            ~  interval(uwb_start_abs_1, E1_ges_end_2),
            
            between(uwb_end_abs_1, E1_ges_start_2, E1_ges_end_2) &
              uwb_start_abs_1 < E1_ges_start_2
            ~  interval(E1_ges_start_2, uwb_end_abs_1),
            
            uwb_start_abs_1 < E1_ges_start_2 &
              uwb_end_abs_1 > E1_ges_end_2
            ~  interval(E1_ges_start_2, E1_ges_end_2)
          )
        ) %>%
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 1,
               Part = "Entry 2")
    ) %>%
      
      # 1st ##
      bind_rows(
        t_kont_1_S3D9EF6C %>%
          ungroup() %>%
          mutate(
            Int_dist_all = case_when(
              # falls start und end drin
              uwb_start_abs_1 >= HZ1_1_ges_start &
                uwb_end_abs_1 <= HZ1_1_ges_end
              ~ interval(uwb_start_abs_1, uwb_end_abs_1),
              
              between(uwb_start_abs_1, HZ1_1_ges_start, HZ1_1_ges_end) &
                uwb_end_abs_1 > HZ1_1_ges_end
              ~  interval(uwb_start_abs_1, HZ1_1_ges_end),
              
              between(uwb_end_abs_1, HZ1_1_ges_start, HZ1_1_ges_end) &
                uwb_start_abs_1 < HZ1_1_ges_start
              ~  interval(HZ1_1_ges_start, uwb_end_abs_1),
              
              uwb_start_abs_1 < HZ1_1_ges_start &
                uwb_end_abs_1 > HZ1_1_ges_end
              ~  interval(HZ1_1_ges_start, HZ1_1_ges_end)
            )
          ) %>%
          
          filter(!is.na(Int_dist_all)) %>%
          mutate(Scenario = 1,
                 Part = "Half Time 1")
      )  %>%
      
      
      # HT ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= P1_ges_start &
              uwb_end_abs_1 <= P1_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, P1_ges_start, P1_ges_end) &
              uwb_end_abs_1 > P1_ges_end
            ~  interval(uwb_start_abs_1, P1_ges_end),
            
            between(uwb_end_abs_1, P1_ges_start, P1_ges_end) &
              uwb_start_abs_1 < P1_ges_start
            ~  interval(P1_ges_start, uwb_end_abs_1),
            
            uwb_start_abs_1 < P1_ges_start &
              uwb_end_abs_1 > P1_ges_end
            ~  interval(P1_ges_start, P1_ges_end)
          )
        ) %>%
        
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 1,
               Part = "Break")
    ) %>%
      
      # 2nd ####
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= HZ2_1_ges_start &
              uwb_end_abs_1 <= HZ2_1_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, HZ2_1_ges_start, HZ2_1_ges_end) &
              uwb_end_abs_1 > HZ2_1_ges_end
            ~  interval(uwb_start_abs_1, HZ2_1_ges_end),
            
            between(uwb_end_abs_1, HZ2_1_ges_start, HZ2_1_ges_end) &
              uwb_start_abs_1 < HZ2_1_ges_start
            ~  interval(HZ2_1_ges_start, uwb_end_abs_1),
            
            
            uwb_start_abs_1 < HZ2_1_ges_start &
              uwb_end_abs_1 > HZ2_1_ges_end
            ~  interval(HZ2_1_ges_start, HZ2_1_ges_end)
          )
        ) %>%
        
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 1,
               Part = "Half Time 2")
      
    ) %>%
      
      # Ex ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= A1_ges_start &
              uwb_end_abs_1 <= A1_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, A1_ges_start, A1_ges_end) &
              uwb_end_abs_1 > A1_ges_end
            ~  interval(uwb_start_abs_1, A1_ges_end),
            
            between(uwb_end_abs_1, A1_ges_start, A1_ges_end) &
              uwb_start_abs_1 < A1_ges_start
            ~  interval(A1_ges_start, uwb_end_abs_1),
            
            uwb_start_abs_1 < A1_ges_start &
              uwb_end_abs_1 > A1_ges_end
            ~  interval(A1_ges_start, A1_ges_end)
          )
        ) %>%
        
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 1,
               Part = "Outlet")
      
    ) %>%
      
      # Scenario 2
      
      bind_rows(
        t_kont_1_S3D9EF6C %>%
          ungroup() %>%
          mutate(
            # En ####
            
            Int_dist_all = case_when(
              uwb_start_abs_1 >= E2_ges_start &
                uwb_end_abs_1 <= E2_ges_end
              ~ interval(uwb_start_abs_1, uwb_end_abs_1),
              
              between(uwb_start_abs_1, E2_ges_start, E2_ges_end) &
                uwb_end_abs_1 > E2_ges_end
              ~  interval(uwb_start_abs_1, E2_ges_end),
              
              between(uwb_end_abs_1, E2_ges_start, E2_ges_end) &
                uwb_start_abs_1 < E2_ges_start
              ~  interval(E2_ges_start, uwb_end_abs_1),
              
              uwb_start_abs_1 < E2_ges_start &
                uwb_end_abs_1 > E2_ges_end
              ~  interval(E2_ges_start, E2_ges_end)
            )
          ) %>%
          filter(!is.na(Int_dist_all)) %>%
          mutate(Scenario = 2,
                 Part = "Entry")
      ) %>%
      
      # 1st ##
      bind_rows(
        t_kont_1_S3D9EF6C %>%
          ungroup() %>%
          mutate(
            Int_dist_all = case_when(
              uwb_start_abs_1 >= HZ1_2_ges_start &
                uwb_end_abs_1 <= HZ1_2_ges_end
              ~ interval(uwb_start_abs_1, uwb_end_abs_1),
              
              between(uwb_start_abs_1, HZ1_2_ges_start, HZ1_2_ges_end) &
                uwb_end_abs_1 > HZ1_2_ges_end
              ~  interval(uwb_start_abs_1, HZ1_2_ges_end),
              
              between(uwb_end_abs_1, HZ1_2_ges_start, HZ1_2_ges_end) &
                uwb_start_abs_1 < HZ1_2_ges_start
              ~  interval(HZ1_2_ges_start, uwb_end_abs_1),
              
              uwb_start_abs_1 < HZ1_2_ges_start &
                uwb_end_abs_1 > HZ1_2_ges_end
              ~  interval(HZ1_2_ges_start, HZ1_2_ges_end)
            )
          ) %>%
          
          
          filter(!is.na(Int_dist_all)) %>%
          mutate(Scenario = 2,
                 Part = "Half Time 1")
      )  %>%
      
      
      # HT ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= P2_ges_start &
              uwb_end_abs_1 <= P2_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, P2_ges_start, P2_ges_end) &
              uwb_end_abs_1 > P2_ges_end
            ~  interval(uwb_start_abs_1, P2_ges_end),
            
            between(uwb_end_abs_1, P2_ges_start, P2_ges_end) &
              uwb_start_abs_1 < P2_ges_start
            ~  interval(P2_ges_start, uwb_end_abs_1),
            
            uwb_start_abs_1 < P2_ges_start &
              uwb_end_abs_1 > P2_ges_end
            ~  interval(P2_ges_start, P2_ges_end)
          )
        ) %>%
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 2,
               Part = "Break")
    ) %>%
      
      # 2nd ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= HZ2_2_ges_start &
              uwb_end_abs_1 <= HZ2_2_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, HZ2_2_ges_start, HZ2_2_ges_end) &
              uwb_end_abs_1 > HZ2_2_ges_end
            ~  interval(uwb_start_abs_1, HZ2_2_ges_end),
            
            between(uwb_end_abs_1, HZ2_2_ges_start, HZ2_2_ges_end) &
              uwb_start_abs_1 < HZ2_2_ges_start
            ~  interval(HZ2_2_ges_start, uwb_end_abs_1),
            
            uwb_start_abs_1 < HZ2_2_ges_start &
              uwb_end_abs_1 > HZ2_2_ges_end
            ~  interval(HZ2_2_ges_start, HZ2_2_ges_end)
          )
        ) %>%
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 2,
               Part = "Half Time 2")
    ) %>%
      
      # Ex ####
    
    bind_rows(
      t_kont_1_S3D9EF6C %>%
        ungroup() %>%
        mutate(
          Int_dist_all = case_when(
            uwb_start_abs_1 >= A2_ges_start &
              uwb_end_abs_1 <= A2_ges_end
            ~ interval(uwb_start_abs_1, uwb_end_abs_1),
            
            between(uwb_start_abs_1, A2_ges_start, A2_ges_end) &
              uwb_end_abs_1 > A2_ges_end
            ~  interval(uwb_start_abs_1, A2_ges_end),
            
            between(uwb_end_abs_1, A2_ges_start, A2_ges_end) &
              uwb_start_abs_1 < A2_ges_start
            ~  interval(A2_ges_start, uwb_end_abs_1),
            
            uwb_start_abs_1 < A2_ges_start &
              uwb_end_abs_1 > A2_ges_end
            ~  interval(A2_ges_start, A2_ges_end)
          )
        ) %>%
        
        
        filter(!is.na(Int_dist_all)) %>%
        mutate(Scenario = 2,
               Part = "Outlet")
      
    ) %>%
      
      # Scenario 3
      
      bind_rows(
        t_kont_1_S3D9EF6C %>%
          ungroup() %>%
          mutate(
            # En ####
            
            Int_dist_all = case_when(
              uwb_start_abs_1 >= E3_ges_start &
                uwb_end_abs_1 <= E3_ges_end
              ~ interval(uwb_start_abs_1, uwb_end_abs_1),
              
              between(uwb_start_abs_1, E3_ges_start, E3_ges_end) &
                uwb_end_abs_1 > E3_ges_end
              ~  interval(uwb_start_abs_1, E3_ges_end),
              
              between(uwb_end_abs_1, E3_ges_start, E3_ges_end) &
                uwb_start_abs_1 < E3_ges_start
              ~  interval(E3_ges_start, uwb_end_abs_1),
              
              uwb_start_abs_1 < E3_ges_start &
                uwb_end_abs_1 > E3_ges_end
              ~  interval(E3_ges_start, E3_ges_end)
            )
          ) %>%
          filter(!is.na(Int_dist_all)) %>%
          mutate(Scenario = 3,
                 Part = "Entry") %>%
          
          # 1st ##
          bind_rows(
            t_kont_1_S3D9EF6C %>%
              ungroup() %>%
              mutate(
                Int_dist_all = case_when(
                  uwb_start_abs_1 >= HZ1_3_ges_start &
                    uwb_end_abs_1 <= HZ1_3_ges_end
                  ~ interval(uwb_start_abs_1, uwb_end_abs_1),
                  
                  between(uwb_start_abs_1, HZ1_3_ges_start, HZ1_3_ges_end) &
                    uwb_end_abs_1 > HZ1_3_ges_end
                  ~  interval(uwb_start_abs_1, HZ1_3_ges_end),
                  
                  between(uwb_end_abs_1, HZ1_3_ges_start, HZ1_3_ges_end) &
                    uwb_start_abs_1 < HZ1_3_ges_start
                  ~  interval(HZ1_3_ges_start, uwb_end_abs_1),
                  
                  uwb_start_abs_1 < HZ1_3_ges_start &
                    uwb_end_abs_1 > HZ1_3_ges_end
                  ~  interval(HZ1_3_ges_start, HZ1_3_ges_end)
                )
              ) %>%
              
              filter(!is.na(Int_dist_all)) %>%
              mutate(Scenario = 3,
                     Part = "Half Time 1")
          )  %>%
          
          # HT ####
        bind_rows(
          t_kont_1_S3D9EF6C %>%
            ungroup() %>%
            mutate(
              Int_dist_all = case_when(
                uwb_start_abs_1 >= P3_ges_start &
                  uwb_end_abs_1 <= P3_ges_end
                ~ interval(uwb_start_abs_1, uwb_end_abs_1),
                
                between(uwb_start_abs_1, P3_ges_start, P3_ges_end) &
                  uwb_end_abs_1 > P3_ges_end
                ~  interval(uwb_start_abs_1, P3_ges_end),
                
                between(uwb_end_abs_1, P3_ges_start, P3_ges_end) &
                  uwb_start_abs_1 < P3_ges_start
                ~  interval(P3_ges_start, uwb_end_abs_1),
                
                uwb_start_abs_1 < P3_ges_start &
                  uwb_end_abs_1 > P3_ges_end
                ~  interval(P3_ges_start, P3_ges_end)
              )
            ) %>%
            
            filter(!is.na(Int_dist_all)) %>%
            mutate(Scenario = 3,
                   Part = "Break")
        ) %>%
          
          # 2nd####
        bind_rows(
          t_kont_1_S3D9EF6C %>%
            ungroup() %>%
            mutate(
              Int_dist_all = case_when(
                uwb_start_abs_1 >= HZ2_3_ges_start &
                  uwb_end_abs_1 <= HZ2_3_ges_end
                ~ interval(uwb_start_abs_1, uwb_end_abs_1),
                
                between(uwb_start_abs_1, HZ2_3_ges_start, HZ2_3_ges_end) &
                  uwb_end_abs_1 > HZ2_3_ges_end
                ~  interval(uwb_start_abs_1, HZ2_3_ges_end),
                
                between(uwb_end_abs_1, HZ2_3_ges_start, HZ2_3_ges_end) &
                  uwb_start_abs_1 < HZ2_3_ges_start
                ~  interval(HZ2_3_ges_start, uwb_end_abs_1),
                
                uwb_start_abs_1 < HZ2_3_ges_start &
                  uwb_end_abs_1 > HZ2_3_ges_end
                ~  interval(HZ2_3_ges_start, HZ2_3_ges_end)
              )
            ) %>%
            
            
            filter(!is.na(Int_dist_all)) %>%
            mutate(Scenario = 3,
                   Part = "Half Time 2")
          
        ) %>%
          
          # Ex ####
        
        bind_rows(
          t_kont_1_S3D9EF6C %>%
            ungroup() %>%
            mutate(
              Int_dist_all = case_when(
                uwb_start_abs_1 >= A3_ges_start &
                  uwb_end_abs_1 <= A3_ges_end
                ~ interval(uwb_start_abs_1, uwb_end_abs_1),
                
                between(uwb_start_abs_1, A3_ges_start, A3_ges_end) &
                  uwb_end_abs_1 > A3_ges_end
                ~  interval(uwb_start_abs_1, A3_ges_end),
                
                between(uwb_end_abs_1, A3_ges_start, A3_ges_end) &
                  uwb_start_abs_1 < A3_ges_start
                ~  interval(A3_ges_start, uwb_end_abs_1),
                
                uwb_start_abs_1 < A3_ges_start &
                  uwb_end_abs_1 > A3_ges_end
                ~  interval(A3_ges_start, A3_ges_end)
              )
            ) %>%
            
            
            filter(!is.na(Int_dist_all)) %>%
            mutate(Scenario = 3,
                   Part = "Outlet")
          
        )
      )
    
    t_kont_1_s1_3_fill <- t_kont_1_dist_s1_3 %>%
      
      ungroup() %>%
      group_by(Scenario, Part, TagID, uwb_targetTagID) %>%
      summarize(uwb_start_1 = min(int_start(Int_dist_all)),
                uwb_end_1 = max(int_end(Int_dist_all))) %>%
      ungroup() %>%
      
      mutate(
        t_kont_all_non_w = as.numeric(uwb_end_1 - uwb_start_1),
        t_kont_all_w =
          case_when(
            (Scenario == 1 &
               Part == "Half Time 1") ~ round((t_kont_all_non_w * 2.05), 5),
            (Scenario == 1 &
               Part == "Half Time 2") ~ round((t_kont_all_non_w * 2.25), 5),
            (Scenario == 2 &
               Part == "Half Time 1") ~ round((t_kont_all_non_w * 2.65), 5),
            (Scenario == 2 &
               Part == "Half Time 2") ~ round((t_kont_all_non_w * 2.05), 5),
            (Scenario == 3 &
               Part == "Half Time 1") ~ round((t_kont_all_non_w * 1.96), 5),
            (Scenario == 3 &
               Part == "Half Time 2") ~ round((t_kont_all_non_w * 2.14), 5),
            TRUE ~ t_kont_all_non_w
          )
        
      )
    
    t_kont_1_s1_3_fill
    
  }


stopCluster(cl)
gc()


# aggregate entry 1 und 2 in Scenario 1


t_kont_1_s1_3_w_par_gapfill_agg <-
  t_kont_1_s1_3_w_par_gapfill %>%
  mutate(Part = ifelse(Part == "Outlet", "Exit", Part)) %>%
  filter(Scenario == 1, Part %in% c("Entry 1", "Entry 2")) %>%
  group_by(Scenario, TagID, uwb_targetTagID) %>%
  summarize(
    t_kont_all_non_w = sum(t_kont_all_non_w, na.rm = T),
    t_kont_all_w = sum(t_kont_all_w, na.rm = T)
  ) %>%
  mutate(Part = "Entry") %>%
  bind_rows(
    t_kont_1_s1_3_w_par_gapfill %>%
      mutate(Part = ifelse(Part == "Outlet", "Exit", Part)) %>%
      select(-c(uwb_start_1, uwb_end_1)) %>%
      filter(Part %in% c("Entry 1", "Entry 2") == F)
  )

save(
  t_kont_1_s1_3_w_par_gapfill_agg,
  file = paste0(data_path, "t_kont_1_s1_3_w_par_gapfill_agg.Rdata")
)
