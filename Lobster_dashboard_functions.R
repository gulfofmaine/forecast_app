anom_fun <- function(input_Data, zones, vis, sur_bot,
                     season_sel){
  if(vis == "raw_temp"){
    plot1 <- input_Data  %>% 
      mutate(Month = month(t), yr = year(t), qt = quarter(t), 
             Season = if_else(qt == 1, "Winter", if_else(qt == 2, "Spring", if_else(qt == 3, "Summer", "Fall")))) %>% 
      filter(yr >1981) %>% 
      group_by(Season, zone, yr) %>% dplyr::mutate(season_avg = mean(zone_avg, na.rm=TRUE)) %>% ungroup() %>%
      rename("Monthly" = zone_avg, "Date" = t, "Zone" = zone) %>% dplyr::select(-yr, -qt, -Month) %>% 
      pivot_wider(names_from = Season, values_from = season_avg) %>% 
      pivot_longer(cols = c(-Date, -Zone), names_to = "Season", values_to = "Temperature") %>% 
      filter(Season == season_sel) %>% na.omit() %>% 
      ggplot() + geom_line(aes(x = Date, y = Temperature, color = Zone)) +
      labs(x = "Date", y = "Temperature (deg F)") + 
      scale_color_manual(values = c("#25356f",
                         "#cb9d00",
                         "#961e1d",
                         "#b3875a",
                         "#ffd380",
                         "#76a1a0",
                         "#97c8f0"),
                         name = " Zone")}
    
  if(vis == "anom"){
      base <- input_Data %>% 
        mutate(Month = month(t), yr = year(t)) %>% filter(yr %in% c(1982:2011)) %>% 
        group_by(Month, zone) %>%
        dplyr::summarise(avg = mean(zone_avg, na.rm= TRUE)) 
      
      plot1 <- input_Data %>% 
        mutate(Month = month(t), yr = year(t)) %>% filter(yr >1981) %>%
        left_join(., base, by = c("Month", "zone" = "zone")) %>% mutate(zone_avg = zone_avg - avg) %>% dplyr::select(-avg) %>% 
        ungroup() %>% mutate(qt = quarter(t), Season = if_else(qt == 1, "Winter",if_else(qt == 2, "Spring", if_else(qt == 3, "Summer", "Fall")))) %>% 
        filter(yr >1981) %>% 
        group_by(Season, zone, yr) %>% dplyr::mutate(season_avg = mean(zone_avg, na.rm=TRUE)) %>% ungroup() %>%
        rename("Monthly" = zone_avg, "Date" = t, "Zone" = zone) %>% dplyr::select(-yr, -qt, -Month) %>% 
        pivot_wider(names_from = Season, values_from = season_avg) %>% 
        pivot_longer(cols = c(-Date, -Zone), names_to = "Season", values_to = "Anomaly") %>% 
        filter(Season == season_sel) %>% na.omit() %>% 
        ggplot() + geom_line(aes(x = Date, y = Anomaly, color = Zone)) +
        labs(x = "Date", y = "Temperature (deg F)") + 
        scale_color_manual(values = c("#25356f",
                                      "#cb9d00",
                                      "#961e1d",
                                      "#b3875a",
                                      "#ffd380",
                                      "#76a1a0",
                                      "#97c8f0"),
                           name = "Zone")
    }

  return(plot1)
}


yrday_fun <- function(input_Data, zones, sur_bot, vis){
  if(vis == "raw_temp"){
    plot1 <- input_Data %>% 
      mutate(Month = month(t), yr = year(t)) %>% 
      filter(yr >1981, zone == zones) %>% rename("Date" = t, "Temperature" = zone_avg) %>%  
      mutate(Year = as.factor(yr)) %>% 
      ggplot() + geom_line(aes(x = Month, y = Temperature, color = Year)) +
      labs(x = "Month", y = "Temperature (deg F)", color = "Year")  +
      scale_x_continuous(breaks = seq(1,12,1), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      scale_color_viridis_d(option = "plasma") + 
      scale_y_continuous(limits = c(35,70))}
  
  if(vis == "anom"){
    base <- input_Data %>% filter(zone == zones) %>% 
      mutate(Month = month(t), yr = year(t)) %>% filter(yr %in% c(1982:2011)) %>% 
      group_by(Month, zone) %>%
      dplyr::summarise(avg = mean(zone_avg, na.rm= TRUE)) 
    
    plot1 <- input_Data %>% 
      mutate(Month = month(t), yr = year(t)) %>% filter(yr >1981, zone == zones) %>%
      left_join(., base, by = c("Month", "zone" = "zone")) %>% 
      mutate(Anomaly = zone_avg - avg, Year = as.factor(yr)) %>% rename("Date" = t) %>% 
      ggplot() + geom_line(aes(x = Month, y = Anomaly, col = Year)) + 
      labs(x = "Month", y = "Temperature (deg F)", color = "Year") +
      scale_x_continuous(breaks = seq(1,12,1), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      scale_color_viridis_d(option = "plasma")
  }
  return(plot1)
}
