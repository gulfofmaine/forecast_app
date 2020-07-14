anom_fun <- function(input_Data, zones, vis, sur_bot){
  if(vis == "raw_temp"){
    plot1 <- input_Data %>% 
      mutate(Yearday = yday(t), Month = month(t), yr = year(t)) %>% filter(yr >1981) %>%
      ggplot() + geom_line(aes(x = t, y = zone_avg, color = zone)) +
      labs(x = "Date", y = "Temperature (deg F)") + 
      scale_color_manual(values = c("#25356f",
                         "#cb9d00",
                         "#961e1d",
                         "#b3875a",
                         "#ffd380",
                         "#76a1a0",
                         "#97c8f0"),
                         name = " Zone") +
      scale_y_continuous(limits = c(35,70))}
    
  if(vis == "anom"){
      base <- input_Data %>% 
        mutate(Month = month(t), yr = year(t)) %>% filter(yr %in% c(1982:2011)) %>% 
        group_by(Month, zone) %>%
        dplyr::summarise(avg = mean(zone_avg, na.rm= TRUE)) 
      
      plot1 <- input_Data %>% 
        mutate(Month = month(t), yr = year(t)) %>% filter(yr >1981) %>%
        left_join(., base, by = c("Month", "zone" = "zone")) %>% mutate(anom = zone_avg - avg) %>% 
        ungroup() %>% 
        ggplot() + geom_line(aes(x = t, y = anom, color = zone)) +
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
    plot1 <- input_Data %>% filter(zone == zones) %>% 
      mutate(Month = month(t), yr = year(t)) %>% filter(yr >1981) %>%
      ggplot() + geom_line(aes(x = Month, y = zone_avg, color = as.factor(yr))) +
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
    
    plot1 <- input_Data %>% filter(zone == zones) %>%
      mutate(Month = month(t), yr = year(t)) %>% filter(yr >1981) %>%
      left_join(., base, by = c("Month", "zone" = "zone")) %>% mutate(anom = zone_avg - avg) %>% 
      ungroup() %>% 
      ggplot() + geom_line(aes(x = Month, y = anom, col = as.factor(yr))) + 
      labs(x = "Month", y = "Temperature (deg F)", color = "Year") +
      scale_x_continuous(breaks = seq(1,12,1), 
                         labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      scale_color_viridis_d(option = "plasma")
  }
  return(plot1)
}
