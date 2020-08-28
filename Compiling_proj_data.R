### GOM data

gom_lob_proj <- read_csv("Data/gom_lob_proj.csv")
gom_lob_abun <- read_csv("Data/ASMFC.csv")
gom_landings <- read_csv("Data/GoM_lob_landings.csv")

gom_landings_yr <- gom_landings  %>% group_by(Year) %>% 
  summarise(Landings = sum(Pounds, na.rm=TRUE)) %>% 
  mutate(Landings = Landings/1000000)


gom_lm <- gom_landings_yr %>% 
  left_join(., gom_lob_abun, by = "Year") %>% na.omit() %>% 
  rename("Average" = Abundance) %>% 
  lm(Landings ~ Average, data = .)


gom_landings_pred <- predict(gom_lm, newdata = gom_lob_proj, 
                             interval = "prediction", level = 0.95)

gom_landings_proj <- cbind(gom_lob_proj, gom_landings_pred) %>% 
  rename("Landings_pred" = fit) %>% 
  left_join(gom_landings_yr, by = "Year") %>% 
  left_join(gom_lob_abun, by = "Year")

write_csv(gom_landings_proj, "Data/gom_lob_data.csv")


gom_landings_proj %>% 
  filter(Model == "CMIP5_RCP 8.5_mean") %>% 
  ggplot() + 
  geom_point(aes(Year, Landings), size = 3) + 
  geom_line(aes(Year, Landings_pred), color = "darkred", size = 2) +
  geom_ribbon(aes(x = Year, ymax = upr, ymin = lwr), fill = "darkred", alpha = .1) + 
  theme(legend.position = c(.85,.85)) +
  labs(y = "Landings (millions of pounds)") +
  scale_x_continuous(limits = c(1982,2050))

gom_landings_proj %>% 
  filter(Model == "CMIP5_RCP 8.5_mean") %>% ggplot() + 
  geom_point(aes(Year, Abundance), size = 3) +
  geom_line(aes(Year, Average), size = 2, color = "darkred") +
  geom_ribbon(aes(x = Year, ymax = Average + Std*2, ymin = Average - Std*2), fill = "darkred", alpha = .1) + 
  theme(legend.position = c(.85,.85)) +
  labs(y = "Population (millions of individuals)") +
  scale_x_continuous(limits = c(1982,2050))



### SNE data

sne_landings <- read_csv("Data/sne_landings.csv")
sne_lob_abun <- read_csv("Data/sne_lob_abun.csv")
sne_lob_proj <- read_csv("Data/sne_lob_proj.csv")

sne_landings_yr <- sne_landings  %>% group_by(Year) %>% 
  summarise(Landings = sum(Pounds, na.rm=TRUE)) %>% 
  mutate(Landings = Landings/1000000)

sne_lm <- sne_landings_yr %>% 
  left_join(., sne_lob_abun, by = "Year") %>% na.omit() %>% 
  rename("Average" = Abundance) %>% 
  lm(Landings ~ Average, data = .)


sne_landings_pred <- predict(sne_lm, newdata = sne_lob_proj, 
                             interval = "prediction", level = 0.95)

sne_landings_proj <- cbind(sne_lob_proj, sne_landings_pred) %>% 
  rename("Landings_pred" = fit) %>% 
  left_join(sne_landings_yr, by = "Year") %>% 
  left_join(sne_lob_abun, by = "Year") %>% 
  mutate(Landings)
  
write_csv(sne_landings_proj, "Data/sne_lob_data.csv")


sne_landings_proj %>% 
  filter(Model == "CMIP5_RCP 8.5_mean") %>% 
  ggplot() + 
  geom_point(aes(Year, Landings), size = 3) + 
  geom_line(aes(Year, Landings_pred), color = "darkred", size = 2) +
  geom_ribbon(aes(x = Year, ymax = upr, ymin = lwr), fill = "darkred", alpha = .1) + 
  theme(legend.position = c(.85,.85)) +
  labs(y = "Landings (millions of pounds)") +
  scale_x_continuous(limits = c(1982,2050))
  
sne_landings_proj %>% 
  filter(Model == "CMIP5_RCP 8.5_mean") %>% ggplot() + 
  geom_point(aes(Year, Abundance), size = 3) +
  geom_line(aes(Year, Average), size = 2, color = "darkred") +
  geom_ribbon(aes(x = Year, ymax = Average + Std*2, ymin = Average - Std*2), fill = "darkred", alpha = .1) + 
  theme(legend.position = c(.85,.85)) +
  labs(y = "Population (millions of individuals)") +
  scale_x_continuous(limits = c(1982,2050))

