# plot SF Eel Temperature


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggtext)
#library(tidylog)

# Read in Data ------------------------------------------------------------

# temperature data 1996 to 2008
daily1 <- read_csv("data/sf_eel_stilling_well_1990-2008.csv")

# need to pivot the data around
daily1 <- tidyr::pivot_longer(data = daily1,
                              cols=c(`1990`:`2008`),
                              names_to="year", values_to="temp_c") %>%
  # now fix the date col
  mutate(date = ydm(paste0(year, "-", Date))) %>%
  arrange(date) %>% select(-year, -Date)

# add 2009-2010
dat <- read_csv("data/SF_Eel_2008-2010_Q_and_watertemp_AngeloHQ.csv")

# convert date
dat %>%
  mutate(Datetime = mdy_hm(Datetime),
         YYYY = year(Datetime),
         mon = month(Datetime),
         DD = day(Datetime),
         HH = hour(Datetime)) %>%
  group_by(YYYY, mon, DD) %>%
  summarize(cms_mean = mean(Q_cms_HQ_Gage),
            water_inside = mean(WaterTemp_inside_GHQ),
            water_outside = mean(WaterTemp_outside_GHQ)) %>%
  mutate(date = ymd(paste0(YYYY, "-", mon, "-", DD))) %>%
  ungroup() %>%
  select(-c(YYYY, mon, DD, cms_mean, water_inside)) %>%
  rename(temp_c = water_outside) %>%
  # filter to Apr through Sep
  filter(month(date)>3, month(date)<10) -> daily2

# combine these two
daily <- bind_rows(daily1, daily2)

# check distinct?
daily %>% distinct(date) %>% tally()

# now get more recent temp data
dat11 <- read_csv("data/sf_eel_stilling_well_2011-2020_watertemp.csv") %>% janitor::clean_names() %>%
  rename(datetime = timestamp,
         temp_c= water_temp_outside_cs450_ghq_angelo_hq_sf_eel_gage) %>%
  select(datetime, temp_c) %>%
  mutate(YYYY = year(datetime),
         mon = month(datetime),
         DD = day(datetime)) %>%
  group_by(YYYY, mon, DD) %>%
  summarize(temp_c = mean(temp_c)) %>%
  mutate(date = ymd(paste0(YYYY, "-", mon, "-", DD))) %>%
  ungroup() %>%
  select(-c(YYYY, mon, DD)) %>%
  # filter to Apr through Sep
  filter(month(date)>3, month(date)<10)

# bind to full dataset!
sf_daily <- bind_rows(daily, dat11)

# double check
sf_daily %>% distinct(date) %>% tally()

# add year and water year
sf_daily <- wateRshedTools::add_WYD(sf_daily, datecolumn = "date")

# SWEET!! Save
save(sf_daily, file = "data/sf_eel_daily_temps_apr_sep_1996-2020.rda")

# Plot --------------------------------------------------------------------

# fix outliers?
sf_daily <- sf_daily %>%
  mutate(temp_c = case_when(
    date==ymd("2007-04-30") ~ NA_real_,
    TRUE ~ temp_c)
  )

# Need to add additional dates where other dead/injured/sick CGS were obs

#1989-07-23 no temp data
#1989-08-19 no temp data
#1995-08-11
#2004-06-01
#2006-06-27

library(ggtext)

label1 <- "*Water temperature on May 18 in 2008 was the __highest__ for this day of
the year based on data from 1990-2020*"


(g1 <- ggplot(data=sf_daily, aes(x=DOWY, y=temp_c, group=WY)) +
   #facet_wrap(WY~.) +
   geom_line(data=sf_daily, aes(x=DOWY, y=temp_c), color="gray50", lwd=0.5, alpha=0.8) +
   geom_line(data=sf_daily %>% filter(WY==2008), aes(x=DOWY, y=temp_c), color="gray10", lwd=1.25, alpha=0.8) +
   scale_x_continuous(breaks = c(213, 244, 274, 305, 336),
                      labels = c("M", "J", "J", "A", "S"))+
   theme_classic(base_family = "Roboto")+
   geom_point(data=sf_daily %>% filter(DOWY==230, !WY==2008), aes(x=DOWY, y=temp_c, fill=temp_c), pch=21, size=3, alpha=0.7) +
   geom_textbox(data=sf_daily %>% filter(DOWY==230, WY==2008),
                aes(x=DOWY, y=temp_c, label=label1),
                alpha=0.85, color="tan4",
                width = grid::unit(0.25, "npc"), # 73% of plot panel width
                hjust = 0.5, vjust = -0.25) +
   geom_point(data=sf_daily %>% filter(DOWY==230, WY==2008), aes(x=DOWY, y=temp_c, fill=temp_c), pch=21, size=6) +
   # additional pts for sick/dead DICAMPs
   geom_point(data=sf_daily %>% filter(DOWY==315, WY==1995), aes(x=DOWY, y=temp_c), fill="seagreen", pch=22, size=4.5) +
   geom_point(data=sf_daily %>% filter(DOWY==245, WY==2004), aes(x=DOWY, y=temp_c), fill="seagreen", pch=22, size=4.5) +
   geom_point(data=sf_daily %>% filter(DOWY==270, WY==2004), aes(x=DOWY, y=temp_c), fill="seagreen", pch=22, size=4.5) +
   geom_point(data=sf_daily %>% filter(DOWY==315, WY==1995), aes(x=DOWY, y=temp_c), pch=4, size=7, color="black") +
   geom_point(data=sf_daily %>% filter(DOWY==245, WY==2004), aes(x=DOWY, y=temp_c), pch=4, size=7, color="black") +
   geom_point(data=sf_daily %>% filter(DOWY==270, WY==2004), aes(x=DOWY, y=temp_c), pch=4, size=7, lwd=4, color="black") +
   # set theme
   theme(legend.position = c(0.8, 0.95), legend.direction = "horizontal",
         legend.key.height = unit(0.4,"cm")) +
   scale_fill_viridis_c(expression("Temp" ( degree*C)), option = "A", limits = c(9,18)) +
   labs(x="", y=expression("Water Temperature " ~( degree*C)),
        title="Daily mean water temperatures on SF Eel",
        caption = "Data from SF Eel Angelo HQ Stilling Gage <http://sensor.berkeley.edu/>"))


# save
ggsave(filename = "figures/summer_water_temps_avg_daily.jpg", width = 10, height = 7, dpi=300)
ggsave(filename = "figures/summer_water_temps_avg_daily.pdf", width = 10, height = 7, dpi=300, device=cairo_pdf)

# Calculate Monthly Temps -------------------------------------------------

# JUST MAY
# filter to just may
sf_may <- filter(sf_daily, month(date)==5)

# (g2 <- ggplot() +
#     geom_line(data=sf_may, aes(x=DOWY, y=temp_c, group=WY), color="gray50") +
#     scale_x_continuous(breaks = c(213, 220, 227, 234, 241),
#                        labels = c("May 1", "May 8", "May 15", "May 22", "May 29"))+
#     theme_classic(base_family = "RobotoCondensed") +
#     #gghighlight(WY==2008)+
#     geom_point(data=sf_may %>% filter(DOWY==230), aes(x=DOWY, y=temp_c, fill=temp_c), pch=21, size=4) +
#     scale_fill_viridis_c(expression("Temp" ( degree*C)), option = "A") +
#     labs(x="", y=expression("Water Temperature " ~( degree*C)),
#          title="Daily mean water temperatures on SF Eel in May",
#          subtitle ="Points = May 18th",
#          caption = "Data from SF Eel Angelo HQ Stilling Gage <http://sensor.berkeley.edu/>"))
# ggsave(filename = "figures/summer_water_temps_avg_daily.png", width = 10, height = 7, dpi=300)

# calculate MWAT (maximum weekly average temperature)
sf_may <- sf_may %>% mutate(wk = week(date))

# use rollify
library(tibbletime)
rolling_mean <- rollify(mean, window = 7)
rolling_max <- rollify(max, window = 7, )

sf_mean_wat <- sf_may %>%
  mutate(avg_wat = rolling_mean(temp_c),
         max_wat = rolling_max(temp_c))


(g2 <- ggplot() +
    geom_line(data=sf_mean_wat, aes(x=DOWY, y=max_wat, group=WY), color="gray20",
              size=0.5, alpha=0.5, show.legend = FALSE)+
    geom_line(data=sf_mean_wat %>% filter(WY==2008), aes(x=DOWY, y=max_wat, group=WY),
              color="red4", size=1.5, alpha=0.9, show.legend = FALSE)+
    geom_point(data=sf_mean_wat %>% filter(WY ==2008, DOWY==230),
               aes(x=DOWY, y=max_wat), fill="maroon", size=5, pch=21)+
    geom_text(data=sf_mean_wat %>% filter(WY ==2008, DOWY==230),
              aes(x=DOWY, y=max_wat, label="May 18, 2008"), nudge_y = 0.5, color="black")+

    scale_x_continuous(breaks = c(213, 220, 227, 234, 241),
                       labels = c("May 1", "May 8", "May 15", "May 22", "May 29"))+
    labs(x="", y=expression("MWAT " ~( degree*C)),
         title="Maximum Weekly Average Water Temperature on SF Eel in May [1990-2020]",
         subtitle = "2008 highlighted in red",
         caption = "Data from SF Eel Angelo HQ Stilling Gage <http://sensor.berkeley.edu/>") +
    theme_classic(base_family = "Roboto"))

ggsave(filename = "figures/summer_water_temps_mwat_may.jpg", width = 10, height = 7, dpi=300)





# Cowplot Moo Moo ---------------------------------------------------------

library(cowplot)
library(extrafont)


pgrid <- plot_grid(g1, g2, nrow = 2, labels = "AUTO")
pgrid

ggsave(pgrid, filename = "figures/combined_figure_water_temps_daily_mwat.png", width = 11, height = 7.5, dpi=300)
ggsave(pgrid, filename = "figures/combined_figure_water_temps_daily_mwat.jpg", width = 11, height = 7.5, dpi=600)
ggsave(pgrid, filename = "figures/combined_figure_water_temps_daily_mwat.pdf", width = 11, height = 7.5, dpi=600, device=cairo_pdf)
