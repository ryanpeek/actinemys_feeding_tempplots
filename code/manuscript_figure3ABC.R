# side pool plot

library(tidyverse)
library(lubridate)
library(ggtext)
library(readxl)
library(patchwork)
#library(cowplot)
#library(extrafont)
#library(tidylog)


# Load Temperature Data 1996-2020 -----------------------------------------

load("data/sf_eel_daily_temps_apr_sep_1996-2020.rda")

# Load DITE Observations --------------------------------------------------

# Need to add additional dates where other dead/injured/sick CGS were obs

# FROM MARY POWER (added to PBG csv below)
#1989-07-23 no temp data
#1989-08-19 no temp data
#1995-08-11
#2004-06-01
#2006-06-27

# FROM PHILLIP GEORGAKAKOS
dite <- read_csv("data/dicamp_sfEel_sightings_pbg.csv") %>%
   mutate(date=mdy(date))

# filter to only dead individuals
dited <- dite %>% filter(dead_alive!="alive") %>%
   slice(-3) # drop duplicate obs on Jul 22 2017

# add the DOWY col for plotting
dited <- left_join(dited, sf_daily, by="date")

# PLOT 3A --------------------------------------------------------------

# fix outliers?
sf_daily <- sf_daily %>%
  mutate(temp_c = case_when(
    date==ymd("2007-04-30") ~ NA_real_,
    TRUE ~ temp_c)
  )

label1 <- "*Water temperature on May 18 in 2008 was the __highest__ for this day of
the year based on data from 1990-2020*"


(g1 <- ggplot(data=sf_daily, aes(x=DOWY, y=temp_c, group=WY)) +
    #facet_wrap(WY~.) +
    geom_line(data=sf_daily, aes(x=DOWY, y=temp_c), color="gray50", lwd=0.5, alpha=0.8) +
    geom_line(data=sf_daily %>% filter(WY==2008), aes(x=DOWY, y=temp_c), color="gray10", lwd=1.25, alpha=0.8) +
    geom_line(data=sf_daily %>% filter(WY==2017), aes(x=DOWY, y=temp_c), color="dodgerblue4", lwd=1.25, alpha=0.8) +
    scale_x_continuous(breaks = c(213, 244, 274, 305, 336),
                       labels = c("May", "Jun", "Jul", "Aug", "Sep"))+
    theme_classic(base_family = "Roboto Condensed", base_size = 15)+
    geom_point(data=sf_daily %>% filter(DOWY==230, !WY==2008), aes(x=DOWY, y=temp_c, fill=temp_c), pch=21, size=3, alpha=0.7) +
    geom_textbox(data=sf_daily %>% filter(DOWY==230, WY==2008),
                 aes(x=DOWY, y=temp_c, label=label1),
                 color="black", fill=alpha("lightgoldenrod1",0.2),
                 width = grid::unit(0.25, "npc"), # 73% of plot panel width
                 hjust = 1.05, vjust = 0.35) +
    geom_point(data=sf_daily %>% filter(DOWY==230, WY==2008), aes(x=DOWY, y=temp_c, fill=temp_c), pch=21, size=6) +

    # additional pts for dead DICAMPS (Phil & Mary)
    geom_point(data=dited, aes(x=DOWY, y=temp_c, fill=temp_c),
               #fill="chocolate2",
               pch=22, size=4.5) +
    #geom_point(data=dited, aes(x=DOWY, y=temp_c), pch=4, size=7, color="black") +
    ggrepel::geom_label_repel(data=dited, aes(x=DOWY, y=temp_c, label=WY), size=4, alpha=0.9, color="black") +

    # set theme
    theme(legend.position = c(0.8, 0.95), legend.direction = "horizontal",
          legend.key.height = unit(0.4,"cm")) +
    scale_fill_viridis_c(expression("Temp" ( degree*C)), option = "A", limits = c(9,23)) +
    labs(x="", y=expression("Water Temperature " ~( degree*C)),
         title="Daily mean water temperatures on SF Eel"))
         #caption = "Data from SF Eel Angelo HQ Stilling Gage <http://sensor.berkeley.edu/>"))

g1 + plot_annotation(tag_levels = "A")

#plot_grid(g1, nrow=1, labels="A", label_fontfamily = "Roboto Condensed")

# save
ggsave(filename = "figs/fig1A_sfeel_dailymean_1990-2020_v2.tiff", width = 10, height = 7, dpi=350)
#ggsave(filename = "figures/fig1A_sfeel_dailymean_1990-2020_v2.pdf", width = 10, height = 7, dpi=300, device=cairo_pdf)

# PLOT 3B -----------------------------------------------------------------

# calculate rolling weekly average temperature
sf_may <- filter(sf_daily, month(date)==5)
sf_may <- sf_may %>% mutate(wk = week(date))

# use rollify
library(tibbletime)
rolling_mean <- rollify(mean, window = 7)
rolling_max <- rollify(max, window = 7, )

sf_mean_wat <- sf_may %>%
  mutate(avg_wat = rolling_mean(temp_c),
         max_wat = rolling_max(temp_c))

# plot
(g2 <- ggplot() +
    geom_line(data=sf_mean_wat, aes(x=DOWY, y=max_wat, group=WY), color="gray20",
              size=0.5, alpha=0.5, show.legend = FALSE)+
    geom_line(data=sf_mean_wat %>% filter(WY==2008), aes(x=DOWY, y=max_wat, group=WY),
              color="red4", size=1.5, alpha=0.9, show.legend = FALSE)+
    geom_point(data=sf_mean_wat %>% filter(WY ==2008, DOWY==230),
               aes(x=DOWY, y=max_wat), fill="maroon", size=5, pch=21)+
    ggrepel::geom_label_repel(data=sf_mean_wat %>% filter(WY ==2008, DOWY==230),
              aes(x=DOWY, y=max_wat, label="May 18, 2008"), nudge_y = 0.8, color="black", size=4)+

    scale_x_continuous(breaks = c(213, 220, 227, 234, 241),
                       labels = c("May 1", "May 8", "May 15", "May 22", "May 29"))+
    labs(x="", y=expression("MWAT " ~( degree*C)),
         title="May Maximum Weekly Average Water Temperature \non SF Eel [1990-2020]",
         subtitle = "2008 highlighted in red") +
         #caption = "Data from SF Eel Angelo HQ Stilling Gage <http://sensor.berkeley.edu/>") +
    theme_classic(base_family = "Roboto Condensed", base_size = 15))

g2 + plot_annotation(tag_levels = list(c("B")))

ggsave(filename = "figs/fig1B_sfeel_max7day_may.tiff", width = 10, height = 7, dpi=350)
#ggsave(filename = "figs/fig1B_sfeel_max7day_may.pdf", width = 10, height = 7, dpi=300, device=cairo_pdf)

# save stack
g1 / g2 + plot_annotation(tag_levels = "A")
ggsave(filename = "figs/fig1AB_sfeel_temps.png", width = 8, height = 7, dpi=350)

# PLOT 3A-3B Cowplot Moo Moo --------------------------------------------------

# library(cowplot)
# library(extrafont)


# (pgrid <- plot_grid(g1, g2, nrow = 2, labels = "AUTO", rel_heights = c(1, 0.8), label_fontfamily = "Roboto Condensed"))
#
# ggsave(pgrid, filename = "figures/fig1AB_combined_figure_water_temps_daily_mwat.jpg", width = 11, height = 9, dpi=300)


# 3C DATA ------------------------------------------------------------

# Load ibuttons
ibuts <- read_xls("data/sfeel_may_2008_temperatures_side_pool.xls")

# clean names
ibuts %>%
  janitor::clean_names() %>%
  rename("main_channel"=5, "side_pool"=6, "fox_ck"=7, "elder"=8) -> clean_dat

# rearrange for easier plotting
clean_dat <- clean_dat %>%
  pivot_longer(cols = c(main_channel:elder), names_to="site", values_to = "temps") %>%
  arrange(site, datetime)

# add the daily average from science center Stilling Well Mean daily
sf_daily_08 <- filter(sf_daily, WY==2008, month(date)==5)
# add a time so we can plot this data in the same frame:
sf_daily_08 <- sf_daily_08 %>%
  filter(DOY > 132) %>%
  mutate(datetime = ymd_hms(paste0(as.character(date), " 12:00:00")),
         site="stilling_well") %>%
  rename(temps=temp_c) %>%
  select(datetime, temps, site)

# PLOT 3C --------------------------------------------------------------------

(g3 <- ggplot() +
   geom_line(data=clean_dat, aes(x=datetime, y=temps, color=site), lwd=0.7, alpha=0.8) +
   geom_line(data=clean_dat %>% filter(site=="side_pool"), aes(x=datetime, y=temps, color=site), lwd=1., alpha=0.8) +
   theme_classic(base_family = "Roboto Condensed", base_size = 15) +
   geom_point(data=clean_dat %>% filter(site=="side_pool",
                                        datetime == ymd_hms("2008-05-18 17:29:00")),
              aes(x=datetime, y=temps), fill="red4", pch=21, size=5, alpha=0.9) +
   geom_text(data=clean_dat %>% filter(site=="side_pool",
                                       datetime == ymd_hms("2008-05-18 17:29:00")),
             aes(x=datetime, y=temps, label="May 18"), nudge_y = .95, color="black")+
   theme(legend.position = c(0.8, 0.77), legend.direction = "vertical",
         legend.key.height = unit(0.4,"cm"),
         plot.margin=unit(c(0.5,0.8,0.7,0.7),"cm")) +
   labs(x="", y=expression("Water Temperature " ~( degree*C)),
        title="2008 hourly water temperatures on SF Eel in different locations",
        caption = "Data collected from Thermochron© i-buttons recording every 2 hours")+
   viridis::scale_color_viridis(discrete=TRUE, "iButton Location", labels=c("Elder Creek", "Fox Creek", "SF Eel: Main Channel", "SF Eel: Side Pool")) +
    theme(legend.text = element_text(size=12)))


# all together
#plot_grid(g3, nrow=1, labels="C", label_fontfamily = "Roboto Condensed")

ggsave(filename = "figs/fig1C_sfeel_water_temps_ibuttons.png", width = 10, height = 7, dpi=350)
#ggsave(filename = "figures/fig1C_sfeel_water_temps_ibuttons.pdf", width = 10, height = 7, dpi=300, device=cairo_pdf)



# All Combined ------------------------------------------------------------

# use patchwork here
g1 / g2 / g3 + plot_annotation(tag_levels = "A",
                               theme = theme(plot.margin = margin(0.05, r = 0.1, b = 0.05, l = 0.05)))

ggsave(filename = "figs/fig1ABC_sfeel_temps.png", width = 8, height = 11, dpi=350)

ggsave(filename = "figs/fig1ABC_sfeel_temps.tiff", width = 8, height = 11, dpi=400)

#ggsave(filename = "figs/fig1ABC_sfeel_temps.tiff", width = 8, height = 11, dpi=350)

# ggsave(pgrid, filename = "figures/combined_figure_water_temps_3ABC.png", width = 11, height = 11, dpi=300)
# ggsave(pgrid, filename = "figures/combined_figure_water_temps_3ABC.jpg", width = 11, height = 11, dpi=600)

