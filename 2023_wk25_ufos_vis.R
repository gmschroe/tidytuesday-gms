# 2023 Tidy Tuesday, Week 25
# UFO sightings
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-06-20/readme.md
# Gabrielle M. Schroeder

# clear workspace and load libraries ----
rm(list = ls())
library(tidytuesdayR) 
library(tidyverse)
library(lubridate)
library(systemfonts)
library(ggtext)
library(geomtextpath)
library(knitr)
library(patchwork)

# load data ----
tuesdata <- tidytuesdayR::tt_load(2023, week = 25)
ufo_sightings <- tuesdata$`ufo_sightings`
places <- tuesdata$`places`

rm(list = c('tuesdata'))

# get time zones from places data frame ----

# remove duplicate places
places_distinct <- places |> 
  distinct(city, state, country_code, timezone) 

# add time zone to ufo sightings --> ufo_sightings_tz data frame
ufo_sightings_tz <- left_join(ufo_sightings, places_distinct)

# convert to local time zone; get calendar date and time ---- 
# both reported_date_time and reported_date_time_utc are times in UTC
# will convert by time zone in loop (not sure if there is a tidy way to do this - 
# difficult to work with multiple time zones in one vector, so instead 
# converting by time zone and then extracting info needed as character or int)

reported_date_time <- ufo_sightings_tz$reported_date_time
timezone <- ufo_sightings_tz$timezone
n_rows <- length(timezone) 
all_timezone <- unique(timezone)
n_timezone = length(all_timezone)

# initialise arrays for holding calendar date and hour (in local time)
month_day <- rep('', n_rows)
time_hour <- rep(0, n_rows)

# get local times for each time zone; calculate month and day
for (i in 1:n_timezone) {
  # rows with time zone i
  tz_bool <- timezone == all_timezone[i]
  
  # times in that time zone
  times_i <- reported_date_time[tz_bool]
  
  # convert to time zone
  times_i_local <- with_tz(times_i, all_timezone[i])
  
  # extract month-day (as character) and hour (as int)
  month_day[tz_bool] <- format(times_i_local, "%m-%d")
  time_hour[tz_bool] <- hour(times_i_local)
  
  rm(list = c('times_i', 'times_i_local', 'tz_bool'))
}

# add to dataframe
ufo_sightings_tz <- ufo_sightings_tz |>
  mutate(
    month_day = month_day,
    time_hour = time_hour
  )
rm(list = c('month_day', 'time_hour', 'reported_date_time', 'timezone', 'i', 'n_rows'))


# check extracted times by country -----
ufo_sightings_tz |>
  filter(country_code == 'GB') |>
  select(c('reported_date_time_utc', 'month_day', 'time_hour', 'state', 
           'country_code', 'timezone'))

# consolidate twilight and day labels ----

ufo_sightings_tz <- ufo_sightings_tz |> 
  # consolidate part of day labels
  mutate(
    day_part_coarse = factor(
      case_when(
        str_detect(day_part, 'dusk') ~ 'dusk',
        str_detect(day_part, 'dawn') ~ 'dawn',
        str_detect(day_part, 'morning') | str_detect(day_part, 'afternoon') ~ 'day',
        TRUE ~ day_part)
      )
  ) 

#############################################################################
# Final vis
# plot sightings binned by day and time

# plot functions
source('2023_wk25_ufos_lib.R')

# country data to plot
country_plot <- 'US' # set up for US, GB, and CA

# country specific settings
# United States
if (country_plot == 'US') {
  country_name <- 'the <b>United States</b>'
  legend_val <- c(10, 100, 250, 500)
  size_range <- c(0.01, 3)
  plot_dates <- as_date(c('0000-01-01', '0000-02-29', '0000-07-04'))
# United Kingdom
} else if (country_plot == 'GB') {
  country_name <- 'the <b>United Kingdom</b>'
  legend_val <- c(1, 5, 10, 14)
  size_range <-c(0.5,2.5)
  plot_dates <- as_date(c('0000-01-01', '0000-01-01')) # hack to easily make geom_textpath work with one date
} else if (country_plot == 'CA') {
  country_name <- '<b>Canada</b>'
  legend_val <- c(1, 5, 10, 14)
  size_range <-c(0.5,2.5)
  plot_dates <- as_date(c('0000-01-01', '0000-01-01')) # hack to easily make geom_textpath work with one date
}
plot_dates_y <- rep(-3, length(plot_dates))

# time labels
time_dates <-as_date(
    c('0000-12-01', '0000-12-31',
      '0000-06-01', '0000-06-30')
    ) 
time_y_early <- c(-3) #rep(c(-3), 4)
time_label_early <- c('00:00 - 00:59')
time_group <- rep(1:2, each = 2)

time_y_late <- c(26)
time_label_late <- c('23:00 - 23:59')

# legend - location of each point
legend_pts <- seq(51, 30, by = -7)
legend_date <- rep(as_date('0000-07-01'), length(legend_pts))

# get first and last year of sightings in country
country_sightings <- ufo_sightings_tz |>
  filter(country_code == country_plot)
year_lim <- year(c(min(country_sightings$reported_date_time),
              max(country_sightings$reported_date_time)))

# colours, typefaces, limits, repeatedly used values
clrs_theme <- c(
  '#201247',
           '#32285c',
           '#443e72',
           '#585588',
           '#6c6d9f',
           '#8186b5',
           '#97a0cc',
           '#afbae3'
)
clr_dark <- c('#878787') 
clr_light <- c('grey95')
all_months <- as_date("0000-01-01") %m+% months(c(0:11))
n_months <- length(all_months)
n_hours <- 24
y_min <- -5
font_family <- 'Open Sans'
font_family_header <- 'Teko'
lw <- 0.5

# country data binned by day and time
ufo_sightings_binned <- ufo_sightings_tz |>
  filter(country_code == country_plot) |> # filter to one country 
  group_by(month_day, time_hour) |> # group by day and time
  summarise(n_sightings = n()) |> # counts for each day/time combination
  mutate(month_day = as_date(month_day, format = '%m-%d')) # back to date

scale_lim <- c(min(ufo_sightings_binned$n_sightings),
               max(ufo_sightings_binned$n_sightings))
x_lim <- c(as_date('0000-01-01'), as_date('0001-01-01'))

# show times with highest number of sightings
ufo_sightings_binned |>
  arrange(desc(n_sightings))

# plot
# if (length(dev.list()) > 0) {dev.off()}
# dev.new(width = 7.5, height = 7.5, unit = 'in', noRStudioGD = TRUE)

ufo_plot <- ggplot(
  data = ufo_sightings_binned, # binned ufo data
  mapping = aes(
    x = month_day, 
    y = time_hour, 
    size = n_sightings, 
    colour = n_sightings
  )
) + 
  # month ticks lines
  plot_month_ticks(all_months, clrs_theme[4], lw, y_coord = c(y_min, -1.25)) +
  
  # inner and outer rings
  plot_rings(clr = clrs_theme[4], lw = lw, x_lim = x_lim) + 
  
  # legend
  geom_point(
    data = tibble(
      x = legend_date,
      y = legend_pts,
      val = legend_val
    ),
    mapping = aes(x = x, y = y, colour = val, size = val),
    inherit.aes = FALSE
  ) + 
  annotate('text', x = c(legend_date[1], legend_date), 
           y = c(57, legend_pts + 2 + 2 * legend_val/scale_lim[2]), 
           label = c('Number of sightings:',legend_val),
           family = font_family, colour = clrs_theme[7], size = 3) + 
  
  # dates to highlight
  geom_point(
    data = tibble(
      x = plot_dates, y = plot_dates_y
    ),
    mapping = aes(x = x, y = y),
    size = 1.25, colour = clrs_theme[7], shape = 8
  ) +
  geom_textpath(
    data = tibble(
      x = plot_dates, y = plot_dates_y,
      label = format(as_date(plot_dates), '%b. %d')
    ),
    mapping = aes(x = x, y = y, label = label),
    family = font_family, colour = clrs_theme[7], size = 2.5, hjust = 1.25,
    fontface = 'bold',
    text_only = TRUE
  ) +
  
  # plot time labels
  plot_time_labels(time_dates, time_y_early, time_label_early, clrs_theme[7]) +
  plot_time_labels(time_dates, time_y_late, time_label_late, clrs_theme[7]) +
  
  # plot sightings 
  geom_point(alpha = 1, shape = 16) +
  
  # sightings colour and sizes
  scale_colour_gradient(limits = scale_lim,
                        low = clr_dark, high = clr_light) +
  scale_size(limits = scale_lim, range = size_range) +
  
  # title
  geom_textbox(
    data = tibble(
      x = as_date('01-01', format = '%m-%d'),
      y = 39,
      label = paste0(
        '<span style="font-size:38pt;">',
        'UFO Sightings','</span>'
      )
    ),
    aes(
      x = x,
      y = y,
      label = label),
    family = font_family_header,
    hjust = 0.5,
    halign = 0.5,
    vjust = 1,
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,     
    box.padding = unit(rep(0, 4), 'pt'),
    width = unit(3, 'inch'),
    lineheight = 2.25,
    colour = clrs_theme[8]
  ) +
  
  # subtitle
  geom_textbox(
    data = tibble(
      x = as_date('01-01', format = '%m-%d'),
      y = 50,
      label = paste0(
        '<span style="font-size:8.5pt;">',
        'Number of reported Unidentified Flying Object (UFO) sightings in ', country_name,
        ' within each hour of each day of the year from ',
        year_lim[1], ' to ', year_lim[2], '.','</span>'
      )
    ),
    aes(
      x = x,
      y = y,
      label = label),
    family = font_family,
    hjust = 0.5,
    halign = 0.5,
    vjust = 1,
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,     
    box.padding = unit(rep(0, 4), 'pt'),
    width = unit(2.85, 'inch'),
    lineheight = 1,
    colour = clrs_theme[7]
  ) 

# axes and theme
ufo_plot <- add_ufo_polar_theme(  
  ufo_plot, y_min, x_lim, all_months, font_family, 
  clr_text = clrs_theme[7],
  clr_background = clrs_theme[1],
  clr_edge = 'white'
)

# caption
ufo_plot <- add_ufo_caption(ufo_plot, clr = clrs_theme[5]) 

ufo_plot

file_name <- file.path('plots', paste0('2023_wk25_ufos_', country_plot, '.png'))
ggsave(file_name,
       height = 7.5, width = 7.5, units = "in", dpi = 600)
plot_crop(file_name)

#############################################################################
# Final vis 2: 
# number of UFO sightings by hour/day, broken down by part of day

# uses same country as above plot - run above section to get variables

# info for parts of day
parts_of_day <- c('dawn', 'day', 'dusk', 'night')
clrs_day <- c('#FFBA3B', '#60CAE0' , '#BC5090', '#2D77CC')
n_parts <- length(parts_of_day)

# initialise list to hold each plot
ufo_plot_part <- list()

# plot each
for (i in 1:n_parts) {
  part_of_day_plot <- parts_of_day[i]
  clr_part_of_day <- clrs_day[i]
  
  ufo_plot_part[[i]] <- plot_ufo_part_of_day(
    ufo_sightings_tz,
    country_plot,
    part_of_day_plot,
    clr_part_of_day,
    scale_lim,
    size_range/4,
    clrs_theme
  )
}

# layout with all four parts of day
p <- (ufo_plot_part[[1]] | ufo_plot_part[[2]])/
     (ufo_plot_part[[3]] | ufo_plot_part[[4]])

# add plot level annotations and theme
p <- p + plot_annotation(
  theme = theme(
    plot.background = element_rect(fill = clrs_theme[1], colour = 'white'),
    plot.caption = element_markdown(
      family = font_family, colour = clrs_theme[5],
      hjust = 0, lineheight = 1.2, size = 7.5,
      margin = margin(l = 0.5, t = -0.2, b = 0.5, unit = 'cm')
    ),
    plot.title = element_markdown(
      family = font_family_header, colour = clrs_theme[8],
      hjust = 0, lineheight = 1.2, size = 40,
      margin = margin(t = 0.5, l = 0.5, unit = 'cm')
    ), 
    plot.subtitle = element_markdown(
      family = font_family, colour = clrs_theme[7],
      hjust = 0, lineheight = 1.2, size = 12,
      margin = margin(t = 0.25, l = 0.5, b = 0.25, unit = 'cm')
    ),
    plot.caption.position = 'panel'
  ), 
  caption =
    paste0('Visualisation by <b>Gabrielle M. Schroeder</b><br>',
           'Data sources: National UFO Reporting Center and sunrise-sunset.org<br>',
           'Data cleaned by Jon Harmon for Tidy Tuesday week 25, 2023'),
  title = paste0('UFO Sightings by time of day'),
  subtitle = paste0(
    'Reported Unidentified Flying Object (UFO) sightings in ', country_name
  )
)

p

# save
file_name <- file.path('plots', paste0('2023_wk25_ufos_', country_plot, '_by_part.png'))
ggsave(file_name,
       height = 9, width = 8, units = "in", dpi = 600)
plot_crop(file_name)
