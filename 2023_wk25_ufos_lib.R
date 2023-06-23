# functions for making UFO vis
# week 25, 2023
# code is a bit rough and functions haven't been tested - if any don't work, check
# that all variables are passed to or defined in function (vs. in global workspace)
#
# Gabrielle M. Schroeder

# inner and outer rings
plot_rings <- function(clr, # ring colour
                       lw,  # ring linewidth
                       x_lim, # x limits of plot (dates)
                       y_pts = c(-1.25, 24.25) # y coord of inner and outer rings
) {
  geom_segment(
    data = tibble(
      x = rep(x_lim[1], 2),
      x_end = rep(x_lim[2], 2),
      y = y_pts,
      y_end = y_pts
    ),
    mapping = aes(x = x, y = y, xend = x_end, yend = y_end),
    colour = clr,
    inherit.aes = FALSE,
    linewidth = lw
  ) 
}

# month tick marks
plot_month_ticks <- function(all_months, # months to plot
                             clr, # colour to plot ticks
                             lw, # line width to plot ticks
                             y_coord # coordinates to start and end ticks
) {
  geom_segment(
    data = tibble(
      x = all_months,
      x_end = all_months,
      y = rep(y_coord[1], length(x)),
      y_end = rep(y_coord[2], length(x))
    ), 
    mapping = aes(x = x, y = y, xend = x_end, yend = y_end),
    colour = clr,
    inherit.aes = FALSE,
    linewidth = lw
  )
}

# time labels
plot_time_labels <- function(
    time_dates, # vector, 2 days per text label (to make arc)
    time_y, # y coordinate (scalar)
    time_label, # label for time (length 1)
    clr,
    font_family = 'Open Sans',
    font_size = 2.5
) {
  # grouping variable (to mark different arcs)
  time_group <- rep(1:(length(time_dates)/2), each = 2)
  time_y <- rep(time_y, length(time_dates))
  
  geom_textpath(
    data = tibble(
      x = time_dates, y = time_y, group = time_group
    ),
    mapping = aes(x = x, y = y, group = group),
    label = time_label,
    family = font_family, colour = clr, size = font_size, hjust = 0.5,
    fontface = 'italic',
    text_only = TRUE
  ) 
}

# axes and theme
add_ufo_polar_theme <- function(
    ufo_plot,
    y_min,
    x_lim,
    all_months,
    font_family,
    clr_text,
    clr_background,
    clr_edge = NA,
    text_size = 10
) {
  n_hours <- 24
  
  ufo_plot <- ufo_plot +
    coord_polar() +
    scale_y_reverse(limits = c(n_hours*2.5, y_min), breaks = c()) +
    scale_x_date(breaks = all_months, 
                 date_labels="%b",
                 limits = c(x_lim)
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(font_family, colour = clr_text, 
                               size = text_size),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = c(clr_background), colour = clr_edge),
      plot.margin = margin(0, 0, 0, 0, unit = 'pt')
    ) + 
    labs(x = '', y = '') +
    theme(legend.position = 'none')
  
  return(ufo_plot)
}

# caption
add_ufo_caption <- function(ufo_plot, clr, font_family = 'Open Sans',
                            font_size = 7.5, margin_t = -1.15) {
  
  ufo_plot <- ufo_plot +
    labs(caption =
           paste0('Visualisation by <b>Gabrielle M. Schroeder</b><br>',
                  'Data sources: National UFO Reporting Center and sunrise-sunset.org<br>',
                  'Data cleaned by Jon Harmon for Tidy Tuesday week 25, 2023')
    ) +
    theme(
      plot.caption = element_markdown(
        family = font_family, colour = clr,
        hjust = 0, lineheight = 1.2, size = font_size,
        margin = margin(t = margin_t, unit = 'cm')
      )
    )
  
  return(ufo_plot)
}

plot_ufo_part_of_day <- function(
    ufo_sightings, # ufo data frame
    country_plot, # country to plot, as country code
    part_of_day_plot, # part of day to plot - 'dawn', 'day', 'dusk', or 'night'
    clr_part_of_day, # pts colour (lower end of colour scale)
    scale_lim, # scale for points - so can have same scale across plots
    size_range, # min and max sizes of points
    clrs_theme, # theme colours, length 8 (dark to light)
    lw = 0.5, 
    font_family = 'Open Sans',
    font_family_header = 'Teko',
    y_min = -5
    
    ) {
  
  # country data binned by day, time, and part of day
  ufo_sightings_binned <-  ufo_sightings |>
    # filter to one country + part of day 
    filter(country_code == country_plot & day_part_coarse == part_of_day_plot) |> 
    group_by(month_day, time_hour) |> # group by day and time
    summarise(n_sightings = n()) |>   # counts for each day/time combination
    mutate(month_day = as_date(month_day, format = '%m-%d')) # back to date
  
  # total number of sightings for plot subtitle
  total_sightings <- sum(ufo_sightings_binned$n_sightings)
  
  # plot variables
  x_lim <- c(as_date('0000-01-01'), as_date('0001-01-01')) # date x limits
  all_months <- as_date("0000-01-01") %m+% months(c(0:11)) # define months for ticks
  n_hours <- 24
  
  # time labels
  time_dates <-as_date(
    c('0000-11-01', '0000-12-31')
  ) 
  time_y_early <- c(-5) 
  time_label_early <- c('00:00 - 00:59')
  time_group <- c(1, 1)
  time_y_late <- c(28)
  time_label_late <- c('23:00 - 23:59')
  
  
  # plot
  ufo_part_of_day <- ggplot(
    data = ufo_sightings_binned, # binned ufo data
    mapping = aes(
      x = month_day, 
      y = time_hour, 
      size = n_sightings, 
      colour = n_sightings
    )
  ) + 
    # month ticks lines (every 3 months)
    plot_month_ticks(all_months[seq(1, 12, by = 3)], clrs_theme[4], lw, y_coord = c(y_min, -1.25)) +
    
    # inner and outer rings
    plot_rings(clr = clrs_theme[4], lw = lw, x_lim = x_lim) + 
    
    # plot time labels
    plot_time_labels(time_dates, time_y_early, time_label_early, clrs_theme[7]) +
    plot_time_labels(time_dates, time_y_late, time_label_late, clrs_theme[7]) +
    
    # plot sightings 
    geom_point(alpha = 1, shape = 16) +
    
    # sightings colour and sizes
    scale_colour_gradient(limits = scale_lim,
                          low = clr_part_of_day, high = 'white') +
    scale_size(limits = scale_lim, range = size_range) +
    
    # title
    geom_textbox(
      data = tibble(
        x = as_date('01-01', format = '%m-%d'),
        y = 47.5,
        label = paste0(
          '<span style="font-size:30pt;">',
          str_to_title(part_of_day_plot), '</span>'
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
        x = as_date('07-01', format = '%m-%d'),
        y = 56,
        label = paste0(
          '<span style="font-size:10pt;">',
          formatC(total_sightings, format = 'd', big.mark = ','),
          ' sightings','</span>'
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
  ufo_part_of_day <- add_ufo_polar_theme(  
    ufo_part_of_day, y_min, x_lim, all_months[seq(1, 12, by = 3)], font_family, 
    clr_text = clrs_theme[7],
    clr_background = clrs_theme[1]
  )
  
  return(ufo_part_of_day)
}
