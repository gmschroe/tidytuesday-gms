# Tidy Tuesday 2023, week 22: verified oldest people

# ----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(systemfonts)
library(ggtext)
library(ggrepel)

# load data ----

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

# sort by DOB, make gender a factor, and add end date for non-deceased
centenarians <- centenarians |>
  arrange(desc(age)) |>
  mutate(
    gender = factor(gender),
    end_date = as_date(ifelse(still_alive == 'deceased', death_date, as_date(now())))
  ) 

show(centenarians)

# men and women in separate data frames ----
cent_male <- centenarians |>
  filter(gender == 'male')

cent_female <- centenarians |>
  filter(gender == 'female')

# function for finding and keeping record-breakers ----
keep_record_cent <- function(centenarians) {
  
  # first remove people if someone was older at the time of their death
  
  # last date each person was living
  end_date <- (centenarians$end_date)

  # get age of everyone at each date (if they were living)
  # column i = ages of person i at each end date 
  birth_date <- (centenarians$birth_date)
  age_at_end_date <- outer(end_date, birth_date, FUN = "-")/365.25 # note - will be mislabelled as days
  
  # remove ages for dates that are after a person's death
  after_death <- outer(end_date, end_date, ">")
  age_at_end_date[after_death] <- -Inf # -Inf (rather than NA) so can find person with oldest age using max.col
  
  # get person with max date at each date 
  max_idx <- max.col(age_at_end_date)
  
  # save variable indicating if each person was oldest living person at the time of their death
  oldest <- max_idx == 1:nrow(centenarians) 
  centenarians <- centenarians |>
    mutate(oldest = oldest)
  
  # find non-record breakers
  centenarians <- centenarians |>
    arrange(end_date) |> # arrange by death/current date
    mutate(
      record_age_at_end_date = cummax(age),                # the record age at time of death
      is_record = (age >= record_age_at_end_date) & oldest # is their age greater than or equal to the record?
    ) 

  return(centenarians)
}

# record-breaking men and women in one dataframe ----
cent_male <- keep_record_cent(cent_male) 
cent_female <- keep_record_cent(cent_female)

cent <- rbind(cent_male, cent_female) |>
  arrange(is_record)

show(cent |> filter(is_record == TRUE))


# plot function ----

# all data for one gender
plot_life_lines <- function(
    cent, # tibble
    plot_gender, # 'male' or 'female'
    highlight_clr, # colour of record-breaking ages
    font_family = 'Charter', # for numbers and small text
    font_family_text = 'Neco' # for large text
) {
  
  # x-axis
  x_step <- 25
  x_min <- 0
  x_buff <- 30
  x_max <- ceiling(max(cent$age)/x_step)*x_step
  x_seq <- seq(x_min, x_max, x_step)
  
  # filter to specified gender and add labels
  cent <- cent |> 
    filter(gender == plot_gender) |>
    mutate(
      label = paste0(name, ', ', round(age), ' y.o.')) |># (', year(birth_date), ' - ', year(end_date), ')'))
      arrange(is_record)
  
  # y axis step and limits
  min_birth_yr <- year(min(cent$birth_date))
  max_birth_yr <- year(max(cent$birth_date))
  
  y_step <-  20
  y_buff <- 12
  y_min <- floor(min_birth_yr/y_step)*y_step # overall min
  y_max <- 2037#ceiling(year(max(cent$end_date))/y_step)*y_step # overall max
  
  y_max_birth <- ceiling(max_birth_yr/y_step)*y_step # birth max
  y_min_end <- floor(year(min(cent$end_date))/y_step)*y_step # end min
  
  # custom grid
  grid_buff <- 13
  grid_y <- rep(ymd(min_birth_yr - grid_buff, truncated = 2L), length(x_seq)) + years(x_seq)
  grid_y_end <- rep(ymd(max_birth_yr + grid_buff, truncated = 2L), length(x_seq)) + years(x_seq)
  
  # text labels and colours dependent on sex
  if (plot_gender == 'female') {
    gender_title <- 'Women'
    cent_labels <- cent |>
      filter(is_record == TRUE)
  } else {
    gender_title <- "Men"  
    cent_labels <- cent |>
      filter(is_record == TRUE) |>
      arrange(age)
    cent_labels$end_date[2] <- cent_labels$end_date[2] - years(2) # manual shift of one label to prevent overlap
  }
  gender_text = tolower(gender_title)
  
  # colours
  clr_background <- c('#FCF8EB')
  clr_text1 <- c('#302F2C')
  clr_text2 <- c('#4D4A45')
  clr_text3 <- c('#7A766D')
  clr_grid <- c('grey70')
  clr_lines <- c('#B0A99D')#c('#a9a397')
  
  # plot
  main_plot <- ggplot(data = cent) + 
    
    # custom x grid
    annotate(
      "text", 
      x = x_seq, 
      y = rep(ymd(min_birth_yr - grid_buff - 4, truncated = 2L), length(x_seq)) + years(x_seq), 
      label = paste(x_seq, 'y.o.'),
      size = 2,
      family = font_family,
      color = clr_text2
    ) + 
    geom_segment(
      data = tibble(
        x = x_seq,
        xend = x_seq,
        y = grid_y, 
        yend = grid_y_end
      ),
      mapping = aes(
        x = x, xend = xend, y = y, yend = yend
      ),
      colour = clr_grid,
      linetype = 3,
      linewidth = 0.2
    ) +
    
    # title
    geom_textbox(
      data = tibble(
        x = 0,
        y = ymd(y_max + y_buff, truncated = 2L),
        label = paste0(
          '<span style="font-size:22pt; color:', highlight_clr, ';">',
          'The ', gender_title, '<br>with the<br>Longest Lives<br></span>',
          '<span style="font-size:8pt;font-family:', font_family,';">',
          '<br>Each line represents the life of <br> one of the 100 oldest ', 
          gender_text, '<br> who has ever lived.</span>')
      ),
      aes(
        x = x,
        y = y,
        label = label),
      family = font_family_text,
      hjust = 0,
      vjust = 1,
      inherit.aes = FALSE,
      box.colour = NA, fill = NA,     
      box.padding = unit(rep(0, 4), 'pt'),
      width = unit(2.5, 'inch'),
      lineheight = 1,
      colour = clr_text1
    ) + 
    
    # plot line for each person
    geom_segment(
      mapping = aes(
        x = x_min, 
        xend = age, 
        y = birth_date %m+% years(x_min), # date at x min years old (0 in)
        yend = end_date,
        colour = factor(is_record):gender,
        linewidth = as.numeric(is_record)
        ),
      ) + 
    
    # plot points for record ages
    geom_point(data = cent |> filter(is_record == TRUE),
               mapping = aes(
                 x = age,
                 y = end_date
                 ),
               colour = highlight_clr,
               size = 0.5
               ) + 
    scale_linewidth(range = c(0.075,0.5)) +
    # axes
    # y (year)
    scale_y_date(
      limits = ymd(c(y_min - y_buff, y_max + y_buff), truncated = 2L),
      breaks = ymd(seq(y_min, y_max, by = y_step), truncated = 2L),
      date_labels = '%Y',
    ) +
    # x (age)
    scale_x_continuous(
      limits = c(x_min, x_max + x_buff),
      breaks = c() 
    ) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.1, linetype = 1),
      plot.background = element_rect(fill = c(clr_background), colour = 'white'),
      axis.ticks.length = unit(0.025, 'inch'), 
      axis.ticks.y = element_line(linewidth = 0.2),
      text = element_text(font_family, colour = clr_text1),
      axis.title.y = element_text(family = font_family_text, size = 10, 
                                  angle = 0, vjust = 0.025, margin = unit(c(0, -0.17, 0, 0), 'inch')),
      axis.text.y = element_text(colour = clr_text2, size = 6), 
      plot.margin = unit(c(0.1, 0.1, 0, 0.1), 'inch'),
      panel.background = element_blank()
    ) +
    labs(
      y = 'year',
      x = ''
    ) + 
    
    # additional annotations 
    # born
    annotate(
      'text',
      x = 0,
      y = ymd(max_birth_yr + grid_buff + 3, truncated = 2L),
      label = 'born',
      family = font_family_text,
      size = 2.5,
      vjust = 0,
      colour = clr_text2
    ) +
    # age (x-axis)
    annotate(
      'text',
      x = (x_max*1.2 + x_min)/2, # shift slightly towards max age
      y = ymd(y_min + (x_max + x_min)/2, truncated = 2L),
      label = 'age',
      family = font_family_text,
      size = 3.5,
      vjust = 1,
      colour = clr_text1
    ) +
    # record ages
    geom_text(
      data = cent_labels,
      mapping = aes(
        x = age,
        y = end_date,
        label = label
      ),
      hjust = 0,
      size = 1.7,
      family = font_family,
      nudge_x = 2,
      colour = highlight_clr
    ) +
    # annotation: highlighted lines    
    geom_textbox(
      data = tibble(
        x = x_max + x_buff,
        y = ymd(min_birth_yr + rev(x_seq)[3], truncated = 2L) - years(seq(0, 6*3, by = 6)),
        label = c(
          paste0(
            #'<span style="font-size:8pt;">',
            'The <b style="color:', highlight_clr,
            ';">highlighted lines</b>'),
          paste0(
            'are ', gender_text, ' who set record ages:'
          ),
          paste0('no other ', gender_text, ' had ever surpassed'),
          c('their age.')
        )
      ),
      mapping = aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      family = font_family,
      colour = clr_text2,
      size = 2.4,
      hjust = 1,
      vjust = 0,
      halign = 1,
      fill = NA,
      box.color = NA,
      lineheight = 1,
      width = unit(1.8, 'inch'),
      box.padding = unit(rep(0, 4), 'pt')
    ) +
    # author, data 
    geom_textbox(
      data = tibble(
        x = x_max + x_buff,
        y = ymd(y_min - y_buff, truncated = 2L),
        label = paste0(
          'Visualisation by <b>Gabrielle M. Schroeder</b> |',
          'Data: Wikipedia, "List of verified oldest people"<br>',
          'Only people with verified ages are shown.'
        )
      ),
      mapping = aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      family = font_family,
      colour = clr_text3,
      size = 2,
      hjust = 1,
      halign = 1,
      fill = NA,
      box.color = NA,
      lineheight = 1.2,
      width = unit(4, 'inch'),
      box.padding = unit(rep(0, 4), 'pt')
    ) + 
    coord_fixed(ratio = (1/(365.25*1.25)), clip = 'off') +
    theme(legend.position = 'none') +
    scale_colour_manual(values = c(clr_lines,highlight_clr))

}

# plots for men and women ----

#p_f = plot_life_lines(cent, plot_gender = 'female', highlight_clr = c('#125F66'))
#p_m = plot_life_lines(cent, plot_gender = 'male', highlight_clr = c('#750647'))

#plot_gender <- 'female'
plot_gender <- 'female'

if (plot_gender == 'male') {
  highlight_clr <- c('#750647')
} else {
  highlight_clr <- c('#125F66')
}

cent_plot <- plot_life_lines(cent, plot_gender = plot_gender, highlight_clr = highlight_clr)

#if (length(dev.list()) > 0) {dev.off()}
#dev.new(width = 7, height = 5, unit = 'in', noRStudioGD = TRUE)
fig_file <- file.path('plots', paste0('2023_wk22_', plot_gender,'_age.png'))
png(fig_file, height = 5, width = 5, units = "in", res = 600)
cent_plot
dev.off()

#ggsave(fig_file, height = 5, units = "in", dpi = 600)
