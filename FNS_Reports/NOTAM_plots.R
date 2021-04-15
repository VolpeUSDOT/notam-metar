regions <- c('Western', 'Central', 'Eastern')
colors <- c('#fb8072', '#80b1d3', "#b3de69")
c <- colors[2]
seasons <- unique(ninetieth_day_seasons$season)

for (i in 1:length(regions)) {
  day <- ninetieth_days$date[i]
  r <- regions[1:i]
  # c <- colors[i]
  region_set_name = ifelse(length(r) > 1,
                           paste(r, collapse = ' + '), r)
  region_set_short <- paste(substr(r, 1, 1), collapse="")
  plural = ifelse(length(r) > 1, "s", "")
  
  # plot histograms of NOTAM counts by day
  daily_counts <- orig_dt_hr %>%
    filter(Region %in% r) %>%
    group_by(date) %>%
    summarize(daily_count = sum(hourly_count))
  
  ninetieth_value <- quantile(daily_counts$daily_count, probs = .9)
  
  gd <- ggplot(daily_counts, aes(x=daily_count)) +
    geom_histogram(fill = c, colour = 'black', size = .5, binwidth = 25)
    
  hist_max <- max(ggplot_build(gd)$data[[1]]$y)
    
  gd <- gd +
    geom_vline(aes(xintercept = ninetieth_value)) +
    geom_label(mapping = aes(ninetieth_value, hist_max,
                            label = round(ninetieth_value, 0)),
                size = 3) +
    xlab("Count of NOTAMs") + ylab("Count of days") +
    labs(title = "Histograms of NOTAM counts, by day",
         subtitle = paste0(region_set_name, ' Service Area', plural))
    
  ggsave(filename = file.path(output_dir, paste0(region_set_short,
                                                 '_daily_histogram.png')),
         plot = gd,
         width = 6, height = 6)
  
  # # plot demand for 90th percentile day in WSA, WSA/CSA & full NAS ----
  # 
  # dt_busy_day <- get_notams_for_day(day, r)
  # for_melting <- dt_busy_day %>%
  #   mutate(hour = minutes_elapsed / minutes_in_hour, .before = 1) %>%
  #   select(hour, incoming_notams, cumulative_notams)
  # melted_model <- melt(for_melting, id="hour")
  # 
  # p <- ggplot(data = melted_model) + xlab("Zulu time (hours)")
  # # plot cumulative NOTAMs for the day
  # pl <- p +
  #   geom_step(data = melted_model %>% filter(variable == "cumulative_notams"),
  #             colour = c,
  #             aes(hour, value, colour = variable),
  #             size = 1.25) +
  #   ylab("Cumulative NOTAMs") +
  #   labs(title = paste0(region_set_name, ' Service Area', plural))
  # # plot incoming NOTAMs per hour
  # pr <- p + aes(x = floor(hour)) +
  #   geom_bar(data = melted_model %>% filter(variable == "incoming_notams"),
  #            colour = '#111111',
  #            fill = c,
  #            size = .5) +
  #   xlab("Zulu time (hours)") +
  #   ylab("NOTAMs submitted each hour")
  # 
  # ga <- egg::ggarrange(pl,
  #                      pr,
  #                      ncol = 2)
  # ggsave(filename = file.path(output_dir, paste0(region_set_short, '_ninetieth_',
  #                                                day, '_',
  #                                                'demand.png')),
  #        plot = ga,
  #        width = 6, height = 3)
  # 
  # # plot demand for same as above but by season ----
  # cumulative_seasons <- tibble()
  # per_hour_seasons <- tibble()
  # ninetieth_day_seasons_NAS <- ninetieth_day_seasons %>%
  #   filter(service_areas == concat_service_acronyms(regions, i))
  # 
  # for (i in 1:length(seasons)) {
  #   day <- ninetieth_day_seasons_NAS$date[i]
  #   s <- seasons[i]
  #   dt_busy_day <- get_notams_for_day(day, r)
  #   dt_busy_day <- dt_busy_day %>% mutate(hour = minutes_elapsed / 60)
  #   dt_busy_day$season <- s
  #   cumulative_seasons <- rbind(cumulative_seasons,
  #                               dt_busy_day %>% select("season",
  #                                                      "hour",
  #                                                      "cumulative_notams")
  #                               )
  #   per_hour_seasons <- rbind(per_hour_seasons,
  #                             dt_busy_day %>% select("season",
  #                                                    "hour",
  #                                                    "incoming_notams")
  #   )
  # }
  # 
  # gc <- ggplot(data = cumulative_seasons) + 
  #         facet_wrap(~season) +
  #         geom_step(colour = c,
  #                   aes(hour, cumulative_notams, colour = variable),
  #                   size = 1.25) +
  #         xlab("Zulu time (hours)") +
  #         ylab("Cumulative NOTAMs") +
  #   labs(title = paste0(region_set_name, ' Service Area', plural))
  # 
  # gi <- ggplot(data = per_hour_seasons) + 
  #         facet_wrap(~season) +
  #         geom_bar(fill = c,
  #                  colour = '#111111',
  #                  aes(floor(hour)),
  #                  size = .5) +
  #         xlab("Zulu time (hours)") +
  #         ylab("NOTAMs submitted each hour") +
  #   labs(title = paste0(region_set_name, ' Service Area', plural))
  # 
  # ggsave(filename = file.path(output_dir, paste0(region_set_short, '_ninetieth_',
  #                                                'season_demand_cumulative.png')),
  #        plot = gc,
  #        width = 6, height = 6)
  # 
  # ggsave(filename = file.path(output_dir, paste0(region_set_short, '_ninetieth_',
  #                                                'season_demand_by_hour.png')),
  #        plot = gi,
  #        width = 6, height = 6)
}


