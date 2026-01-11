source(here::here("fxns.R"))

# Combine all excel sheets into one --------------------------------------
# df <-
#   fs::dir_ls(
#     here::here("data/raw"),
#     regexp = "xlsx"
#   ) |>
#   lapply(convert_to_csv) |>
#   lapply(data.table::fread)

# df <- do.call(rbind, df)
# vroom::vroom_write(
#   df,
#   here::here("data/working/working.csv"),
#   append = FALSE,
#   delim = ","
# )

# Find unique w3w addresses ----------------------------------------------

# tbl <- unique(df$what3words_location) |> tibble::as_tibble()

# vroom::vroom_write(tbl, here::here("data/working/unique_w3w.csv"))

# figure out location areas ----------------------------------------------

# join_and_clean_area <- function(df, w3w_df) {
#   join_simple <- df |>
#     # dplyr::rename(Location_Description = location, what3words_location = w3w) |>
#   dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, " ", ".")) |>
#   dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, "\\.+", ".")) |>
#   dplyr::mutate(Location_Description = stringr::str_remove_all(Location_Description, "SP")) |>
#   dplyr::mutate(
# # Need to confirm that the "top 0 next 5" thing is poperly encoded as just 5
#     # Nitrate_ppm = stringr::str_extract(Nitrate_ppm, "[:digit:]+$") |> as.numeric(),
#     # E_coli_count_Colonies_per_1ml = stringr::str_extract(E_coli_count_Colonies_per_1ml, "[:digit:]+$") |> as.numeric(),
#     Location_Description = stringr::str_replace_all(Location_Description, "STW", "sewage treatment works"),
#     Location_Description = stringr::str_replace_all(Location_Description, "Debenham11", "Debenham 11"),
#     Location_Description = dplyr::case_when(
#       Location_Description == "12 Debenham" ~ "Debenham 12",
#       .default = Location_Description
#     ),
#     what3words_location = dplyr::case_when(
#       # Location_Description == "12 Debenham" ~ "costs.trust.preclude",
#       stringr::str_starts(Location_Description, "Debenham 12") ~ "costs.trusts.precluded",
#       # pretty.needed.chill is the default location for the UK. So we need to fix Debenham 13 which has that
#       stringr::str_starts(Location_Description, "Debenham 13") ~ "trickled.magnum.slurping",
#       stringr::str_starts(Location_Description, "Kirton Sluice") ~ "wobbles.birdcage.soon",
#       stringr::str_detect(Location_Description, "Potsford Brook Charfield Upstream of sewage treatment works") ~ "that.consonant.tango",
#       stringr::str_detect(Location_Description, "Debenham 13 Downstream of sewage treatment works outfall") ~ "pretty.needed.chill",
#       stringr::str_detect(what3words_location, "worm.oozed.zone") ~ "worm.ooze.zoned",
#       stringr::str_detect(what3words_location, "ruins.outward.crackled") ~ "trooper.poodle.cleanser",
#       .default = what3words_location
#     ),
#     Location_Description = stringr::str_replace_all(Location_Description, "([:space:]{2,})", " "),
#     Location_Description = stringr::str_remove_all(Location_Description, "\\(.*\\)"),
#     # Location_geocode = stringr::str_remove_all(Location_Description, "Debenham[:space:]*[:digit:]{1,2}\\.*[:space:]*"),
#     loc_match = stringr::str_remove_all(Location_Description, "[:punct:]")
#   ) |>
#   dplyr::group_by(loc_match) |>
#     tidyr::fill(what3words_location, .direction = "downup") |>
#   dplyr::ungroup()

# df <- dplyr::left_join(
#   x = join_simple,
#   y = w3w_df,
#   by = c("what3words_location" = "value"),
#   relationship = "many-to-many"
# ) |>
#   dplyr::rowwise()

#   return(df)
# }

# w3w_df <- here::here("Deben_c2c.csv") |> vroom::vroom()
# a_df <- here::here("data/working/location_areas.csv") |> vroom::vroom()
# df_area <- join_and_clean_area(a_df, w3w_df) |>
#   tidyr::drop_na() |>
#     sf::st_as_sf(
#       coords = c("x", "y"),
#       crs = sf::st_crs(4326)
#     )
#   mapview::mapview(df_area, zcol = "Category")

# sf::st_write(
#   df_area,
#   here::here("data/working/location_areas.fgb"),
# append = FALSE
# )

# join w3w with coords ---------------------------------------------------

df <- here::here("data/working/working.csv") |> vroom::vroom()

w3w_df <- here::here("Deben_c2c.csv") |> vroom::vroom()

df <- join_and_clean(df, w3w_df)

df_sf <-
  df |>
  tidyr::drop_na(x:y) |>
  sf::st_as_sf(
    coords = c("x", "y"),
    crs = sf::st_crs(4326)
  )
# mapview::mapview(df_sf)

# spatial join df with survey areas --------------------------------------

survey_areas_sf <- sf::st_read(here::here(
  "data/working/qgis/survey_areas.fgb"
)) |>
  sf::st_make_valid()

# mapview::mapview(survey_areas_sf)

df_sf <- sf::st_join(
  df_sf,
  survey_areas_sf
) |>
  dplyr::mutate(Area_name = stringr::str_replace(areaname, "Tiday", "Tidal")) |>
  dplyr::select(-c(id, areaname))
# mapview::mapview(df_sf, zcol = "Area_name")

# library(ggplot2)
# ggplot(
#   data = df_sf,
#   # aes(x = "x", y = "y")
# ) +
#   geom_sf(aes(color = E_coli_count_Colonies_per_1ml))

# df_sf |>
#   # dplyr::filter(
#   #   # Location_Description == "Woodbridge Tide Mill"
#   #   Area_name == "River Fynn and River Lark"
#   # ) |>
#   ggplot(
#     aes(x = date_actual, y = E_coli_count_Colonies_per_1ml)
#   ) +
#   geom_smooth(se = FALSE, color = "grey50", alpha = 0.6) +
#   labs(y = "E.coli Colonies per 1ml", x = "Date") +
#   theme(
#     panel.background = element_rect(fill = "grey98"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "none"
#   ) +
#   # geom_point(
#   geom_jitter(
#     # aes(color = E_coli_count_Colonies_per_1ml),
#     aes(color = Area_name),
#     size = 4,
#     width = 10
#   ) +
#   scale_y_log10() +
#   scico::scale_color_scico_d() +
#   # scale_color_viridis_c(option = "mako")

#   facet_wrap(vars(Area_name))
# dish_tb <- tibble::tibble(
#   x = seq(1, 100),
#   y = runif(100, 10, 100),
#   size = runif(100, 2, 6)
# )

# ggplot(dish_tb) +
#   geom_point(
#     aes(x, y, size = size),
#     color = "#688aed",
#     alpha = 0.8
#   ) +
#   coord_polar() +
#   theme(
#     panel.background = element_rect(fill = "grey98"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "none"
#   )
