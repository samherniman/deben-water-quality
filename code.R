source(here::here("fxns.R"))

# Combine all excel sheets into one --------------------------------------
df <- 
  fs::dir_ls(
    here::here("data/raw"),
    regexp = "xlsx"
  ) |>  
lapply(convert_to_csv) |> 
lapply(data.table::fread)

df <- do.call(rbind, df)
vroom::vroom_write(
  df, 
  here::here("data/working/working.csv"), 
  append = FALSE,
  delim = ","
)

# Find unique w3w addresses ----------------------------------------------


# tbl <- unique(df$what3words_location) |> tibble::as_tibble()

# vroom::vroom_write(tbl, here::here("data/working/unique_w3w.csv"))


# join w3w with coords ---------------------------------------------------

w3w_df <- here::here("Deben_c2c.csv") |> vroom::vroom()
df <- df |> 
  dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, " ", ".")) |> 
  dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, "\\.+", "."))
# df$what3words_location


join_simple <- df |> 
  # dplyr::select(Location_Description, what3words_location, x, y) |> 
  
  dplyr::mutate(Location_Description = stringr::str_remove_all(Location_Description, "SP")) |> 
  dplyr::mutate(
    Location_Description = stringr::str_replace_all(Location_Description, "STW", "sewage treatment works"),
    Location_Description = stringr::str_replace_all(Location_Description, "Debenham11", "Debenham 11"),
    Location_Description = dplyr::case_when(
      Location_Description == "12 Debenham" ~ "Debenham 12",
      # Location_Description == "Debenham11" ~ "Debenham 11",
      .default = Location_Description
      # stringr::str_detect(Location_Description, "12[:punct:]*Debenham") ~ "Debenham 12"
    ),
    what3words_location = dplyr::case_when(
      # Location_Description == "12 Debenham" ~ "costs.trust.preclude",
      stringr::str_starts(Location_Description, "Debenham 12") ~ "costs.trusts.precluded",
      # pretty.needed.chill is the default location for the UK. So we need to fix Debenham 13 which has that
      stringr::str_starts(Location_Description, "Debenham 13") ~ "trickled.magnum.slurping",
      stringr::str_starts(Location_Description, "Kirton Sluice") ~ "wobbles.birdcage.soon",
      stringr::str_detect(Location_Description, "Potsford Brook Charfield Upstream of sewage treatment works") ~ "that.consonant.tango",
      stringr::str_detect(Location_Description, "Debenham 13 Downstream of sewage treatment works outfall") ~ "pretty.needed.chill",
      stringr::str_detect(what3words_location, "worm.oozed.zone") ~ "worm.ooze.zoned",
      stringr::str_detect(what3words_location, "ruins.outward.crackled") ~ "trooper.poodle.cleanser",
      .default = what3words_location
    ),
    Location_Description = stringr::str_replace_all(Location_Description, "([:space:]{2,})", " "),
    Location_Description = stringr::str_remove_all(Location_Description, "\\(.*\\)"),
    Location_geocode = stringr::str_remove_all(Location_Description, "Debenham[:space:]*[:digit:]{1,2}\\.*[:space:]*"),
    loc_match = stringr::str_remove_all(Location_Description, "[:punct:]")

  ) |> 
  dplyr::group_by(loc_match) |> 
    tidyr::fill(what3words_location, .direction = "downup") |> 
dplyr::ungroup()
  # dplyr::filter(is.na(what3words_location))
  
  # tidygeocoder::geocode(Location_Description, method = 'osm', lat = latitude , long = longitude)


# 12 debenham = debenham 12 = Debenham SP12 
# split on semicolon

join_df <- dplyr::left_join(
  x = join_simple,
  y = w3w_df,
  by = c("what3words_location" = "value"),
  relationship = "many-to-many"
) |> 
  dplyr::select(Location_Description, what3words_location, x, y)
  # dplyr::filter(is.na(x))


# 13