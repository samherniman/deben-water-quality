convert_to_csv <- function(x, output_dir = "data/working/csv") {
  df <- readxl::read_excel(
    x,
    skip = 4,
    col_names = FALSE
  )
  names(df) <- c(
  "Location_Description",
  "Sample_Date_and_Time",
  "what3words_location",
  "Approx_River_Depth_at_Sample_Point_cm",
  "Sampling_Depth_cm_estimated",
  "Weather_Conditions",
  "River_Flow",
  "River_state",
  "E_coli_count_Colonies_per_1ml",
  "Phosphate_PO4_mgpL",
  "Phosphate_P_mgpL",
  "Nitrate_ppm",
  "Ammonia_ppm"
  )
  out_path <- here::here(output_dir, fs::path_file(x) |> fs::path_ext_set("csv"))
  df |> 
    dplyr::filter(stringr::str_starts(Location_Description, "Location", negate = TRUE)) |> 
    dplyr::filter(!is.na(Location_Description)) |> 
    janitor::remove_empty(which = "rows", cutoff = 0.09) |> 
    dplyr::mutate(
      datasheet_date = fs::path_file(x) |> 
        fs::path_ext_remove() |> 
        stringr::str_extract("[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$") |> 
        lubridate::ymd()
    ) |> 
  vroom::vroom_write( 
    out_path,
    delim = ","
  )
  return(out_path)
}

is_month <- function(string, negate = FALSE) {
  stringr::str_extract(tolower(string), 
  pattern = paste(tolower(month.name), collapse = "|", sep = "|")
  # negate = negate
)
}

join_and_clean <- function(df, w3w_df) {
  join_simple <- df |> 
  dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, " ", ".")) |> 
  dplyr::mutate(what3words_location = stringr::str_replace_all(what3words_location, "\\.+", ".")) |> 
  dplyr::mutate(Location_Description = stringr::str_remove_all(Location_Description, "SP")) |> 
  dplyr::mutate(
    Location_Description = stringr::str_replace_all(Location_Description, "STW", "sewage treatment works"),
    Location_Description = stringr::str_replace_all(Location_Description, "Debenham11", "Debenham 11"),
    Location_Description = dplyr::case_when(
      Location_Description == "12 Debenham" ~ "Debenham 12",
      .default = Location_Description
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

df <- dplyr::left_join(
  x = join_simple,
  y = w3w_df,
  by = c("what3words_location" = "value"),
  relationship = "many-to-many"
) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    non_standard_month = is_month(Sample_Date_and_Time),
    non_standard_day = stringr::str_extract(Sample_Date_and_Time, "[:digit:]{1,2}(st|nd|rd|th)"),
    datasheet_date = lubridate::ymd(datasheet_date),
    # date logic is sound by time is not yet finished
    # time = stringr::str_extract(Sample_Date_and_Time, "[:digit:]{1,2}[:punct:]*[:digit:]{1,2}[:punct:]*[am|pm]*"),
    # tm_lb = lubridate::hm(time),
    date_actual = paste(
      lubridate::year(datasheet_date),
      dplyr::first(c(non_standard_month, lubridate::month(datasheet_date)), na_rm = TRUE),
      dplyr::first(c(non_standard_day, lubridate::day(datasheet_date)), na_rm = TRUE),
      sep = "-"
     ) |> lubridate::ymd()
    
  ) |> 
  dplyr::select(-c(non_standard_month, non_standard_day, datasheet_date, Location_geocode, loc_match))
  
  return(df)
}

