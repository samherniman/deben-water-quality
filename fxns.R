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

