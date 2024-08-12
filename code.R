source(here::here("fxns.R"))

df <- 
  fs::dir_ls(
    here::here("data/raw"),
    regexp = "xlsx"
  ) |>  
lapply(xl_paths, convert_to_csv) |> 
lapply(data.table::fread)
df <- do.call(rbind, df)
