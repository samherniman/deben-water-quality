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

df <- join_and_clean(df, w3w_df)


