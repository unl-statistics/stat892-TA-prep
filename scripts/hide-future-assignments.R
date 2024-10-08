# From https://github.com/stat20/stat20/blob/main/assets/scripts/ignore-future-docs.r

# read in course settings
course_settings <- yaml::read_yaml("_schedule.yml")

# extract auto-publish parameters
timezone <- course_settings$`auto-publish`$timezone
publish_days_before <- course_settings$`auto-publish`$`publish-days-before`
live_as_of <- course_settings$`auto-publish`$`live-as-of`

# Set up weeks to map to dates.
week_mapping <- course_settings$calendar |>
  purrr::map_df(as.data.frame) |>
  dplyr::mutate(date = lubridate::ymd(date, tz = timezone),
                week_start = date,
                week_end = date + lubridate::days(7)) |>
  dplyr::select(-date)


publish_cutoff <- if (live_as_of == "Sys.time()") {
  lubridate::with_tz(time = eval(parse(text = live_as_of)),
                     tz = timezone)
} else {
  lubridate::ymd(live_as_of, tz = timezone)
}

weeks_to_publish <- dplyr::filter(
  week_mapping,
  week_start <= publish_cutoff + lubridate::days(publish_days_before)
)
weeks_to_exclude <- dplyr::anti_join(week_mapping, weeks_to_publish)

# collect list of all materials
materials_list <- purrr::map(course_settings$schedule, "materials") |>
  purrr::list_flatten()

# Get only things that have a path field
remote_files_list <- purrr::keep(materials_list, ~"path" %in% names(.))
local_files_list <- purrr::keep(materials_list, ~"href" %in% names(.))

include_files <- include_links <- tibble::tibble()

# Deal with assignments and things that have an href field
# instead of a path field
if (length(local_files_list) > 0) {

  local_files_df <- local_files_list |>
    purrr::map_df(as.data.frame)

  if (!"pub.date" %in% names(local_files_df)) {
    materials$pub.date <- NA
  }

  local_files_df <- local_files_df |>
    tidyr::nest(categories = categories) |>
    dplyr::filter(!is.na(href)) |>
    dplyr::arrange(href) |>
    dplyr::mutate_at(
      .vars = c("date", "pub.date"),
      lubridate::ymd, tz=timezone
    ) |>
    dplyr::mutate(
      week_start = lubridate::floor_date(date, "week", week_start = 7)
    ) |>
    dplyr::left_join(week_mapping) |>
    dplyr::mutate(
      dir = dirname(href),
      file = basename(href),
      pub.date = dplyr::if_else(is.na(pub.date),
                         date - lubridate::days(publish_days_before),
                         pub.date) |> lubridate::as_datetime(tz = timezone),
      is_live = (pub.date <= publish_cutoff) & !is.na(href),
      file_date = lubridate::ymd(date, tz = timezone),
      categories = purrr::map2(categories, name, ~c(as.character(unlist(.x)), .y))
    )

  include_files <- dplyr::bind_rows(include_files, dplyr::filter(local_files_df, is_live))
}

message("Materials assembled")

## This writes out yaml for things that don't have href fields
if (length(remote_files_list) > 0) {
  remote_files_df <- remote_files_list |>
    purrr::map_df(as.data.frame) |>
    dplyr::mutate_at(
      .vars = c("date", "pub.date"),
      lubridate::ymd, tz=timezone
    ) |>
    dplyr::mutate(
      week_start = lubridate::floor_date(date, "week", week_start = 7)
    ) |>
    dplyr::left_join(week_mapping) |>
    dplyr::mutate(
      pub.date = ifelse(is.na(pub.date),
                        date - lubridate::days(publish_days_before),
                        pub.date) |> lubridate::as_datetime(tz = timezone),
      is_live = (pub.date <= publish_cutoff) & !is.na(path),
      file_date = lubridate::ymd(date, tz = timezone),
      type = categories,
      categories = purrr::map2(categories, name, ~c(as.character(unlist(.x)), .y))
    ) |>
    dplyr::mutate(image = sprintf("figs/%s.svg", tolower(type))) |>
    dplyr::select(path, title, date, pub.date, categories, type, image, is_live)

  message("Reading assembled")

  include_links <- remote_files_df |>
    dplyr::filter(is_live)

  include_links <- include_links |>
    split(1:nrow(include_links)) |>
    purrr::map(as.list)

  include_links <- purrr::modify(include_links,
                                 ~purrr::modify_in(., "categories", unlist))


}

names(include_links) <- NULL
if (length(include_links) > 0) {
  yaml::write_yaml(include_links, "reading.yml", handlers = yml_handlers)
} else {
  writeLines("", con = "reading.yml")
}

source("scripts/change-yml.R")
message("Sourced change-yml.R")

listing_includes <- c()
if (length(include_files$href) > 0) {
  listing_includes <- c(listing_includes, include_files$href)
}
if (length(include_links) > 0) {
  listing_includes <- c(listing_includes, include_links)
}

message("listing yml files created")
# Write out exclude files for homeworks and assignments that aren't yet relevant
idx_lines <- readLines("index.qmd")
yaml_lines <- which(idx_lines == "---")
index_yml <- yaml::read_yaml(text = idx_lines[min(yaml_lines):max(yaml_lines)])
index_yml$listing[[1]]$contents = listing_includes
index_yml$input_file <- "index.qmd"
index_yml$output_file <- "index.qmd"


# Exclude unpublished stuff from the listing by updating the yaml
do.call("change_yaml_matter", index_yml)

message("Updating yaml in index.qmd")

# This bit renames files with _ in front to actually prevent them from rendering
# This sometimes screws with quarto's render process, especially anytime
# something changes.

# renamed_files <- materials |>
#   dplyr::mutate(
#     show_file = stringr::str_remove(file, "^_"),
#     hide_file = paste0("_", show_file),
#     is_visible = file.exists(file.path(dir, show_file)),
#     current_path = ifelse(is_visible, file.path(dir, show_file), file.path(dir, hide_file)),
#     rename_path = ifelse(is_live, file.path(dir, show_file), file.path(dir, hide_file))
#   )
#   dplyr::filter(is_live != is_visible) |>
#   dplyr::mutate(renamed = purrr::map2_lgl(current_path, rename_path, file.rename))
#
# if (nrow(renamed_files) > 0) {
#   cli::cli_alert_success("The following files have publish dates after {publish_cutoff} and therefore are ignored: {renamed_files$rename_path}")
# } else {
#   cli::cli_alert_info("All files have published dates in the past. No files will be ignored.")
# }

