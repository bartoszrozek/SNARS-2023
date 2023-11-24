library(utf8)

load_file <- function(path_) {
    #this encoding is not working idk
    text <- readLines(path_, encoding = "win-1250") |>
        utf8_encode() |>
        trimws()
    
    return(text)
}

get_stops <- function(text) {
    start <- which(text == "#KD")
    stop <- which(text == "#ZA")
    stops_ids <- text[(start + 2):(stop - 1)] |>
        trimws() |>
        stringi::stri_replace_all_regex(pattern = "\\s{2,}", replacement = "|") |>
        stringi::stri_replace(fixed = ",", replacement = "")
    stops_df <- fread(
        text = stops_ids,
        sep = "|",
        col.names = c("id", "name", "code", "city")
    )[, c("code", "city") := NULL]
    
    start <- which(text == "#ZA")
    stop <- which(text == "#ZP")
    stops_text <- text[(start + 2):(stop - 2)]
    stops_idxs <-
        stringi::stri_detect(stops_text, regex = "^[0-9]{6}") |> which()
    stop_types <-
        stringi::stri_extract_all(stops_text[stops_idxs + 1], regex = "[L-NS]*\\d+") |>
        purrr::map_chr(\(numbers) set_stop_type(numbers))
    stops <-
        stringi::stri_replace_all_regex(stops_text[stops_idxs], 
                                        pattern = "\\s{2,}", replacement = "|") |>
        stringi::stri_replace_all(fixed = "Kier.: ", "") |>
        stringi::stri_replace_all(fixed = "Y= ", "") |>
        stringi::stri_replace_all(fixed = "X= ", "") |>
        na.omit()
    connections_df <- fread(
        text = stops,
        sep = "|",
        fill = T,
        dec = ".",
        col.names = c("id_full", "n", "st", "direction", "y", "x", "pu")
    )[, c("n", "st", "pu", "direction") := NULL]
    connections_df$stop_type <- stop_types
    #some artefacts in data
    suppressWarnings(connections_df[, `:=`(id = floor(id_full / 100),
                                           y = as.numeric(y),
                                           x = as.numeric(x))])
    
    stops_df <- merge(stops_df, connections_df, 
                      by = "id")
    stops_df[, `:=`(
        name = changeBuggedEncoding(name),
        id = NULL
    )]
    setnames(stops_df, "id_full", "id")
    stops_df <- stops_df |> 
        #make life simplier
        na.omit()
    return(stops_df)
}

get_connections <- function(text, stops_df = NULL) {
    if (is.null(stops_df)){
        stops_df <- get_stops(text)
    }
    courses_idx <-
        stringi::stri_detect(text, regex = "^\\s*[A-z]*-[A-z]*\\d*/[A-z]*/\\d*.\\d*") |> which()
    courses <- fread(
        text = text[courses_idx],
        fill = T,
        col.names = c("course_id", "stop_id", "day_type", "time", "stop_type"),
        dec = "."
    )
    courses[, time_from_mn := (floor(time) + (time %% 1) * 5 / 3) * 60]
    courses[, `:=`(
        next_stop_id = shift(stop_id, type = "lead"),
        travel_time = shift(time_from_mn, type = "lead") - time_from_mn
    ), by = course_id]
    #it is the final stop, no connection here
    courses[stop_type != "P"]
    courses[,stop_type := NULL]
    courses <- merge(courses, unique(stops_df[,.(id, stop_type)]), 
                     by.x = "stop_id", by.y = "id", all.x = T) |> 
        #make life simplier
        na.omit()
    return(courses)
}


changeBuggedEncoding <- function(string_) {
    string_ |>
        stringi::stri_replace_all(fixed = "\\xb9", "ą") |>
        stringi::stri_replace_all(fixed = "\\xb3", "ł") |>
        stringi::stri_replace_all(fixed = "\\x8c", "Ś") |>
        stringi::stri_replace_all(fixed = "\\x9c", "ś") |>
        stringi::stri_replace_all(fixed = "\\xf1", "ń") |>
        stringi::stri_replace_all(fixed = "\\xe6", "ć") |>
        stringi::stri_replace_all(fixed = "\\xea", "ę") |>
        stringi::stri_replace_all(fixed = "\\xa3", "Ł") |>
        stringi::stri_replace_all(fixed = "\\xf3", "ó") |>
        stringi::stri_replace_all(fixed = "\\xaf", "Ż") |>
        stringi::stri_replace_all(fixed = "\\xbf", "ż")
}

set_stop_type <- function(lines_numbers) {
    if (length(lines_numbers) == 1) {
        return(NA)
    }
    lines_numbers <- lines_numbers[-1]
    if (any(stringi::stri_detect(lines_numbers, regex = "[LN]"))) {
        return("A")
    }
    if (any(stringi::stri_detect(lines_numbers, regex = "[S]"))) {
        return("K")
    }
    if (max(as.numeric(lines_numbers)) > 99) {
        return("A")
    }
    return ("T")
}