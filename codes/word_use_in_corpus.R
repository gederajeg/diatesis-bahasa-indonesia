library(tidyverse)

freqlist_all <- read_rds("/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/My Drive/2022-07-30-tatabahasa-indonesia-kontemporer/freqlist_all.rds")

genres_files <- "/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/My Drive/2022-07-30-tatabahasa-indonesia-kontemporer/genres_file_names_save.txt"

gdrive_path <- "/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/My Drive/2022-07-30-tatabahasa-indonesia-kontemporer/"

genres_file_names_path <- read_lines(genres_files)
genres_file_names_path <- paste(gdrive_path, genres_file_names_path, sep = "")

words_use_in_corpus <- function(freqlist_df = freqlist_all, 
                                rgx = NULL, 
                                case_insensitive = TRUE, 
                                context_char = 50, 
                                to_lower_conc = FALSE) {
  genres_file_names_save <- read_lines(genres_files)
  genres_file_names_save <- paste(gdrive_path, genres_file_names_save, sep = "")
  rgx <- paste("\\b", rgx, "\\b", sep = "")
  corpus_genres <- freqlist_df |> 
    map(~filter(., str_detect(word_form, regex(rgx, ignore_case = case_insensitive)))) |> 
    map_dbl(~dim(.)[1])
  if (all(corpus_genres == 0)) {
    message("Sorry, the pattern is not attested in the whole corpus!")
  } else {
    corpus_genres_id <- which(corpus_genres >= 1)
    # read the corpus into list
    corpsall <- map_df(genres_file_names_save[corpus_genres_id], read_rds)
    usages <- corpsall |> 
      filter(str_detect(bodytext, regex(rgx, ignore_case = case_insensitive))) # |> 
    # mutate(usages = paste(genre, "___", bodytext, sep = "")) |> 
    # pull(usages)
    genres <- usages$genre |> 
      unique() |> 
      str_replace("^._", "")
    usages_conc <- usages |> 
      split(usages$genre) |> 
      map(~concord_others(.$bodytext, 
                          pattern = rgx, 
                          case_insensitive = case_insensitive, 
                          context_char = context_char, 
                          to_lower_corpus = to_lower_conc)) |> 
      map2_df(.y = genres, ~mutate(., GENRE = .y))
    return(usages_conc[sample(1:nrow(usages_conc)), ])
  }
}

concord_tbik <- function(rgx = NULL,
                         case_insensitive = TRUE, 
                         context_char = 50, 
                         to_lower_conc = FALSE) {
  corpsall <- map_df(genres_file_names_path, read_rds)
  usages <- corpsall |> 
    filter(str_detect(bodytext, regex(rgx, ignore_case = case_insensitive)))
  genres <- usages$genre |> 
    unique() |> 
    str_replace("^._", "")
  usages_conc <- usages |> 
    split(usages$genre) |> 
    map(~concord_others(.$bodytext, 
                        pattern = rgx, 
                        case_insensitive = case_insensitive, 
                        context_char = context_char, 
                        to_lower_corpus = to_lower_conc)) |> 
    map2_df(.y = genres, ~mutate(., GENRE = .y))
  return(usages_conc[sample(1:nrow(usages_conc)), ])
}
