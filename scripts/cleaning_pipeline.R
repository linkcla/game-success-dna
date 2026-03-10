# ¡¡ RESULTADO EN 'df_final' !!

library(lubridate)
library(stringr)


df_raw <- read_csv("./../data/games_march2025_cleaned.csv")
df_info_reviews <- read_csv("./../data/steam_reviews_summary.csv")
df_txt_raw <- read_csv("./../data/information_extracted_from_txt_cols.csv")
df_sum_tags <- read_csv("./../data/sum_tags.csv")

# Convertim la variable 'release_date' a 4 variables:
#   - 'year'
#   - 'month'
#   - 'week_day'
#   - 'days_since_release' contant a partir del 01/04/2025

# Eliminam 'reviews' perquè emprarem les d'en Toni d'un altre dataset

# Eliminam 'header_image', 'metacritic_url' perquè no serveix

# Eliminam 'website', 'support_url' i 'support_email' perquè les transformam a
# variables booleans que ens diuen si tenen aquest element o no.

# Eliminam 'metacritic_score' i 'user_score' perquè farem scraping nosaltres.

# Eliminam 'screenshots' i 'movies' perquè les tranformam a columnes que ens
# diuen la quantitat de cada element.

# Eliminam 'score_rank' perquè quasi tot són NA

# Eliminam 'positive' i 'negative' perquè hem de treure les mètriques de les
# reviews que empra en Toni.

# Eliminam 'peak_ccu' perquè mesura els màxims d'usuaris concurrents del dia
# anterior a l'scraping.

# Eliminam 'detailed_description', 'about_the_game', 'short_description', 
# 'notes', 'tags' i 'categories' ja que mitjançant tècniques de Text Mining ja 
# hem extret l'informació


removing_cols <- c(
  "min_owners", "max_owners", "release_date", "reviews", "header_image",
  "website", "support_url", "support_email", "metacritic_score", "metacritic_url",
  "screenshots", "movies", "user_score", "score_rank", "positive", "negative", 
  "peak_ccu", "pct_pos_total", "num_reviews_total", "pct_pos_recent", 
  "num_reviews_recent", "detailed_description", "about_the_game",
  "short_description", "notes", "tags", "categories"
)

df_clean <- df_raw |>
  filter(
    # Eliminar els jocs que el 50% dels usuaris que el tenen no l'han jugat suficient (al menys 30 minuts)
    median_playtime_forever >= 30 &
      num_reviews_total > 0 &
      !appid %in% c("1449560", "6475", "404730")
  ) |>
  # Conversió del rang en text a la mitjana del rang. PART 1
  separate(estimated_owners, into = c("min_owners", "max_owners"), sep = " - ", convert = TRUE) |>
  mutate(
    # Conversió del rang en text a la mitjana del rang. PART 2
    owners_avg = (min_owners + max_owners) / 2,
    # Conversió a data
    release_date = ymd(release_date),
    year = year(release_date),
    month = month(release_date, label = TRUE, abbr = FALSE, locale = "en_US"),
    week_day = wday(release_date, label = TRUE, week_start = 1, abbr = FALSE, locale = "en_US"),
    days_since_release = as.numeric(dmy("01/04/2025") - release_date),
    across(
      c(required_age, pct_pos_total, pct_pos_recent, num_reviews_recent),
      ~ if_else(. == -1, 0, .)
    ),
    has_website = !is.na(website),
    has_support_url = !is.na(support_url),
    has_support_email = !is.na(support_email),
    supported_languages = str_extract_all(supported_languages, "(?<=')(?!, )[^']+(?=')"),
    full_audio_languages = str_extract_all(full_audio_languages, "(?<=')(?!, )[^']+(?=')"),

    # Convertim a llista
    developers = str_remove_all(developers, "^\\[|\\]$"),
    publishers = str_remove_all(publishers, "^\\[|\\]$"),
    developers = str_replace_all(developers, "', '", ";"),
    publishers = str_replace_all(publishers, "', '", ";"),
    developers = str_remove_all(developers, "^'|'$"),
    publishers = str_remove_all(publishers, "^'|'$"),
    # Si no té publisher, li assignam el developer com publisher
    publishers = if_else(publishers == "", developers, publishers),
    developers = str_split(developers, "\\s*;\\s*"),
    publishers = str_split(publishers, "\\s*;\\s*"),
    genres = str_extract_all(genres, "(?<=')(?!, )[^']+(?=')"),
    screenshots_count = if_else(screenshots == "[]", 0, str_count(screenshots, ",") + 1),
    movies_count = if_else(movies == "[]", 0, str_count(movies, ",") + 1),
  ) |>
  select(-all_of(removing_cols)) |>
  merge(df_info_reviews, by = "appid", all.x = TRUE)

source("./extract_information_from_packages_col.R")

removing_cols <- c("short_len", "about_len", "detail_len", "has_website", 
                   "has_support_url", "has_support_email", "has_header_image", 
                   "support_quality","owners_mean", "is_multiplatform", 
                   "is_free", "has_screenshots", "has_videos", "developer_freq",
                   "publisher_freq", "tfidf_short_norm", "tfidf_about_norm", 
                   "tfidf_detail_norm", "tfidf_short_terms", "tfidf_about_terms", 
                   "tfidf_detail_terms", "tfidf_short_max", "tfidf_about_max", 
                   "tfidf_detail_max", "tfidf_short_mean", "tfidf_about_mean", 
                   "tfidf_detail_mean", "svd_1", "svd_2", "svd_3", "cluster", 
                   "top_terms_short_txt", "top_terms_detail_txt")

df_txt_clean <- df_txt_raw|> 
  select(-all_of(removing_cols)) |>
  mutate(
    across(
      c(has_violence, has_gore, has_nudity_sexual, has_substances, has_substances,
        has_language, has_horror, has_horror, has_self_harm, has_gambling,
        has_adult_general),
      ~ as.logical(.)
    )
  )

df_final <- merge(df_final, df_txt_clean, by = "appid", all.x = TRUE)
df_sum_tags$name <- NULL
df_final <- merge(df_final, df_sum_tags, by = "appid", all.x = TRUE)