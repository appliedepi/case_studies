pacman::p_load(
  rio,
  gert,
  stringr,
  babeldown,
  fs,
  here
)

# # NEW LANGUAGE TRANSLATION PROTOCOL ------------------------------------------------------------------------

# # 0. Create a new branch for a new language

# # 1. First install the babeldown package
# #install.packages('babeldown', repos = c('https://ropensci.r-universe.dev', 'https://cloud.r-project.org'))

# # 2. Register new language in _quarto.yml
# # From a book whose main language is English, register a new language in the Quarto configuration _quarto.yml
# babelquarto::register_main_language(main_language = "en")
# babelquarto::register_further_languages(further_languages = "de")
# # https://docs.ropensci.org/babeldown/articles/quarto.html

chapter_list = c("fulton.qmd", "r_practical.qmd", "tbe.qmd")

# # Function to copy and rename files
# copy_and_rename_files <- function(chapters, language_tag) {
#   for (chapter in chapters) {
#     source_file <- file.path("pages", chapter)
#     target_file <- file.path("pages", gsub("\\.qmd$", paste0(".", language_tag, ".qmd"), chapter))
#     if (file_exists(source_file)) {
#       file_copy(source_file, target_file)
#       message(paste("Copied and renamed:", source_file, "to", target_file))
#     } else {
#       message(paste("Source file does not exist:", source_file))
#     }
#   }
# }
# 
# # Copy and rename files with the specified language tag "es"
# copy_and_rename_files(chapter_list, "es")

# # 3. Translate the book
# # babeldown helps you translate each book chapter. It will translate chapters one by one with babeldown before having a human review the translation.

# 1 Detect chapters changed in the book
readRenviron(".env")
DEEPL_API_URL <- Sys.getenv("DEEPL_API_URL")
DEEPL_API_KEY <- Sys.getenv("DEEPL_API_KEY")

# # Rendering new language
# # Translate a cover page
babeldown::deepl_translate_quarto(
     book_path = here::here(),
     chapter = "index.qmd",
     force = TRUE,
     render = FALSE, # Whether to run babelquarto::render_bool() after translation.
     source_lang = "EN",
     target_lang = "ES",
     formality = "more")

# # Translate chapter page


# # Write a loop to run the translation for each chapter based on chapter_list
for (i in 1:length(chapter_list)) {
     babeldown::deepl_translate_quarto(
          book_path = here::here("pages"),
          chapter = chapter_list[i],
          force = TRUE,
          render = FALSE, # Whether to run babelquarto::render_bool() after translation.
          source_lang = "EN",
          target_lang = "ES",
          formality = "more")
}

# # 4. Review the translation, make a PR and re-render the book



# # UPDATE LANGUAGE TRANSLATION PROTOCOL ------------------------------------------------------------------------
# 
# 
# diffs <- system("git diff --name-only main...", intern = TRUE) # to include only committed file, use ... after main
# chapters_changed <- diffs[stringr::str_detect(diffs, "\\.qmd$")] # Filter only .qmd files
# 
# # 2. Create vector of target languages
# 
# deepL_lang = c("FR", "ES", "JA", "PT-PT", "TR", "RU") #, "VN"
# target_lang = c("fr", "es", "jp", "pt", "tr", "ru") #, "vn" 
# 
# 
# # 3. Create a vector of chapters changed
# # Create a named list where each original chapter filename maps to its new versions with language codes
# chapters_changed_new <- setNames(
#   lapply(chapters_changed, function(chapter) {
#     sapply(target_lang, function(lang) {
#     sub("\\.qmd$", paste0(".", lang, ".qmd"), chapter)
#   })}),
#   chapters_changed)
# 
# 
# # 4. Write a loop to run the updated translation for each chapter based on chapter_changes_update, noted that the target_lang argument should be also adapted based on target_lang 
# 
# for (old_chapter in names(chapters_changed_new)) {
#   # Get the vector of new filenames for the current original chapter
#   new_chapters <- chapters_changed_new[[old_chapter]]
#   
#   # Loop over each new filename and its corresponding language
#   for (idx in seq_along(new_chapters)) {
#     new_chapter <- new_chapters[idx]
#     lang <- deepL_lang[idx]
# 
#     try({
#     babeldown::deepl_update(
#       path = here::here(old_chapter),
#       out_path = here::here(new_chapter),
#       source_lang = "EN",
#       target_lang = lang,
#       formality = "less",
#       yaml_fields = NULL)
#     }, silent = FALSE)    
#   }
# }


