library(httr)


if (!dir.exists("data")) {
  dir.create("data")
}

repo_url <- "https://api.github.com/repos/CSAS-Data-Challenge/2026/contents"
response <- GET(repo_url)
content <- content(response, "parsed")

csv_files <- Filter(function(x) grepl("\\.csv$", x$name), content)

for (file in csv_files) {
  download.file(
    file$download_url,
    destfile = file.path("data", file$name),
    quiet = TRUE
  )
}
