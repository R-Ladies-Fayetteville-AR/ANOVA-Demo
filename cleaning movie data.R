
#cleaning movie data so that there's less work shown at the beginning of workshop

movie_data <- read.csv("disney_movies_total_gross.csv", stringsAsFactors = F, na.strings = "")

str(movie_data)

#reformatting data

#reformatting date
movie_data$release_date <- as.Date(movie_data$release_date, format = "%b %d,%y")

#reformatting money values
movie_data$total_gross <- as.numeric(gsub('[$,]','',movie_data$total_gross))
movie_data$inflation_adjusted_gross <- as.numeric(gsub('[$,]','',movie_data$inflation_adjusted_gross))

#reformatting genre to factor
movie_data$genre <- as.factor(movie_data$genre)

summary(movie_data)

#save as .Rdata so that no R reformatting is necessary upon loading
save(movie_data, file = "Movie Data.Rdata")
