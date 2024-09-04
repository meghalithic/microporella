# Function to convert vector to norm length 
f.normalize_vector <- function(vector) {
    norm_length <- sqrt(sum(vector^2))
    normalized_vector <- vector / norm_length
    return(normalized_vector)
}