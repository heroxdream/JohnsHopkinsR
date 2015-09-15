complete <- function(directory, id = 1:332) {

    stat_df = data.frame(id = id, nobs = rep(id))
    for (i in seq(dim(stat_df)[1])) {
        curr_id = stat_df[i, ]$id
        str_i = as.character(curr_id)
        while (nchar(str_i) < 3) {
            str_i = paste("0", str_i, sep = "")
        }
        counter = 0
        t = read.csv(paste(directory, "/", str_i, ".csv", sep = ""))
        for (j in seq(dim(t)[1])) {
            if (sum(is.na(t[j, ])) == 0) {
                counter = counter + 1
            }
        }
        stat_df[i, 2] = counter
    }
    stat_df
}