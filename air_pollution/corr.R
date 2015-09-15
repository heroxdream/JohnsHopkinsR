corr <- function(directory, threshold = 0) {
    stat_df = complete(directory, 1:332)
    above_threshold = subset(stat_df, nobs > threshold)
    n = dim(above_threshold)[1]
    if (n == 0) {
        return(vector(mode="numeric", length=0))
    }
    corr_vec = rep(0, n)
    for (i in seq(n)) {
        file_id = as.character(above_threshold[i, 1])
        complete_count = above_threshold[i, 2]
        while (nchar(file_id) < 3) {
            file_id = paste("0", file_id, sep = "")
        }
        cor_data = matrix(nrow = complete_count, ncol = 2)
        t = read.csv(paste(directory, "/", file_id, ".csv", sep = ""))
        pointer = 1
        for (j in seq(dim(t)[1])) {
            if (sum(is.na(t[j, ])) == 0) {
                cor_data[pointer, 1] = t[j, 2]
                cor_data[pointer, 2] = t[j, 3]
                pointer = pointer + 1
            }
        }
        corr_vec[i] = cor(cor_data[, 1], cor_data[, 2])
    }

    corr_vec
}