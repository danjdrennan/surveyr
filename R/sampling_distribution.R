sampling_distribution <- function(y, n){
    if(!(is.vector(y) && is.numeric(y))){
        stop("y must be a numeric vector")
    }
    if(n > length(y)){
        stop("n must be less than N = length(y)")
    }
    sampled_data <- draw_samples(y, n)$sampled_data
    return(rowMeans(sampled_data))
}
