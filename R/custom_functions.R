#' Extract dates from cru rasterStack
#'
#' @param rs a rasterStack of cru data
#'
#' @return a tibble of layer ID, month, year and "yyyy-mm" character code.
#' @export
#'
#' @examples
mm_yyyy_df.rs <- function(rs){
    spl.dts <- strsplit(gsub("X", "", names(rs)), "\\.") 
    df <- data_frame(
        layer = 1:length(spl.dts),
        year = spl.dts %>% map(1) %>% unlist, 
        month = spl.dts %>% map(2) %>% unlist) %>%
        mutate(code = paste(year, month, sep = "-"))
    return(df)
}