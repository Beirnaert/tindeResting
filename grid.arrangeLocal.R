grid.arrangeLocal <- function (..., newpage = TRUE) {
    if (newpage) 
        grid::grid.newpage()
    g <- arrangeGrobLocal(...)
    grid::grid.draw(g)
    invisible(g)
}