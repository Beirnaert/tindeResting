arrangeGrobLocal <- function (..., grobs = list(...), layout_matrix, vp = NULL, name = "arrange", 
          as.table = TRUE, respect = FALSE, clip = "off", nrow = NULL, 
          ncol = NULL, widths = NULL, heights = NULL, top = NULL, bottom = NULL, 
          left = NULL, right = NULL, padding = unit(0.5, "line")) 
{
    n <- length(grobs)
    if (!is.null(ncol) && !is.null(widths)) {
        stopifnot(length(widths) == ncol)
    }
    if (!is.null(nrow) && !is.null(heights)) {
        stopifnot(length(heights) == nrow)
    }
    if (is.null(ncol) && !is.null(widths)) {
        ncol <- length(widths)
    }
    if (is.null(nrow) && !is.null(heights)) {
        nrow <- length(heights)
    }
    if (is.null(nrow) && !is.null(ncol)) {
        nrow <- ceiling(n/ncol)
    }
    if (is.null(ncol) && !is.null(nrow)) {
        ncol <- ceiling(n/nrow)
    }
    stopifnot(nrow * ncol >= n)
    if (is.null(nrow) && is.null(ncol) && is.null(widths) && 
        is.null(heights)) {
        nm <- grDevices::n2mfrow(n)
        nrow = nm[1]
        ncol = nm[2]
    }
    inherit.ggplot <- unlist(lapply(grobs, inherits, what = "ggplot"))
    inherit.trellis <- unlist(lapply(grobs, inherits, what = "trellis"))
    if (any(inherit.ggplot)) {
        stopifnot(requireNamespace("ggplot2", quietly = TRUE))
        toconv <- which(inherit.ggplot)
        grobs[toconv] <- lapply(grobs[toconv], ggplot2::ggplotGrob)
    }
    if (any(inherit.trellis)) {
        stopifnot(requireNamespace("lattice", quietly = TRUE))
        toconv <- which(inherit.trellis)
        grobs[toconv] <- lapply(grobs[toconv], latticeGrob)
    }
    if (missing(layout_matrix)) {
        positions <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
        positions$b <- positions$t
        positions$r <- positions$l
        if (as.table) 
            positions <- positions[order(positions$t), ]
        positions <- positions[seq_along(grobs), ]
    }
    else {
        cells <- sort(unique(as.vector(layout_matrix)))
        range_cell <- function(ii) {
            ind <- which(layout_matrix == ii, arr.ind = TRUE)
            c(l = min(ind[, "col"], na.rm = TRUE), r = max(ind[, 
                                                               "col"], na.rm = TRUE), t = min(ind[, "row"], 
                                                                                              na.rm = TRUE), b = max(ind[, "row"], na.rm = TRUE))
        }
        positions <- data.frame(do.call(rbind, lapply(cells, 
                                                      range_cell)))
        ncol <- max(positions$r)
        nrow <- max(positions$b)
        positions <- positions[seq_along(grobs), ]
    }
    if (is.null(widths)) 
        widths <- unit(rep(1, ncol), "null")
    if (is.null(heights)) 
        heights <- unit(rep(1, nrow), "null")
    if (!grid::is.unit(widths)) 
        widths <- unit(widths, "null")
    if (!grid::is.unit(heights)) 
        heights <- unit(heights, "null")
    gt <- gtable::gtable(name = name, respect = respect, heights = heights, 
                 widths = widths, vp = vp)
    gt <- gtable::gtable_add_grob(gt, grobs, t = positions$t, b = positions$b, 
                          l = positions$l, r = positions$r, z = seq_along(grobs), 
                          clip = clip)
    if (is.character(top)) {
        top <- grid::textGrob(top)
    }
    if (grid::is.grob(top)) {
        h <- grid::grobHeight(top) + padding
        gt <- gtable::gtable_add_rows(gt, heights = h, 0)
        gt <- gtable::gtable_add_grob(gt, top, t = 1, l = 1, r = ncol(gt), 
                              z = Inf, clip = clip)
    }
    if (is.character(bottom)) {
        bottom <- grid::textGrob(bottom)
    }
    if (grid::is.grob(bottom)) {
        h <- grid::grobHeight(bottom) + padding
        gt <- gtable::gtable_add_rows(gt, heights = h, -1)
        gt <- gtable::gtable_add_grob(gt, bottom, t = nrow(gt), l = 1, 
                              r = ncol(gt), z = Inf, clip = clip)
    }
    if (is.character(left)) {
        left <- grid::textGrob(left, rot = 90)
    }
    if (grid::is.grob(left)) {
        w <- grid::grobWidth(left) + padding
        gt <- gtable::gtable_add_cols(gt, widths = w, 0)
        gt <- gtable::gtable_add_grob(gt, left, t = 1, b = nrow(gt), 
                              l = 1, r = 1, z = Inf, clip = clip)
    }
    if (is.character(right)) {
        right <- grid::textGrob(right, rot = -90)
    }
    if (grid::is.grob(right)) {
        w <- grid::grobWidth(right) + padding
        gt <- gtable::gtable_add_cols(gt, widths = w, -1)
        gt <- gtable::gtable_add_grob(gt, right, t = 1, b = nrow(gt), 
                              l = ncol(gt), r = ncol(gt), z = Inf, clip = clip)
    }
    gt
}