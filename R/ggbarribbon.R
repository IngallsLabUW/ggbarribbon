# Code for ggbarribbon package

# StatBarRibbon ----
StatBarRibbon <- ggplot2::ggproto(
  "StatBarRibbon", ggplot2::Stat,
  setup_data = function(data, params) {
    if(class(data$x)[1]=="mapped_discrete"){
      stop("x and y aesthetics must be continuous, not factors")
    }
    data <- data %>%
      dplyr::group_by(PANEL, group) %>%
      dplyr::arrange(x) %>%
      dplyr::mutate(x_next=dplyr::lead(x), y_next=dplyr::lead(y)) %>%
      dplyr::filter(!is.na(x_next))
    data
  },
  compute_panel = function(data, scales, interp_res) {
    out <- mapply(ggbump::sigmoid, data$x, data$x_next, data$y, data$y_next, 
                  n=abs(data$x-data$x_next)*interp_res+1, SIMPLIFY = FALSE) %>%
      mapply(FUN = cbind, fill=data$fill, PANEL=data$PANEL, 
             group=data$group, SIMPLIFY = FALSE) %>%
      do.call(what = "rbind") %>%
      dplyr::group_by(group, x) %>%
      dplyr::slice(1) %>%
      dplyr::group_by(x) %>%
      dplyr::mutate(y_rel=y/sum(y)) %>%
      dplyr::mutate(ymin=c(0, head(cumsum(y_rel), -1))) %>%
      dplyr::mutate(ymax=cumsum(y_rel))
  },
  required_aes = c("x", "y")
)


# geom_bar_ribbon ----
#' Stacked bar chart interpolated using ribbons
#' 
#' This geom interpolates smoothly between the values that would normally be 
#' provided to geom_bar using a sigmoid curve.Often, this is useful when a 
#' stacked bar plot is desired but the values along the perpindicular axis are 
#' numeric rather than discrete. A numeric axis often results in overplotting 
#' or weird spacing issues when the data could instead be interpolated and 
#' show up more nicely.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param interp_res The resolution at which the data should be interpolated,
#' in units of # per y-axis value. 1 is a good fit for depth data, and 10-20 is
#' good for latitudinal data.
#'
#' @export
#'
#' @examples
# library(ggplot2)
# metab_df <- data.frame(
#   metab=LETTERS[1:10],
#   depth=rep(c(0, 5, 25, 125, 175), each=10),
#   area=runif(50)
# )
# ggplot(metab_df) +
#   geom_bar_ribbon(aes(y=area, x=-depth, fill=metab), color="black") +
#   coord_flip()
geom_bar_ribbon <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, interp_res = 1, ...) {
  ggplot2::layer(
    stat = StatBarRibbon, data = data, mapping = mapping, geom = "ribbon",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(interp_res = interp_res, na.rm = na.rm, ...)
  )
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
