# Code for ggbarribbon package

# StatBarRibbon ----
StatBarRibbon <- ggplot2::ggproto(
  "StatBarRibbon", ggplot2::Stat,
  setup_data = function(data, params) {
    data <- data %>%
      dplyr::group_by(PANEL, group) %>%
      dplyr::mutate(x_next=lead(x), y_next=lead(y)) %>%
      dplyr::filter(!is.na(x_next))
    data
  },
  compute_panel = function(data, scales) {
    out <- mapply(ggbump::sigmoid, data$x, data$x_next, data$y, data$y_next, 
                  n=abs(data$x-data$x_next)+1, SIMPLIFY = FALSE) %>%
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
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' metab_df <- data.frame(
#' metab=LETTERS[1:10], 
#' depth=rep(c(0, 5, 25, 125, 175), each=10),
#' area=runif(50)
#' )
#' ggplot(metab_df) +
#' geom_bar_ribbon(aes(y=area, x=-depth, fill=metab), color="black") +
#' coord_flip()
geom_bar_ribbon <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBarRibbon, data = data, mapping = mapping, geom = "ribbon",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
