#' Create an info box
#'
#' @description Create an info box that is a color background with a title/content centered
#' in the middle.
#'
#' @details
#' This function implements the drawing of a "card", suitable for a dashboard. The idea
#' is that one creates a color background, then plots a title and text centered in the
#' plot window. The plot fills the entire boundary.
#'
#' @param title A title (centered in the middle)
#' @param content Content to show (centered, larger than title)
#' @param fill Color for the background
#' @param col Color of the text
#'
#' @return A ggplot object that represents the info box.
#' @export
#'
#' @examples
#' \dontrun{
#' info_box("Title","Content", fill ="blue", col="white")
#' }
info_box <- function(title, content, fill="blue", col="white") {

  # Check for edge cases of fill and color
  if ( rgb(red=t(col2rgb(fill)), maxColorValue=255) == "#FFFFFF" &&
       rgb(red=t(col2rgb(col)), maxColorValue=255) == "#FFFFFF" ) col <- "black"
  if ( rgb(red=t(col2rgb(fill)), maxColorValue=255) == "#000000" &&
       rgb(red=t(col2rgb(col)), maxColorValue=255) == "#000000" ) col <- "white"

  # Create a full-size void plot.
  ggplot2::ggplot( data.frame(x=c(0,1), y=c(0,1)), ggplot2::aes(x=x,y=y)) +
    ggplot2::theme_void() +
    # Use ggtext to create a text box.
    ggtext::geom_textbox(
      label = glue::glue(
        "<br><span style = 'font-size:16pt'>{title}</span><br><br><span style = 'font-size:48pt'>{content}</span>"
      ),
      # Centered in the plot
      x=0.5,y=0.5,
      # Full height/width
      width=1, height=1,
      fill=fill,col=col,
      box.r=ggplot2::unit(0,"npc"),
      #size=30,
      valign=0.5, halign=0.5
    )
}

#' Create info card
#'
#' @description Create a info card, which is a plot taking the whole screen, based on
#' the input table `.x` and field `tag`.
#'
#' @details This function is a little brittle. It assumes there is a tag
#' column (to filter on) and a Description, Count and Color fields for
#' the plot.
#'
#' @param .x A table of information to extract a row with `tag`=tag.
#' @param tag The tag for the `tag column,`
#'
#' @return
#' @export
#'
#' @examples
create_info_card <- function(.x, tag) {
  .x <- dplyr::filter(.x, tag == !!tag)

  info_box(dplyr::pull(.x, "Description"),
           dplyr::pull(.x, "Count"),
           fill=dplyr::pull(.x, "Color"), col="white")
}



#' Title
#'
#' @param .x
#' @param tags
#' @param wrap
#'
#' @return
#' @export
#'
#' @examples
grant_bar_chart <- function(.x, tags, wrap=15) {

  # Clean up the table for plotting
  .x <- .x %>%
    # Filter on only the tags listed.
    dplyr::filter(tag %in% tags) %>%
    # Reverse the order of the desired tags (since it plots from bottom to top).
    dplyr::slice(rev(match(tags, tag))) %>%
    # Keep description narrow
    dplyr::mutate(Description =stringr::str_wrap(Description, wrap)) %>%
    # Transform to factor so it isn't misunderstood
    dplyr::mutate(Description = factor(Description, levels = Description))

  # Shouldn't need this soon.
  grant_status_color <- dplyr::select(.x, tag,Color) %>% tibble::deframe()

  ggplot2::ggplot(.x, ggplot2::aes( x = Count, y = Description, fill = tag)) +
    ggplot2::geom_col(width = 0.9, col = "black") +
    ggplot2::scale_fill_discrete(type = grant_status_color) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color="black", size = 8),
      legend.key = ggplot2::element_blank(),
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = 0.5),
      strip.background = element_rect(fill = "#F2F2F2",
                                      colour = "black", size = 0.7)
    ) +
    ggplot2::xlim(c(0,ceiling(max(.x$Count)+0.2*max(.x$Count)))) +
    ggplot2::geom_text(ggplot2::aes(label = Count), nudge_x= 0.05 * max(.x$Count), size=7)



}

