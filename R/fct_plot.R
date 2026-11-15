# Plot helpers --------------------------------------------------------------

HORIZONTAL_LINE_COLOUR <- "#9aa0a6"
VERTICAL_LINE_COLOUR <- "#C2185B"

hline_shape <- function(y = 0, colour = HORIZONTAL_LINE_COLOUR, dash = "dash", width = 1) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    y0 = y,
    y1 = y,
    xref = "paper",
    yref = "y",
    line = list(color = colour, dash = dash, width = width)
  )
}

vline_shape <- function(x, colour = VERTICAL_LINE_COLOUR, dash = "dash", width = 1) {
  list(
    type = "line",
    x0 = as.character(x),
    x1 = as.character(x),
    y0 = 0,
    y1 = 1,
    xref = "x",
    yref = "paper",
    line = list(color = colour, dash = dash, width = width)
  )
}
