\name{run_app}
\alias{run_app}
\title{Run the model accuracy application}
\usage{
run_app(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
}
\arguments{
\item{onStart}{A function passed to \code{shiny::shinyApp}.}
\item{options}{Named options passed to \code{shiny::shinyApp}.}
\item{enableBookmarking}{Bookmarking mode passed to \code{shiny::shinyApp}.}
\item{uiPattern}{URL pattern passed to \code{shiny::shinyApp}.}
\item{...}{Arguments stored as golem options.}
}
\value{
The result of \code{shiny::shinyApp}, returned invisibly by the golem wrapper.
}
\description{
Launches the Shiny app from the installed package. Development use is
\code{devtools::load_all("."); modeldash::run_app()}.
}
