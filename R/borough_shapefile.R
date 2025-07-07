#' London Borough Boundaries
#'
#' A  `sf` object containing the spatial boundaries of London boroughs,
#' projected in the British National Grid (EPSG:27700). This dataset includes the borough
#' names and associated MULTIPOLYGON geometries.
#'
#' @format A `sf` object with 33 features and 1 field:
#' \describe{
#'   \item{Borough}{Name of the London Borough (character)}
#'   \item{geometry}{MULTIPOLYGON geometry column in BNG projection}
#' }
#'
#' @source London Datastore
#' @examples
#' library(sf)
#' plot(st_geometry(borough_shapefile))
#'
#' @keywords datasets sf spatial
"borough_shapefile"
