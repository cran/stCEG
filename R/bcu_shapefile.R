#' London Basic Command Units (BCUs)
#'
#' A simple features (`sf`) object containing the spatial boundaries of London's Basic Command Units (BCUs),
#' used for policing. The dataset includes BCU names and their corresponding MULTIPOLYGON geometries.
#' The data is projected in the British National Grid (EPSG:27700).
#'
#' @format A `sf` object with 13 features and 1 field:
#' \describe{
#'   \item{BCU}{Name of the Basic Command Unit (character)}
#'   \item{geometry}{MULTIPOLYGON geometry column in BNG projection}
#' }
#'
#' @source Merged from Borough shapefile from London Datastore.
#'
#' @examples
#' library(sf)
#' plot(st_geometry(bcu_shapefile))
#'
#' @keywords datasets sf spatial
"bcu_shapefile"
