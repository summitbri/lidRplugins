#include <Rcpp.h>
#include <vector>
#include <limits>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_most_nadir(NumericMatrix las, List layout, bool use_min = true) {
  int ncols = layout["ncol"];
  int nrows = layout["nrow"];
  double xmin = layout["xmin"];
  double xmax = layout["xmax"];
  double ymin = layout["ymin"];
  double ymax = layout["ymax"];
  double xres = (xmax - xmin) / ncols;
  double yres = (ymax - ymin) / nrows;

  // Initialize output vector
  std::vector<int> output(ncols * nrows, -1);

  // Loop through points in LAS data
  for (int i = 0; i < las.nrow(); i++) {
    double x = las(i, 0);
    double y = las(i, 1);
    double scan_angle = fabs(las(i, 3)); // Assumes 4th column is scan angle

    int col = floor((x - xmin) / xres);
    int row = floor((ymax - y) / yres);
    if (y == ymin) row = nrows - 1;
    if (x == xmax) col = ncols - 1;

    if (row < 0 || row >= nrows || col < 0 || col >= ncols)
      stop("Point out of raster bounds.");

    int cell = row * ncols + col;

    // Update output based on the scan angle
    if (output[cell] == -1) {
      output[cell] = i;
    } else {
      double existing_angle = fabs(las(output[cell], 3));
      if ((use_min && scan_angle < existing_angle) || (!use_min && scan_angle > existing_angle)) {
        output[cell] = i;
      }
    }
  }

  // Return filtered indices
  IntegerVector indices;
  for (int i : output) {
    if (i != -1) indices.push_back(i);
  }
  return indices;
}
