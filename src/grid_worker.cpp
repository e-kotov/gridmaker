#include <Rcpp.h>
#include <cstdio>
#include <string>

using namespace Rcpp;

// Helper to update C++ string buffer via snprintf
// But direct character writing is often faster if structure is known.
// We stick to snprintf for safety and clarity relative to R's sprintf.

//' Generate Grid Polygons and IDs (C++ Kernel)
//'
//' Creates the geometry (sfc compatible list of matrices) and
//' INSPIRE IDs in a single pass for maximum efficiency.
//'
//' @param x_llc Numeric vector of X coordinates (bottom-left)
//' @param y_llc Numeric vector of Y coordinates (bottom-left)
//' @param cellsize Grid resolution in meters
//' @param epsg EPSG code (e.g. 3035)
//' @param size_lbl Label for short ID (e.g. "1km" or "100m")
//' @param divider Divisor for short ID coordinates
//' @param axis_order "NE" or "EN"
//' @param id_format "both", "long", "short", "none"
//' @param generate_ids Logical. If FALSE, only geometry is generated.
//'
//' @return List containing geometry and IDs
//' @export
// [[Rcpp::export]]
List grid_worker_rcpp(NumericVector x_llc, NumericVector y_llc, double cellsize,
                      int epsg, std::string size_lbl, double divider,
                      std::string axis_order, std::string id_format,
                      bool generate_ids = true) {
  int n = x_llc.size();

  // Allocate Outputs
  List geometries(n);
  CharacterVector ids_long;
  CharacterVector ids_short;

  if (generate_ids) {
    ids_long = CharacterVector(n);
    ids_short = CharacterVector(n);
  }

  // Template Polygon (5 points)
  NumericMatrix template_mat(5, 2);
  template_mat(0, 0) = 0;
  template_mat(0, 1) = 0;
  template_mat(1, 0) = cellsize;
  template_mat(1, 1) = 0;
  template_mat(2, 0) = cellsize;
  template_mat(2, 1) = cellsize;
  template_mat(3, 0) = 0;
  template_mat(3, 1) = cellsize;
  template_mat(4, 0) = 0;
  template_mat(4, 1) = 0;

  // Format optimization
  bool do_long = generate_ids && (id_format == "both" || id_format == "long");
  bool do_short = generate_ids && (id_format == "both" || id_format == "short");
  bool is_ne = (axis_order == "NE");

  // Buffer for sprintf
  char buffer[128];

  // Pre-calculate fixed parts if helpful, but snprintf is fast enough.
  long cs_int = (long)cellsize;

  // Reusable attribute for sfg (can't share attributes across objects safely in
  // R?) R uses independent attributes for each S3 object usually.
  CharacterVector sfg_class = CharacterVector::create("XY", "POLYGON", "sfg");

  for (int i = 0; i < n; i++) {
    double x = x_llc[i];
    double y = y_llc[i];

    // --- 1. Geometry ---
    // Deep copy template
    NumericMatrix poly = clone(template_mat);

    // Shift
    for (int r = 0; r < 5; r++) {
      poly(r, 0) += x;
      poly(r, 1) += y;
    }

    // Wrap as sfg
    List sfg = List::create(poly);
    sfg.attr("class") = sfg_class;
    geometries[i] = sfg;

    // --- 2. ID Generation ---
    if (do_long) {
      snprintf(buffer, sizeof(buffer), "CRS%dRES%ldmN%lldE%lld", epsg, cs_int,
               (long long)y, (long long)x);
      ids_long[i] = buffer;
    }

    if (do_short) {
      long long y_scaled = (long long)(y / divider);
      long long x_scaled = (long long)(x / divider);

      if (is_ne) {
        snprintf(buffer, sizeof(buffer), "%sN%lldE%lld", size_lbl.c_str(),
                 y_scaled, x_scaled);
      } else {
        snprintf(buffer, sizeof(buffer), "%sE%lldN%lld", size_lbl.c_str(),
                 x_scaled, y_scaled);
      }
      ids_short[i] = buffer;
    }
  }

  return List::create(_["geometry"] = geometries, _["id_long"] = ids_long,
                      _["id_short"] = ids_short);
}

//' Generate INSPIRE IDs Only (C++ Kernel)
//'
//' Efficiently generates INSPIRE IDs for a set of coordinates.
//'
//' @param x_llc Numeric vector of X coordinates (bottom-left)
//' @param y_llc Numeric vector of Y coordinates (bottom-left)
//' @param cellsize Grid resolution in meters
//' @param epsg EPSG code
//' @param size_lbl Label for short ID
//' @param divider Divisor for short ID coordinates
//' @param axis_order "NE" or "EN"
//' @param id_format "both", "long", "short"
//'
//' @return List containing IDs
//' @export
// [[Rcpp::export]]
List generate_ids_rcpp(NumericVector x_llc, NumericVector y_llc,
                       double cellsize, int epsg, std::string size_lbl,
                       double divider, std::string axis_order,
                       std::string id_format) {
  int n = x_llc.size();
  CharacterVector ids_long(n);
  CharacterVector ids_short(n);

  bool do_long = (id_format == "both" || id_format == "long");
  bool do_short = (id_format == "both" || id_format == "short");
  bool is_ne = (axis_order == "NE");

  char buffer[128];
  long cs_int = (long)cellsize;

  for (int i = 0; i < n; i++) {
    double x = x_llc[i];
    double y = y_llc[i];

    if (do_long) {
      snprintf(buffer, sizeof(buffer), "CRS%dRES%ldmN%lldE%lld", epsg, cs_int,
               (long long)y, (long long)x);
      ids_long[i] = buffer;
    }

    if (do_short) {
      long long y_scaled = (long long)(y / divider);
      long long x_scaled = (long long)(x / divider);

      if (is_ne) {
        snprintf(buffer, sizeof(buffer), "%sN%lldE%lld", size_lbl.c_str(),
                 y_scaled, x_scaled);
      } else {
        snprintf(buffer, sizeof(buffer), "%sE%lldN%lld", size_lbl.c_str(),
                 x_scaled, y_scaled);
      }
      ids_short[i] = buffer;
    }
  }

  return List::create(_["id_long"] = ids_long, _["id_short"] = ids_short);
}

// Helper: Count trailing zeros (integer based)
// Note: This duplicates the logic of .tz_count() in R/utils.R, but is kept
// separate for performance. Calling R functions from within this C++ parser
// would add significant overhead during vectorized ID parsing.
int count_trailing_zeros_cpp(double x) {
  long long val = (long long)x;
  int n = 0;
  if (val == 0)
    return 0;
  while (val % 10 == 0) {
    n++;
    val /= 10;
  }
  return n;
}

// Helper: Parse cellsize string (e.g. "1km", "100m") to meters
double parse_res_to_m(std::string res_str) {
  if (res_str.empty())
    return NA_REAL;

  double mul = 1.0;
  if (res_str.length() > 2 && res_str.substr(res_str.length() - 2) == "km") {
    mul = 1000.0;
    try {
      return std::stod(res_str.substr(0, res_str.length() - 2)) * mul;
    } catch (...) {
      return NA_REAL;
    }
  } else if (res_str.length() > 1 && res_str.back() == 'm') {
    try {
      return std::stod(res_str.substr(0, res_str.length() - 1));
    } catch (...) {
      return NA_REAL;
    }
  }
  return NA_REAL;
}

//' Parse INSPIRE IDs to Coordinates (C++ Kernel)
//'
//' @param inspire Character vector of IDs
//' @param is_long Logical indicator for long IDs
//' @param is_short Logical indicator for short IDs
//' @return DataFrame with columns: crs, cellsize, x, y
//' @export
// [[Rcpp::export]]
DataFrame parse_inspire_ids_rcpp(CharacterVector inspire, LogicalVector is_long,
                                 LogicalVector is_short) {
  int n = inspire.size();

  // Columns
  IntegerVector crs(n, NA_INTEGER);
  NumericVector cellsize(n, NA_REAL);
  NumericVector x(n, NA_REAL);
  NumericVector y(n, NA_REAL);

  for (int i = 0; i < n; i++) {
    SEXP s_sexp = STRING_ELT(inspire, i);
    if (s_sexp == NA_STRING)
      continue;

    // Use low-level const char* to avoid std::string allocation and Rcpp
    // overhead
    const char *s = CHAR(s_sexp);

    if (is_long[i]) {
      // Format: CRS{epsg}RES{res}mN{y}E{x}
      // Example: CRS3035RES1000mN3000000E4000000

      // We expect "CRS" at start.
      if (s[0] == 'C' && s[1] == 'R' && s[2] == 'S') {
        char *end;
        crs[i] = (int)strtol(s + 3, &end, 10);

        // Expect "RES"
        if (end[0] == 'R' && end[1] == 'E' && end[2] == 'S') {
          cellsize[i] = (double)strtol(end + 3, &end, 10);

          // Expect "mN"
          if (end[0] == 'm' && end[1] == 'N') {
            y[i] = (double)strtoll(end + 2, &end, 10);

            // Expect "E"
            if (end[0] == 'E') {
              x[i] = (double)strtoll(end + 1, NULL, 10);
            }
          }
        }
      }

    } else if (is_short[i]) {
      // Format: {res}{N|E}{val}{E|N}{val}
      // Example: 1kmN3000E4000

      char *end;
      double cs = strtod(s, &end);

      // Parse unit "km" or "m"
      if (end[0] == 'k' && end[1] == 'm') {
        cs *= 1000.0;
        end += 2;
      } else if (end[0] == 'm') {
        end += 1;
      } else {
        continue; // Invalid unit
      }

      cellsize[i] = cs;

      // First Axis
      char axis1 = end[0];
      end++;
      long long val1 = strtoll(end, &end, 10);

      // Second Axis
      char axis2 = end[0];
      end++;
      long long val2 = strtoll(end, &end, 10);

      // Calculate multiplier
      double multiplier = std::pow(10.0, count_trailing_zeros_cpp(cs));
      double v1 = (double)val1 * multiplier;
      double v2 = (double)val2 * multiplier;

      if (axis1 == 'N' && axis2 == 'E') {
        y[i] = v1;
        x[i] = v2;
      } else if (axis1 == 'E' && axis2 == 'N') {
        x[i] = v1;
        y[i] = v2;
      } else {
        // Invalid axis format
        cellsize[i] = NA_REAL;
      }
    }
  }

  // improved return construction
  List ret = List::create(Named("crs") = crs, Named("cellsize") = cellsize,
                          Named("y") = y, Named("x") = x);
  ret.attr("class") = "data.frame";
  ret.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  return ret;
}
