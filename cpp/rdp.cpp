#include <Rcpp.h>

// Define the Point structure
struct Point {
  double x, y;
};

// Function to calculate perpendicular distance
double perpendicularDistance(const Point& point, const Point& lineStart, const Point& lineEnd) {
  double numer = std::abs((lineEnd.x - lineStart.x) * (lineStart.y - point.y) - (lineStart.x - point.x) * (lineEnd.y - lineStart.y));
  double denom = std::sqrt(std::pow(lineEnd.x - lineStart.x, 2) + std::pow(lineEnd.y - lineStart.y, 2));
  return numer / denom;
}

// Function to apply the Douglas-Peucker algorithm
std::vector<Point> rdp(const std::vector<Point>& points, double epsilon) {
  if (points.size() <= 2) {
    return points;
  }
  
  double dmax = 0;
  size_t index = 0;
  size_t end = points.size();
  
  // Find the point with the maximum distance
  for (size_t i = 1; i < end - 1; ++i) {
    double d = perpendicularDistance(points[i], points.front(), points.back());
    if (d > dmax) {
      index = i;
      dmax = d;
    }
  }
  
  // If the maximum distance is greater than epsilon, recursively simplify
  if (dmax > epsilon) {
    std::vector<Point> results1 = rdp(std::vector<Point>(points.begin(), points.begin() + index + 1), epsilon);
    std::vector<Point> results2 = rdp(std::vector<Point>(points.begin() + index, points.end()), epsilon);
    results1.pop_back(); // Avoid duplicate points
    results1.insert(results1.end(), results2.begin(), results2.end());
    return results1;
  } else {
    return {points.front(), points.back()};
  }
}

// Rcpp wrapper function to make rdp available in R
// [[Rcpp::export]]
Rcpp::List rdpWrapper(Rcpp::NumericMatrix points, double epsilon) {
  std::vector<Point> cppPoints;
  
  // Convert the NumericMatrix to a vector of Point structures
  for (int i = 0; i < points.nrow(); ++i) {
    cppPoints.push_back({points(i, 0), points(i, 1)});
  }
  
  // Call the rdp function
  std::vector<Point> result = rdp(cppPoints, epsilon);
  
  // Convert the result back to a NumericMatrix
  Rcpp::NumericMatrix resultMatrix(result.size(), 2);
  for (size_t i = 0; i < result.size(); ++i) {
    resultMatrix(i, 0) = result[i].x;
    resultMatrix(i, 1) = result[i].y;
  }
  
  return Rcpp::List::create(Rcpp::Named("points") = resultMatrix);
}
