//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' Point distance with C++
//' @description
//' \code{pointDistance2} performs pythagorus and cbinds all columns from \code{to} to new dists column. It
//' is only defined for one point (\code{from}) to many (\code{to}) points.
//' \code{pointDistance3} performs pythagorus and is to be used internally within \code{distanceFromEachPoint}
//' as an alternative to
//' \code{.pointDistance}, where it does many points (\code{from}) to many (\code{to}) points, one \code{from}
//' point at a time. The results are then rbinded internally. It does not cbind extra columns from \code{to}.
//'
//' @inheritParams distanceFromEachPoint
//' @return
//' \code{pointDistance2} - a matrix with all the \code{to} columns plus one extra \code{dists} column.
//' \code{pointDistance3} - a matrix with x and y columns from \code{to} plus one extra \code{dists} column.
//'
//' @rdname distances
// [[Rcpp::export]]
NumericMatrix pointDistance2(NumericMatrix to, NumericMatrix from) { // from = x1 and y1; to = x0 and y0
  // NumericVector pointDistance2(NumericMatrix to, NumericMatrix from) { // from = x1 and y1; to = x0 and y0

  int nr = to.nrow();
  int nc = to.ncol();
  NumericMatrix out (nr, nc + 1);

  CharacterVector fromColNames = colnames(from);
  CharacterVector toColNames = colnames(to);
  // LogicalVector whFromX(from.ncol());
  int whFromX = 0;
  int whFromY = 0;
  int whToX = 0;
  int whToY = 0;

  for( int i=0; i < fromColNames.size(); i++ ){
    if((fromColNames[i] == "x")) {
      whFromX = i;
    } else if((fromColNames[i] == "y")) {
      whFromY = i;
    }
  }

  for( int i=0; i < toColNames.size(); i++ ){
    if((toColNames[i] == "x")) {
      whToX = i;
    } else if((toColNames[i] == "y")) {
      whToY = i;
    }
  }

  //
  // Rcpp::IntegerVector v = Rcpp::seq(0, from.ncol()-1);
  //
  NumericVector x0 = to(_,whToX);
  double x1 = from(0,whFromX);
  NumericVector y0 = to(_,whToY);
  double y1 = from(0,whFromY);
  //
  out(_, nc ) = sqrt( pow(x0 - x1, 2.0) + pow(y0 - y1, 2.0));
  for( int i=0; i < nc; i++ ){
    out(_, i) = to(_,i);
  }
  toColNames.push_back("dists");
  colnames(out) = toColNames; //CharacterVector::create(toColNames, "dists");
  // return out;

  return out;
}


//NumericMatrix pointDistance2(NumericVector x0, double x1, NumericVector y0, double y1) {
//  int n = x0.size();
//  NumericMatrix out (n, 3);
//
//  out(_,2) = sqrt( pow(x0 - x1, 2.0) + pow(y0 - y1, 2.0));
//  out(_,0) = x0;
//  out(_,1) = y0;
//  colnames(out) = CharacterVector::create("x", "y", "dists");
//  return out;
//
//}

//' @param fromX Numeric vector of x coordinates for 'from' points
//' @param fromY Numeric vector of y coordinates for 'from' points
//' @param toX Numeric vector of x coordinates for 'to' points
//' @param toY Numeric vector of y coordinates for 'to' points
//' @param maxDistance Numeric scalar. The maximum distance cutoff for returned distances.
//' @return
//' A matrix with 3 columns, x0, y0 and dists.
//' @rdname distances
//' @details
//' A slightly faster way to calculate distances.
// [[Rcpp::export]]
NumericMatrix pointDistance3(NumericVector fromX, NumericVector toX,
                             NumericVector fromY, NumericVector toY, double maxDistance) {
  int nfrom = fromX.size();
  int nto = toX.size();
  int index = 0;
  NumericMatrix out (nfrom * nto, 3);


  for(int from=0; from < nfrom; ++from) {
    for(int to=0; to < nto; ++to) {
      double dis = sqrt( pow(fromX(from) - toX(to), 2.0) + pow(fromY(from) - toY(to), 2.0));
      // int index = to + from * nto;
      if(dis <= maxDistance) {
        out(index,2) = dis;
        out(index,0) = toX(to);
        out(index,1) = toY(to);
        index++;
      }
    }
  }

  NumericMatrix out2 = out;
  if(std::isfinite(maxDistance)) {
    NumericMatrix out2 (index, 3);
    for(int it = 0 ; it < (index); ++it) {
      out2(it,2) = out(it,2);
      out2(it,0) = out(it,0);
      out2(it,1) = out(it,1);
    }
    colnames(out2) = CharacterVector::create("x", "y", "dists");
    return out2;
  }

  colnames(out) = CharacterVector::create("x", "y", "dists");
  return out;
}

//' @title
//' Rcpp duplicated on integers using Rcpp Sugar
//' @description
//' \code{.duplicatedInt} does same as \code{duplicated} in R, but only on integers, and faster.
//' It uses Rcpp sugar
//'
//' @param x Integer Vector
//' @return
//' A logical vector, as per \code{duplicated}
//'
//' @rdname duplicated
// [[Rcpp::export]]
LogicalVector duplicatedInt(IntegerVector x) {
  return duplicated(x);
}


//' @title
//' Rcpp Sugar version of runif
//' @description
//' Slightly faster than runif, and used a lot
//'
//' @param N Integer Vector
//' @return
//' A vector of uniform random numbers as per \code{runif}
//'
//' @rdname rcpp-extras
// [[Rcpp::export]]
NumericVector runifC(const int N) {
  NumericVector X(N);
  X = runif(N);
  return X;
}
