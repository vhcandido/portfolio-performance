#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bb_strat(
		NumericVector x, 
		NumericVector up1, 
		NumericVector dn1, 
		NumericVector up2, 
		NumericVector dn2) {
	NumericVector positions(x.size());
	for(int i = 1; i < x.size(); ++i) {
		if(positions[i-1] == 0) {
			// Entering BUY/SELL region or staying OUT
			if(x[i] < dn1[i] && x[i-1] >= dn1[i-1] && x[i] > dn2[i])
				positions[i] = 1;
			else if(x[i] > up1[i] && x[i-1] <= up1[i-1] && x[i] < up2[i])
				positions[i] = -1;
			else
				positions[i] = positions[i-1];
		} else if((positions[i-1] == 1 && x[i] > up1[i] && x[i-1] <= up1[i-1]) ||
				(positions[i-1] == -1 && x[i] < dn1[i] && x[i-1] >= dn1[i-1]))
			positions[i] = 0;
		else
			positions[i] = positions[i-1];
	}

	return(positions);
}
