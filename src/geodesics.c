/* File: vincenty.geodesics.c */

#include <R.h> 
#include <Rinternals.h>
#include <Rmath.h>

#include <math.h>

//define some global constants
double a = 6378137, b = 6356752.3142,  f = 1/298.257223563;  // WGS-84 ellipsiod

/*
 * Calculate destination point given start point lat/long (numeric degrees), 
 * bearing (numeric degrees) & distance (in m).
 *
 * from: Vincenty direct formula - T Vincenty, "Direct and Inverse Solutions of Geodesics on the 
 *       Ellipsoid with application of nested equations", Survey Review, vol XXII no 176, 1975
 *       http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
 */

SEXP Dest(SEXP latitude1, SEXP longitude1, SEXP bearing, SEXP distance) {
	//bring in the key data
	latitude1 = coerceVector(latitude1, REALSXP); double lat1 = REAL(latitude1)[0] * (PI/180); //first lat in radians
	longitude1 = coerceVector(longitude1, REALSXP); double lon1 = REAL(longitude1)[0] * (PI/180); //first lon in radians
	bearing = coerceVector(bearing, REALSXP); double alpha1 = REAL(bearing)[0] * (PI/180); //bearing in radians
	distance = coerceVector(distance, REALSXP); double s = REAL(distance)[0]; //distance in m
	
	//define all the variables
	double sinAlpha1, cosAlpha1, tanU1, cosU1, sinU1, sigma1, sinAlpha, cosSqAlpha;
	double uSq, A, B, sigma, sigmaP, cos2SigmaM, sinSigma, cosSigma, deltaSigma;
	double tmp, lat2, lambda, C, L, revAz;
	
	//start doing some of the calculations
	sinAlpha1 = sin(alpha1);
	cosAlpha1 = cos(alpha1);
	tanU1 = (1-f) * tan(lat1);
	cosU1 = 1 / sqrt((1 + tanU1*tanU1));
	sinU1 = tanU1*cosU1;
	sigma1 = atan2(tanU1, cosAlpha1);
	sinAlpha = cosU1 * sinAlpha1;
	cosSqAlpha = 1 - sinAlpha*sinAlpha;
	uSq = cosSqAlpha * (a*a - b*b) / (b*b);
	A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
	B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));

	sigma = s / (b*A), sigmaP = 2*PI;
	cos2SigmaM = cos(2*sigma1 + sigma);
	sinSigma = sin(sigma);
	cosSigma = cos(sigma);
	while (fabs(sigma-sigmaP) > 1e-12) {
		cos2SigmaM = cos(2*sigma1 + sigma);
		sinSigma = sin(sigma);
		cosSigma = cos(sigma);
		deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)-B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)));
		sigmaP = sigma;
		sigma = s / (b*A) + deltaSigma;
	}

	tmp = sinU1*sinSigma - cosU1*cosSigma*cosAlpha1;
	lat2 = atan2(sinU1*cosSigma + cosU1*sinSigma*cosAlpha1, (1-f)*sqrt(sinAlpha*sinAlpha + tmp*tmp));
	lambda = atan2(sinSigma*sinAlpha1, cosU1*cosSigma - sinU1*sinSigma*cosAlpha1);
	C = f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha));
	L = lambda - (1-C) * f * sinAlpha * (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)));

	revAz = atan2(sinAlpha, -tmp);  // final bearing
	
	SEXP ans; PROTECT(ans = allocVector(REALSXP, 3));
	REAL(ans)[0] = lat2 * (180 / PI);
	REAL(ans)[1] = lon1 * (180 / PI) + L * (180 / PI);
	REAL(ans)[2] = revAz * (180 / PI);
	
	UNPROTECT(1);
	return(ans);
}

/*
 * Calculates geodetic distance between two points specified by latitude/longitude using 
 * Vincenty inverse formula for ellipsoids
 *
 * @param   {Number} lat1, lon1: first point in decimal degrees
 * @param   {Number} lat2, lon2: second point in decimal degrees
 * @returns (Number} distance in metres between points
 */
SEXP Dist(SEXP latitude1, SEXP longitude1, SEXP latitude2, SEXP longitude2) {
	//bring in the key data
	PROTECT(latitude1 = coerceVector(latitude1, REALSXP)); double *lat1 = REAL(latitude1);
	PROTECT(longitude1 = coerceVector(longitude1, REALSXP)); double *lon1 = REAL(longitude1);
	PROTECT(latitude2 = coerceVector(latitude2, REALSXP)); double *lat2 = REAL(latitude2);
	PROTECT(longitude2 = coerceVector(longitude2, REALSXP)); double *lon2 = REAL(longitude2);
	
	int npnts = length(latitude1); //get the number of points

	//setup the output vector and allocate everything as NA to begin with
	double *out; SEXP ans;
	PROTECT(ans = allocVector(REALSXP, npnts));
	out = REAL(ans); //pointer to output dataset

	//cycle through each of the pairings and return the lengths
	int ii;
	for (ii=0;ii<npnts;ii++) {
		if (lat1[ii] == lat2[ii] && lon1[ii] == lon2[ii]) {
			out[ii] = 0;
		} else {			
			double L = (lon2[ii]-lon1[ii]) * (PI/180);
			double U1 = atan((1-f) * tan(lat1[ii] * (PI/180)));
			double U2 = atan((1-f) * tan(lat2[ii] * (PI/180)));
			double sinU1 = sin(U1), cosU1 = cos(U1);
			double sinU2 = sin(U2), cosU2 = cos(U2);
		  
			double lambda = L, lambdaP, iterLimit = 100;
			double sinLambda, cosLambda, sinSigma, cosSigma, sigma, sinAlpha, cosSqAlpha, cos2SigmaM, C;
			do {
				sinLambda = sin(lambda);
				cosLambda = cos(lambda);
				sinSigma = sqrt((cosU2*sinLambda) * (cosU2*sinLambda) + (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda));
				if (sinSigma==0) return 0;  // co-incident points
				cosSigma = sinU1*sinU2 + cosU1*cosU2*cosLambda;
				sigma = atan2(sinSigma, cosSigma);
				sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma;
				cosSqAlpha = 1 - sinAlpha*sinAlpha;
				cos2SigmaM = cosSigma - 2*sinU1*sinU2/cosSqAlpha;
				if (isnan(cos2SigmaM)) cos2SigmaM = 0;  // equatorial line: cosSqAlpha=0 (§6)
				C = f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha));
				lambdaP = lambda;
				lambda = L + (1-C) * f * sinAlpha * (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)));
			} while (fabs(lambda-lambdaP) > 1e-12 && --iterLimit>0);

			double uSq = cosSqAlpha * (a*a - b*b) / (b*b);
			double A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)));
			double B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)));
			double deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)-B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)));
			double s = b*A*(sigma-deltaSigma);
			if (iterLimit==0) s=-9999;  // formula failed to converge
			out[ii] = s;
		}
	}
	UNPROTECT(5);
	return(ans);
}

