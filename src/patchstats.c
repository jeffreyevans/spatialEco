/*
this is code to calculate patch-based landscape statistics
*/
#include <R.h> 
#include <Rinternals.h>

//global variables
extern int nrow, ncol;
extern int *data; 

/* 
tdata is a matrix of data with patches uniquely numbered
IDs are the unique patch id values
AREAS is the area of the cell in geographic coordinate systems
TOPS, BOTTOMS AND SIDES	are the lengths of cell perimeters in geographic coordinate systems
*/
//this is specific to projected coordinate systmes
SEXP projectedPS(SEXP tdata, SEXP IDs) 
	{
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, INTSXP));
	data = INTEGER(tdata); //this is a raster matrix of patches
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    nrow = dims[0]; ncol = dims[1]; //assign the number of rows and columns in the matrix
	//define patch ids
	PROTECT(IDs = coerceVector(IDs, INTSXP));
	int *ID = INTEGER(IDs); //this is the unique IDs of the patches
	int npatch = length(IDs);
	
	//setup temporary outputs
	SEXP ncells, ncellscore, nperimeters, ninternals;
	PROTECT(ncells = allocVector(INTSXP, npatch)); int *ncell = INTEGER(ncells); //number of cells per patch
	PROTECT(ncellscore = allocVector(INTSXP, npatch)); int *ncellcore = INTEGER(ncellscore); //number of core cells (core in 8 directions)	
	PROTECT(nperimeters = allocVector(INTSXP, npatch)); int *nperim = INTEGER(nperimeters); //number of edges on teh perimeter
	PROTECT(ninternals = allocVector(INTSXP, npatch)); int *nintern = INTEGER(ninternals);  // number of same patch shared edges

	//int ncell[npatch], ncellcore[npatch], nperim[npatch], nintern[npatch];
	//set everything to 0
	int ii,row,col; 
	for (ii=0;ii<npatch;ii++) ncell[ii] = nperim[ii] = nintern[ii] = ncellcore[ii] = 0;
	
	//work with the data
	//get the area and associated metrics
	int np,ni,core; //temporary values representing nperim, nintern
	int tval, rook[4], queen[4]; //values of the 9 cells of interest 
	/*
	queen[3],rook[0],queen[0]
	rook[3],tval,rook[1]
	queen[2],rook[2],queen[1]
	*/
	for (row=0; row<nrow; row++)	{
		for (col=0; col<ncol; col++)	{	
			tval = data[row+nrow*col];
			if (tval!=NA_INTEGER)	{
				np = ni = 0;
				//go through the eight neighbouring cells and collect data
				rook[0] = (row>0) ? data[(row-1)+nrow*(col)]:-9999;
				rook[1] = (col<ncol-1) ? data[(row)+nrow*(col+1)]:-9999;
				rook[2] = (row<nrow-1) ? data[(row+1)+nrow*(col)]:-9999;
				rook[3] = (col>0) ? data[(row)+nrow*(col-1)]:-9999;
				queen[0] = (row>0 && col<ncol-1) ? data[(row-1)+nrow*(col+1)]:-9999;
				queen[1] = (row<nrow-1 && col<ncol-1) ? data[(row+1)+nrow*(col+1)]:-9999;
				queen[2] = (row<nrow-1 && col>0) ? data[(row+1)+nrow*(col-1)]:-9999;
				queen[3] = (row>0 && col>0) ? data[(row-1)+nrow*(col-1)]:-9999;
				//cycle through and get temp values of edges
				for (ii=0;ii<4;ii++){if (tval==rook[ii]){ni++;} else {np++;}};
				//check if cell is acore cell
				core=1;	if (np==0){core=0;for (ii=0;ii<4;ii++){if (tval!=queen[ii]) core++;}}
				//assign the values to the proper patch id info
				for (ii=0;ii<npatch;ii++){
					if (ID[ii]==tval){
						ncell[ii] ++;
						nperim[ii] += np;
						nintern[ii] += ni;
						if (core==0) ncellcore[ii] ++;
						break;
					}
				}				
			}
		}
	}
	//setup the output matrix
	SEXP ans;
	PROTECT(ans = allocMatrix(INTSXP, npatch, 5)); int *out = INTEGER(ans); //pointer to output dataset
	for (row=0; row<npatch; row++)	{
		out[row] = ID[row];
		out[row + npatch] = ncell[row];
		out[row + npatch*2] = ncellcore[row];
		out[row + npatch*3] = nperim[row];
		out[row + npatch*4] = nintern[row];
	}

	//return the output data
	UNPROTECT(7);
    return(ans); 
}

//this is specific to geographic coordinate systems
SEXP geographicPS(SEXP tdata, SEXP IDs, SEXP AREAS, SEXP TOPS, SEXP BOTTOMS, SEXP SIDES) 
	{
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, INTSXP));
	data = INTEGER(tdata); //this is a raster matrix of patches
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    nrow = dims[0]; ncol = dims[1]; //assign the number of rows and columns in the matrix
	//define patch ids
	PROTECT(IDs = coerceVector(IDs, REALSXP));
	double *ID = REAL(IDs); //this is the unique IDs of the patches
	int npatch = length(IDs);
	//define the area and perimeter information
	PROTECT(AREAS = coerceVector(AREAS, REALSXP)); double *areas = REAL(AREAS); //get a pointer to the areas
	PROTECT(TOPS = coerceVector(TOPS, REALSXP)); double *tops = REAL(TOPS); //get a pointer to the length of the top of the cells
	PROTECT(BOTTOMS = coerceVector(BOTTOMS, REALSXP)); double *bottoms = REAL(BOTTOMS); //get a pointer to the length of the bottom of the cells
	PROTECT(SIDES = coerceVector(SIDES, REALSXP)); double *sides = REAL(SIDES); //get a pointer to the length of the sides of the cells
		
	//setup temporary outputs
	SEXP ncells, ncellscore, nperimeters, ninternals, outareas, outareascore, outperimeters;
	PROTECT(ncells = allocVector(REALSXP, npatch)); double *ncell = REAL(ncells); //number of cells per patch
	PROTECT(ncellscore = allocVector(REALSXP, npatch)); double *ncellcore = REAL(ncellscore); //number of core cells (core in 8 directions)	
	PROTECT(nperimeters = allocVector(REALSXP, npatch)); double *nperim = REAL(nperimeters); //number of edges on teh perimeter
	PROTECT(ninternals = allocVector(REALSXP, npatch)); double *nintern = REAL(ninternals);  // number of same patch shared edges
	PROTECT(outareas = allocVector(REALSXP, npatch)); double *outarea = REAL(outareas); //areas per patch
	PROTECT(outareascore = allocVector(REALSXP, npatch)); double *outareacore = REAL(outareascore); //core area cells (core in 8 directions)	
	PROTECT(outperimeters = allocVector(REALSXP, npatch)); double *outperim = REAL(outperimeters); //length of perimeter
	//set everything to 0
	int ii,row,col; 
	for (ii=0;ii<npatch;ii++) { ncell[ii] = nperim[ii] = nintern[ii] = ncellcore[ii] = outarea[ii] = outareacore[ii] = outperim[ii] = 0.0; }
	
	//work with the data
	//get the area and associated metrics
	double np,ni,core,perim; //temporary values representing nperim, nintern
	int tval, rook[4], queen[4]; //values of the 9 cells of interest 
	/*
	queen[3],rook[0],queen[0]
	rook[3],tval,rook[1]
	queen[2],rook[2],queen[1]
	*/
	for (row=0; row<nrow; row++)	{
		for (col=0; col<ncol; col++)	{
			tval = data[row+nrow*col];
			if (tval!=NA_INTEGER)	{
				np = ni = 0; perim = 0.0;
				//go through the eight neighbouring cells and collect data
				rook[0] = (row>0) ? data[(row-1)+nrow*(col)]:-9999;
				rook[1] = (col<ncol-1) ? data[(row)+nrow*(col+1)]:-9999;
				rook[2] = (row<nrow-1) ? data[(row+1)+nrow*(col)]:-9999;
				rook[3] = (col>0) ? data[(row)+nrow*(col-1)]:-9999;
				queen[0] = (row>0 && col<ncol-1) ? data[(row-1)+nrow*(col+1)]:-9999;
				queen[1] = (row<nrow-1 && col<ncol-1) ? data[(row+1)+nrow*(col+1)]:-9999;
				queen[2] = (row<nrow-1 && col>0) ? data[(row+1)+nrow*(col-1)]:-9999;
				queen[3] = (row>0 && col>0) ? data[(row-1)+nrow*(col-1)]:-9999;
				//cycle through and get temp values of edges
				if (tval==rook[0]) { ni++; } else { np++;perim+=sides[col]; }
				if (tval==rook[1]) { ni++; } else { np++;perim+=tops[col]; }
				if (tval==rook[2]) { ni++; } else { np++;perim+=sides[col]; }
				if (tval==rook[3]) { ni++; } else { np++;perim+=bottoms[col]; }
				//check if cell is acore cell
				core=1;	if (np==0){core=0;for (ii=0;ii<4;ii++){if (tval!=queen[ii]) core++;}}
				//assign the values to the proper patch id info
				for (ii=0;ii<npatch;ii++){
					if (ID[ii]==tval){
						ncell[ii] ++; nperim[ii] += np; nintern[ii] += ni;
						outarea[ii] += areas[col]; //printf('%f',outarea[ii]);
						outperim[ii] += perim;
						if (core==0) { ncellcore[ii] ++; outareacore[ii]+=areas[col]; }
						break;
					}
				}				
			}
		}
	}
	//setup the output matrix
	SEXP ans;
	PROTECT(ans = allocMatrix(REALSXP, npatch, 8)); double *out = REAL(ans); //pointer to output dataset
	for (row=0; row<npatch; row++)	{
		out[row] = ID[row];
		out[row + npatch] = ncell[row];
		out[row + npatch*2] = ncellcore[row];
		out[row + npatch*3] = nperim[row];
		out[row + npatch*4] = nintern[row];
		out[row + npatch*5] = outarea[row];
		out[row + npatch*6] = outareacore[row];
		out[row + npatch*7] = outperim[row];
	}

	//return the output data
	UNPROTECT(14);
    return(ans); 
}

