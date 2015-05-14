/*
 * file: runBML.h
 * this file is the interface to an abstraction that implements the BML model using C
 */

#ifndef _runBML_h
#define _runBML_h



/*function: crunBMLGrid
 * this function is the c version of BML model
 */


void crunBMLGrid (double *grid, int *dimension , int *red_loc, int *blue_loc, int *nred , int *nblue, int *numsteps);
#endif
