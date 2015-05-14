/* Appendix: C source code.
 *file: CrunBML.c
 * this file is the c version of moving cars simultaneously.
 */

#include <stdio.h>
#include "runBML.h"
#include <string.h>


/* private function PROTOTYPE*/
void move_cars(double *grid, int *dimension, int direction, int *location, int *number);


/* main routine*/
void crunBMLGrid (double *grid, int *dimension , int *red_loc, int *blue_loc,int *nred , int *nblue, int *numsteps)

{
    
    int i;
    /* store color */
    int dir;
    
    for (i = 0; i< *numsteps ; i++){
      /* move blue cars*/
        dir = 2;
        move_cars(grid, dimension,dir,blue_loc,nblue);
        /* move red cars*/
        dir = 1;
        move_cars(grid, dimension,dir,red_loc,nred);
    }
    return;
}


void move_cars(double *grid,int *dimension, int direction, int *location, int *number)
{
    int color = direction;
    int i,t,k;
    
    /* generate a array to store index of array represent of the original grid */
    int new_loc[2*number[0]];
    
    for (i=0; i < number[0]; i++){
        if (color == 2) { // move blue cars
        /* for blue cars, move upwards */
            new_loc[i]  = location[i]-1 ;
        /* column won't change */
            new_loc[i + number[0]] = location[i + number[0]];
          /* border: move from top to bottom */
            if (new_loc[i]<1) new_loc[i] = dimension[0];         
        }
        
        else {  /* move red cars*/
            new_loc[i]  = location[i] ;
            /* move to the right */
            new_loc[i + number[0]] = location[i + number[0]]+1;
            /* border : move from left to right */
            if ( new_loc[i + number[0]] > dimension[1]) new_loc[i + number[0]] = 1;
        }
        
    }
    
    /* Check if a car can be moved, if it can be moved, change it's value to -1 indicates it can be moved. */
    
    for(k=0; k < number[0];k++) {
        
        /* Create an index to change location x row into grid index*/
        
        
        int indice = location[k]-1+dimension[0]*(location[k+number[0]]-1);
        int indice_new =new_loc[k]-1+dimension[0]*(new_loc[k+number[0]]-1) ;
        
        /* Change the value of cars that can be moved into -1*/
        
        if (grid[indice_new] == 0) grid[indice] = -1;
    }
    
    /* based on the index, move the cars */
    
    for (t=0; t < number[0];t++) {
        if ( grid[location[t]-1+dimension[0]*(location[t+number[0]]-1)] == -1)
        {    
          /* change the corresponding cars */
            grid[new_loc[t]-1+dimension[0]*(new_loc[t+number[0]]-1)] = color;
            grid[location[t]-1+dimension[0]*(location[t+number[0]]-1)] = 0;
            location[t] = new_loc[t];
            location[t+number[0]] = new_loc[t+number[0]] ;
        }
    }
}




