//...... matrix equation source file solves the matrix equation AX = B

//#define TEST_MATRIX_EQUATION
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <iostream.h>

#define SUMVAL 1.0e-12
#define RATVAL 1.0e-12

#include "matrix_equation.H"
#undef DOUBLE_CHECK
#define DOUBLE_CHECK

//...... operator overloaded functions

void operator += (BIG_FLOAT &v1, C_DOUBLE &dbl) {
  dbl.ptr = &v1;
}

void operator += (C_DOUBLE &dbl, BIG_FLOAT &v2) {
  BIG_FLOAT *v1;
  BIG_FLOAT temp;

  v1 = dbl.ptr;

  if (*v1 != 0.0) {

    temp = fabs(*v1 + v2) / (fabs(*v1) + fabs(v2));
    if (temp < SUMVAL) {
      cerr << "Warning, precision problem |v1+v2| / (|v1|+|v2|) = "
	   << (double)temp << "\n";
    }

    if (v2 != 0.0) {
      temp = fabs(v2/(*v1));
      if ((temp < RATVAL) || (temp > 1.0/RATVAL)) {
        cerr << "Warning, precision problem (v2/v1) = " << (double)temp << "\n";
      }
    }

  }

  *v1 += v2;

}

/************************************************************************
* print_matrix - prints out the matrix A                                *
************************************************************************/
void print_matrix(BIG_FLOAT **a, int n) {
  int i,j;

  for (i=0; i<n; i++) {
    for (j=0; j<n; j++) {
      cerr << (double)a[i][j] << " ";
    }
    cerr << "\n";
  }
  cerr << "\n";

}

/************************************************************************
* verify the matrix equation AX = B                                     *
************************************************************************/
int verify(BIG_FLOAT **A, BIG_FLOAT *X, BIG_FLOAT *B, int n) {
  int i,j;
  BIG_FLOAT temp;
  int retval;

  retval = 1;

  for (i=0; i<n; i++) {

    temp = -B[i];
    for (j=0; j<n; j++) {
      temp += A[i][j]*X[j];
    }

    if (fabs(temp/(fabs(B[i])+10.0)) > 1.0e-3) {
      cerr << "Warning, verify comes up with " << (double)temp << " for row "
	   << i << "\n";
      retval = 0;
    }else{
//      fprintf(stderr, "Good, verify comes up with %f for row %d\n",temp,i);
    }

  }

  return retval;

}

/************************************************************************
* swap_rows - swap the rows in the matrix equation                      *
************************************************************************/
void swap_rows(BIG_FLOAT **A, BIG_FLOAT *X, int n) {
  int i,j,k;
  BIG_FLOAT temp;

  for (i=0; i<n/2; i++) {

    k = n-i-1;

//...... swap the A matrix

    for (j=0; j<n; j++) {
      temp = A[j][i];
      A[j][i] = A[j][k];
      A[j][k] = temp;
    }

//...... swap the X vector

    temp = X[i];
    X[i] = X[k];
    X[k] = temp;

  }


}

/************************************************************************
* swap_columns - swap the columns in the matrix equation                *
************************************************************************/
void swap_columns(BIG_FLOAT **A, int *X, int column1, int column2, int n) {
  int j;
  BIG_FLOAT temp;
  int itemp;

//...... swap the columns in the A matrix

  for (j=0; j<n; j++) {
    temp = A[j][column1];
    A[j][column1] = A[j][column2];
    A[j][column2] = temp;
  }

//...... swap the X vector

  itemp = X[column1];
  X[column1] = X[column2];
  X[column2] = itemp;

}


/************************************************************************
* matrix_equation - solves the matrix equation AX = B                   *
************************************************************************/
int matrix_equation(BIG_FLOAT **A, BIG_FLOAT *B, BIG_FLOAT *X, int n) {
  int i,j,k;
  BIG_FLOAT **a;
  BIG_FLOAT *b;
  int *Xindex;
  BIG_FLOAT *Xtemp;
  BIG_FLOAT temp;
  //C_DOUBLE dbl;
  int print_flag;

//...... set up the print flag

  print_flag = 0;

//...... create temporary storage

  a = new BIG_FLOAT*[n];
  for (i=0; i<n; i++) a[i] = new BIG_FLOAT[n];

  b = new BIG_FLOAT[n];
  Xtemp = new BIG_FLOAT[n];
  Xindex = new int[n];
  for (i=0; i<n; i++) {
    Xindex[i] = i;
    Xtemp[i] = 0.0;
  }

  for (i=0; i<n; i++) {
    for (j=0; j<n; j++) {
      a[i][j] = A[i][j];
    }
    b[i] = B[i];
  }

//...... loop over the rows

  for (i=0; i<n; i++) {

//...... pivot first

    full_pivot(a,b,Xindex,n,i);

//...... normalize the row

    temp = 1.0/a[i][i];
    a[i][i] = 1.0;
    for (j=i+1; j<n; j++) a[i][j] *= temp;
    b[i] *= temp;

//...... subract the ith row from everybody to put a zero in the ith column

    for (j=i+1; j<n; j++) {
      temp = a[j][i];
      a[j][i] = 0.0;
      for (k=i+1; k<n; k++) {
	a[j][k] += DOUBLE_CHECK (-temp*a[i][k]);
      }
      b[j] += DOUBLE_CHECK (-temp*b[i]);
    }

    if (print_flag) {
      print_matrix(a,n);
    }

  }

//...... solve for X

  for (i=n-1; i>=0; i--) {
    Xtemp[i] = 0.0;
//    for (j=i+1; j<n; j++) {
    for (j=n-1; j>i; j--) {
      Xtemp[i] += DOUBLE_CHECK (-a[i][j]*Xtemp[j]);
    }
    Xtemp[i] += DOUBLE_CHECK (b[i]);
  }

  for (i=0; i<n; i++) {
    X[Xindex[i]] = Xtemp[i];
  }

//...... verify that we have the correct solution

  if (!verify(A,X,B,n)) {
    fprintf(stderr,"Error, matrix equation not solved\n");
//    print_matrix(A,n);
  }

//...... delete temporary storage

  for (i=0; i<n; i++) delete [] a[i];  //RVI 2/18/98 
  delete [] a; //RVI 2/18/98 
  delete [] b; //RVI 2/18/98 
  delete [] Xtemp; //RVI 2/18/98 
  delete [] Xindex; //RVI 2/18/98 

//...... return a successful status

  return 1;

}

/************************************************************************
* iterate the solution to improve its accuracy                          *
************************************************************************/
void iterate(BIG_FLOAT **A, BIG_FLOAT *B, BIG_FLOAT *X, int n) {
  BIG_FLOAT *dB;
  BIG_FLOAT *dX;
  int i,j;

//...... create dB and dX

  dB = new BIG_FLOAT[n];
  dX = new BIG_FLOAT[n];
  if ((dB == NULL) || (dX == NULL)) {
    fprintf(stderr,"Warning, not enough memory to iterate matrix solution\n");
    return;
  }else{
    for (i=0; i<n; i++) {
      dB[i] = 0.0;
      dX[i] = 0.0;
    }
  }

  for (i=0; i<n; i++) {
    for (j=0; j<n; j++) {
      dB[i] += A[i][j]*X[j];
    }
    dB[i] -= B[i];
  }

//...... solve the new equation

  matrix_equation(A,dB,dX,n);
  for (i=0; i<n; i++) X[i] -= dX[i];

//...... cleanup the memory

  delete [] dB; //RVI 2/18/98 
  delete [] dX; //RVI 2/18/98 

}

/************************************************************************
* matrix_equation - for 12 x 12 matrix A[12][12]                        *
************************************************************************/
int matrix_equation(BIG_FLOAT A[12][12], BIG_FLOAT *B, BIG_FLOAT *X, int n) {
  BIG_FLOAT **a;
  int i,j;
  int retval;

  fprintf(stderr,"In 12x12 matrix routine\n");


//......create matrix

  a = new BIG_FLOAT*[12];
  for (i=0; i<n; i++) a[i] = new BIG_FLOAT[n];

//...... copy matrix

  for (i=0; i<n; i++) {
    for (j=0; j<n; j++) {
      a[i][j] = A[i][j];
    }
  }

//...... call the matrix equation

  retval = matrix_equation(a,B,X,n);

//...... delete matrix

  for (i=0; i<n; i++) delete [] a[i]; //RVI 2/18/98 
  delete [] a; //RVI 2/18/98

//...... return from subroutine

  return retval;

}


/************************************************************************
* full pivot					 			*
************************************************************************/
void full_pivot(BIG_FLOAT **A, BIG_FLOAT *B, int *X, int n, int index) {
  int i,j;
  int row, col;
  BIG_FLOAT temp;

//...... normalize the equations so that B is 1 (or if B = 0, use max A)

  for (i=index; i<n; i++) {

    temp = B[i];

    if (temp == 0.0) {
//      fprintf(stderr,"Warning, B[%d] = 0.0 so using A...\n",i);
      for (j=index; j<n; j++) {
	if (fabs(temp) < fabs(A[i][j])) temp = A[i][j];
      }
    }

    if (temp != 0.0) {
      for (j=index; j<n; j++) {
        A[i][j] /= temp;
      }
      B[i] /= temp;
    }

  }

//...... find the maximum element in the matrix

  temp = A[index][index];
  row = index;
  col = index;
  for (i=index; i<n; i++) {
    for (j=index; j<n; j++) {
      if (fabs(temp) < fabs(A[i][j])) {
	row = i;
	col = j;
	temp = A[i][j];
      }
    }
  }

//...... swap the pivot row

  if (row != index) {
    for (j=index; j<n; j++) {
      temp = A[index][j];
      A[index][j] = A[row][j];
      A[row][j] = temp;
    }
    temp = B[index];
    B[index] = B[row];
    B[row] = temp;
  }

//...... swap the pivot column

  if (col != index) {
    swap_columns(A,X,index,col,n);
  }

}



#ifdef TEST_MATRIX_EQUATION

#include "random.H"
#define TESTDIM 12
int main() {
  int i,j,k;
  BIG_FLOAT A[TESTDIM][TESTDIM];
  BIG_FLOAT B[TESTDIM];
  BIG_FLOAT X[TESTDIM];
  C_RANDOM random;
  int status;
  long cpu;
  BIG_FLOAT time;

  for (i=0; i<TESTDIM; i++) {
    for (j=0; j<TESTDIM; j++) {
      A[i][j] = random.get_random_float();
    }
    B[i] = random.get_random_float();
  }

  cpu = clock();
  for (k=0; k<1000; k++) {
    status = matrix_equation(A,B,X,TESTDIM);
  }
  cpu = clock() - cpu;
  time = (BIG_FLOAT(cpu)/1000000.0) / k;
  fprintf(stderr,"Time for solving %d x %d matrix = %f seconds\n",
	TESTDIM,TESTDIM,time);

}

#endif
