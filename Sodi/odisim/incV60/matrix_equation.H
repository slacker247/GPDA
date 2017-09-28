//...... matrix equation header file

#ifndef matequ_obj
#define matequ_obj

//typedef long double BIG_FLOAT;
typedef double BIG_FLOAT;

int matrix_equation(BIG_FLOAT **A, BIG_FLOAT *B, BIG_FLOAT *X, int n);
void iterate(BIG_FLOAT **A, BIG_FLOAT *B, BIG_FLOAT *X, int n);
void full_pivot(BIG_FLOAT **A, BIG_FLOAT *B, int *X, int n, int index);
void swap_rows(BIG_FLOAT **A, BIG_FLOAT *X, int n);
void swap_columns(BIG_FLOAT **A, int *X, int column1, int column2, int n);
int verify(BIG_FLOAT **A, BIG_FLOAT *X, BIG_FLOAT *B, int n);

//...... precision checking definitions

class C_DOUBLE {
  public:
    BIG_FLOAT value;
    BIG_FLOAT *ptr;
};

#define DOUBLE_CHECK dbl; dbl += 

void operator += (BIG_FLOAT &v1, C_DOUBLE &dbl);
void operator += (C_DOUBLE &dbl, BIG_FLOAT &v2);

#endif
