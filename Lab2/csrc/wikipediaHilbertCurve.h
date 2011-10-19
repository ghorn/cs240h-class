#ifndef __WIKIPEDIA_HILBERT_CURVE_H__
#define __WIKIPEDIA_HILBERT_CURVE_H__

//rotate/flip a quadrant appropriately
void d2xy(int n, int d, int *x, int *y);

//convert (x,y) to d
int xy2d (int n, int x, int y);

#endif // __WIKIPEDIA_HILBERT_CURVE_H__
