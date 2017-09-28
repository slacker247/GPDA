/* pi.h
 * Joel N. Weber II  <nemo@koa.iolani.honolulu.hi.us>  6/12/96
 * Public Domain
 *
 * Just a nice little hack to try to get around Solaris's refusal to define
 * M_PI.  Please include this file in any .w files that use M_PI _AFTER_
 * <math.h>.  If you aren't using wbuild, you can probably just copy
 * the following lines directly into your widget; I don't know how to
 * get wbuild to handle #ifndef
 */

#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif

