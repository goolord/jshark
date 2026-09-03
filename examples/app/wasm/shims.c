/* WASM exports matching examples/Hvm2Demo/Kernels.hs (keep in sync). */
#include <stdint.h>

#define MANDEL_MAX_ITER 64 /* must match Kernels.maxIter */

typedef int64_t jshark_hvm2_i64;

static double i64_to_f64(jshark_hvm2_i64 x) {
  union {
    jshark_hvm2_i64 i;
    double d;
  } u;
  u.i = x;
  return u.d;
}

static jshark_hvm2_i64 mandel_iter(double cr, double ci) {
  int n = 0;
  double zr = 0.0;
  double zi = 0.0;
  while (n < MANDEL_MAX_ITER && (zr * zr + zi * zi) < 4.0) {
    double nzr = zr * zr - zi * zi + cr;
    double nzi = 2.0 * zr * zi + ci;
    zr = nzr;
    zi = nzi;
    n++;
  }
  return (jshark_hvm2_i64)n;
}

jshark_hvm2_i64 mandel(jshark_hvm2_i64 cr_i, jshark_hvm2_i64 ci_i) {
  return mandel_iter(i64_to_f64(cr_i), i64_to_f64(ci_i));
}

/* Browser-fast path: f64 args avoid BigInt per sample (see Client sampleGrid). */
__attribute__((export_name("mandel_f64")))
double mandel_f64(double cr, double ci) {
  return (double)mandel_iter(cr, ci);
}
