/* Auto-generated WASM export shims for JShark HVM2 kernels. */
#include <stdint.h>

typedef int64_t jshark_hvm2_i64;

__attribute__((export_name("mandel")))
jshark_hvm2_i64 jshark_hvm2_export_mandel(jshark_hvm2_i64 a0, jshark_hvm2_i64 a1) {
  extern jshark_hvm2_i64 mandel(jshark_hvm2_i64, jshark_hvm2_i64);
  return mandel(a0, a1);
}


