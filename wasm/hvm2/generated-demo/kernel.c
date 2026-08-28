#include "hvm2_wasm.h"

#include <math.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG
  #define debug(...) fprintf(stderr, __VA_ARGS__)
#else
  #define debug(...)
#endif

#define COMPILED

// Types
// --------

typedef  uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef  int32_t i32;
typedef    float f32;
typedef   double f64;

typedef  _Atomic(u8)  a8;
typedef _Atomic(u16) a16;
typedef _Atomic(u32) a32;
typedef _Atomic(u64) a64;

// Configuration
// -------------

// Threads per CPU
#ifndef TPC_L2
#ifndef TPC_L2
#define TPC_L2 2
#endif
#endif
#define TPC (1ul << TPC_L2)

// Types
// -----

// Local Types
typedef u8  Tag;  // Tag  ::= 3-bit (rounded up to u8)
typedef u32 Val;  // Val  ::= 29-bit (rounded up to u32)
typedef u32 Port; // Port ::= Tag + Val (fits a u32)
typedef u64 Pair; // Pair ::= Port + Port (fits a u64)

typedef a32 APort; // atomic Port
typedef a64 APair; // atomic Pair

// Rules
typedef u8 Rule; // Rule ::= 3-bit (rounded up to 8)

// Numbs
typedef u32 Numb; // Numb ::= 29-bit (rounded up to u32)

// Tags
#define VAR 0x0 // variable
#define REF 0x1 // reference
#define ERA 0x2 // eraser
#define NUM 0x3 // number
#define CON 0x4 // constructor
#define DUP 0x5 // duplicator
#define OPR 0x6 // operator
#define SWI 0x7 // switch

// Interaction Rule Values
#define LINK 0x0
#define CALL 0x1
#define VOID 0x2
#define ERAS 0x3
#define ANNI 0x4
#define COMM 0x5
#define OPER 0x6
#define SWIT 0x7

// Numbers
static const f32 U24_MAX = (f32) (1 << 24) - 1;
static const f32 U24_MIN = 0.0;
static const f32 I24_MAX = (f32) (1 << 23) - 1;
static const f32 I24_MIN = (f32) (i32) ((-1u) << 23);
#define TY_SYM 0x00
#define TY_U24 0x01
#define TY_I24 0x02
#define TY_F24 0x03
#define OP_ADD 0x04
#define OP_SUB 0x05
#define FP_SUB 0x06
#define OP_MUL 0x07
#define OP_DIV 0x08
#define FP_DIV 0x09
#define OP_REM 0x0A
#define FP_REM 0x0B
#define OP_EQ  0x0C
#define OP_NEQ 0x0D
#define OP_LT  0x0E
#define OP_GT  0x0F
#define OP_AND 0x10
#define OP_OR  0x11
#define OP_XOR 0x12
#define OP_SHL 0x13
#define FP_SHL 0x14
#define OP_SHR 0x15
#define FP_SHR 0x16

// Constants
#define FREE 0x00000000
#define ROOT ((Port)((G_VARS_LEN - 1) << 3)) // last var slot (shrunk)
#define NONE 0xFFFFFFFF

// Cache Padding
#define CACHE_PAD 64

// Global Net
#define HLEN (1ul << 16) // max 16k high-priority redexes
#define RLEN (1ul << 18) // max 16m low-priority redexes
#define G_NODE_LEN (1ul << 23) // max 536m nodes
#define G_VARS_LEN (1ul << 23) // max 536m vars
#define G_RBAG_LEN (TPC * RLEN)

typedef struct Net {
  APair node_buf[G_NODE_LEN]; // global node buffer
  APort vars_buf[G_VARS_LEN]; // global vars buffer
  APair rbag_buf[G_RBAG_LEN]; // global rbag buffer
  a64 itrs; // interaction count
  a32 idle; // idle thread counter
} Net;

#define DEF_RBAG_LEN 0xFFF
#define DEF_NODE_LEN 0xFFF

// Top-Level Definition
typedef struct Def {
  char name[256];
  bool safe;
  u32  rbag_len;
  u32  node_len;
  u32  vars_len;
  Port root;
  Pair node_buf[DEF_NODE_LEN];
  Pair rbag_buf[DEF_RBAG_LEN];
} Def;

typedef struct Book Book;

// A Foreign Function
typedef struct {
  char name[256];
  Port (*func)(Net*, Book*, Port);
} FFn;

// Book of Definitions
typedef struct Book {
  u32 defs_len;
  Def defs_buf[32];
  u32 ffns_len;
  FFn ffns_buf[32];
} Book;

// Local Thread Memory
typedef struct TM {
  u32  tid; // thread id
  u32  itrs; // interaction count
  u32  nput; // next node allocation attempt index
  u32  vput; // next vars allocation attempt index
  u32  hput; // next hbag push index
  u32  rput; // next rbag push index
  u32  sidx; // steal index
  u32  nloc[0xFFF]; // global node allocation indices
  u32  vloc[0xFFF]; // global vars allocation indices
  Pair hbag_buf[HLEN]; // high-priority redexes
} TM;

// Debugger
// --------

typedef struct {
  char x[13];
} Show;

void put_u16(char* B, u16 val);
Show show_port(Port port);
Show show_rule(Rule rule);
void print_net(Net* net);
void pretty_print_numb(Numb word);
void pretty_print_port(Net* net, Book* book, Port port);

// Port: Constructor and Getters
// -----------------------------

static inline Port new_port(Tag tag, Val val) {
  return (val << 3) | tag;
}

static inline Tag get_tag(Port port) {
  return port & 7;
}

static inline Val get_val(Port port) {
  return port >> 3;
}

// Pair: Constructor and Getters
// -----------------------------

static inline const Pair new_pair(Port fst, Port snd) {
  return ((u64)snd << 32) | fst;
}

static inline Port get_fst(Pair pair) {
  return pair & 0xFFFFFFFF;
}

static inline Port get_snd(Pair pair) {
  return pair >> 32;
}

Pair set_par_flag(Pair pair) {
  Port p1 = get_fst(pair);
  Port p2 = get_snd(pair);
  if (get_tag(p1) == REF) {
    return new_pair(new_port(get_tag(p1), get_val(p1) | 0x10000000), p2);
  } else {
    return pair;
  }
}

Pair clr_par_flag(Pair pair) {
  Port p1 = get_fst(pair);
  Port p2 = get_snd(pair);
  if (get_tag(p1) == REF) {
    return new_pair(new_port(get_tag(p1), get_val(p1) & 0xFFFFFFF), p2);
  } else {
    return pair;
  }
}

bool get_par_flag(Pair pair) {
  Port p1 = get_fst(pair);
  if (get_tag(p1) == REF) {
    return (get_val(p1) >> 28) == 1;
  } else {
    return false;
  }
}

// Utils
// -----

// Swaps two ports.
static inline void swap(Port *a, Port *b) {
  Port x = *a; *a = *b; *b = x;
}

static inline u32 min(u32 a, u32 b) {
  return (a < b) ? a : b;
}

static inline f32 clamp(f32 x, f32 min, f32 max) {
  const f32 t = x < min ? min : x;
  return (t > max) ? max : t;
}

// A simple spin-wait barrier using atomic operations
a64 a_reached = 0; // number of threads that reached the current barrier
a64 a_barrier = 0; // number of barriers passed during this program
void sync_threads() {
  u64 barrier_old = atomic_load_explicit(&a_barrier, memory_order_relaxed);
  if (atomic_fetch_add_explicit(&a_reached, 1, memory_order_relaxed) == (TPC - 1)) {
    // Last thread to reach the barrier resets the counter and advances the barrier
    atomic_store_explicit(&a_reached, 0, memory_order_relaxed);
    atomic_store_explicit(&a_barrier, barrier_old + 1, memory_order_release);
  } else {
    u32 tries = 0;
    while (atomic_load_explicit(&a_barrier, memory_order_acquire) == barrier_old) {
      sched_yield();
    }
  }
}

// Global sum function
static a32 GLOBAL_SUM = 0;
u32 global_sum(u32 x) {
  atomic_fetch_add_explicit(&GLOBAL_SUM, x, memory_order_relaxed);
  sync_threads();
  u32 sum = atomic_load_explicit(&GLOBAL_SUM, memory_order_relaxed);
  sync_threads();
  atomic_store_explicit(&GLOBAL_SUM, 0, memory_order_relaxed);
  return sum;
}

// TODO: write a time64() function that returns the time as fast as possible as a u64
static inline u64 time64() { return 0; }

// Ports / Pairs / Rules
// ---------------------

// True if this port has a pointer to a node.
static inline bool is_nod(Port a) {
  return get_tag(a) >= CON;
}

// True if this port is a variable.
static inline bool is_var(Port a) {
  return get_tag(a) == VAR;
}

// Given two tags, gets their interaction rule.
static inline Rule get_rule(Port a, Port b) {
  const u8 table[8][8] = {
    //VAR  REF  ERA  NUM  CON  DUP  OPR  SWI
    {LINK,LINK,LINK,LINK,LINK,LINK,LINK,LINK}, // VAR
    {LINK,VOID,VOID,VOID,CALL,CALL,CALL,CALL}, // REF
    {LINK,VOID,VOID,VOID,ERAS,ERAS,ERAS,ERAS}, // ERA
    {LINK,VOID,VOID,VOID,ERAS,ERAS,OPER,SWIT}, // NUM
    {LINK,CALL,ERAS,ERAS,ANNI,COMM,COMM,COMM}, // CON
    {LINK,CALL,ERAS,ERAS,COMM,ANNI,COMM,COMM}, // DUP
    {LINK,CALL,ERAS,OPER,COMM,COMM,ANNI,COMM}, // OPR
    {LINK,CALL,ERAS,SWIT,COMM,COMM,COMM,ANNI}, // SWI
  };
  return table[get_tag(a)][get_tag(b)];
}

// Same as above, but receiving a pair.
static inline Rule get_pair_rule(Pair AB) {
  return get_rule(get_fst(AB), get_snd(AB));
}

// Should we swap ports A and B before reducing this rule?
static inline bool should_swap(Port A, Port B) {
  return get_tag(B) < get_tag(A);
}

// Gets a rule's priority
static inline bool is_high_priority(Rule rule) {
  // TODO: this needs to be more readable
  return (bool)((0b00011101 >> rule) & 1);
}

// Adjusts a newly allocated port.
static inline Port adjust_port(Net* net, TM* tm, Port port) {
  Tag tag = get_tag(port);
  Val val = get_val(port);
  if (is_nod(port)) return new_port(tag, tm->nloc[val]);
  if (is_var(port)) return new_port(tag, tm->vloc[val]);
  return new_port(tag, val);
}

// Adjusts a newly allocated pair.
static inline Pair adjust_pair(Net* net, TM* tm, Pair pair) {
  Port p1 = adjust_port(net, tm, get_fst(pair));
  Port p2 = adjust_port(net, tm, get_snd(pair));
  return new_pair(p1, p2);
}

// Numbs
// -----

// Constructor and getters for SYM (operation selector)
static inline Numb new_sym(u32 val) {
  return (val << 5) | TY_SYM;
}

static inline u32 get_sym(Numb word) {
  return (word >> 5);
}

// Constructor and getters for U24 (unsigned 24-bit integer)
static inline Numb new_u24(u32 val) {
  return (val << 5) | TY_U24;
}

static inline u32 get_u24(Numb word) {
  return word >> 5;
}

// Constructor and getters for I24 (signed 24-bit integer)
static inline Numb new_i24(i32 val) {
  return ((u32)val << 5) | TY_I24;
}

static inline i32 get_i24(Numb word) {
  return ((i32)word) << 3 >> 8;
}

// Constructor and getters for F24 (24-bit float)
static inline Numb new_f24(float val) {
  u32 bits = *(u32*)&val;
  u32 shifted_bits = bits >> 8;
  u32 lost_bits = bits & 0xFF;
  // round ties to even
  shifted_bits += (!isnan(val)) & ((lost_bits - ((lost_bits >> 7) & !shifted_bits)) >> 7);
  // ensure NaNs don't become infinities
  shifted_bits |= isnan(val);
  return (shifted_bits << 5) | TY_F24;
}

static inline float get_f24(Numb word) {
  u32 bits = (word << 3) & 0xFFFFFF00;
  return *(float*)&bits;
}

// Flip flag
static inline Tag get_typ(Numb word) {
  return word & 0x1F;
}

static inline bool is_num(Numb word) {
  return get_typ(word) >= TY_U24 && get_typ(word) <= TY_F24;
}

static inline bool is_cast(Numb word) {
  return get_typ(word) == TY_SYM && get_sym(word) >= TY_U24 && get_sym(word) <= TY_F24;
}

// Partial application
static inline Numb partial(Numb a, Numb b) {
  return (b & ~0x1F) | get_sym(a);
}

// Cast a number to another type.
// The semantics are meant to spiritually resemble rust's numeric casts:
// - i24 <-> u24: is just reinterpretation of bits
// - f24  -> i24,
//   f24  -> u24: casts to the "closest" integer representing this float,
//                saturating if out of range and 0 if NaN
// - i24  -> f24,
//   u24  -> f24: casts to the "closest" float representing this integer.
static inline Numb cast(Numb a, Numb b) {
  if (get_sym(a) == TY_U24 && get_typ(b) == TY_U24) return b;
  if (get_sym(a) == TY_U24 && get_typ(b) == TY_I24) {
    // reinterpret bits
    i32 val = get_i24(b);
    return new_u24(*(u32*) &val);
  }
  if (get_sym(a) == TY_U24 && get_typ(b) == TY_F24) {
    f32 val = get_f24(b);
    if (isnan(val)) {
      return new_u24(0);
    }
    return new_u24((u32) clamp(val, U24_MIN, U24_MAX));
  }

  if (get_sym(a) == TY_I24 && get_typ(b) == TY_U24) {
    // reinterpret bits
    u32 val = get_u24(b);
    return new_i24(*(i32*) &val);
  }
  if (get_sym(a) == TY_I24 && get_typ(b) == TY_I24) return b;
  if (get_sym(a) == TY_I24 && get_typ(b) == TY_F24) {
    f32 val = get_f24(b);
    if (isnan(val)) {
      return new_i24(0);
    }
    return new_i24((i32) clamp(val, I24_MIN, I24_MAX));
  }

  if (get_sym(a) == TY_F24 && get_typ(b) == TY_U24) return new_f24((f32) get_u24(b));
  if (get_sym(a) == TY_F24 && get_typ(b) == TY_I24) return new_f24((f32) get_i24(b));
  if (get_sym(a) == TY_F24 && get_typ(b) == TY_F24) return b;

  return new_u24(0);
}

// Operate function
static inline Numb operate(Numb a, Numb b) {
  Tag at = get_typ(a);
  Tag bt = get_typ(b);
  if (at == TY_SYM && bt == TY_SYM) {
    return new_u24(0);
  }
  if (is_cast(a) && is_num(b)) {
    return cast(a, b);
  }
  if (is_cast(b) && is_num(a)) {
    return cast(b, a);
  }
  if (at == TY_SYM && bt != TY_SYM) {
    return partial(a, b);
  }
  if (at != TY_SYM && bt == TY_SYM) {
    return partial(b, a);
  }
  if (at >= OP_ADD && bt >= OP_ADD) {
    return new_u24(0);
  }
  if (at < OP_ADD && bt < OP_ADD) {
    return new_u24(0);
  }
  Tag op, ty;
  Numb swp;
  if (at >= OP_ADD) {
    op = at; ty = bt;
  } else {
    op = bt; ty = at; swp = a; a = b; b = swp;
  }
  switch (ty) {
    case TY_U24: {
      u32 av = get_u24(a);
      u32 bv = get_u24(b);
      switch (op) {
        case OP_ADD: return new_u24(av + bv);
        case OP_SUB: return new_u24(av - bv);
        case FP_SUB: return new_u24(bv - av);
        case OP_MUL: return new_u24(av * bv);
        case OP_DIV: return new_u24(av / bv);
        case FP_DIV: return new_u24(bv / av);
        case OP_REM: return new_u24(av % bv);
        case FP_REM: return new_u24(bv % av);
        case OP_EQ:  return new_u24(av == bv);
        case OP_NEQ: return new_u24(av != bv);
        case OP_LT:  return new_u24(av < bv);
        case OP_GT:  return new_u24(av > bv);
        case OP_AND: return new_u24(av & bv);
        case OP_OR:  return new_u24(av | bv);
        case OP_XOR: return new_u24(av ^ bv);
        case OP_SHL: return new_u24(av << (bv & 31));
        case FP_SHL: return new_u24(bv << (av & 31));
        case OP_SHR: return new_u24(av >> (bv & 31));
        case FP_SHR: return new_u24(bv >> (av & 31));
        default:     return new_u24(0);
      }
    }
    case TY_I24: {
      i32 av = get_i24(a);
      i32 bv = get_i24(b);
      switch (op) {
        case OP_ADD: return new_i24(av + bv);
        case OP_SUB: return new_i24(av - bv);
        case FP_SUB: return new_i24(bv - av);
        case OP_MUL: return new_i24(av * bv);
        case OP_DIV: return new_i24(av / bv);
        case FP_DIV: return new_i24(bv / av);
        case OP_REM: return new_i24(av % bv);
        case FP_REM: return new_i24(bv % av);
        case OP_EQ:  return new_u24(av == bv);
        case OP_NEQ: return new_u24(av != bv);
        case OP_LT:  return new_u24(av < bv);
        case OP_GT:  return new_u24(av > bv);
        case OP_AND: return new_i24(av & bv);
        case OP_OR:  return new_i24(av | bv);
        case OP_XOR: return new_i24(av ^ bv);
        default:     return new_i24(0);
      }
    }
    case TY_F24: {
      float av = get_f24(a);
      float bv = get_f24(b);
      switch (op) {
        case OP_ADD: return new_f24(av + bv);
        case OP_SUB: return new_f24(av - bv);
        case FP_SUB: return new_f24(bv - av);
        case OP_MUL: return new_f24(av * bv);
        case OP_DIV: return new_f24(av / bv);
        case FP_DIV: return new_f24(bv / av);
        case OP_REM: return new_f24(fmodf(av, bv));
        case FP_REM: return new_f24(fmodf(bv, av));
        case OP_EQ:  return new_u24(av == bv);
        case OP_NEQ: return new_u24(av != bv);
        case OP_LT:  return new_u24(av < bv);
        case OP_GT:  return new_u24(av > bv);
        case OP_AND: return new_f24(atan2f(av, bv));
        case OP_OR:  return new_f24(logf(bv) / logf(av));
        case OP_XOR: return new_f24(powf(av, bv));
        case OP_SHL: return new_f24(sin(av + bv));
        case OP_SHR: return new_f24(tan(av + bv));
        default:     return new_f24(0);
      }
    }
    default: return new_u24(0);
  }
}

// RBag
// ----

// FIXME: what about some bound checks?

static inline void push_redex(Net* net, TM* tm, Pair redex) {
  #ifdef DEBUG
  bool free_local = tm->hput < HLEN;
  bool free_global = tm->rput < RLEN;
  if (!free_global || !free_local) {
    debug("push_redex: limited resources, maybe corrupting memory\n");
  }
  #endif

  if (is_high_priority(get_pair_rule(redex))) {
    tm->hbag_buf[tm->hput++] = redex;
  } else {
    atomic_store_explicit(&net->rbag_buf[tm->tid*(G_RBAG_LEN/TPC) + (tm->rput++)], redex, memory_order_relaxed);
  }
}

static inline Pair pop_redex(Net* net, TM* tm) {
  if (tm->hput > 0) {
    return tm->hbag_buf[--tm->hput];
  } else if (tm->rput > 0) {
    return atomic_exchange_explicit(&net->rbag_buf[tm->tid*(G_RBAG_LEN/TPC) + (--tm->rput)], 0, memory_order_relaxed);
  } else {
    return 0;
  }
}

static inline u32 rbag_len(Net* net, TM* tm) {
  return tm->rput + tm->hput;
}

// TM
// --

static TM* tm[TPC];

TM* tm_new(u32 tid) {
  TM* tm   = malloc(sizeof(TM));
  tm->tid  = tid;
  tm->itrs = 0;
  tm->nput = 1;
  tm->vput = 1;
  tm->rput = 0;
  tm->hput = 0;
  tm->sidx = 0;
  return tm;
}

void alloc_static_tms() {
  for (u32 t = 0; t < TPC; ++t) {
    tm[t] = tm_new(t);
  }
}

void free_static_tms() {
  for (u32 t = 0; t < TPC; ++t) {
    free(tm[t]);
  }
}

// Net
// ----

// Stores a new node on global.
static inline void node_create(Net* net, u32 loc, Pair val) {
  atomic_store_explicit(&net->node_buf[loc], val, memory_order_relaxed);
}

// Stores a var on global.
static inline void vars_create(Net* net, u32 var, Port val) {
  atomic_store_explicit(&net->vars_buf[var], val, memory_order_relaxed);
}

// Reads a node from global.
static inline Pair node_load(Net* net, u32 loc) {
  return atomic_load_explicit(&net->node_buf[loc], memory_order_relaxed);
}

// Reads a var from global.
static inline Port vars_load(Net* net, u32 var) {
  return atomic_load_explicit(&net->vars_buf[var], memory_order_relaxed);
}

// Stores a node on global.
static inline void node_store(Net* net, u32 loc, Pair val) {
  atomic_store_explicit(&net->node_buf[loc], val, memory_order_relaxed);
}

// Exchanges a node on global by a value. Returns old.
static inline Pair node_exchange(Net* net, u32 loc, Pair val) {
  return atomic_exchange_explicit(&net->node_buf[loc], val, memory_order_relaxed);
}

// Exchanges a var on global by a value. Returns old.
static inline Port vars_exchange(Net* net, u32 var, Port val) {
  return atomic_exchange_explicit(&net->vars_buf[var], val, memory_order_relaxed);
}

// Takes a node.
static inline Pair node_take(Net* net, u32 loc) {
  return node_exchange(net, loc, 0);
}

// Takes a var.
static inline Port vars_take(Net* net, u32 var) {
  return vars_exchange(net, var, 0);
}


// Net
// ---

// Initializes a net.
static inline Net* net_new() {
  Net* net = calloc(1, sizeof(Net));

  atomic_store(&net->itrs, 0);
  atomic_store(&net->idle, 0);

  return net;
}

// Allocator
// ---------

u32 node_alloc_1(Net* net, TM* tm, u32* lps) {
  while (true) {
    u32 lc = tm->tid*(G_NODE_LEN/TPC) + (tm->nput%(G_NODE_LEN/TPC));
    Pair elem = net->node_buf[lc];
    tm->nput += 1;
    if (lc > 0 && elem == 0) {
      return lc;
    }
    // FIXME: check this decently
    if (++(*lps) >= G_NODE_LEN/TPC) printf("OOM\n");
  }
}

u32 vars_alloc_1(Net* net, TM* tm, u32* lps) {
  while (true) {
    u32 lc = tm->tid*(G_NODE_LEN/TPC) + (tm->vput%(G_NODE_LEN/TPC));
    Port elem = net->vars_buf[lc];
    tm->vput += 1;
    if (lc > 0 && elem == 0) {
      return lc;
    }
    // FIXME: check this decently
    if (++(*lps) >= G_NODE_LEN/TPC) printf("OOM\n");
  }
}

u32 node_alloc(Net* net, TM* tm, u32 num) {
  u32 got = 0;
  u32 lps = 0;
  while (got < num) {
    u32 lc = tm->tid*(G_NODE_LEN/TPC) + (tm->nput%(G_NODE_LEN/TPC));
    Pair elem = net->node_buf[lc];
    tm->nput += 1;
    if (lc > 0 && elem == 0) {
      tm->nloc[got++] = lc;
    }
    // FIXME: check this decently
    if (++lps >= G_NODE_LEN/TPC) printf("OOM\n");
  }
  return got;
}

u32 vars_alloc(Net* net, TM* tm, u32 num) {
  u32 got = 0;
  u32 lps = 0;
  while (got < num) {
    u32 lc = tm->tid*(G_NODE_LEN/TPC) + (tm->vput%(G_NODE_LEN/TPC));
    Port elem = net->vars_buf[lc];
    tm->vput += 1;
    if (lc > 0 && elem == 0) {
      tm->vloc[got++] = lc;
    }
    // FIXME: check this decently
    if (++lps >= G_NODE_LEN/TPC) printf("OOM\n");
  }
  return got;
}

// Gets the necessary resources for an interaction. Returns success.
static inline bool get_resources(Net* net, TM* tm, u32 need_rbag, u32 need_node, u32 need_vars) {
  u32 got_rbag = min(RLEN - tm->rput, HLEN - tm->hput);
  u32 got_node = node_alloc(net, tm, need_node);
  u32 got_vars = vars_alloc(net, tm, need_vars);

  return got_rbag >= need_rbag && got_node >= need_node && got_vars >= need_vars;
}

// Linking
// -------

// Peeks a variable's final target without modifying it.
static inline Port peek(Net* net, Port var) {
  u32 enter_lim = 4096;
  while (get_tag(var) == VAR && enter_lim-- > 0) {
    Port val = vars_load(net, get_val(var));
    if (val == NONE) break;
    if (val == 0) break;
    var = val;
  }
  return var;
}

// Finds a variable's value.
static inline Port enter(Net* net, Port var) {
  // While `B` is VAR: extend it (as an optimization)
  u32 enter_lim = 4096;
  while (get_tag(var) == VAR && enter_lim-- > 0) {
    // Takes the current `var` substitution as `val`
    Port val = vars_exchange(net, get_val(var), NONE);
    // If there was no `val`, stop, as there is no extension
    if (val == NONE || val == 0) {
      break;
    }
    // Otherwise, delete `B` (we own both) and continue
    vars_take(net, get_val(var));
    var = val;
  }
  return var;
}

// Atomically Links `A ~ B`.
static inline void link(Net* net, TM* tm, Port A, Port B) {
  // Attempts to directionally point `A ~> B`
  while (true) {
    // If `A` is NODE: swap `A` and `B`, and continue
    if (get_tag(A) != VAR && get_tag(B) == VAR) {
      Port X = A; A = B; B = X;
    }

    // If `A` is NODE: create the `A ~ B` redex
    if (get_tag(A) != VAR) {
      push_redex(net, tm, new_pair(A, B)); // TODO: move global ports to local
      break;
    }

    // Extends B (as an optimization)
    B = enter(net, B);

    // Since `A` is VAR: point `A ~> B`.
    // Stores `A -> B`, taking the current `A` subst as `A'`
    Port A_ = vars_exchange(net, get_val(A), B);
    // If there was no `A'`, stop, as we lost B's ownership
    if (A_ == NONE) {
      break;
    }
    //if (A_ == 0) { ? } // FIXME: must handle on the move-to-global algo
    // Otherwise, delete `A` (we own both) and link `A' ~ B`
    vars_take(net, get_val(A));
    A = A_;
  }
}

// Links `A ~ B` (as a pair).
static inline void link_pair(Net* net, TM* tm, Pair AB) {
  link(net, tm, get_fst(AB), get_snd(AB));
}

// Interactions
// ------------

// The Link Interaction.
static inline bool interact_link(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 1, 0, 0)) {
    debug("interact_link: get_resources failed\n");
    return false;
  }

  // Links.
  link_pair(net, tm, new_pair(a, b));

  return true;
}

// Declared here for use in call interactions.
static inline bool interact_eras(Net* net, TM* tm, Port a, Port b);

// The Call Interaction.
#ifdef COMPILED
bool interact_call_main(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !n0 || !n1 || !n2) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  if (b != NONE) {
    link(net, tm, new_port(VAR,v0), b);
  } else {
    b = new_port(VAR,v0);
  }
  node_create(net, n0, new_pair(new_port(VAR,v1),new_port(VAR,v0)));
  link(net, tm, new_port(REF,0x0000000d), new_port(CON,n0));
  node_create(net, n2, new_pair(new_port(NUM,0x00020001),new_port(VAR,v1)));
  node_create(net, n1, new_pair(new_port(NUM,0x00000001),new_port(CON,n2)));
  link(net, tm, new_port(REF,0x0000000a), new_port(CON,n1));
  return true;
}

bool interact_call_ParTree_Leaf(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !n0 || !n1 || !n2 || !n3) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v1), k8);
  } else {
    k8 = new_port(VAR,v1);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k7) == CON && node_load(net, get_val(k7)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k7));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  if (k16 != NONE) {
    link(net, tm, new_port(VAR,v1), k16);
  } else {
    k16 = new_port(VAR,v1);
  }
  if (k15 != NONE) {
    link(net, tm, new_port(VAR,v0), k15);
  } else {
    k15 = new_port(VAR,v0);
  }
  if (!k13) {
    node_create(net, n3, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n3), k12);
    } else {
      k12 = new_port(CON,n3);
    }
  }
  // fast void
  if (get_tag(k11) == ERA || get_tag(k11) == NUM) {
    tm->itrs += 1;
  } else {
    if (k11 != NONE) {
      link(net, tm, new_port(NUM,0x00000021), k11);
    } else {
      k11 = new_port(NUM,0x00000021);
    }
  }
  if (!k9) {
    node_create(net, n2, new_pair(k11,k12));
    if (k7 != NONE) {
      link(net, tm, new_port(CON,n2), k7);
    } else {
      k7 = new_port(CON,n2);
    }
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_ParTree_Leaf_tag(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  if (0) {
    return false;
  }
  // fast void
  if (get_tag(b) == ERA || get_tag(b) == NUM) {
    tm->itrs += 1;
  } else {
    if (b != NONE) {
      link(net, tm, new_port(NUM,0x00000021), b);
    } else {
      b = new_port(NUM,0x00000021);
    }
  }
  return true;
}

bool interact_call_ParTree_Node(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  if (k12 != NONE) {
    link(net, tm, new_port(VAR,v2), k12);
  } else {
    k12 = new_port(VAR,v2);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k11) == CON && node_load(net, get_val(k11)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k11));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  bool k21 = 0;
  Pair k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast anni
  if (get_tag(k20) == CON && node_load(net, get_val(k20)) != 0) {
    tm->itrs += 1;
    k21 = 1;
    k22 = node_take(net, get_val(k20));
    k23 = get_fst(k22);
    k24 = get_snd(k22);
  }
  if (k24 != NONE) {
    link(net, tm, new_port(VAR,v2), k24);
  } else {
    k24 = new_port(VAR,v2);
  }
  if (k23 != NONE) {
    link(net, tm, new_port(VAR,v1), k23);
  } else {
    k23 = new_port(VAR,v1);
  }
  if (!k21) {
    node_create(net, n5, new_pair(k23,k24));
    if (k20 != NONE) {
      link(net, tm, new_port(CON,n5), k20);
    } else {
      k20 = new_port(CON,n5);
    }
  }
  if (k19 != NONE) {
    link(net, tm, new_port(VAR,v0), k19);
  } else {
    k19 = new_port(VAR,v0);
  }
  if (!k17) {
    node_create(net, n4, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,n4), k16);
    } else {
      k16 = new_port(CON,n4);
    }
  }
  // fast void
  if (get_tag(k15) == ERA || get_tag(k15) == NUM) {
    tm->itrs += 1;
  } else {
    if (k15 != NONE) {
      link(net, tm, new_port(NUM,0x00000001), k15);
    } else {
      k15 = new_port(NUM,0x00000001);
    }
  }
  if (!k13) {
    node_create(net, n3, new_pair(k15,k16));
    if (k11 != NONE) {
      link(net, tm, new_port(CON,n3), k11);
    } else {
      k11 = new_port(CON,n3);
    }
  }
  if (!k9) {
    node_create(net, n2, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n2), k8);
    } else {
      k8 = new_port(CON,n2);
    }
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_ParTree_Node_tag(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  if (0) {
    return false;
  }
  // fast void
  if (get_tag(b) == ERA || get_tag(b) == NUM) {
    tm->itrs += 1;
  } else {
    if (b != NONE) {
      link(net, tm, new_port(NUM,0x00000001), b);
    } else {
      b = new_port(NUM,0x00000001);
    }
  }
  return true;
}

bool interact_call_f24_to_u24(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !n0 || !n1) {
    return false;
  }
  vars_create(net, v0, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  if (k4 != NONE) {
    link(net, tm, new_port(VAR,v0), k4);
  } else {
    k4 = new_port(VAR,v0);
  }
  bool k5 = 0;
  Port k6 = NONE;
  // fast oper
  if (get_tag(k3) == NUM && get_tag(new_port(NUM,0x00000020)) == NUM) {
    tm->itrs += 1;
    k5 = 1;
    k6 = new_port(NUM, operate(get_val(k3), get_val(new_port(NUM,0x00000020))));
  }
  if (k6 != NONE) {
    link(net, tm, new_port(VAR,v0), k6);
  } else {
    k6 = new_port(VAR,v0);
  }
  if (!k5) {
    node_create(net, n1, new_pair(new_port(NUM,0x00000020),k6));
    if (k3 != NONE) {
      link(net, tm, new_port(OPR, n1), k3);
    } else {
      k3 = new_port(OPR, n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_jshark_grid(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val vb = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !vb || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  vars_create(net, vb, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  bool k21 = 0;
  Pair k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast anni
  if (get_tag(k20) == CON && node_load(net, get_val(k20)) != 0) {
    tm->itrs += 1;
    k21 = 1;
    k22 = node_take(net, get_val(k20));
    k23 = get_fst(k22);
    k24 = get_snd(k22);
  }
  bool k25 = 0;
  Pair k26 = 0;
  Port k27 = NONE;
  Port k28 = NONE;
  // fast anni
  if (get_tag(k24) == CON && node_load(net, get_val(k24)) != 0) {
    tm->itrs += 1;
    k25 = 1;
    k26 = node_take(net, get_val(k24));
    k27 = get_fst(k26);
    k28 = get_snd(k26);
  }
  bool k29 = 0;
  Pair k30 = 0;
  Port k31 = NONE;
  Port k32 = NONE;
  // fast anni
  if (get_tag(k28) == CON && node_load(net, get_val(k28)) != 0) {
    tm->itrs += 1;
    k29 = 1;
    k30 = node_take(net, get_val(k28));
    k31 = get_fst(k30);
    k32 = get_snd(k30);
  }
  if (k32 != NONE) {
    link(net, tm, new_port(VAR,v9), k32);
  } else {
    k32 = new_port(VAR,v9);
  }
  if (k31 != NONE) {
    link(net, tm, new_port(VAR,v6), k31);
  } else {
    k31 = new_port(VAR,v6);
  }
  if (!k29) {
    node_create(net, na, new_pair(k31,k32));
    if (k28 != NONE) {
      link(net, tm, new_port(CON,na), k28);
    } else {
      k28 = new_port(CON,na);
    }
  }
  bool k33 = 0;
  Port k34 = NONE;
  Port k35 = NONE;
  // fast copy
  if (get_tag(k27) == NUM) {
    tm->itrs += 1;
    k33 = 1;
    k34 = k27;
    k35 = k27;
  }
  if (k35 != NONE) {
    link(net, tm, new_port(VAR,v8), k35);
  } else {
    k35 = new_port(VAR,v8);
  }
  bool k36 = 0;
  Port k37 = NONE;
  // fast oper
  if (get_tag(k34) == NUM && get_tag(new_port(NUM,0x000000e0)) == NUM) {
    tm->itrs += 1;
    k36 = 1;
    k37 = new_port(NUM, operate(get_val(k34), get_val(new_port(NUM,0x000000e0))));
  }
  bool k38 = 0;
  Port k39 = NONE;
  // fast oper
  if (get_tag(k37) == NUM && get_tag(new_port(VAR,v6)) == NUM) {
    tm->itrs += 1;
    k38 = 1;
    k39 = new_port(NUM, operate(get_val(k37), get_val(new_port(VAR,v6))));
  }
  if (k39 != NONE) {
    link(net, tm, new_port(VAR,v7), k39);
  } else {
    k39 = new_port(VAR,v7);
  }
  if (!k38) {
    node_create(net, n9, new_pair(new_port(VAR,v6),k39));
    if (k37 != NONE) {
      link(net, tm, new_port(OPR, n9), k37);
    } else {
      k37 = new_port(OPR, n9);
    }
  }
  if (!k36) {
    node_create(net, n8, new_pair(new_port(NUM,0x000000e0),k37));
    if (k34 != NONE) {
      link(net, tm, new_port(OPR, n8), k34);
    } else {
      k34 = new_port(OPR, n8);
    }
  }
  if (!k33) {
    node_create(net, n7, new_pair(k34,k35));
    if (k27 != NONE) {
      link(net, tm, new_port(DUP,n7), k27);
    } else {
      k27 = new_port(DUP,n7);
    }
  }
  if (!k25) {
    node_create(net, n6, new_pair(k27,k28));
    if (k24 != NONE) {
      link(net, tm, new_port(CON,n6), k24);
    } else {
      k24 = new_port(CON,n6);
    }
  }
  if (k23 != NONE) {
    link(net, tm, new_port(VAR,v5), k23);
  } else {
    k23 = new_port(VAR,v5);
  }
  if (!k21) {
    node_create(net, n5, new_pair(k23,k24));
    if (k20 != NONE) {
      link(net, tm, new_port(CON,n5), k20);
    } else {
      k20 = new_port(CON,n5);
    }
  }
  if (k19 != NONE) {
    link(net, tm, new_port(VAR,v4), k19);
  } else {
    k19 = new_port(VAR,v4);
  }
  if (!k17) {
    node_create(net, n4, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,n4), k16);
    } else {
      k16 = new_port(CON,n4);
    }
  }
  if (k15 != NONE) {
    link(net, tm, new_port(VAR,v3), k15);
  } else {
    k15 = new_port(VAR,v3);
  }
  if (!k13) {
    node_create(net, n3, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n3), k12);
    } else {
      k12 = new_port(CON,n3);
    }
  }
  if (k11 != NONE) {
    link(net, tm, new_port(VAR,v2), k11);
  } else {
    k11 = new_port(VAR,v2);
  }
  if (!k9) {
    node_create(net, n2, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n2), k8);
    } else {
      k8 = new_port(CON,n2);
    }
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n13, new_pair(new_port(VAR,vb),new_port(VAR,v9)));
  node_create(net, n12, new_pair(new_port(NUM,0x00000001),new_port(CON,n13)));
  node_create(net, n11, new_pair(new_port(VAR,v4),new_port(CON,n12)));
  node_create(net, n10, new_pair(new_port(VAR,v1),new_port(CON,n11)));
  node_create(net, nf, new_pair(new_port(VAR,v2),new_port(CON,n10)));
  node_create(net, ne, new_pair(new_port(VAR,v3),new_port(CON,nf)));
  node_create(net, nd, new_pair(new_port(VAR,v5),new_port(CON,ne)));
  node_create(net, nc, new_pair(new_port(VAR,va),new_port(CON,nd)));
  node_create(net, nb, new_pair(new_port(VAR,v0),new_port(CON,nc)));
  link(net, tm, new_port(REF,0x00000007), new_port(CON,nb));
  node_create(net, n14, new_pair(new_port(VAR,v8),new_port(VAR,va)));
  link(net, tm, new_port(REF,0x00000005), new_port(CON,n14));
  node_create(net, n15, new_pair(new_port(VAR,v7),new_port(VAR,vb)));
  link(net, tm, new_port(REF,0x00000005), new_port(CON,n15));
  return true;
}

bool interact_call_jshark_grid__bend0(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  Val n16 = node_alloc_1(net, tm, &nl);
  Val n17 = node_alloc_1(net, tm, &nl);
  Val n18 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15 || !n16 || !n17 || !n18) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  bool k21 = 0;
  Pair k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast anni
  if (get_tag(k20) == CON && node_load(net, get_val(k20)) != 0) {
    tm->itrs += 1;
    k21 = 1;
    k22 = node_take(net, get_val(k20));
    k23 = get_fst(k22);
    k24 = get_snd(k22);
  }
  bool k25 = 0;
  Pair k26 = 0;
  Port k27 = NONE;
  Port k28 = NONE;
  // fast anni
  if (get_tag(k24) == CON && node_load(net, get_val(k24)) != 0) {
    tm->itrs += 1;
    k25 = 1;
    k26 = node_take(net, get_val(k24));
    k27 = get_fst(k26);
    k28 = get_snd(k26);
  }
  bool k29 = 0;
  Pair k30 = 0;
  Port k31 = NONE;
  Port k32 = NONE;
  // fast anni
  if (get_tag(k28) == CON && node_load(net, get_val(k28)) != 0) {
    tm->itrs += 1;
    k29 = 1;
    k30 = node_take(net, get_val(k28));
    k31 = get_fst(k30);
    k32 = get_snd(k30);
  }
  bool k33 = 0;
  Pair k34 = 0;
  Port k35 = NONE;
  Port k36 = NONE;
  // fast anni
  if (get_tag(k32) == CON && node_load(net, get_val(k32)) != 0) {
    tm->itrs += 1;
    k33 = 1;
    k34 = node_take(net, get_val(k32));
    k35 = get_fst(k34);
    k36 = get_snd(k34);
  }
  if (k36 != NONE) {
    link(net, tm, new_port(VAR,va), k36);
  } else {
    k36 = new_port(VAR,va);
  }
  bool k37 = 0;
  Port k38 = NONE;
  Port k39 = NONE;
  // fast copy
  if (get_tag(k35) == NUM) {
    tm->itrs += 1;
    k37 = 1;
    k38 = k35;
    k39 = k35;
  }
  if (k39 != NONE) {
    link(net, tm, new_port(VAR,v9), k39);
  } else {
    k39 = new_port(VAR,v9);
  }
  bool k40 = 0;
  Port k41 = NONE;
  // fast oper
  if (get_tag(k38) == NUM && get_tag(new_port(NUM,0x000000a0)) == NUM) {
    tm->itrs += 1;
    k40 = 1;
    k41 = new_port(NUM, operate(get_val(k38), get_val(new_port(NUM,0x000000a0))));
  }
  bool k42 = 0;
  Port k43 = NONE;
  // fast oper
  if (get_tag(k41) == NUM && get_tag(new_port(VAR,v7)) == NUM) {
    tm->itrs += 1;
    k42 = 1;
    k43 = new_port(NUM, operate(get_val(k41), get_val(new_port(VAR,v7))));
  }
  bool k44 = 0;
  Port k45 = NONE;
  // fast oper
  if (get_tag(k43) == NUM && get_tag(new_port(NUM,0x0000002e)) == NUM) {
    tm->itrs += 1;
    k44 = 1;
    k45 = new_port(NUM, operate(get_val(k43), get_val(new_port(NUM,0x0000002e))));
  }
  node_create(net, nf, new_pair(new_port(REF,0x00000008),new_port(REF,0x00000009)));
  node_create(net, n18, new_pair(new_port(VAR,v9),new_port(VAR,va)));
  node_create(net, n17, new_pair(new_port(VAR,v8),new_port(CON,n18)));
  node_create(net, n16, new_pair(new_port(VAR,v6),new_port(CON,n17)));
  node_create(net, n15, new_pair(new_port(VAR,v5),new_port(CON,n16)));
  node_create(net, n14, new_pair(new_port(VAR,v4),new_port(CON,n15)));
  node_create(net, n13, new_pair(new_port(VAR,v3),new_port(CON,n14)));
  node_create(net, n12, new_pair(new_port(VAR,v2),new_port(CON,n13)));
  node_create(net, n11, new_pair(new_port(VAR,v1),new_port(CON,n12)));
  node_create(net, n10, new_pair(new_port(VAR,v0),new_port(CON,n11)));
  node_create(net, ne, new_pair(new_port(CON,nf),new_port(CON,n10)));
  if (k45 != NONE) {
    link(net, tm, new_port(SWI,ne), k45);
  } else {
    k45 = new_port(SWI,ne);
  }
  if (!k44) {
    node_create(net, nd, new_pair(new_port(NUM,0x0000002e),k45));
    if (k43 != NONE) {
      link(net, tm, new_port(OPR, nd), k43);
    } else {
      k43 = new_port(OPR, nd);
    }
  }
  if (!k42) {
    node_create(net, nc, new_pair(new_port(VAR,v7),k43));
    if (k41 != NONE) {
      link(net, tm, new_port(OPR, nc), k41);
    } else {
      k41 = new_port(OPR, nc);
    }
  }
  if (!k40) {
    node_create(net, nb, new_pair(new_port(NUM,0x000000a0),k41));
    if (k38 != NONE) {
      link(net, tm, new_port(OPR, nb), k38);
    } else {
      k38 = new_port(OPR, nb);
    }
  }
  if (!k37) {
    node_create(net, na, new_pair(k38,k39));
    if (k35 != NONE) {
      link(net, tm, new_port(DUP,na), k35);
    } else {
      k35 = new_port(DUP,na);
    }
  }
  if (!k33) {
    node_create(net, n9, new_pair(k35,k36));
    if (k32 != NONE) {
      link(net, tm, new_port(CON,n9), k32);
    } else {
      k32 = new_port(CON,n9);
    }
  }
  bool k46 = 0;
  Port k47 = NONE;
  Port k48 = NONE;
  // fast copy
  if (get_tag(k31) == NUM) {
    tm->itrs += 1;
    k46 = 1;
    k47 = k31;
    k48 = k31;
  }
  if (k48 != NONE) {
    link(net, tm, new_port(VAR,v8), k48);
  } else {
    k48 = new_port(VAR,v8);
  }
  if (k47 != NONE) {
    link(net, tm, new_port(VAR,v7), k47);
  } else {
    k47 = new_port(VAR,v7);
  }
  if (!k46) {
    node_create(net, n8, new_pair(k47,k48));
    if (k31 != NONE) {
      link(net, tm, new_port(DUP,n8), k31);
    } else {
      k31 = new_port(DUP,n8);
    }
  }
  if (!k29) {
    node_create(net, n7, new_pair(k31,k32));
    if (k28 != NONE) {
      link(net, tm, new_port(CON,n7), k28);
    } else {
      k28 = new_port(CON,n7);
    }
  }
  if (k27 != NONE) {
    link(net, tm, new_port(VAR,v6), k27);
  } else {
    k27 = new_port(VAR,v6);
  }
  if (!k25) {
    node_create(net, n6, new_pair(k27,k28));
    if (k24 != NONE) {
      link(net, tm, new_port(CON,n6), k24);
    } else {
      k24 = new_port(CON,n6);
    }
  }
  if (k23 != NONE) {
    link(net, tm, new_port(VAR,v5), k23);
  } else {
    k23 = new_port(VAR,v5);
  }
  if (!k21) {
    node_create(net, n5, new_pair(k23,k24));
    if (k20 != NONE) {
      link(net, tm, new_port(CON,n5), k20);
    } else {
      k20 = new_port(CON,n5);
    }
  }
  if (k19 != NONE) {
    link(net, tm, new_port(VAR,v4), k19);
  } else {
    k19 = new_port(VAR,v4);
  }
  if (!k17) {
    node_create(net, n4, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,n4), k16);
    } else {
      k16 = new_port(CON,n4);
    }
  }
  if (k15 != NONE) {
    link(net, tm, new_port(VAR,v3), k15);
  } else {
    k15 = new_port(VAR,v3);
  }
  if (!k13) {
    node_create(net, n3, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n3), k12);
    } else {
      k12 = new_port(CON,n3);
    }
  }
  if (k11 != NONE) {
    link(net, tm, new_port(VAR,v2), k11);
  } else {
    k11 = new_port(VAR,v2);
  }
  if (!k9) {
    node_create(net, n2, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n2), k8);
    } else {
      k8 = new_port(CON,n2);
    }
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_jshark_grid__bend0__C0(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val vb = vars_alloc_1(net, tm, &vl);
  Val vc = vars_alloc_1(net, tm, &vl);
  Val vd = vars_alloc_1(net, tm, &vl);
  Val ve = vars_alloc_1(net, tm, &vl);
  Val vf = vars_alloc_1(net, tm, &vl);
  Val v10 = vars_alloc_1(net, tm, &vl);
  Val v11 = vars_alloc_1(net, tm, &vl);
  Val v12 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  Val n16 = node_alloc_1(net, tm, &nl);
  Val n17 = node_alloc_1(net, tm, &nl);
  Val n18 = node_alloc_1(net, tm, &nl);
  Val n19 = node_alloc_1(net, tm, &nl);
  Val n1a = node_alloc_1(net, tm, &nl);
  Val n1b = node_alloc_1(net, tm, &nl);
  Val n1c = node_alloc_1(net, tm, &nl);
  Val n1d = node_alloc_1(net, tm, &nl);
  Val n1e = node_alloc_1(net, tm, &nl);
  Val n1f = node_alloc_1(net, tm, &nl);
  Val n20 = node_alloc_1(net, tm, &nl);
  Val n21 = node_alloc_1(net, tm, &nl);
  Val n22 = node_alloc_1(net, tm, &nl);
  Val n23 = node_alloc_1(net, tm, &nl);
  Val n24 = node_alloc_1(net, tm, &nl);
  Val n25 = node_alloc_1(net, tm, &nl);
  Val n26 = node_alloc_1(net, tm, &nl);
  Val n27 = node_alloc_1(net, tm, &nl);
  Val n28 = node_alloc_1(net, tm, &nl);
  Val n29 = node_alloc_1(net, tm, &nl);
  Val n2a = node_alloc_1(net, tm, &nl);
  Val n2b = node_alloc_1(net, tm, &nl);
  Val n2c = node_alloc_1(net, tm, &nl);
  Val n2d = node_alloc_1(net, tm, &nl);
  Val n2e = node_alloc_1(net, tm, &nl);
  Val n2f = node_alloc_1(net, tm, &nl);
  Val n30 = node_alloc_1(net, tm, &nl);
  Val n31 = node_alloc_1(net, tm, &nl);
  Val n32 = node_alloc_1(net, tm, &nl);
  Val n33 = node_alloc_1(net, tm, &nl);
  Val n34 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !vb || !vc || !vd || !ve || !vf || !v10 || !v11 || !v12 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15 || !n16 || !n17 || !n18 || !n19 || !n1a || !n1b || !n1c || !n1d || !n1e || !n1f || !n20 || !n21 || !n22 || !n23 || !n24 || !n25 || !n26 || !n27 || !n28 || !n29 || !n2a || !n2b || !n2c || !n2d || !n2e || !n2f || !n30 || !n31 || !n32 || !n33 || !n34) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  vars_create(net, vb, NONE);
  vars_create(net, vc, NONE);
  vars_create(net, vd, NONE);
  vars_create(net, ve, NONE);
  vars_create(net, vf, NONE);
  vars_create(net, v10, NONE);
  vars_create(net, v11, NONE);
  vars_create(net, v12, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  bool k21 = 0;
  Pair k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast anni
  if (get_tag(k20) == CON && node_load(net, get_val(k20)) != 0) {
    tm->itrs += 1;
    k21 = 1;
    k22 = node_take(net, get_val(k20));
    k23 = get_fst(k22);
    k24 = get_snd(k22);
  }
  bool k25 = 0;
  Pair k26 = 0;
  Port k27 = NONE;
  Port k28 = NONE;
  // fast anni
  if (get_tag(k24) == CON && node_load(net, get_val(k24)) != 0) {
    tm->itrs += 1;
    k25 = 1;
    k26 = node_take(net, get_val(k24));
    k27 = get_fst(k26);
    k28 = get_snd(k26);
  }
  bool k29 = 0;
  Pair k30 = 0;
  Port k31 = NONE;
  Port k32 = NONE;
  // fast anni
  if (get_tag(k28) == CON && node_load(net, get_val(k28)) != 0) {
    tm->itrs += 1;
    k29 = 1;
    k30 = node_take(net, get_val(k28));
    k31 = get_fst(k30);
    k32 = get_snd(k30);
  }
  bool k33 = 0;
  Pair k34 = 0;
  Port k35 = NONE;
  Port k36 = NONE;
  // fast anni
  if (get_tag(k32) == CON && node_load(net, get_val(k32)) != 0) {
    tm->itrs += 1;
    k33 = 1;
    k34 = node_take(net, get_val(k32));
    k35 = get_fst(k34);
    k36 = get_snd(k34);
  }
  if (k36 != NONE) {
    link(net, tm, new_port(VAR,v12), k36);
  } else {
    k36 = new_port(VAR,v12);
  }
  // fast void
  if (get_tag(k35) == ERA || get_tag(k35) == NUM) {
    tm->itrs += 1;
  } else {
    if (k35 != NONE) {
      link(net, tm, new_port(ERA,0x00000000), k35);
    } else {
      k35 = new_port(ERA,0x00000000);
    }
  }
  if (!k33) {
    node_create(net, n1c, new_pair(k35,k36));
    if (k32 != NONE) {
      link(net, tm, new_port(CON,n1c), k32);
    } else {
      k32 = new_port(CON,n1c);
    }
  }
  bool k37 = 0;
  Port k38 = NONE;
  Port k39 = NONE;
  // fast copy
  if (get_tag(k31) == NUM) {
    tm->itrs += 1;
    k37 = 1;
    k38 = k31;
    k39 = k31;
  }
  bool k40 = 0;
  Port k41 = NONE;
  // fast oper
  if (get_tag(k39) == NUM && get_tag(new_port(NUM,0x00000100)) == NUM) {
    tm->itrs += 1;
    k40 = 1;
    k41 = new_port(NUM, operate(get_val(k39), get_val(new_port(NUM,0x00000100))));
  }
  bool k42 = 0;
  Port k43 = NONE;
  // fast oper
  if (get_tag(k41) == NUM && get_tag(new_port(VAR,v3)) == NUM) {
    tm->itrs += 1;
    k42 = 1;
    k43 = new_port(NUM, operate(get_val(k41), get_val(new_port(VAR,v3))));
  }
  if (k43 != NONE) {
    link(net, tm, new_port(VAR,v11), k43);
  } else {
    k43 = new_port(VAR,v11);
  }
  if (!k42) {
    node_create(net, n1b, new_pair(new_port(VAR,v3),k43));
    if (k41 != NONE) {
      link(net, tm, new_port(OPR, n1b), k41);
    } else {
      k41 = new_port(OPR, n1b);
    }
  }
  if (!k40) {
    node_create(net, n1a, new_pair(new_port(NUM,0x00000100),k41));
    if (k39 != NONE) {
      link(net, tm, new_port(OPR, n1a), k39);
    } else {
      k39 = new_port(OPR, n1a);
    }
  }
  bool k44 = 0;
  Port k45 = NONE;
  // fast oper
  if (get_tag(k38) == NUM && get_tag(new_port(NUM,0x00000140)) == NUM) {
    tm->itrs += 1;
    k44 = 1;
    k45 = new_port(NUM, operate(get_val(k38), get_val(new_port(NUM,0x00000140))));
  }
  bool k46 = 0;
  Port k47 = NONE;
  // fast oper
  if (get_tag(k45) == NUM && get_tag(new_port(VAR,v2)) == NUM) {
    tm->itrs += 1;
    k46 = 1;
    k47 = new_port(NUM, operate(get_val(k45), get_val(new_port(VAR,v2))));
  }
  if (k47 != NONE) {
    link(net, tm, new_port(VAR,v10), k47);
  } else {
    k47 = new_port(VAR,v10);
  }
  if (!k46) {
    node_create(net, n19, new_pair(new_port(VAR,v2),k47));
    if (k45 != NONE) {
      link(net, tm, new_port(OPR, n19), k45);
    } else {
      k45 = new_port(OPR, n19);
    }
  }
  if (!k44) {
    node_create(net, n18, new_pair(new_port(NUM,0x00000140),k45));
    if (k38 != NONE) {
      link(net, tm, new_port(OPR, n18), k38);
    } else {
      k38 = new_port(OPR, n18);
    }
  }
  if (!k37) {
    node_create(net, n17, new_pair(k38,k39));
    if (k31 != NONE) {
      link(net, tm, new_port(DUP,n17), k31);
    } else {
      k31 = new_port(DUP,n17);
    }
  }
  if (!k29) {
    node_create(net, n16, new_pair(k31,k32));
    if (k28 != NONE) {
      link(net, tm, new_port(CON,n16), k28);
    } else {
      k28 = new_port(CON,n16);
    }
  }
  bool k48 = 0;
  Port k49 = NONE;
  Port k50 = NONE;
  // fast copy
  if (get_tag(k27) == NUM) {
    tm->itrs += 1;
    k48 = 1;
    k49 = k27;
    k50 = k27;
  }
  if (k50 != NONE) {
    link(net, tm, new_port(VAR,vf), k50);
  } else {
    k50 = new_port(VAR,vf);
  }
  bool k51 = 0;
  Port k52 = NONE;
  // fast oper
  if (get_tag(k49) == NUM && get_tag(new_port(NUM,0x08000009)) == NUM) {
    tm->itrs += 1;
    k51 = 1;
    k52 = new_port(NUM, operate(get_val(k49), get_val(new_port(NUM,0x08000009))));
  }
  if (k52 != NONE) {
    link(net, tm, new_port(VAR,ve), k52);
  } else {
    k52 = new_port(VAR,ve);
  }
  if (!k51) {
    node_create(net, n15, new_pair(new_port(NUM,0x08000009),k52));
    if (k49 != NONE) {
      link(net, tm, new_port(OPR, n15), k49);
    } else {
      k49 = new_port(OPR, n15);
    }
  }
  if (!k48) {
    node_create(net, n14, new_pair(k49,k50));
    if (k27 != NONE) {
      link(net, tm, new_port(DUP,n14), k27);
    } else {
      k27 = new_port(DUP,n14);
    }
  }
  if (!k25) {
    node_create(net, n13, new_pair(k27,k28));
    if (k24 != NONE) {
      link(net, tm, new_port(CON,n13), k24);
    } else {
      k24 = new_port(CON,n13);
    }
  }
  bool k53 = 0;
  Port k54 = NONE;
  // fast oper
  if (get_tag(k23) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k53 = 1;
    k54 = new_port(NUM, operate(get_val(k23), get_val(new_port(NUM,0x00000080))));
  }
  bool k55 = 0;
  Port k56 = NONE;
  // fast oper
  if (get_tag(k54) == NUM && get_tag(new_port(VAR,vc)) == NUM) {
    tm->itrs += 1;
    k55 = 1;
    k56 = new_port(NUM, operate(get_val(k54), get_val(new_port(VAR,vc))));
  }
  if (k56 != NONE) {
    link(net, tm, new_port(VAR,vd), k56);
  } else {
    k56 = new_port(VAR,vd);
  }
  if (!k55) {
    node_create(net, n12, new_pair(new_port(VAR,vc),k56));
    if (k54 != NONE) {
      link(net, tm, new_port(OPR, n12), k54);
    } else {
      k54 = new_port(OPR, n12);
    }
  }
  if (!k53) {
    node_create(net, n11, new_pair(new_port(NUM,0x00000080),k54));
    if (k23 != NONE) {
      link(net, tm, new_port(OPR, n11), k23);
    } else {
      k23 = new_port(OPR, n11);
    }
  }
  if (!k21) {
    node_create(net, n10, new_pair(k23,k24));
    if (k20 != NONE) {
      link(net, tm, new_port(CON,n10), k20);
    } else {
      k20 = new_port(CON,n10);
    }
  }
  bool k57 = 0;
  Port k58 = NONE;
  Port k59 = NONE;
  // fast copy
  if (get_tag(k19) == NUM) {
    tm->itrs += 1;
    k57 = 1;
    k58 = k19;
    k59 = k19;
  }
  if (k59 != NONE) {
    link(net, tm, new_port(VAR,vb), k59);
  } else {
    k59 = new_port(VAR,vb);
  }
  if (k58 != NONE) {
    link(net, tm, new_port(VAR,va), k58);
  } else {
    k58 = new_port(VAR,va);
  }
  if (!k57) {
    node_create(net, nf, new_pair(k58,k59));
    if (k19 != NONE) {
      link(net, tm, new_port(DUP,nf), k19);
    } else {
      k19 = new_port(DUP,nf);
    }
  }
  if (!k17) {
    node_create(net, ne, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,ne), k16);
    } else {
      k16 = new_port(CON,ne);
    }
  }
  bool k60 = 0;
  Port k61 = NONE;
  Port k62 = NONE;
  // fast copy
  if (get_tag(k15) == NUM) {
    tm->itrs += 1;
    k60 = 1;
    k61 = k15;
    k62 = k15;
  }
  if (k62 != NONE) {
    link(net, tm, new_port(VAR,v9), k62);
  } else {
    k62 = new_port(VAR,v9);
  }
  bool k63 = 0;
  Port k64 = NONE;
  // fast oper
  if (get_tag(k61) == NUM && get_tag(new_port(NUM,0x08000009)) == NUM) {
    tm->itrs += 1;
    k63 = 1;
    k64 = new_port(NUM, operate(get_val(k61), get_val(new_port(NUM,0x08000009))));
  }
  if (k64 != NONE) {
    link(net, tm, new_port(VAR,v8), k64);
  } else {
    k64 = new_port(VAR,v8);
  }
  if (!k63) {
    node_create(net, nd, new_pair(new_port(NUM,0x08000009),k64));
    if (k61 != NONE) {
      link(net, tm, new_port(OPR, nd), k61);
    } else {
      k61 = new_port(OPR, nd);
    }
  }
  if (!k60) {
    node_create(net, nc, new_pair(k61,k62));
    if (k15 != NONE) {
      link(net, tm, new_port(DUP,nc), k15);
    } else {
      k15 = new_port(DUP,nc);
    }
  }
  if (!k13) {
    node_create(net, nb, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,nb), k12);
    } else {
      k12 = new_port(CON,nb);
    }
  }
  bool k65 = 0;
  Port k66 = NONE;
  Port k67 = NONE;
  // fast copy
  if (get_tag(k11) == NUM) {
    tm->itrs += 1;
    k65 = 1;
    k66 = k11;
    k67 = k11;
  }
  bool k68 = 0;
  Port k69 = NONE;
  Port k70 = NONE;
  // fast copy
  if (get_tag(k67) == NUM) {
    tm->itrs += 1;
    k68 = 1;
    k69 = k67;
    k70 = k67;
  }
  bool k71 = 0;
  Port k72 = NONE;
  Port k73 = NONE;
  // fast copy
  if (get_tag(k70) == NUM) {
    tm->itrs += 1;
    k71 = 1;
    k72 = k70;
    k73 = k70;
  }
  bool k74 = 0;
  Port k75 = NONE;
  // fast oper
  if (get_tag(k73) == NUM && get_tag(new_port(NUM,0x08000009)) == NUM) {
    tm->itrs += 1;
    k74 = 1;
    k75 = new_port(NUM, operate(get_val(k73), get_val(new_port(NUM,0x08000009))));
  }
  if (k75 != NONE) {
    link(net, tm, new_port(VAR,v7), k75);
  } else {
    k75 = new_port(VAR,v7);
  }
  if (!k74) {
    node_create(net, na, new_pair(new_port(NUM,0x08000009),k75));
    if (k73 != NONE) {
      link(net, tm, new_port(OPR, na), k73);
    } else {
      k73 = new_port(OPR, na);
    }
  }
  if (k72 != NONE) {
    link(net, tm, new_port(VAR,v6), k72);
  } else {
    k72 = new_port(VAR,v6);
  }
  if (!k71) {
    node_create(net, n9, new_pair(k72,k73));
    if (k70 != NONE) {
      link(net, tm, new_port(DUP,n9), k70);
    } else {
      k70 = new_port(DUP,n9);
    }
  }
  bool k76 = 0;
  Port k77 = NONE;
  // fast oper
  if (get_tag(k69) == NUM && get_tag(new_port(NUM,0x08000009)) == NUM) {
    tm->itrs += 1;
    k76 = 1;
    k77 = new_port(NUM, operate(get_val(k69), get_val(new_port(NUM,0x08000009))));
  }
  if (k77 != NONE) {
    link(net, tm, new_port(VAR,v5), k77);
  } else {
    k77 = new_port(VAR,v5);
  }
  if (!k76) {
    node_create(net, n8, new_pair(new_port(NUM,0x08000009),k77));
    if (k69 != NONE) {
      link(net, tm, new_port(OPR, n8), k69);
    } else {
      k69 = new_port(OPR, n8);
    }
  }
  if (!k68) {
    node_create(net, n7, new_pair(k69,k70));
    if (k67 != NONE) {
      link(net, tm, new_port(DUP,n7), k67);
    } else {
      k67 = new_port(DUP,n7);
    }
  }
  if (k66 != NONE) {
    link(net, tm, new_port(VAR,v4), k66);
  } else {
    k66 = new_port(VAR,v4);
  }
  if (!k65) {
    node_create(net, n6, new_pair(k66,k67));
    if (k11 != NONE) {
      link(net, tm, new_port(DUP,n6), k11);
    } else {
      k11 = new_port(DUP,n6);
    }
  }
  if (!k9) {
    node_create(net, n5, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n5), k8);
    } else {
      k8 = new_port(CON,n5);
    }
  }
  bool k78 = 0;
  Port k79 = NONE;
  Port k80 = NONE;
  // fast copy
  if (get_tag(k7) == NUM) {
    tm->itrs += 1;
    k78 = 1;
    k79 = k7;
    k80 = k7;
  }
  if (k80 != NONE) {
    link(net, tm, new_port(VAR,v3), k80);
  } else {
    k80 = new_port(VAR,v3);
  }
  if (k79 != NONE) {
    link(net, tm, new_port(VAR,v2), k79);
  } else {
    k79 = new_port(VAR,v2);
  }
  if (!k78) {
    node_create(net, n4, new_pair(k79,k80));
    if (k7 != NONE) {
      link(net, tm, new_port(DUP,n4), k7);
    } else {
      k7 = new_port(DUP,n4);
    }
  }
  if (!k5) {
    node_create(net, n3, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n3), k4);
    } else {
      k4 = new_port(CON,n3);
    }
  }
  bool k81 = 0;
  Port k82 = NONE;
  // fast oper
  if (get_tag(k3) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k81 = 1;
    k82 = new_port(NUM, operate(get_val(k3), get_val(new_port(NUM,0x00000080))));
  }
  bool k83 = 0;
  Port k84 = NONE;
  // fast oper
  if (get_tag(k82) == NUM && get_tag(new_port(VAR,v0)) == NUM) {
    tm->itrs += 1;
    k83 = 1;
    k84 = new_port(NUM, operate(get_val(k82), get_val(new_port(VAR,v0))));
  }
  if (k84 != NONE) {
    link(net, tm, new_port(VAR,v1), k84);
  } else {
    k84 = new_port(VAR,v1);
  }
  if (!k83) {
    node_create(net, n2, new_pair(new_port(VAR,v0),k84));
    if (k82 != NONE) {
      link(net, tm, new_port(OPR, n2), k82);
    } else {
      k82 = new_port(OPR, n2);
    }
  }
  if (!k81) {
    node_create(net, n1, new_pair(new_port(NUM,0x00000080),k82));
    if (k3 != NONE) {
      link(net, tm, new_port(OPR, n1), k3);
    } else {
      k3 = new_port(OPR, n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n1e, new_pair(new_port(VAR,vd),new_port(VAR,v12)));
  node_create(net, n1d, new_pair(new_port(VAR,v1),new_port(CON,n1e)));
  link(net, tm, new_port(REF,0x00000011), new_port(CON,n1d));
  node_create(net, n29, new_pair(new_port(VAR,v9),new_port(VAR,v0)));
  node_create(net, n28, new_pair(new_port(NUM,0x00000100),new_port(OPR,n29)));
  node_create(net, n27, new_pair(new_port(VAR,va),new_port(OPR,n28)));
  node_create(net, n26, new_pair(new_port(NUM,0x000000e0),new_port(OPR,n27)));
  node_create(net, n25, new_pair(new_port(VAR,v8),new_port(OPR,n26)));
  node_create(net, n24, new_pair(new_port(NUM,0x000000a0),new_port(OPR,n25)));
  node_create(net, n23, new_pair(new_port(VAR,v5),new_port(OPR,n24)));
  node_create(net, n22, new_pair(new_port(NUM,0x00000080),new_port(OPR,n23)));
  node_create(net, n21, new_pair(new_port(VAR,v4),new_port(OPR,n22)));
  node_create(net, n20, new_pair(new_port(NUM,0x000000e0),new_port(OPR,n21)));
  node_create(net, n1f, new_pair(new_port(VAR,v10),new_port(OPR,n20)));
  link(net, tm, new_port(REF,0x00000014), new_port(CON,n1f));
  node_create(net, n34, new_pair(new_port(VAR,vf),new_port(VAR,vc)));
  node_create(net, n33, new_pair(new_port(NUM,0x00000100),new_port(OPR,n34)));
  node_create(net, n32, new_pair(new_port(VAR,vb),new_port(OPR,n33)));
  node_create(net, n31, new_pair(new_port(NUM,0x000000e0),new_port(OPR,n32)));
  node_create(net, n30, new_pair(new_port(VAR,ve),new_port(OPR,n31)));
  node_create(net, n2f, new_pair(new_port(NUM,0x000000a0),new_port(OPR,n30)));
  node_create(net, n2e, new_pair(new_port(VAR,v7),new_port(OPR,n2f)));
  node_create(net, n2d, new_pair(new_port(NUM,0x00000080),new_port(OPR,n2e)));
  node_create(net, n2c, new_pair(new_port(VAR,v6),new_port(OPR,n2d)));
  node_create(net, n2b, new_pair(new_port(NUM,0x000000e0),new_port(OPR,n2c)));
  node_create(net, n2a, new_pair(new_port(VAR,v11),new_port(OPR,n2b)));
  link(net, tm, new_port(REF,0x00000014), new_port(CON,n2a));
  return true;
}

bool interact_call_jshark_grid__bend0__C1(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val vb = vars_alloc_1(net, tm, &vl);
  Val vc = vars_alloc_1(net, tm, &vl);
  Val vd = vars_alloc_1(net, tm, &vl);
  Val ve = vars_alloc_1(net, tm, &vl);
  Val vf = vars_alloc_1(net, tm, &vl);
  Val v10 = vars_alloc_1(net, tm, &vl);
  Val v11 = vars_alloc_1(net, tm, &vl);
  Val v12 = vars_alloc_1(net, tm, &vl);
  Val v13 = vars_alloc_1(net, tm, &vl);
  Val v14 = vars_alloc_1(net, tm, &vl);
  Val v15 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  Val n16 = node_alloc_1(net, tm, &nl);
  Val n17 = node_alloc_1(net, tm, &nl);
  Val n18 = node_alloc_1(net, tm, &nl);
  Val n19 = node_alloc_1(net, tm, &nl);
  Val n1a = node_alloc_1(net, tm, &nl);
  Val n1b = node_alloc_1(net, tm, &nl);
  Val n1c = node_alloc_1(net, tm, &nl);
  Val n1d = node_alloc_1(net, tm, &nl);
  Val n1e = node_alloc_1(net, tm, &nl);
  Val n1f = node_alloc_1(net, tm, &nl);
  Val n20 = node_alloc_1(net, tm, &nl);
  Val n21 = node_alloc_1(net, tm, &nl);
  Val n22 = node_alloc_1(net, tm, &nl);
  Val n23 = node_alloc_1(net, tm, &nl);
  Val n24 = node_alloc_1(net, tm, &nl);
  Val n25 = node_alloc_1(net, tm, &nl);
  Val n26 = node_alloc_1(net, tm, &nl);
  Val n27 = node_alloc_1(net, tm, &nl);
  Val n28 = node_alloc_1(net, tm, &nl);
  Val n29 = node_alloc_1(net, tm, &nl);
  Val n2a = node_alloc_1(net, tm, &nl);
  Val n2b = node_alloc_1(net, tm, &nl);
  Val n2c = node_alloc_1(net, tm, &nl);
  Val n2d = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !vb || !vc || !vd || !ve || !vf || !v10 || !v11 || !v12 || !v13 || !v14 || !v15 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15 || !n16 || !n17 || !n18 || !n19 || !n1a || !n1b || !n1c || !n1d || !n1e || !n1f || !n20 || !n21 || !n22 || !n23 || !n24 || !n25 || !n26 || !n27 || !n28 || !n29 || !n2a || !n2b || !n2c || !n2d) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  vars_create(net, vb, NONE);
  vars_create(net, vc, NONE);
  vars_create(net, vd, NONE);
  vars_create(net, ve, NONE);
  vars_create(net, vf, NONE);
  vars_create(net, v10, NONE);
  vars_create(net, v11, NONE);
  vars_create(net, v12, NONE);
  vars_create(net, v13, NONE);
  vars_create(net, v14, NONE);
  vars_create(net, v15, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  bool k21 = 0;
  Pair k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast anni
  if (get_tag(k20) == CON && node_load(net, get_val(k20)) != 0) {
    tm->itrs += 1;
    k21 = 1;
    k22 = node_take(net, get_val(k20));
    k23 = get_fst(k22);
    k24 = get_snd(k22);
  }
  bool k25 = 0;
  Pair k26 = 0;
  Port k27 = NONE;
  Port k28 = NONE;
  // fast anni
  if (get_tag(k24) == CON && node_load(net, get_val(k24)) != 0) {
    tm->itrs += 1;
    k25 = 1;
    k26 = node_take(net, get_val(k24));
    k27 = get_fst(k26);
    k28 = get_snd(k26);
  }
  bool k29 = 0;
  Pair k30 = 0;
  Port k31 = NONE;
  Port k32 = NONE;
  // fast anni
  if (get_tag(k28) == CON && node_load(net, get_val(k28)) != 0) {
    tm->itrs += 1;
    k29 = 1;
    k30 = node_take(net, get_val(k28));
    k31 = get_fst(k30);
    k32 = get_snd(k30);
  }
  bool k33 = 0;
  Pair k34 = 0;
  Port k35 = NONE;
  Port k36 = NONE;
  // fast anni
  if (get_tag(k32) == CON && node_load(net, get_val(k32)) != 0) {
    tm->itrs += 1;
    k33 = 1;
    k34 = node_take(net, get_val(k32));
    k35 = get_fst(k34);
    k36 = get_snd(k34);
  }
  bool k37 = 0;
  Pair k38 = 0;
  Port k39 = NONE;
  Port k40 = NONE;
  // fast anni
  if (get_tag(k36) == CON && node_load(net, get_val(k36)) != 0) {
    tm->itrs += 1;
    k37 = 1;
    k38 = node_take(net, get_val(k36));
    k39 = get_fst(k38);
    k40 = get_snd(k38);
  }
  bool k41 = 0;
  Pair k42 = 0;
  Port k43 = NONE;
  Port k44 = NONE;
  // fast anni
  if (get_tag(k40) == CON && node_load(net, get_val(k40)) != 0) {
    tm->itrs += 1;
    k41 = 1;
    k42 = node_take(net, get_val(k40));
    k43 = get_fst(k42);
    k44 = get_snd(k42);
  }
  if (k44 != NONE) {
    link(net, tm, new_port(VAR,v15), k44);
  } else {
    k44 = new_port(VAR,v15);
  }
  if (k43 != NONE) {
    link(net, tm, new_port(VAR,v14), k43);
  } else {
    k43 = new_port(VAR,v14);
  }
  if (!k41) {
    node_create(net, n1b, new_pair(k43,k44));
    if (k40 != NONE) {
      link(net, tm, new_port(CON,n1b), k40);
    } else {
      k40 = new_port(CON,n1b);
    }
  }
  bool k45 = 0;
  Port k46 = NONE;
  Port k47 = NONE;
  // fast copy
  if (get_tag(k39) == NUM) {
    tm->itrs += 1;
    k45 = 1;
    k46 = k39;
    k47 = k39;
  }
  bool k48 = 0;
  Port k49 = NONE;
  Port k50 = NONE;
  // fast copy
  if (get_tag(k47) == NUM) {
    tm->itrs += 1;
    k48 = 1;
    k49 = k47;
    k50 = k47;
  }
  if (k50 != NONE) {
    link(net, tm, new_port(VAR,v13), k50);
  } else {
    k50 = new_port(VAR,v13);
  }
  if (k49 != NONE) {
    link(net, tm, new_port(VAR,v11), k49);
  } else {
    k49 = new_port(VAR,v11);
  }
  if (!k48) {
    node_create(net, n1a, new_pair(k49,k50));
    if (k47 != NONE) {
      link(net, tm, new_port(DUP,n1a), k47);
    } else {
      k47 = new_port(DUP,n1a);
    }
  }
  if (k46 != NONE) {
    link(net, tm, new_port(VAR,vf), k46);
  } else {
    k46 = new_port(VAR,vf);
  }
  if (!k45) {
    node_create(net, n19, new_pair(k46,k47));
    if (k39 != NONE) {
      link(net, tm, new_port(DUP,n19), k39);
    } else {
      k39 = new_port(DUP,n19);
    }
  }
  if (!k37) {
    node_create(net, n18, new_pair(k39,k40));
    if (k36 != NONE) {
      link(net, tm, new_port(CON,n18), k36);
    } else {
      k36 = new_port(CON,n18);
    }
  }
  bool k51 = 0;
  Port k52 = NONE;
  Port k53 = NONE;
  // fast copy
  if (get_tag(k35) == NUM) {
    tm->itrs += 1;
    k51 = 1;
    k52 = k35;
    k53 = k35;
  }
  bool k54 = 0;
  Port k55 = NONE;
  Port k56 = NONE;
  // fast copy
  if (get_tag(k53) == NUM) {
    tm->itrs += 1;
    k54 = 1;
    k55 = k53;
    k56 = k53;
  }
  bool k57 = 0;
  Port k58 = NONE;
  // fast oper
  if (get_tag(k56) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k57 = 1;
    k58 = new_port(NUM, operate(get_val(k56), get_val(new_port(NUM,0x00000080))));
  }
  bool k59 = 0;
  Port k60 = NONE;
  // fast oper
  if (get_tag(k58) == NUM && get_tag(new_port(VAR,v11)) == NUM) {
    tm->itrs += 1;
    k59 = 1;
    k60 = new_port(NUM, operate(get_val(k58), get_val(new_port(VAR,v11))));
  }
  bool k61 = 0;
  Port k62 = NONE;
  // fast oper
  if (get_tag(k60) == NUM && get_tag(new_port(NUM,0x00000049)) == NUM) {
    tm->itrs += 1;
    k61 = 1;
    k62 = new_port(NUM, operate(get_val(k60), get_val(new_port(NUM,0x00000049))));
  }
  if (k62 != NONE) {
    link(net, tm, new_port(VAR,v12), k62);
  } else {
    k62 = new_port(VAR,v12);
  }
  if (!k61) {
    node_create(net, n17, new_pair(new_port(NUM,0x00000049),k62));
    if (k60 != NONE) {
      link(net, tm, new_port(OPR, n17), k60);
    } else {
      k60 = new_port(OPR, n17);
    }
  }
  if (!k59) {
    node_create(net, n16, new_pair(new_port(VAR,v11),k60));
    if (k58 != NONE) {
      link(net, tm, new_port(OPR, n16), k58);
    } else {
      k58 = new_port(OPR, n16);
    }
  }
  if (!k57) {
    node_create(net, n15, new_pair(new_port(NUM,0x00000080),k58));
    if (k56 != NONE) {
      link(net, tm, new_port(OPR, n15), k56);
    } else {
      k56 = new_port(OPR, n15);
    }
  }
  bool k63 = 0;
  Port k64 = NONE;
  // fast oper
  if (get_tag(k55) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k63 = 1;
    k64 = new_port(NUM, operate(get_val(k55), get_val(new_port(NUM,0x00000080))));
  }
  bool k65 = 0;
  Port k66 = NONE;
  // fast oper
  if (get_tag(k64) == NUM && get_tag(new_port(VAR,vf)) == NUM) {
    tm->itrs += 1;
    k65 = 1;
    k66 = new_port(NUM, operate(get_val(k64), get_val(new_port(VAR,vf))));
  }
  bool k67 = 0;
  Port k68 = NONE;
  // fast oper
  if (get_tag(k66) == NUM && get_tag(new_port(NUM,0x00000049)) == NUM) {
    tm->itrs += 1;
    k67 = 1;
    k68 = new_port(NUM, operate(get_val(k66), get_val(new_port(NUM,0x00000049))));
  }
  if (k68 != NONE) {
    link(net, tm, new_port(VAR,v10), k68);
  } else {
    k68 = new_port(VAR,v10);
  }
  if (!k67) {
    node_create(net, n14, new_pair(new_port(NUM,0x00000049),k68));
    if (k66 != NONE) {
      link(net, tm, new_port(OPR, n14), k66);
    } else {
      k66 = new_port(OPR, n14);
    }
  }
  if (!k65) {
    node_create(net, n13, new_pair(new_port(VAR,vf),k66));
    if (k64 != NONE) {
      link(net, tm, new_port(OPR, n13), k64);
    } else {
      k64 = new_port(OPR, n13);
    }
  }
  if (!k63) {
    node_create(net, n12, new_pair(new_port(NUM,0x00000080),k64));
    if (k55 != NONE) {
      link(net, tm, new_port(OPR, n12), k55);
    } else {
      k55 = new_port(OPR, n12);
    }
  }
  if (!k54) {
    node_create(net, n11, new_pair(k55,k56));
    if (k53 != NONE) {
      link(net, tm, new_port(DUP,n11), k53);
    } else {
      k53 = new_port(DUP,n11);
    }
  }
  if (k52 != NONE) {
    link(net, tm, new_port(VAR,ve), k52);
  } else {
    k52 = new_port(VAR,ve);
  }
  if (!k51) {
    node_create(net, n10, new_pair(k52,k53));
    if (k35 != NONE) {
      link(net, tm, new_port(DUP,n10), k35);
    } else {
      k35 = new_port(DUP,n10);
    }
  }
  if (!k33) {
    node_create(net, nf, new_pair(k35,k36));
    if (k32 != NONE) {
      link(net, tm, new_port(CON,nf), k32);
    } else {
      k32 = new_port(CON,nf);
    }
  }
  bool k69 = 0;
  Port k70 = NONE;
  Port k71 = NONE;
  // fast copy
  if (get_tag(k31) == NUM) {
    tm->itrs += 1;
    k69 = 1;
    k70 = k31;
    k71 = k31;
  }
  if (k71 != NONE) {
    link(net, tm, new_port(VAR,vd), k71);
  } else {
    k71 = new_port(VAR,vd);
  }
  if (k70 != NONE) {
    link(net, tm, new_port(VAR,vc), k70);
  } else {
    k70 = new_port(VAR,vc);
  }
  if (!k69) {
    node_create(net, ne, new_pair(k70,k71));
    if (k31 != NONE) {
      link(net, tm, new_port(DUP,ne), k31);
    } else {
      k31 = new_port(DUP,ne);
    }
  }
  if (!k29) {
    node_create(net, nd, new_pair(k31,k32));
    if (k28 != NONE) {
      link(net, tm, new_port(CON,nd), k28);
    } else {
      k28 = new_port(CON,nd);
    }
  }
  bool k72 = 0;
  Port k73 = NONE;
  Port k74 = NONE;
  // fast copy
  if (get_tag(k27) == NUM) {
    tm->itrs += 1;
    k72 = 1;
    k73 = k27;
    k74 = k27;
  }
  if (k74 != NONE) {
    link(net, tm, new_port(VAR,vb), k74);
  } else {
    k74 = new_port(VAR,vb);
  }
  if (k73 != NONE) {
    link(net, tm, new_port(VAR,va), k73);
  } else {
    k73 = new_port(VAR,va);
  }
  if (!k72) {
    node_create(net, nc, new_pair(k73,k74));
    if (k27 != NONE) {
      link(net, tm, new_port(DUP,nc), k27);
    } else {
      k27 = new_port(DUP,nc);
    }
  }
  if (!k25) {
    node_create(net, nb, new_pair(k27,k28));
    if (k24 != NONE) {
      link(net, tm, new_port(CON,nb), k24);
    } else {
      k24 = new_port(CON,nb);
    }
  }
  bool k75 = 0;
  Port k76 = NONE;
  Port k77 = NONE;
  // fast copy
  if (get_tag(k23) == NUM) {
    tm->itrs += 1;
    k75 = 1;
    k76 = k23;
    k77 = k23;
  }
  if (k77 != NONE) {
    link(net, tm, new_port(VAR,v9), k77);
  } else {
    k77 = new_port(VAR,v9);
  }
  if (k76 != NONE) {
    link(net, tm, new_port(VAR,v8), k76);
  } else {
    k76 = new_port(VAR,v8);
  }
  if (!k75) {
    node_create(net, na, new_pair(k76,k77));
    if (k23 != NONE) {
      link(net, tm, new_port(DUP,na), k23);
    } else {
      k23 = new_port(DUP,na);
    }
  }
  if (!k21) {
    node_create(net, n9, new_pair(k23,k24));
    if (k20 != NONE) {
      link(net, tm, new_port(CON,n9), k20);
    } else {
      k20 = new_port(CON,n9);
    }
  }
  bool k78 = 0;
  Port k79 = NONE;
  Port k80 = NONE;
  // fast copy
  if (get_tag(k19) == NUM) {
    tm->itrs += 1;
    k78 = 1;
    k79 = k19;
    k80 = k19;
  }
  if (k80 != NONE) {
    link(net, tm, new_port(VAR,v7), k80);
  } else {
    k80 = new_port(VAR,v7);
  }
  if (k79 != NONE) {
    link(net, tm, new_port(VAR,v6), k79);
  } else {
    k79 = new_port(VAR,v6);
  }
  if (!k78) {
    node_create(net, n8, new_pair(k79,k80));
    if (k19 != NONE) {
      link(net, tm, new_port(DUP,n8), k19);
    } else {
      k19 = new_port(DUP,n8);
    }
  }
  if (!k17) {
    node_create(net, n7, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,n7), k16);
    } else {
      k16 = new_port(CON,n7);
    }
  }
  bool k81 = 0;
  Port k82 = NONE;
  Port k83 = NONE;
  // fast copy
  if (get_tag(k15) == NUM) {
    tm->itrs += 1;
    k81 = 1;
    k82 = k15;
    k83 = k15;
  }
  if (k83 != NONE) {
    link(net, tm, new_port(VAR,v5), k83);
  } else {
    k83 = new_port(VAR,v5);
  }
  if (k82 != NONE) {
    link(net, tm, new_port(VAR,v4), k82);
  } else {
    k82 = new_port(VAR,v4);
  }
  if (!k81) {
    node_create(net, n6, new_pair(k82,k83));
    if (k15 != NONE) {
      link(net, tm, new_port(DUP,n6), k15);
    } else {
      k15 = new_port(DUP,n6);
    }
  }
  if (!k13) {
    node_create(net, n5, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n5), k12);
    } else {
      k12 = new_port(CON,n5);
    }
  }
  bool k84 = 0;
  Port k85 = NONE;
  Port k86 = NONE;
  // fast copy
  if (get_tag(k11) == NUM) {
    tm->itrs += 1;
    k84 = 1;
    k85 = k11;
    k86 = k11;
  }
  if (k86 != NONE) {
    link(net, tm, new_port(VAR,v3), k86);
  } else {
    k86 = new_port(VAR,v3);
  }
  if (k85 != NONE) {
    link(net, tm, new_port(VAR,v2), k85);
  } else {
    k85 = new_port(VAR,v2);
  }
  if (!k84) {
    node_create(net, n4, new_pair(k85,k86));
    if (k11 != NONE) {
      link(net, tm, new_port(DUP,n4), k11);
    } else {
      k11 = new_port(DUP,n4);
    }
  }
  if (!k9) {
    node_create(net, n3, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n3), k8);
    } else {
      k8 = new_port(CON,n3);
    }
  }
  bool k87 = 0;
  Port k88 = NONE;
  Port k89 = NONE;
  // fast copy
  if (get_tag(k7) == NUM) {
    tm->itrs += 1;
    k87 = 1;
    k88 = k7;
    k89 = k7;
  }
  if (k89 != NONE) {
    link(net, tm, new_port(VAR,v1), k89);
  } else {
    k89 = new_port(VAR,v1);
  }
  if (k88 != NONE) {
    link(net, tm, new_port(VAR,v0), k88);
  } else {
    k88 = new_port(VAR,v0);
  }
  if (!k87) {
    node_create(net, n2, new_pair(k88,k89));
    if (k7 != NONE) {
      link(net, tm, new_port(DUP,n2), k7);
    } else {
      k7 = new_port(DUP,n2);
    }
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  // fast void
  if (get_tag(k3) == ERA || get_tag(k3) == NUM) {
    tm->itrs += 1;
  } else {
    if (k3 != NONE) {
      link(net, tm, new_port(ERA,0x00000000), k3);
    } else {
      k3 = new_port(ERA,0x00000000);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n24, new_pair(new_port(VAR,v10),new_port(VAR,v14)));
  node_create(net, n23, new_pair(new_port(VAR,ve),new_port(CON,n24)));
  node_create(net, n22, new_pair(new_port(VAR,vc),new_port(CON,n23)));
  node_create(net, n21, new_pair(new_port(VAR,va),new_port(CON,n22)));
  node_create(net, n20, new_pair(new_port(VAR,v8),new_port(CON,n21)));
  node_create(net, n1f, new_pair(new_port(VAR,v6),new_port(CON,n20)));
  node_create(net, n1e, new_pair(new_port(VAR,v4),new_port(CON,n1f)));
  node_create(net, n1d, new_pair(new_port(VAR,v2),new_port(CON,n1e)));
  node_create(net, n1c, new_pair(new_port(VAR,v0),new_port(CON,n1d)));
  link(net, tm, new_port(REF,0x10000007), new_port(CON,n1c));
  node_create(net, n2d, new_pair(new_port(VAR,v13),new_port(VAR,v15)));
  node_create(net, n2c, new_pair(new_port(VAR,v12),new_port(CON,n2d)));
  node_create(net, n2b, new_pair(new_port(VAR,vd),new_port(CON,n2c)));
  node_create(net, n2a, new_pair(new_port(VAR,vb),new_port(CON,n2b)));
  node_create(net, n29, new_pair(new_port(VAR,v9),new_port(CON,n2a)));
  node_create(net, n28, new_pair(new_port(VAR,v7),new_port(CON,n29)));
  node_create(net, n27, new_pair(new_port(VAR,v5),new_port(CON,n28)));
  node_create(net, n26, new_pair(new_port(VAR,v3),new_port(CON,n27)));
  node_create(net, n25, new_pair(new_port(VAR,v1),new_port(CON,n26)));
  link(net, tm, new_port(REF,0x10000007), new_port(CON,n25));
  return true;
}

bool interact_call_main__bend0(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v3), k8);
  } else {
    k8 = new_port(VAR,v3);
  }
  bool k9 = 0;
  Port k10 = NONE;
  Port k11 = NONE;
  // fast copy
  if (get_tag(k7) == NUM) {
    tm->itrs += 1;
    k9 = 1;
    k10 = k7;
    k11 = k7;
  }
  if (k11 != NONE) {
    link(net, tm, new_port(VAR,v2), k11);
  } else {
    k11 = new_port(VAR,v2);
  }
  bool k12 = 0;
  Port k13 = NONE;
  // fast oper
  if (get_tag(k10) == NUM && get_tag(new_port(NUM,0x000000a0)) == NUM) {
    tm->itrs += 1;
    k12 = 1;
    k13 = new_port(NUM, operate(get_val(k10), get_val(new_port(NUM,0x000000a0))));
  }
  bool k14 = 0;
  Port k15 = NONE;
  // fast oper
  if (get_tag(k13) == NUM && get_tag(new_port(VAR,v0)) == NUM) {
    tm->itrs += 1;
    k14 = 1;
    k15 = new_port(NUM, operate(get_val(k13), get_val(new_port(VAR,v0))));
  }
  bool k16 = 0;
  Port k17 = NONE;
  // fast oper
  if (get_tag(k15) == NUM && get_tag(new_port(NUM,0x0000002e)) == NUM) {
    tm->itrs += 1;
    k16 = 1;
    k17 = new_port(NUM, operate(get_val(k15), get_val(new_port(NUM,0x0000002e))));
  }
  node_create(net, n8, new_pair(new_port(REF,0x0000000b),new_port(REF,0x0000000c)));
  node_create(net, na, new_pair(new_port(VAR,v2),new_port(VAR,v3)));
  node_create(net, n9, new_pair(new_port(VAR,v1),new_port(CON,na)));
  node_create(net, n7, new_pair(new_port(CON,n8),new_port(CON,n9)));
  if (k17 != NONE) {
    link(net, tm, new_port(SWI,n7), k17);
  } else {
    k17 = new_port(SWI,n7);
  }
  if (!k16) {
    node_create(net, n6, new_pair(new_port(NUM,0x0000002e),k17));
    if (k15 != NONE) {
      link(net, tm, new_port(OPR, n6), k15);
    } else {
      k15 = new_port(OPR, n6);
    }
  }
  if (!k14) {
    node_create(net, n5, new_pair(new_port(VAR,v0),k15));
    if (k13 != NONE) {
      link(net, tm, new_port(OPR, n5), k13);
    } else {
      k13 = new_port(OPR, n5);
    }
  }
  if (!k12) {
    node_create(net, n4, new_pair(new_port(NUM,0x000000a0),k13));
    if (k10 != NONE) {
      link(net, tm, new_port(OPR, n4), k10);
    } else {
      k10 = new_port(OPR, n4);
    }
  }
  if (!k9) {
    node_create(net, n3, new_pair(k10,k11));
    if (k7 != NONE) {
      link(net, tm, new_port(DUP,n3), k7);
    } else {
      k7 = new_port(DUP,n3);
    }
  }
  if (!k5) {
    node_create(net, n2, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n2), k4);
    } else {
      k4 = new_port(CON,n2);
    }
  }
  bool k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast copy
  if (get_tag(k3) == NUM) {
    tm->itrs += 1;
    k18 = 1;
    k19 = k3;
    k20 = k3;
  }
  if (k20 != NONE) {
    link(net, tm, new_port(VAR,v1), k20);
  } else {
    k20 = new_port(VAR,v1);
  }
  if (k19 != NONE) {
    link(net, tm, new_port(VAR,v0), k19);
  } else {
    k19 = new_port(VAR,v0);
  }
  if (!k18) {
    node_create(net, n1, new_pair(k19,k20));
    if (k3 != NONE) {
      link(net, tm, new_port(DUP,n1), k3);
    } else {
      k3 = new_port(DUP,n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_main__bend0__C0(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v2), k8);
  } else {
    k8 = new_port(VAR,v2);
  }
  // fast void
  if (get_tag(k7) == ERA || get_tag(k7) == NUM) {
    tm->itrs += 1;
  } else {
    if (k7 != NONE) {
      link(net, tm, new_port(ERA,0x00000000), k7);
    } else {
      k7 = new_port(ERA,0x00000000);
    }
  }
  if (!k5) {
    node_create(net, n4, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n4), k4);
    } else {
      k4 = new_port(CON,n4);
    }
  }
  bool k9 = 0;
  Port k10 = NONE;
  Port k11 = NONE;
  // fast copy
  if (get_tag(k3) == NUM) {
    tm->itrs += 1;
    k9 = 1;
    k10 = k3;
    k11 = k3;
  }
  bool k12 = 0;
  Port k13 = NONE;
  // fast oper
  if (get_tag(k11) == NUM && get_tag(new_port(NUM,0x00000809)) == NUM) {
    tm->itrs += 1;
    k12 = 1;
    k13 = new_port(NUM, operate(get_val(k11), get_val(new_port(NUM,0x00000809))));
  }
  if (k13 != NONE) {
    link(net, tm, new_port(VAR,v1), k13);
  } else {
    k13 = new_port(VAR,v1);
  }
  if (!k12) {
    node_create(net, n3, new_pair(new_port(NUM,0x00000809),k13));
    if (k11 != NONE) {
      link(net, tm, new_port(OPR, n3), k11);
    } else {
      k11 = new_port(OPR, n3);
    }
  }
  bool k14 = 0;
  Port k15 = NONE;
  // fast oper
  if (get_tag(k10) == NUM && get_tag(new_port(NUM,0x0000080b)) == NUM) {
    tm->itrs += 1;
    k14 = 1;
    k15 = new_port(NUM, operate(get_val(k10), get_val(new_port(NUM,0x0000080b))));
  }
  if (k15 != NONE) {
    link(net, tm, new_port(VAR,v0), k15);
  } else {
    k15 = new_port(VAR,v0);
  }
  if (!k14) {
    node_create(net, n2, new_pair(new_port(NUM,0x0000080b),k15));
    if (k10 != NONE) {
      link(net, tm, new_port(OPR, n2), k10);
    } else {
      k10 = new_port(OPR, n2);
    }
  }
  if (!k9) {
    node_create(net, n1, new_pair(k10,k11));
    if (k3 != NONE) {
      link(net, tm, new_port(DUP,n1), k3);
    } else {
      k3 = new_port(DUP,n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n5, new_pair(new_port(VAR,v3),new_port(VAR,v2)));
  link(net, tm, new_port(REF,0x00000001), new_port(CON,n5));
  node_create(net, n6, new_pair(new_port(VAR,v4),new_port(VAR,v3)));
  link(net, tm, new_port(REF,0x00000005), new_port(CON,n6));
  node_create(net, n8, new_pair(new_port(VAR,v6),new_port(VAR,v4)));
  node_create(net, n7, new_pair(new_port(VAR,v5),new_port(CON,n8)));
  link(net, tm, new_port(REF,0x00000011), new_port(CON,n7));
  node_create(net, nb, new_pair(new_port(NUM,0x08000006),new_port(VAR,v5)));
  node_create(net, na, new_pair(new_port(NUM,0x08400009),new_port(OPR,nb)));
  node_create(net, n9, new_pair(new_port(VAR,v0),new_port(OPR,na)));
  link(net, tm, new_port(REF,0x00000014), new_port(CON,n9));
  node_create(net, ne, new_pair(new_port(NUM,0x07f00006),new_port(VAR,v6)));
  node_create(net, nd, new_pair(new_port(NUM,0x08400009),new_port(OPR,ne)));
  node_create(net, nc, new_pair(new_port(VAR,v1),new_port(OPR,nd)));
  link(net, tm, new_port(REF,0x00000014), new_port(CON,nc));
  return true;
}

bool interact_call_main__bend0__C1(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  if (k12 != NONE) {
    link(net, tm, new_port(VAR,v6), k12);
  } else {
    k12 = new_port(VAR,v6);
  }
  bool k13 = 0;
  Port k14 = NONE;
  Port k15 = NONE;
  // fast copy
  if (get_tag(k11) == NUM) {
    tm->itrs += 1;
    k13 = 1;
    k14 = k11;
    k15 = k11;
  }
  bool k16 = 0;
  Port k17 = NONE;
  Port k18 = NONE;
  // fast copy
  if (get_tag(k15) == NUM) {
    tm->itrs += 1;
    k16 = 1;
    k17 = k15;
    k18 = k15;
  }
  if (k18 != NONE) {
    link(net, tm, new_port(VAR,v5), k18);
  } else {
    k18 = new_port(VAR,v5);
  }
  if (k17 != NONE) {
    link(net, tm, new_port(VAR,v3), k17);
  } else {
    k17 = new_port(VAR,v3);
  }
  if (!k16) {
    node_create(net, nc, new_pair(k17,k18));
    if (k15 != NONE) {
      link(net, tm, new_port(DUP,nc), k15);
    } else {
      k15 = new_port(DUP,nc);
    }
  }
  if (k14 != NONE) {
    link(net, tm, new_port(VAR,v1), k14);
  } else {
    k14 = new_port(VAR,v1);
  }
  if (!k13) {
    node_create(net, nb, new_pair(k14,k15));
    if (k11 != NONE) {
      link(net, tm, new_port(DUP,nb), k11);
    } else {
      k11 = new_port(DUP,nb);
    }
  }
  if (!k9) {
    node_create(net, na, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,na), k8);
    } else {
      k8 = new_port(CON,na);
    }
  }
  bool k19 = 0;
  Port k20 = NONE;
  Port k21 = NONE;
  // fast copy
  if (get_tag(k7) == NUM) {
    tm->itrs += 1;
    k19 = 1;
    k20 = k7;
    k21 = k7;
  }
  bool k22 = 0;
  Port k23 = NONE;
  Port k24 = NONE;
  // fast copy
  if (get_tag(k21) == NUM) {
    tm->itrs += 1;
    k22 = 1;
    k23 = k21;
    k24 = k21;
  }
  bool k25 = 0;
  Port k26 = NONE;
  // fast oper
  if (get_tag(k24) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k25 = 1;
    k26 = new_port(NUM, operate(get_val(k24), get_val(new_port(NUM,0x00000080))));
  }
  bool k27 = 0;
  Port k28 = NONE;
  // fast oper
  if (get_tag(k26) == NUM && get_tag(new_port(VAR,v3)) == NUM) {
    tm->itrs += 1;
    k27 = 1;
    k28 = new_port(NUM, operate(get_val(k26), get_val(new_port(VAR,v3))));
  }
  bool k29 = 0;
  Port k30 = NONE;
  // fast oper
  if (get_tag(k28) == NUM && get_tag(new_port(NUM,0x00000049)) == NUM) {
    tm->itrs += 1;
    k29 = 1;
    k30 = new_port(NUM, operate(get_val(k28), get_val(new_port(NUM,0x00000049))));
  }
  if (k30 != NONE) {
    link(net, tm, new_port(VAR,v4), k30);
  } else {
    k30 = new_port(VAR,v4);
  }
  if (!k29) {
    node_create(net, n9, new_pair(new_port(NUM,0x00000049),k30));
    if (k28 != NONE) {
      link(net, tm, new_port(OPR, n9), k28);
    } else {
      k28 = new_port(OPR, n9);
    }
  }
  if (!k27) {
    node_create(net, n8, new_pair(new_port(VAR,v3),k28));
    if (k26 != NONE) {
      link(net, tm, new_port(OPR, n8), k26);
    } else {
      k26 = new_port(OPR, n8);
    }
  }
  if (!k25) {
    node_create(net, n7, new_pair(new_port(NUM,0x00000080),k26));
    if (k24 != NONE) {
      link(net, tm, new_port(OPR, n7), k24);
    } else {
      k24 = new_port(OPR, n7);
    }
  }
  bool k31 = 0;
  Port k32 = NONE;
  // fast oper
  if (get_tag(k23) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k31 = 1;
    k32 = new_port(NUM, operate(get_val(k23), get_val(new_port(NUM,0x00000080))));
  }
  bool k33 = 0;
  Port k34 = NONE;
  // fast oper
  if (get_tag(k32) == NUM && get_tag(new_port(VAR,v1)) == NUM) {
    tm->itrs += 1;
    k33 = 1;
    k34 = new_port(NUM, operate(get_val(k32), get_val(new_port(VAR,v1))));
  }
  bool k35 = 0;
  Port k36 = NONE;
  // fast oper
  if (get_tag(k34) == NUM && get_tag(new_port(NUM,0x00000049)) == NUM) {
    tm->itrs += 1;
    k35 = 1;
    k36 = new_port(NUM, operate(get_val(k34), get_val(new_port(NUM,0x00000049))));
  }
  if (k36 != NONE) {
    link(net, tm, new_port(VAR,v2), k36);
  } else {
    k36 = new_port(VAR,v2);
  }
  if (!k35) {
    node_create(net, n6, new_pair(new_port(NUM,0x00000049),k36));
    if (k34 != NONE) {
      link(net, tm, new_port(OPR, n6), k34);
    } else {
      k34 = new_port(OPR, n6);
    }
  }
  if (!k33) {
    node_create(net, n5, new_pair(new_port(VAR,v1),k34));
    if (k32 != NONE) {
      link(net, tm, new_port(OPR, n5), k32);
    } else {
      k32 = new_port(OPR, n5);
    }
  }
  if (!k31) {
    node_create(net, n4, new_pair(new_port(NUM,0x00000080),k32));
    if (k23 != NONE) {
      link(net, tm, new_port(OPR, n4), k23);
    } else {
      k23 = new_port(OPR, n4);
    }
  }
  if (!k22) {
    node_create(net, n3, new_pair(k23,k24));
    if (k21 != NONE) {
      link(net, tm, new_port(DUP,n3), k21);
    } else {
      k21 = new_port(DUP,n3);
    }
  }
  if (k20 != NONE) {
    link(net, tm, new_port(VAR,v0), k20);
  } else {
    k20 = new_port(VAR,v0);
  }
  if (!k19) {
    node_create(net, n2, new_pair(k20,k21));
    if (k7 != NONE) {
      link(net, tm, new_port(DUP,n2), k7);
    } else {
      k7 = new_port(DUP,n2);
    }
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  // fast void
  if (get_tag(k3) == ERA || get_tag(k3) == NUM) {
    tm->itrs += 1;
  } else {
    if (k3 != NONE) {
      link(net, tm, new_port(ERA,0x00000000), k3);
    } else {
      k3 = new_port(ERA,0x00000000);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, ne, new_pair(new_port(VAR,v8),new_port(VAR,v6)));
  node_create(net, nd, new_pair(new_port(VAR,v7),new_port(CON,ne)));
  link(net, tm, new_port(REF,0x00000003), new_port(CON,nd));
  node_create(net, n10, new_pair(new_port(VAR,v2),new_port(VAR,v7)));
  node_create(net, nf, new_pair(new_port(VAR,v0),new_port(CON,n10)));
  link(net, tm, new_port(REF,0x1000000a), new_port(CON,nf));
  node_create(net, n12, new_pair(new_port(VAR,v5),new_port(VAR,v8)));
  node_create(net, n11, new_pair(new_port(VAR,v4),new_port(CON,n12)));
  link(net, tm, new_port(REF,0x1000000a), new_port(CON,n11));
  return true;
}

bool interact_call_main__local_0_sum_tree(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  if (0 || !v0) {
    return false;
  }
  vars_create(net, v0, NONE);
  if (b != NONE) {
    link(net, tm, new_port(VAR,v0), b);
  } else {
    b = new_port(VAR,v0);
  }
  link(net, tm, new_port(REF,0x0000000e), new_port(VAR,v0));
  return true;
}

bool interact_call_main__local_0_sum_tree__fold0(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !n0 || !n1) {
    return false;
  }
  vars_create(net, v0, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  if (k4 != NONE) {
    link(net, tm, new_port(VAR,v0), k4);
  } else {
    k4 = new_port(VAR,v0);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k3) == CON && node_load(net, get_val(k3)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k3));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v0), k8);
  } else {
    k8 = new_port(VAR,v0);
  }
  if (k7 != NONE) {
    link(net, tm, new_port(REF,0x00000010), k7);
  } else {
    k7 = new_port(REF,0x00000010);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k3 != NONE) {
      link(net, tm, new_port(CON,n1), k3);
    } else {
      k3 = new_port(CON,n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call_main__local_0_sum_tree__fold0__C0(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v2), k8);
  } else {
    k8 = new_port(VAR,v2);
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n4, new_pair(new_port(VAR,v3),new_port(VAR,v2)));
  node_create(net, n3, new_pair(new_port(NUM,0x00000080),new_port(OPR,n4)));
  node_create(net, n2, new_pair(new_port(VAR,v0),new_port(OPR,n3)));
  link(net, tm, new_port(REF,0x1000000e), new_port(CON,n2));
  node_create(net, n5, new_pair(new_port(VAR,v1),new_port(VAR,v3)));
  link(net, tm, new_port(REF,0x1000000e), new_port(CON,n5));
  return true;
}

bool interact_call_main__local_0_sum_tree__fold0__C1(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !n0 || !n1 || !n2 || !n3 || !n4) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k5 = NONE;
  Port k3 = NONE;
  Port k4 = NONE;
  //fast switch
  if (get_tag(b) == CON) {
    k2 = node_load(net, get_val(b));
    k5 = enter(net,get_fst(k2));
    if (get_tag(k5) == NUM) {
      tm->itrs += 3;
      vars_take(net, v1);
      k1 = 1;
      if (get_u24(get_val(k5)) == 0) {
        node_take(net, get_val(b));
        k3 = get_snd(k2);
        k4 = new_port(ERA,0);
      } else {
        node_store(net, get_val(b), new_pair(new_port(NUM,new_u24(get_u24(get_val(k5))-1)), get_snd(k2)));
        k3 = new_port(ERA,0);
        k4 = b;
      }
    } else {
      node_store(net, get_val(b), new_pair(k5,get_snd(k2)));
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(REF,0x0000000f), k3);
  } else {
    k3 = new_port(REF,0x0000000f);
  }
  bool k6 = 0;
  Pair k7 = 0;
  Port k8 = NONE;
  Port k9 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k6 = 1;
    k7 = node_take(net, get_val(k4));
    k8 = get_fst(k7);
    k9 = get_snd(k7);
  }
  bool k10 = 0;
  Pair k11 = 0;
  Port k12 = NONE;
  Port k13 = NONE;
  // fast anni
  if (get_tag(k9) == CON && node_load(net, get_val(k9)) != 0) {
    tm->itrs += 1;
    k10 = 1;
    k11 = node_take(net, get_val(k9));
    k12 = get_fst(k11);
    k13 = get_snd(k11);
  }
  if (k13 != NONE) {
    link(net, tm, new_port(VAR,v0), k13);
  } else {
    k13 = new_port(VAR,v0);
  }
  if (k12 != NONE) {
    link(net, tm, new_port(VAR,v0), k12);
  } else {
    k12 = new_port(VAR,v0);
  }
  if (!k10) {
    node_create(net, n4, new_pair(k12,k13));
    if (k9 != NONE) {
      link(net, tm, new_port(CON,n4), k9);
    } else {
      k9 = new_port(CON,n4);
    }
  }
  // fast void
  if (get_tag(k8) == ERA || get_tag(k8) == NUM) {
    tm->itrs += 1;
  } else {
    if (k8 != NONE) {
      link(net, tm, new_port(ERA,0x00000000), k8);
    } else {
      k8 = new_port(ERA,0x00000000);
    }
  }
  if (!k6) {
    node_create(net, n3, new_pair(k8,k9));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n3), k4);
    } else {
      k4 = new_port(CON,n3);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(new_port(SWI,n1),new_port(VAR,v1)));
    node_create(net, n1, new_pair(new_port(CON,n2),new_port(VAR,v1)));
    node_create(net, n2, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON, n0), b);
    } else {
      b = new_port(CON, n0);
    }
  }
  return true;
}

bool interact_call_mandel(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  if (k8 != NONE) {
    link(net, tm, new_port(VAR,v2), k8);
  } else {
    k8 = new_port(VAR,v2);
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n6, new_pair(new_port(NUM,0x00000003),new_port(VAR,v2)));
  node_create(net, n5, new_pair(new_port(NUM,0x00000003),new_port(CON,n6)));
  node_create(net, n4, new_pair(new_port(NUM,0x00000003),new_port(CON,n5)));
  node_create(net, n3, new_pair(new_port(VAR,v1),new_port(CON,n4)));
  node_create(net, n2, new_pair(new_port(VAR,v0),new_port(CON,n3)));
  link(net, tm, new_port(REF,0x00000012), new_port(CON,n2));
  return true;
}

bool interact_call_mandel__local_0_rec6(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val vb = vars_alloc_1(net, tm, &vl);
  Val vc = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  Val n16 = node_alloc_1(net, tm, &nl);
  Val n17 = node_alloc_1(net, tm, &nl);
  Val n18 = node_alloc_1(net, tm, &nl);
  Val n19 = node_alloc_1(net, tm, &nl);
  Val n1a = node_alloc_1(net, tm, &nl);
  Val n1b = node_alloc_1(net, tm, &nl);
  Val n1c = node_alloc_1(net, tm, &nl);
  Val n1d = node_alloc_1(net, tm, &nl);
  Val n1e = node_alloc_1(net, tm, &nl);
  Val n1f = node_alloc_1(net, tm, &nl);
  Val n20 = node_alloc_1(net, tm, &nl);
  Val n21 = node_alloc_1(net, tm, &nl);
  Val n22 = node_alloc_1(net, tm, &nl);
  Val n23 = node_alloc_1(net, tm, &nl);
  Val n24 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !vb || !vc || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15 || !n16 || !n17 || !n18 || !n19 || !n1a || !n1b || !n1c || !n1d || !n1e || !n1f || !n20 || !n21 || !n22 || !n23 || !n24) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  vars_create(net, vb, NONE);
  vars_create(net, vc, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  if (k20 != NONE) {
    link(net, tm, new_port(VAR,va), k20);
  } else {
    k20 = new_port(VAR,va);
  }
  bool k21 = 0;
  Port k22 = NONE;
  Port k23 = NONE;
  // fast copy
  if (get_tag(k19) == NUM) {
    tm->itrs += 1;
    k21 = 1;
    k22 = k19;
    k23 = k19;
  }
  bool k24 = 0;
  Port k25 = NONE;
  Port k26 = NONE;
  // fast copy
  if (get_tag(k23) == NUM) {
    tm->itrs += 1;
    k24 = 1;
    k25 = k23;
    k26 = k23;
  }
  if (k26 != NONE) {
    link(net, tm, new_port(VAR,v9), k26);
  } else {
    k26 = new_port(VAR,v9);
  }
  if (k25 != NONE) {
    link(net, tm, new_port(VAR,v8), k25);
  } else {
    k25 = new_port(VAR,v8);
  }
  if (!k24) {
    node_create(net, n11, new_pair(k25,k26));
    if (k23 != NONE) {
      link(net, tm, new_port(DUP,n11), k23);
    } else {
      k23 = new_port(DUP,n11);
    }
  }
  bool k27 = 0;
  Port k28 = NONE;
  // fast oper
  if (get_tag(k22) == NUM && get_tag(new_port(NUM,0x000000e0)) == NUM) {
    tm->itrs += 1;
    k27 = 1;
    k28 = new_port(NUM, operate(get_val(k22), get_val(new_port(NUM,0x000000e0))));
  }
  bool k29 = 0;
  Port k30 = NONE;
  // fast oper
  if (get_tag(k28) == NUM && get_tag(new_port(VAR,v8)) == NUM) {
    tm->itrs += 1;
    k29 = 1;
    k30 = new_port(NUM, operate(get_val(k28), get_val(new_port(VAR,v8))));
  }
  if (k30 != NONE) {
    link(net, tm, new_port(VAR,v5), k30);
  } else {
    k30 = new_port(VAR,v5);
  }
  if (!k29) {
    node_create(net, n10, new_pair(new_port(VAR,v8),k30));
    if (k28 != NONE) {
      link(net, tm, new_port(OPR, n10), k28);
    } else {
      k28 = new_port(OPR, n10);
    }
  }
  if (!k27) {
    node_create(net, nf, new_pair(new_port(NUM,0x000000e0),k28));
    if (k22 != NONE) {
      link(net, tm, new_port(OPR, nf), k22);
    } else {
      k22 = new_port(OPR, nf);
    }
  }
  if (!k21) {
    node_create(net, ne, new_pair(k22,k23));
    if (k19 != NONE) {
      link(net, tm, new_port(DUP,ne), k19);
    } else {
      k19 = new_port(DUP,ne);
    }
  }
  if (!k17) {
    node_create(net, nd, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,nd), k16);
    } else {
      k16 = new_port(CON,nd);
    }
  }
  bool k31 = 0;
  Port k32 = NONE;
  Port k33 = NONE;
  // fast copy
  if (get_tag(k15) == NUM) {
    tm->itrs += 1;
    k31 = 1;
    k32 = k15;
    k33 = k15;
  }
  bool k34 = 0;
  Port k35 = NONE;
  Port k36 = NONE;
  // fast copy
  if (get_tag(k33) == NUM) {
    tm->itrs += 1;
    k34 = 1;
    k35 = k33;
    k36 = k33;
  }
  if (k36 != NONE) {
    link(net, tm, new_port(VAR,v7), k36);
  } else {
    k36 = new_port(VAR,v7);
  }
  if (k35 != NONE) {
    link(net, tm, new_port(VAR,v4), k35);
  } else {
    k35 = new_port(VAR,v4);
  }
  if (!k34) {
    node_create(net, nc, new_pair(k35,k36));
    if (k33 != NONE) {
      link(net, tm, new_port(DUP,nc), k33);
    } else {
      k33 = new_port(DUP,nc);
    }
  }
  bool k37 = 0;
  Port k38 = NONE;
  // fast oper
  if (get_tag(k32) == NUM && get_tag(new_port(NUM,0x000000e0)) == NUM) {
    tm->itrs += 1;
    k37 = 1;
    k38 = new_port(NUM, operate(get_val(k32), get_val(new_port(NUM,0x000000e0))));
  }
  bool k39 = 0;
  Port k40 = NONE;
  // fast oper
  if (get_tag(k38) == NUM && get_tag(new_port(VAR,v4)) == NUM) {
    tm->itrs += 1;
    k39 = 1;
    k40 = new_port(NUM, operate(get_val(k38), get_val(new_port(VAR,v4))));
  }
  bool k41 = 0;
  Port k42 = NONE;
  // fast oper
  if (get_tag(k40) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k41 = 1;
    k42 = new_port(NUM, operate(get_val(k40), get_val(new_port(NUM,0x00000080))));
  }
  bool k43 = 0;
  Port k44 = NONE;
  // fast oper
  if (get_tag(k42) == NUM && get_tag(new_port(VAR,v5)) == NUM) {
    tm->itrs += 1;
    k43 = 1;
    k44 = new_port(NUM, operate(get_val(k42), get_val(new_port(VAR,v5))));
  }
  bool k45 = 0;
  Port k46 = NONE;
  // fast oper
  if (get_tag(k44) == NUM && get_tag(new_port(NUM,0x0810000f)) == NUM) {
    tm->itrs += 1;
    k45 = 1;
    k46 = new_port(NUM, operate(get_val(k44), get_val(new_port(NUM,0x0810000f))));
  }
  if (k46 != NONE) {
    link(net, tm, new_port(VAR,v6), k46);
  } else {
    k46 = new_port(VAR,v6);
  }
  if (!k45) {
    node_create(net, nb, new_pair(new_port(NUM,0x0810000f),k46));
    if (k44 != NONE) {
      link(net, tm, new_port(OPR, nb), k44);
    } else {
      k44 = new_port(OPR, nb);
    }
  }
  if (!k43) {
    node_create(net, na, new_pair(new_port(VAR,v5),k44));
    if (k42 != NONE) {
      link(net, tm, new_port(OPR, na), k42);
    } else {
      k42 = new_port(OPR, na);
    }
  }
  if (!k41) {
    node_create(net, n9, new_pair(new_port(NUM,0x00000080),k42));
    if (k40 != NONE) {
      link(net, tm, new_port(OPR, n9), k40);
    } else {
      k40 = new_port(OPR, n9);
    }
  }
  if (!k39) {
    node_create(net, n8, new_pair(new_port(VAR,v4),k40));
    if (k38 != NONE) {
      link(net, tm, new_port(OPR, n8), k38);
    } else {
      k38 = new_port(OPR, n8);
    }
  }
  if (!k37) {
    node_create(net, n7, new_pair(new_port(NUM,0x000000e0),k38));
    if (k32 != NONE) {
      link(net, tm, new_port(OPR, n7), k32);
    } else {
      k32 = new_port(OPR, n7);
    }
  }
  if (!k31) {
    node_create(net, n6, new_pair(k32,k33));
    if (k15 != NONE) {
      link(net, tm, new_port(DUP,n6), k15);
    } else {
      k15 = new_port(DUP,n6);
    }
  }
  if (!k13) {
    node_create(net, n5, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n5), k12);
    } else {
      k12 = new_port(CON,n5);
    }
  }
  bool k47 = 0;
  Port k48 = NONE;
  Port k49 = NONE;
  // fast copy
  if (get_tag(k11) == NUM) {
    tm->itrs += 1;
    k47 = 1;
    k48 = k11;
    k49 = k11;
  }
  if (k49 != NONE) {
    link(net, tm, new_port(VAR,v3), k49);
  } else {
    k49 = new_port(VAR,v3);
  }
  bool k50 = 0;
  Port k51 = NONE;
  // fast oper
  if (get_tag(k48) == NUM && get_tag(new_port(NUM,0x0870000f)) == NUM) {
    tm->itrs += 1;
    k50 = 1;
    k51 = new_port(NUM, operate(get_val(k48), get_val(new_port(NUM,0x0870000f))));
  }
  if (k51 != NONE) {
    link(net, tm, new_port(VAR,v2), k51);
  } else {
    k51 = new_port(VAR,v2);
  }
  if (!k50) {
    node_create(net, n4, new_pair(new_port(NUM,0x0870000f),k51));
    if (k48 != NONE) {
      link(net, tm, new_port(OPR, n4), k48);
    } else {
      k48 = new_port(OPR, n4);
    }
  }
  if (!k47) {
    node_create(net, n3, new_pair(k48,k49));
    if (k11 != NONE) {
      link(net, tm, new_port(DUP,n3), k11);
    } else {
      k11 = new_port(DUP,n3);
    }
  }
  if (!k9) {
    node_create(net, n2, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n2), k8);
    } else {
      k8 = new_port(CON,n2);
    }
  }
  if (k7 != NONE) {
    link(net, tm, new_port(VAR,v1), k7);
  } else {
    k7 = new_port(VAR,v1);
  }
  if (!k5) {
    node_create(net, n1, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n1), k4);
    } else {
      k4 = new_port(CON,n1);
    }
  }
  if (k3 != NONE) {
    link(net, tm, new_port(VAR,v0), k3);
  } else {
    k3 = new_port(VAR,v0);
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n1e, new_pair(new_port(ERA,0x00000000),new_port(VAR,vc)));
  node_create(net, n1d, new_pair(new_port(ERA,0x00000000),new_port(CON,n1e)));
  node_create(net, n1c, new_pair(new_port(VAR,vc),new_port(CON,n1d)));
  node_create(net, n1b, new_pair(new_port(ERA,0x00000000),new_port(CON,n1c)));
  node_create(net, n1a, new_pair(new_port(ERA,0x00000000),new_port(CON,n1b)));
  node_create(net, n19, new_pair(new_port(ERA,0x00000000),new_port(CON,n1a)));
  node_create(net, n18, new_pair(new_port(REF,0x00000013),new_port(CON,n19)));
  node_create(net, n23, new_pair(new_port(VAR,v9),new_port(VAR,va)));
  node_create(net, n22, new_pair(new_port(VAR,v7),new_port(CON,n23)));
  node_create(net, n21, new_pair(new_port(VAR,v3),new_port(CON,n22)));
  node_create(net, n20, new_pair(new_port(VAR,v1),new_port(CON,n21)));
  node_create(net, n1f, new_pair(new_port(VAR,v0),new_port(CON,n20)));
  node_create(net, n17, new_pair(new_port(CON,n18),new_port(CON,n1f)));
  node_create(net, n16, new_pair(new_port(NUM,0x0000000d),new_port(SWI,n17)));
  node_create(net, n15, new_pair(new_port(NUM,0x0000000d),new_port(OPR,n16)));
  node_create(net, n14, new_pair(new_port(VAR,vb),new_port(OPR,n15)));
  node_create(net, n13, new_pair(new_port(NUM,0x00000080),new_port(OPR,n14)));
  node_create(net, n12, new_pair(new_port(VAR,v2),new_port(OPR,n13)));
  link(net, tm, new_port(OPR,n12), new_port(NUM,0x0000000c));
  node_create(net, n24, new_pair(new_port(VAR,v6),new_port(VAR,vb)));
  link(net, tm, new_port(OPR,n24), new_port(NUM,0x0000000c));
  return true;
}

bool interact_call_mandel__local_0_rec6__C0(Net *net, TM *tm, Port a, Port b) {
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val v1 = vars_alloc_1(net, tm, &vl);
  Val v2 = vars_alloc_1(net, tm, &vl);
  Val v3 = vars_alloc_1(net, tm, &vl);
  Val v4 = vars_alloc_1(net, tm, &vl);
  Val v5 = vars_alloc_1(net, tm, &vl);
  Val v6 = vars_alloc_1(net, tm, &vl);
  Val v7 = vars_alloc_1(net, tm, &vl);
  Val v8 = vars_alloc_1(net, tm, &vl);
  Val v9 = vars_alloc_1(net, tm, &vl);
  Val va = vars_alloc_1(net, tm, &vl);
  Val vb = vars_alloc_1(net, tm, &vl);
  Val vc = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  Val n2 = node_alloc_1(net, tm, &nl);
  Val n3 = node_alloc_1(net, tm, &nl);
  Val n4 = node_alloc_1(net, tm, &nl);
  Val n5 = node_alloc_1(net, tm, &nl);
  Val n6 = node_alloc_1(net, tm, &nl);
  Val n7 = node_alloc_1(net, tm, &nl);
  Val n8 = node_alloc_1(net, tm, &nl);
  Val n9 = node_alloc_1(net, tm, &nl);
  Val na = node_alloc_1(net, tm, &nl);
  Val nb = node_alloc_1(net, tm, &nl);
  Val nc = node_alloc_1(net, tm, &nl);
  Val nd = node_alloc_1(net, tm, &nl);
  Val ne = node_alloc_1(net, tm, &nl);
  Val nf = node_alloc_1(net, tm, &nl);
  Val n10 = node_alloc_1(net, tm, &nl);
  Val n11 = node_alloc_1(net, tm, &nl);
  Val n12 = node_alloc_1(net, tm, &nl);
  Val n13 = node_alloc_1(net, tm, &nl);
  Val n14 = node_alloc_1(net, tm, &nl);
  Val n15 = node_alloc_1(net, tm, &nl);
  Val n16 = node_alloc_1(net, tm, &nl);
  Val n17 = node_alloc_1(net, tm, &nl);
  Val n18 = node_alloc_1(net, tm, &nl);
  Val n19 = node_alloc_1(net, tm, &nl);
  Val n1a = node_alloc_1(net, tm, &nl);
  Val n1b = node_alloc_1(net, tm, &nl);
  Val n1c = node_alloc_1(net, tm, &nl);
  Val n1d = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !v1 || !v2 || !v3 || !v4 || !v5 || !v6 || !v7 || !v8 || !v9 || !va || !vb || !vc || !n0 || !n1 || !n2 || !n3 || !n4 || !n5 || !n6 || !n7 || !n8 || !n9 || !na || !nb || !nc || !nd || !ne || !nf || !n10 || !n11 || !n12 || !n13 || !n14 || !n15 || !n16 || !n17 || !n18 || !n19 || !n1a || !n1b || !n1c || !n1d) {
    return false;
  }
  vars_create(net, v0, NONE);
  vars_create(net, v1, NONE);
  vars_create(net, v2, NONE);
  vars_create(net, v3, NONE);
  vars_create(net, v4, NONE);
  vars_create(net, v5, NONE);
  vars_create(net, v6, NONE);
  vars_create(net, v7, NONE);
  vars_create(net, v8, NONE);
  vars_create(net, v9, NONE);
  vars_create(net, va, NONE);
  vars_create(net, vb, NONE);
  vars_create(net, vc, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  bool k5 = 0;
  Pair k6 = 0;
  Port k7 = NONE;
  Port k8 = NONE;
  // fast anni
  if (get_tag(k4) == CON && node_load(net, get_val(k4)) != 0) {
    tm->itrs += 1;
    k5 = 1;
    k6 = node_take(net, get_val(k4));
    k7 = get_fst(k6);
    k8 = get_snd(k6);
  }
  bool k9 = 0;
  Pair k10 = 0;
  Port k11 = NONE;
  Port k12 = NONE;
  // fast anni
  if (get_tag(k8) == CON && node_load(net, get_val(k8)) != 0) {
    tm->itrs += 1;
    k9 = 1;
    k10 = node_take(net, get_val(k8));
    k11 = get_fst(k10);
    k12 = get_snd(k10);
  }
  bool k13 = 0;
  Pair k14 = 0;
  Port k15 = NONE;
  Port k16 = NONE;
  // fast anni
  if (get_tag(k12) == CON && node_load(net, get_val(k12)) != 0) {
    tm->itrs += 1;
    k13 = 1;
    k14 = node_take(net, get_val(k12));
    k15 = get_fst(k14);
    k16 = get_snd(k14);
  }
  bool k17 = 0;
  Pair k18 = 0;
  Port k19 = NONE;
  Port k20 = NONE;
  // fast anni
  if (get_tag(k16) == CON && node_load(net, get_val(k16)) != 0) {
    tm->itrs += 1;
    k17 = 1;
    k18 = node_take(net, get_val(k16));
    k19 = get_fst(k18);
    k20 = get_snd(k18);
  }
  if (k20 != NONE) {
    link(net, tm, new_port(VAR,vb), k20);
  } else {
    k20 = new_port(VAR,vb);
  }
  bool k21 = 0;
  Port k22 = NONE;
  Port k23 = NONE;
  // fast copy
  if (get_tag(k19) == NUM) {
    tm->itrs += 1;
    k21 = 1;
    k22 = k19;
    k23 = k19;
  }
  bool k24 = 0;
  Port k25 = NONE;
  Port k26 = NONE;
  // fast copy
  if (get_tag(k23) == NUM) {
    tm->itrs += 1;
    k24 = 1;
    k25 = k23;
    k26 = k23;
  }
  if (k26 != NONE) {
    link(net, tm, new_port(VAR,va), k26);
  } else {
    k26 = new_port(VAR,va);
  }
  if (k25 != NONE) {
    link(net, tm, new_port(VAR,v9), k25);
  } else {
    k25 = new_port(VAR,v9);
  }
  if (!k24) {
    node_create(net, n13, new_pair(k25,k26));
    if (k23 != NONE) {
      link(net, tm, new_port(DUP,n13), k23);
    } else {
      k23 = new_port(DUP,n13);
    }
  }
  bool k27 = 0;
  Port k28 = NONE;
  // fast oper
  if (get_tag(k22) == NUM && get_tag(new_port(NUM,0x000000e0)) == NUM) {
    tm->itrs += 1;
    k27 = 1;
    k28 = new_port(NUM, operate(get_val(k22), get_val(new_port(NUM,0x000000e0))));
  }
  bool k29 = 0;
  Port k30 = NONE;
  // fast oper
  if (get_tag(k28) == NUM && get_tag(new_port(VAR,v9)) == NUM) {
    tm->itrs += 1;
    k29 = 1;
    k30 = new_port(NUM, operate(get_val(k28), get_val(new_port(VAR,v9))));
  }
  if (k30 != NONE) {
    link(net, tm, new_port(VAR,v6), k30);
  } else {
    k30 = new_port(VAR,v6);
  }
  if (!k29) {
    node_create(net, n12, new_pair(new_port(VAR,v9),k30));
    if (k28 != NONE) {
      link(net, tm, new_port(OPR, n12), k28);
    } else {
      k28 = new_port(OPR, n12);
    }
  }
  if (!k27) {
    node_create(net, n11, new_pair(new_port(NUM,0x000000e0),k28));
    if (k22 != NONE) {
      link(net, tm, new_port(OPR, n11), k22);
    } else {
      k22 = new_port(OPR, n11);
    }
  }
  if (!k21) {
    node_create(net, n10, new_pair(k22,k23));
    if (k19 != NONE) {
      link(net, tm, new_port(DUP,n10), k19);
    } else {
      k19 = new_port(DUP,n10);
    }
  }
  if (!k17) {
    node_create(net, nf, new_pair(k19,k20));
    if (k16 != NONE) {
      link(net, tm, new_port(CON,nf), k16);
    } else {
      k16 = new_port(CON,nf);
    }
  }
  bool k31 = 0;
  Port k32 = NONE;
  Port k33 = NONE;
  // fast copy
  if (get_tag(k15) == NUM) {
    tm->itrs += 1;
    k31 = 1;
    k32 = k15;
    k33 = k15;
  }
  bool k34 = 0;
  Port k35 = NONE;
  Port k36 = NONE;
  // fast copy
  if (get_tag(k33) == NUM) {
    tm->itrs += 1;
    k34 = 1;
    k35 = k33;
    k36 = k33;
  }
  if (k36 != NONE) {
    link(net, tm, new_port(VAR,v8), k36);
  } else {
    k36 = new_port(VAR,v8);
  }
  if (k35 != NONE) {
    link(net, tm, new_port(VAR,v5), k35);
  } else {
    k35 = new_port(VAR,v5);
  }
  if (!k34) {
    node_create(net, ne, new_pair(k35,k36));
    if (k33 != NONE) {
      link(net, tm, new_port(DUP,ne), k33);
    } else {
      k33 = new_port(DUP,ne);
    }
  }
  bool k37 = 0;
  Port k38 = NONE;
  // fast oper
  if (get_tag(k32) == NUM && get_tag(new_port(NUM,0x000000e0)) == NUM) {
    tm->itrs += 1;
    k37 = 1;
    k38 = new_port(NUM, operate(get_val(k32), get_val(new_port(NUM,0x000000e0))));
  }
  bool k39 = 0;
  Port k40 = NONE;
  // fast oper
  if (get_tag(k38) == NUM && get_tag(new_port(VAR,v5)) == NUM) {
    tm->itrs += 1;
    k39 = 1;
    k40 = new_port(NUM, operate(get_val(k38), get_val(new_port(VAR,v5))));
  }
  bool k41 = 0;
  Port k42 = NONE;
  // fast oper
  if (get_tag(k40) == NUM && get_tag(new_port(NUM,0x000000a0)) == NUM) {
    tm->itrs += 1;
    k41 = 1;
    k42 = new_port(NUM, operate(get_val(k40), get_val(new_port(NUM,0x000000a0))));
  }
  bool k43 = 0;
  Port k44 = NONE;
  // fast oper
  if (get_tag(k42) == NUM && get_tag(new_port(VAR,v6)) == NUM) {
    tm->itrs += 1;
    k43 = 1;
    k44 = new_port(NUM, operate(get_val(k42), get_val(new_port(VAR,v6))));
  }
  bool k45 = 0;
  Port k46 = NONE;
  // fast oper
  if (get_tag(k44) == NUM && get_tag(new_port(NUM,0x00000080)) == NUM) {
    tm->itrs += 1;
    k45 = 1;
    k46 = new_port(NUM, operate(get_val(k44), get_val(new_port(NUM,0x00000080))));
  }
  bool k47 = 0;
  Port k48 = NONE;
  // fast oper
  if (get_tag(k46) == NUM && get_tag(new_port(VAR,v1)) == NUM) {
    tm->itrs += 1;
    k47 = 1;
    k48 = new_port(NUM, operate(get_val(k46), get_val(new_port(VAR,v1))));
  }
  if (k48 != NONE) {
    link(net, tm, new_port(VAR,v7), k48);
  } else {
    k48 = new_port(VAR,v7);
  }
  if (!k47) {
    node_create(net, nd, new_pair(new_port(VAR,v1),k48));
    if (k46 != NONE) {
      link(net, tm, new_port(OPR, nd), k46);
    } else {
      k46 = new_port(OPR, nd);
    }
  }
  if (!k45) {
    node_create(net, nc, new_pair(new_port(NUM,0x00000080),k46));
    if (k44 != NONE) {
      link(net, tm, new_port(OPR, nc), k44);
    } else {
      k44 = new_port(OPR, nc);
    }
  }
  if (!k43) {
    node_create(net, nb, new_pair(new_port(VAR,v6),k44));
    if (k42 != NONE) {
      link(net, tm, new_port(OPR, nb), k42);
    } else {
      k42 = new_port(OPR, nb);
    }
  }
  if (!k41) {
    node_create(net, na, new_pair(new_port(NUM,0x000000a0),k42));
    if (k40 != NONE) {
      link(net, tm, new_port(OPR, na), k40);
    } else {
      k40 = new_port(OPR, na);
    }
  }
  if (!k39) {
    node_create(net, n9, new_pair(new_port(VAR,v5),k40));
    if (k38 != NONE) {
      link(net, tm, new_port(OPR, n9), k38);
    } else {
      k38 = new_port(OPR, n9);
    }
  }
  if (!k37) {
    node_create(net, n8, new_pair(new_port(NUM,0x000000e0),k38));
    if (k32 != NONE) {
      link(net, tm, new_port(OPR, n8), k32);
    } else {
      k32 = new_port(OPR, n8);
    }
  }
  if (!k31) {
    node_create(net, n7, new_pair(k32,k33));
    if (k15 != NONE) {
      link(net, tm, new_port(DUP,n7), k15);
    } else {
      k15 = new_port(DUP,n7);
    }
  }
  if (!k13) {
    node_create(net, n6, new_pair(k15,k16));
    if (k12 != NONE) {
      link(net, tm, new_port(CON,n6), k12);
    } else {
      k12 = new_port(CON,n6);
    }
  }
  bool k49 = 0;
  Port k50 = NONE;
  // fast oper
  if (get_tag(k11) == NUM && get_tag(new_port(NUM,0x07f00004)) == NUM) {
    tm->itrs += 1;
    k49 = 1;
    k50 = new_port(NUM, operate(get_val(k11), get_val(new_port(NUM,0x07f00004))));
  }
  if (k50 != NONE) {
    link(net, tm, new_port(VAR,v4), k50);
  } else {
    k50 = new_port(VAR,v4);
  }
  if (!k49) {
    node_create(net, n5, new_pair(new_port(NUM,0x07f00004),k50));
    if (k11 != NONE) {
      link(net, tm, new_port(OPR, n5), k11);
    } else {
      k11 = new_port(OPR, n5);
    }
  }
  if (!k9) {
    node_create(net, n4, new_pair(k11,k12));
    if (k8 != NONE) {
      link(net, tm, new_port(CON,n4), k8);
    } else {
      k8 = new_port(CON,n4);
    }
  }
  bool k51 = 0;
  Port k52 = NONE;
  Port k53 = NONE;
  // fast copy
  if (get_tag(k7) == NUM) {
    tm->itrs += 1;
    k51 = 1;
    k52 = k7;
    k53 = k7;
  }
  if (k53 != NONE) {
    link(net, tm, new_port(VAR,v3), k53);
  } else {
    k53 = new_port(VAR,v3);
  }
  if (k52 != NONE) {
    link(net, tm, new_port(VAR,v2), k52);
  } else {
    k52 = new_port(VAR,v2);
  }
  if (!k51) {
    node_create(net, n3, new_pair(k52,k53));
    if (k7 != NONE) {
      link(net, tm, new_port(DUP,n3), k7);
    } else {
      k7 = new_port(DUP,n3);
    }
  }
  if (!k5) {
    node_create(net, n2, new_pair(k7,k8));
    if (k4 != NONE) {
      link(net, tm, new_port(CON,n2), k4);
    } else {
      k4 = new_port(CON,n2);
    }
  }
  bool k54 = 0;
  Port k55 = NONE;
  Port k56 = NONE;
  // fast copy
  if (get_tag(k3) == NUM) {
    tm->itrs += 1;
    k54 = 1;
    k55 = k3;
    k56 = k3;
  }
  if (k56 != NONE) {
    link(net, tm, new_port(VAR,v1), k56);
  } else {
    k56 = new_port(VAR,v1);
  }
  if (k55 != NONE) {
    link(net, tm, new_port(VAR,v0), k55);
  } else {
    k55 = new_port(VAR,v0);
  }
  if (!k54) {
    node_create(net, n1, new_pair(k55,k56));
    if (k3 != NONE) {
      link(net, tm, new_port(DUP,n1), k3);
    } else {
      k3 = new_port(DUP,n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  node_create(net, n18, new_pair(new_port(VAR,vc),new_port(VAR,vb)));
  node_create(net, n17, new_pair(new_port(VAR,v7),new_port(CON,n18)));
  node_create(net, n16, new_pair(new_port(VAR,v4),new_port(CON,n17)));
  node_create(net, n15, new_pair(new_port(VAR,v2),new_port(CON,n16)));
  node_create(net, n14, new_pair(new_port(VAR,v0),new_port(CON,n15)));
  link(net, tm, new_port(REF,0x00000012), new_port(CON,n14));
  node_create(net, n1d, new_pair(new_port(VAR,v3),new_port(VAR,vc)));
  node_create(net, n1c, new_pair(new_port(NUM,0x00000080),new_port(OPR,n1d)));
  node_create(net, n1b, new_pair(new_port(VAR,va),new_port(OPR,n1c)));
  node_create(net, n1a, new_pair(new_port(NUM,0x000000e0),new_port(OPR,n1b)));
  node_create(net, n19, new_pair(new_port(VAR,v8),new_port(OPR,n1a)));
  link(net, tm, new_port(OPR,n19), new_port(NUM,0x08000007));
  return true;
}

bool interact_call_u24_to_f24(Net *net, TM *tm, Port a, Port b) {
  if (get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }
  u32 vl = 0;
  u32 nl = 0;
  Val v0 = vars_alloc_1(net, tm, &vl);
  Val n0 = node_alloc_1(net, tm, &nl);
  Val n1 = node_alloc_1(net, tm, &nl);
  if (0 || !v0 || !n0 || !n1) {
    return false;
  }
  vars_create(net, v0, NONE);
  bool k1 = 0;
  Pair k2 = 0;
  Port k3 = NONE;
  Port k4 = NONE;
  // fast anni
  if (get_tag(b) == CON && node_load(net, get_val(b)) != 0) {
    tm->itrs += 1;
    k1 = 1;
    k2 = node_take(net, get_val(b));
    k3 = get_fst(k2);
    k4 = get_snd(k2);
  }
  if (k4 != NONE) {
    link(net, tm, new_port(VAR,v0), k4);
  } else {
    k4 = new_port(VAR,v0);
  }
  bool k5 = 0;
  Port k6 = NONE;
  // fast oper
  if (get_tag(k3) == NUM && get_tag(new_port(NUM,0x00000060)) == NUM) {
    tm->itrs += 1;
    k5 = 1;
    k6 = new_port(NUM, operate(get_val(k3), get_val(new_port(NUM,0x00000060))));
  }
  if (k6 != NONE) {
    link(net, tm, new_port(VAR,v0), k6);
  } else {
    k6 = new_port(VAR,v0);
  }
  if (!k5) {
    node_create(net, n1, new_pair(new_port(NUM,0x00000060),k6));
    if (k3 != NONE) {
      link(net, tm, new_port(OPR, n1), k3);
    } else {
      k3 = new_port(OPR, n1);
    }
  }
  if (!k1) {
    node_create(net, n0, new_pair(k3,k4));
    if (b != NONE) {
      link(net, tm, new_port(CON,n0), b);
    } else {
      b = new_port(CON,n0);
    }
  }
  return true;
}

bool interact_call(Net *net, TM *tm, Port a, Port b) {
  u32 fid = get_val(a) & 0xFFFFFFF;
  switch (fid) {
    case 0: return interact_call_main(net, tm, a, b);
    case 1: return interact_call_ParTree_Leaf(net, tm, a, b);
    case 2: return interact_call_ParTree_Leaf_tag(net, tm, a, b);
    case 3: return interact_call_ParTree_Node(net, tm, a, b);
    case 4: return interact_call_ParTree_Node_tag(net, tm, a, b);
    case 5: return interact_call_f24_to_u24(net, tm, a, b);
    case 6: return interact_call_jshark_grid(net, tm, a, b);
    case 7: return interact_call_jshark_grid__bend0(net, tm, a, b);
    case 8: return interact_call_jshark_grid__bend0__C0(net, tm, a, b);
    case 9: return interact_call_jshark_grid__bend0__C1(net, tm, a, b);
    case 10: return interact_call_main__bend0(net, tm, a, b);
    case 11: return interact_call_main__bend0__C0(net, tm, a, b);
    case 12: return interact_call_main__bend0__C1(net, tm, a, b);
    case 13: return interact_call_main__local_0_sum_tree(net, tm, a, b);
    case 14: return interact_call_main__local_0_sum_tree__fold0(net, tm, a, b);
    case 15: return interact_call_main__local_0_sum_tree__fold0__C0(net, tm, a, b);
    case 16: return interact_call_main__local_0_sum_tree__fold0__C1(net, tm, a, b);
    case 17: return interact_call_mandel(net, tm, a, b);
    case 18: return interact_call_mandel__local_0_rec6(net, tm, a, b);
    case 19: return interact_call_mandel__local_0_rec6__C0(net, tm, a, b);
    case 20: return interact_call_u24_to_f24(net, tm, a, b);
    default: return false;
  }
}
#else
static inline bool interact_call(Net* net, TM* tm, Port a, Port b, Book* book) {
  // Loads Definition.
  u32  fid = get_val(a) & 0xFFFFFFF;
  Def* def = &book->defs_buf[fid];

  // Copy Optimization.
  if (def->safe && get_tag(b) == DUP) {
    return interact_eras(net, tm, a, b);
  }

  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, def->rbag_len + 1, def->node_len, def->vars_len)) {
    debug("interact_call: get_resources failed\n");
    return false;
  }

  // Stores new vars.
  for (u32 i = 0; i < def->vars_len; ++i) {
    vars_create(net, tm->vloc[i], NONE);
  }

  // Stores new nodes.
  for (u32 i = 0; i < def->node_len; ++i) {
    node_create(net, tm->nloc[i], adjust_pair(net, tm, def->node_buf[i]));
  }

  // Links.
  for (u32 i = 0; i < def->rbag_len; ++i) {
    link_pair(net, tm, adjust_pair(net, tm, def->rbag_buf[i]));
  }
  link_pair(net, tm, new_pair(adjust_port(net, tm, def->root), b));

  return true;
}
#endif

// The Void Interaction.
static inline bool interact_void(Net* net, TM* tm, Port a, Port b) {
  return true;
}

// The Eras Interaction.
static inline bool interact_eras(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 2, 0, 0)) {
    debug("interact_eras: get_resources failed\n");
    return false;
  }

  // Checks availability
  if (node_load(net, get_val(b)) == 0) {
    return false;
  }

  // Loads ports.
  Pair B  = node_exchange(net, get_val(b), 0);
  Port B1 = get_fst(B);
  Port B2 = get_snd(B);

  // Links.
  link_pair(net, tm, new_pair(a, B1));
  link_pair(net, tm, new_pair(a, B2));

  return true;
}

// The Anni Interaction.
static inline bool interact_anni(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 2, 0, 0)) {
    debug("interact_anni: get_resources failed\n");
    return false;
  }

  // Checks availability
  if (node_load(net, get_val(a)) == 0 || node_load(net, get_val(b)) == 0) {
    return false;
  }

  // Loads ports.
  Pair A  = node_take(net, get_val(a));
  Port A1 = get_fst(A);
  Port A2 = get_snd(A);
  Pair B  = node_take(net, get_val(b));
  Port B1 = get_fst(B);
  Port B2 = get_snd(B);

  // Links.
  link_pair(net, tm, new_pair(A1, B1));
  link_pair(net, tm, new_pair(A2, B2));

  return true;
}

// The Comm Interaction.
static inline bool interact_comm(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 4, 4, 4)) {
    debug("interact_comm: get_resources failed\n");
    return false;
  }

  // Checks availability
  if (node_load(net, get_val(a)) == 0 || node_load(net, get_val(b)) == 0) {
    return false;
  }

  // Loads ports.
  Pair A  = node_take(net, get_val(a));
  Port A1 = get_fst(A);
  Port A2 = get_snd(A);
  Pair B  = node_take(net, get_val(b));
  Port B1 = get_fst(B);
  Port B2 = get_snd(B);

  // Stores new vars.
  vars_create(net, tm->vloc[0], NONE);
  vars_create(net, tm->vloc[1], NONE);
  vars_create(net, tm->vloc[2], NONE);
  vars_create(net, tm->vloc[3], NONE);

  // Stores new nodes.
  node_create(net, tm->nloc[0], new_pair(new_port(VAR, tm->vloc[0]), new_port(VAR, tm->vloc[1])));
  node_create(net, tm->nloc[1], new_pair(new_port(VAR, tm->vloc[2]), new_port(VAR, tm->vloc[3])));
  node_create(net, tm->nloc[2], new_pair(new_port(VAR, tm->vloc[0]), new_port(VAR, tm->vloc[2])));
  node_create(net, tm->nloc[3], new_pair(new_port(VAR, tm->vloc[1]), new_port(VAR, tm->vloc[3])));

  // Links.
  link_pair(net, tm, new_pair(new_port(get_tag(b), tm->nloc[0]), A1));
  link_pair(net, tm, new_pair(new_port(get_tag(b), tm->nloc[1]), A2));
  link_pair(net, tm, new_pair(new_port(get_tag(a), tm->nloc[2]), B1));
  link_pair(net, tm, new_pair(new_port(get_tag(a), tm->nloc[3]), B2));

  return true;
}

// The Oper Interaction.
static inline bool interact_oper(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 1, 1, 0)) {
    debug("interact_oper: get_resources failed\n");
    return false;
  }

  // Checks availability
  if (node_load(net, get_val(b)) == 0) {
    return false;
  }

  // Loads ports.
  Val  av = get_val(a);
  Pair B  = node_take(net, get_val(b));
  Port B1 = get_fst(B);
  Port B2 = enter(net, get_snd(B));

  // Performs operation.
  if (get_tag(B1) == NUM) {
    Val  bv = get_val(B1);
    Numb cv = operate(av, bv);
    link_pair(net, tm, new_pair(new_port(NUM, cv), B2));
  } else {
    node_create(net, tm->nloc[0], new_pair(a, B2));
    link_pair(net, tm, new_pair(B1, new_port(OPR, tm->nloc[0])));
  }

  return true;
}

// The Swit Interaction.
static inline bool interact_swit(Net* net, TM* tm, Port a, Port b) {
  // Allocates needed nodes and vars.
  if (!get_resources(net, tm, 1, 2, 0)) {
    debug("interact_swit: get_resources failed\n");
    return false;
  }

  // Checks availability
  if (node_load(net, get_val(b)) == 0) {
    return false;
  }

  // Loads ports.
  u32  av = get_u24(get_val(a));
  Pair B  = node_take(net, get_val(b));
  Port B1 = get_fst(B);
  Port B2 = get_snd(B);

  // Stores new nodes.
  if (av == 0) {
    node_create(net, tm->nloc[0], new_pair(B2, new_port(ERA,0)));
    link_pair(net, tm, new_pair(new_port(CON, tm->nloc[0]), B1));
  } else {
    node_create(net, tm->nloc[0], new_pair(new_port(ERA,0), new_port(CON, tm->nloc[1])));
    node_create(net, tm->nloc[1], new_pair(new_port(NUM, new_u24(av-1)), B2));
    link_pair(net, tm, new_pair(new_port(CON, tm->nloc[0]), B1));
  }

  return true;
}

// Pops a local redex and performs a single interaction.
static inline bool interact(Net* net, TM* tm, Book* book) {
  // Pops a redex.
  Pair redex = pop_redex(net, tm);

  // If there is no redex, stop.
  if (redex != 0) {
    // Gets redex ports A and B.
    Port a = get_fst(redex);
    Port b = get_snd(redex);

    // Gets the rule type.
    Rule rule = get_rule(a, b);

    // Used for root redex.
    if (get_tag(a) == REF && b == ROOT) {
      rule = CALL;
    // Swaps ports if necessary.
    } else if (should_swap(a,b)) {
      swap(&a, &b);
    }

    // Dispatches interaction rule.
    bool success;
    switch (rule) {
      case LINK: success = interact_link(net, tm, a, b); break;
      #ifdef COMPILED
      case CALL: success = interact_call(net, tm, a, b); break;
      #else
      case CALL: success = interact_call(net, tm, a, b, book); break;
      #endif
      case VOID: success = interact_void(net, tm, a, b); break;
      case ERAS: success = interact_eras(net, tm, a, b); break;
      case ANNI: success = interact_anni(net, tm, a, b); break;
      case COMM: success = interact_comm(net, tm, a, b); break;
      case OPER: success = interact_oper(net, tm, a, b); break;
      case SWIT: success = interact_swit(net, tm, a, b); break;
    }

    // If error, pushes redex back.
    if (!success) {
      push_redex(net, tm, redex);
      return false;
    // Else, increments the interaction count.
    } else if (rule != LINK) {
      tm->itrs += 1;
    }
  }

  return true;
}

// Evaluator
// ---------

void evaluator(Net* net, TM* tm, Book* book) {
  // Initializes the global idle counter
  atomic_store_explicit(&net->idle, TPC - 1, memory_order_relaxed);
  sync_threads();

  // Performs some interactions
  u32  tick = 0;
  bool busy = tm->tid == 0;
  while (tick < 50000000) {
    tick += 1;

    // If we have redexes...
    if (rbag_len(net, tm) > 0) {
      // Update global idle counter
      if (!busy) atomic_fetch_sub_explicit(&net->idle, 1, memory_order_relaxed);
      busy = true;

      // Perform an interaction
      #ifdef DEBUG
      if (!interact(net, tm, book)) debug("interaction failed\n");
      #else
      interact(net, tm, book);
      #endif
    // If we have no redexes...
    } else {
      // Update global idle counter
      if (busy) atomic_fetch_add_explicit(&net->idle, 1, memory_order_relaxed);
      busy = false;

      //// Peeks a redex from target
      u32 sid = (tm->tid - 1) % TPC;
      u32 idx = sid*(G_RBAG_LEN/TPC) + (tm->sidx++);

      // Stealing Everything: this will steal all redexes

      Pair got = atomic_exchange_explicit(&net->rbag_buf[idx], 0, memory_order_relaxed);
      if (got != 0) {
        push_redex(net, tm, got);
        continue;
      } else {
        tm->sidx = 0;
      }

      // Chill...
      sched_yield();
      // Halt if all threads are idle
      if (tick % 256 == 0) {
        if (atomic_load_explicit(&net->idle, memory_order_relaxed) == TPC) {
          break;
        }
      }
    }
  }

  sync_threads();

  atomic_fetch_add(&net->itrs, tm->itrs);
  tm->itrs = 0;
}

// Normalizer
// ----------

// Thread data
typedef struct {
  Net*  net;
  TM*   tm;
  Book* book;
} ThreadArg;

void* thread_func(void* arg) {
  ThreadArg* data = (ThreadArg*)arg;
  evaluator(data->net, data->tm, data->book);
  return NULL;
}

// Sets the initial redex.
void boot_redex(Net* net, Pair redex) {
  net->vars_buf[get_val(ROOT)] = NONE;
  net->rbag_buf[0] = redex;
}

// Evaluates all redexes.
// TODO: cache threads to avoid spawning overhead
void normalize(Net* net, Book* book) {
  // Inits thread_arg objects
  ThreadArg thread_arg[TPC];
  for (u32 t = 0; t < TPC; ++t) {
    thread_arg[t].net  = net;
    thread_arg[t].tm   = tm[t];
    thread_arg[t].book = book;
  }

  // Spawns the evaluation threads
  pthread_t threads[TPC];
  for (u32 t = 0; t < TPC; ++t) {
    pthread_create(&threads[t], NULL, thread_func, &thread_arg[t]);
  }

  // Wait for the threads to finish
  for (u32 t = 0; t < TPC; ++t) {
    pthread_join(threads[t], NULL);
  }
}

// Util: expands a REF Port.
Port expand(Net* net, Book* book, Port port) {
  Port old = vars_load(net, get_val(ROOT));
  Port got = peek(net, port);
  while (get_tag(got) == REF) {
    boot_redex(net, new_pair(got, ROOT));
    normalize(net, book);
    got = peek(net, vars_load(net, get_val(ROOT)));
  }
  vars_create(net, get_val(ROOT), old);
  return got;
}

// Reads back an image.
// Encoding: (<tree>,<tree>) | #RRGGBB
void read_img(Net* net, Port port, u32 width, u32 height, u32* buffer) {
  //pretty_print_port(net, port);
  //printf("\n");
  typedef struct {
    Port port; u32 lv;
    u32 x0; u32 x1;
    u32 y0; u32 y1;
  } Rect;
  Rect stk[24];
  u32 pos = 0;
  stk[pos++] = (Rect){port, 0, 0, width, 0, height};
  while (pos > 0) {
    Rect rect = stk[--pos];
    Port port = enter(net, rect.port);
    u32  lv   = rect.lv;
    u32  x0   = rect.x0;
    u32  x1   = rect.x1;
    u32  y0   = rect.y0;
    u32  y1   = rect.y1;
    if (get_tag(port) == CON) {
      Pair nd = node_load(net, get_val(port));
      Port p1 = get_fst(nd);
      Port p2 = get_snd(nd);
      u32  xm = (x0 + x1) / 2;
      u32  ym = (y0 + y1) / 2;
      if (lv % 2 == 0) {
        stk[pos++] = (Rect){p2, lv+1, xm, x1, y0, y1};
        stk[pos++] = (Rect){p1, lv+1, x0, xm, y0, y1};
      } else {
        stk[pos++] = (Rect){p2, lv+1, x0, x1, ym, y1};
        stk[pos++] = (Rect){p1, lv+1, x0, x1, y0, ym};
      }
      continue;
    }
    if (get_tag(port) == NUM) {
      u32 color = get_u24(get_val(port));
      printf("COL=%08x x0=%04u x1=%04u y0=%04u y1=%04u | %s\n", color, x0, x1, y0, y1, show_port(port).x);
      for (u32 y = y0; y < y1; y++) {
        for (u32 x = x0; x < x1; x++) {
          buffer[y*width + x] = 0xFF000000 | color;
        }
      }
      continue;
    }
    break;
  }
}


//#ifdef IO_DRAWIMAGE
//// Global variables for the window and renderer
//static SDL_Window *window = NULL;
//static SDL_Renderer *renderer = NULL;
//static SDL_Texture *texture = NULL;
//// Function to close the SDL window and clean up resources
//void close_sdl(void) {
  //if (texture != NULL) {
    //SDL_DestroyTexture(texture);
    //texture = NULL;
  //}
  //if (renderer != NULL) {
    //SDL_DestroyRenderer(renderer);
    //renderer = NULL;
  //}
  //if (window != NULL) {
    //SDL_DestroyWindow(window);
    //window = NULL;
  //}
  //SDL_Quit();
//}
//// Function to render an image to the SDL window
//void render(uint32_t width, uint32_t height, uint32_t *buffer) {
  //// Initialize SDL if it hasn't been initialized
  //if (SDL_WasInit(SDL_INIT_VIDEO) == 0) {
    //if (SDL_Init(SDL_INIT_VIDEO) < 0) {
      //fprintf(stderr, "SDL could not initialize! SDL Error: %s\n", SDL_GetError());
      //return;
    //}
  //}
  //// Create window and renderer if they don't exist
  //if (window == NULL) {
    //window = SDL_CreateWindow("SDL Window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
    //if (window == NULL) {
      //fprintf(stderr, "Window could not be created! SDL Error: %s\n", SDL_GetError());
      //return;
    //}
    //renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    //if (renderer == NULL) {
      //SDL_DestroyWindow(window);
      //window = NULL;
      //fprintf(stderr, "Renderer could not be created! SDL Error: %s\n", SDL_GetError());
      //return;
    //}
  //}
  //// Create or recreate the texture if necessary
  //if (texture == NULL) {
    //texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height);
    //if (texture == NULL) {
      //fprintf(stderr, "Texture could not be created! SDL Error: %s\n", SDL_GetError());
      //return;
    //}
  //}
  //// Update the texture with the new buffer
  //if (SDL_UpdateTexture(texture, NULL, buffer, width * sizeof(uint32_t)) < 0) {
    //fprintf(stderr, "Texture could not be updated! SDL Error: %s\n", SDL_GetError());
    //return;
  //}
  //// Clear the renderer
  //SDL_RenderClear(renderer);
  //// Copy the texture to the renderer
  //SDL_RenderCopy(renderer, texture, NULL, NULL);
  //// Update the screen
  //SDL_RenderPresent(renderer);
  //// Process events to prevent the OS from thinking the application is unresponsive
  //SDL_Event e;
  //while (SDL_PollEvent(&e)) {
    //if (e.type == SDL_QUIT) {
      //close_sdl();
      //exit(0);
    //}
  //}
//}
//// IO: DrawImage
//Port io_put_image(Net* net, Book* book, u32 argc, Port* argv) {
  //u32 width = 256;
  //u32 height = 256;
  //// Create a buffer
  //uint32_t *buffer = (uint32_t *)malloc(width * height * sizeof(uint32_t));
  //if (buffer == NULL) {
    //fprintf(stderr, "Failed to allocate memory for buffer\n");
    //return 1;
  //}
  //// Initialize buffer to a dark blue background
  //for (int i = 0; i < width * height; ++i) {
    //buffer[i] = 0xFF000030; // Dark blue background
  //}
  //// Converts a HVM2 tuple-encoded quadtree to a color buffer
  //read_img(net, argv[0], width, height, buffer);
  //// Render the buffer to the screen
  //render(width, height, buffer);
  //// Wait some time
  //SDL_Delay(2000);
  //// Free the buffer
  //free(buffer);
  //return new_port(ERA, 0);
//}
//#else
//// IO: DrawImage
//Port io_put_image(Net* net, Book* book, u32 argc, Port* argv) {
  //printf("DRAWIMAGE: disabled.\n");
  //printf("Image rendering is a WIP. For now, to enable it, you must:\n");
  //printf("1. Generate a C file, with `hvm gen-c your_file.hvm`.\n");
  //printf("2. Manually un-comment the '#define IO_DRAWIMAGE' line on it.\n");
  //printf("3. Have SDL installed and compile it with '-lSDL2'.\n");
  //return new_port(ERA, 0);
//}
//#endif

// Book Loader
// -----------

bool book_load(Book* book, u32* buf) {
  // Reads defs_len
  book->defs_len = *buf++;

  // Parses each def
  for (u32 i = 0; i < book->defs_len; ++i) {
    // Reads fid
    u32 fid = *buf++;

    // Gets def
    Def* def = &book->defs_buf[fid];

    // Reads name
    memcpy(def->name, buf, 256);
    buf += 64;

    // Reads safe flag
    def->safe = *buf++;

    // Reads lengths
    def->rbag_len = *buf++;
    def->node_len = *buf++;
    def->vars_len = *buf++;

    if (def->rbag_len > DEF_RBAG_LEN) {
      fprintf(stderr, "def '%s' has too many redexes: %u\n", def->name, def->rbag_len);
      return false;
    }

    if (def->node_len > DEF_NODE_LEN) {
      fprintf(stderr, "def '%s' has too many nodes: %u\n", def->name, def->node_len);
      return false;
    }

    // Reads root
    def->root = *buf++;

    // Reads rbag_buf
    memcpy(def->rbag_buf, buf, 8*def->rbag_len);
    buf += def->rbag_len * 2;

    // Reads node_buf
    memcpy(def->node_buf, buf, 8*def->node_len);
    buf += def->node_len * 2;
  }

  return true;
}

// Debug Printing
// --------------

void put_u32(char* B, u32 val) {
  for (int i = 0; i < 8; i++, val >>= 4) {
    B[8-i-1] = "0123456789ABCDEF"[val & 0xF];
  }
}

Show show_port(Port port) {
  // NOTE: this is done like that because sprintf seems not to be working
  Show s;
  switch (get_tag(port)) {
    case VAR: memcpy(s.x, "VAR:", 4); put_u32(s.x+4, get_val(port)); break;
    case REF: memcpy(s.x, "REF:", 4); put_u32(s.x+4, get_val(port)); break;
    case ERA: memcpy(s.x, "ERA:________", 12); break;
    case NUM: memcpy(s.x, "NUM:", 4); put_u32(s.x+4, get_val(port)); break;
    case CON: memcpy(s.x, "CON:", 4); put_u32(s.x+4, get_val(port)); break;
    case DUP: memcpy(s.x, "DUP:", 4); put_u32(s.x+4, get_val(port)); break;
    case OPR: memcpy(s.x, "OPR:", 4); put_u32(s.x+4, get_val(port)); break;
    case SWI: memcpy(s.x, "SWI:", 4); put_u32(s.x+4, get_val(port)); break;
  }
  s.x[12] = '\0';
  return s;
}

Show show_rule(Rule rule) {
  Show s;
  switch (rule) {
    case LINK: memcpy(s.x, "LINK", 4); break;
    case VOID: memcpy(s.x, "VOID", 4); break;
    case ERAS: memcpy(s.x, "ERAS", 4); break;
    case ANNI: memcpy(s.x, "ANNI", 4); break;
    case COMM: memcpy(s.x, "COMM", 4); break;
    case OPER: memcpy(s.x, "OPER", 4); break;
    case SWIT: memcpy(s.x, "SWIT", 4); break;
    case CALL: memcpy(s.x, "CALL", 4); break;
    default  : memcpy(s.x, "????", 4); break;
  }
  s.x[4] = '\0';
  return s;
}

//void print_rbag(RBag* rbag) {
  //printf("RBAG | FST-TREE     | SND-TREE    \n");
  //printf("---- | ------------ | ------------\n");
  //for (u32 i = rbag->lo_ini; i < rbag->lo_end; ++i) {
    //Pair redex = rbag->lo_buf[i%RLEN];
    //printf("%04X | %s | %s\n", i, show_port(get_fst(redex)).x, show_port(get_snd(redex)).x);
  //}
  //for (u32 i = 0; i > rbag->hi_end; ++i) {
    //Pair redex = rbag->hi_buf[i];
    //printf("%04X | %s | %s\n", i, show_port(get_fst(redex)).x, show_port(get_snd(redex)).x);
  //}
  //printf("==== | ============ | ============\n");
//}

void print_net(Net* net) {
  printf("NODE | PORT-1       | PORT-2      \n");
  printf("---- | ------------ | ------------\n");
  for (u32 i = 0; i < G_NODE_LEN; ++i) {
    Pair node = node_load(net, i);
    if (node != 0) {
      printf("%04X | %s | %s\n", i, show_port(get_fst(node)).x, show_port(get_snd(node)).x);
    }
  }
  printf("==== | ============ |\n");
  printf("VARS | VALUE        |\n");
  printf("---- | ------------ |\n");
  for (u32 i = 0; i < G_VARS_LEN; ++i) {
    Port var = vars_load(net,i);
    if (var != 0) {
      printf("%04X | %s |\n", i, show_port(vars_load(net,i)).x);
    }
  }
  printf("==== | ============ |\n");
}

void pretty_print_numb(Numb word) {
  switch (get_typ(word)) {
    case TY_SYM: {
      switch (get_sym(word)) {
        // types
        case TY_U24: printf("[u24]"); break;
        case TY_I24: printf("[i24]"); break;
        case TY_F24: printf("[f24]"); break;
        // operations
        case OP_ADD: printf("[+]"); break;
        case OP_SUB: printf("[-]"); break;
        case FP_SUB: printf("[:-]"); break;
        case OP_MUL: printf("[*]"); break;
        case OP_DIV: printf("[/]"); break;
        case FP_DIV: printf("[:/]"); break;
        case OP_REM: printf("[%%]"); break;
        case FP_REM: printf("[:%%]"); break;
        case OP_EQ:  printf("[=]"); break;
        case OP_NEQ: printf("[!]"); break;
        case OP_LT:  printf("[<]"); break;
        case OP_GT:  printf("[>]"); break;
        case OP_AND: printf("[&]"); break;
        case OP_OR:  printf("[|]"); break;
        case OP_XOR: printf("[^]"); break;
        case OP_SHL: printf("[<<]"); break;
        case FP_SHL: printf("[:<<]"); break;
        case OP_SHR: printf("[>>]"); break;
        case FP_SHR: printf("[:>>]"); break;
        default:     printf("[?]"); break;
      }
      break;
    }
    case TY_U24: {
      printf("%u", get_u24(word));
      break;
    }
    case TY_I24: {
      printf("%+d", get_i24(word));
      break;
    }
    case TY_F24: {
      if (isinf(get_f24(word))) {
        if (signbit(get_f24(word))) {
          printf("-inf");
        } else {
          printf("+inf");
        }
      } else if (isnan(get_f24(word))) {
        printf("+NaN");
      } else {
        printf("%.7e", get_f24(word));
      }
      break;
    }
    default: {
      switch (get_typ(word)) {
        case OP_ADD: printf("[+0x%07X]", get_u24(word)); break;
        case OP_SUB: printf("[-0x%07X]", get_u24(word)); break;
        case FP_SUB: printf("[:-0x%07X]", get_u24(word)); break;
        case OP_MUL: printf("[*0x%07X]", get_u24(word)); break;
        case OP_DIV: printf("[/0x%07X]", get_u24(word)); break;
        case FP_DIV: printf("[:/0x%07X]", get_u24(word)); break;
        case OP_REM: printf("[%%0x%07X]", get_u24(word)); break;
        case FP_REM: printf("[:%%0x%07X]", get_u24(word)); break;
        case OP_EQ:  printf("[=0x%07X]", get_u24(word)); break;
        case OP_NEQ: printf("[!0x%07X]", get_u24(word)); break;
        case OP_LT:  printf("[<0x%07X]", get_u24(word)); break;
        case OP_GT:  printf("[>0x%07X]", get_u24(word)); break;
        case OP_AND: printf("[&0x%07X]", get_u24(word)); break;
        case OP_OR:  printf("[|0x%07X]", get_u24(word)); break;
        case OP_XOR: printf("[^0x%07X]", get_u24(word)); break;
        case OP_SHL: printf("[<<0x%07X]", get_u24(word)); break;
        case FP_SHL: printf("[:<<0x%07X]", get_u24(word)); break;
        case OP_SHR: printf("[>>0x%07X]", get_u24(word)); break;
        case FP_SHR: printf("[:>>0x%07X]", get_u24(word)); break;
        default:     printf("[?0x%07X]", get_u24(word)); break;
      }
      break;
    }
  }

}

void pretty_print_port(Net* net, Book* book, Port port) {
  Port stack[4096];
  stack[0] = port;
  u32 len = 1;
  u32 num = 0;
  while (len > 0) {
    Port cur = stack[--len];
    switch (get_tag(cur)) {
      case CON: {
        Pair node = node_load(net,get_val(cur));
        Port p2   = get_snd(node);
        Port p1   = get_fst(node);
        printf("(");
        stack[len++] = new_port(ERA, (u32)(')'));
        stack[len++] = p2;
        stack[len++] = new_port(ERA, (u32)(' '));
        stack[len++] = p1;
        break;
      }
      case ERA: {
        if (get_val(cur) != 0) {
          printf("%c", (char)get_val(cur));
        } else {
          printf("*");
        }
        break;
      }
      case VAR: {
        Port got = vars_load(net, get_val(cur));
        if (got != NONE) {
          stack[len++] = got;
        } else {
          printf("x%x", get_val(cur));
        }
        break;
      }
      case NUM: {
        pretty_print_numb(get_val(cur));
        break;
      }
      case DUP: {
        Pair node = node_load(net,get_val(cur));
        Port p2   = get_snd(node);
        Port p1   = get_fst(node);
        printf("{");
        stack[len++] = new_port(ERA, (u32)('}'));
        stack[len++] = p2;
        stack[len++] = new_port(ERA, (u32)(' '));
        stack[len++] = p1;
        break;
      }
      case OPR: {
        Pair node = node_load(net,get_val(cur));
        Port p2   = get_snd(node);
        Port p1   = get_fst(node);
        printf("$(");
        stack[len++] = new_port(ERA, (u32)(')'));
        stack[len++] = p2;
        stack[len++] = new_port(ERA, (u32)(' '));
        stack[len++] = p1;
        break;
      }
      case SWI: {
        Pair node = node_load(net,get_val(cur));
        Port p2   = get_snd(node);
        Port p1   = get_fst(node);
        printf("?(");
        stack[len++] = new_port(ERA, (u32)(')'));
        stack[len++] = p2;
        stack[len++] = new_port(ERA, (u32)(' '));
        stack[len++] = p1;
        break;
      }
      case REF: {
        u32  fid = get_val(cur) & 0xFFFFFFF;
        Def* def = &book->defs_buf[fid];
        printf("@%s", def->name);
        break;
      }
    }
  }
}

//void pretty_print_rbag(Net* net, RBag* rbag) {
  //for (u32 i = rbag->lo_ini; i < rbag->lo_end; ++i) {
    //Pair redex = rbag->lo_buf[i];
    //if (redex != 0) {
      //pretty_print_port(net, get_fst(redex));
      //printf(" ~ ");
      //pretty_print_port(net, get_snd(redex));
      //printf("\n");
    //}
  //}
  //for (u32 i = 0; i > rbag->hi_end; ++i) {
    //Pair redex = rbag->hi_buf[i];
    //if (redex != 0) {
      //pretty_print_port(net, get_fst(redex));
      //printf(" ~ ");
      //pretty_print_port(net, get_snd(redex));
      //printf("\n");
    //}
  //}
//}

// Demos
// -----

  // stress_test 2^10 x 65536
  //static const u8 BOOK_BUF[] = {6, 0, 0, 0, 0, 0, 0, 0, 109, 97, 105, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 11, 10, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 102, 117, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 25, 0, 0, 0, 2, 0, 0, 0, 102, 117, 110, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 4, 0, 0, 0, 11, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 0, 102, 117, 110, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 128, 20, 0, 0, 0, 9, 0, 0, 128, 44, 0, 0, 0, 13, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 3, 4, 0, 0, 38, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 4, 0, 0, 0, 108, 111, 111, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 41, 0, 0, 0, 5, 0, 0, 0, 108, 111, 111, 112, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 0, 0, 0, 0};

  // stress_test 2^18 x 65536
  //static const u8 BOOK_BUF[] = {6, 0, 0, 0, 0, 0, 0, 0, 109, 97, 105, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 11, 18, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 102, 117, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 25, 0, 0, 0, 2, 0, 0, 0, 102, 117, 110, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 4, 0, 0, 0, 11, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 0, 102, 117, 110, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 128, 20, 0, 0, 0, 9, 0, 0, 128, 44, 0, 0, 0, 13, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 3, 4, 0, 0, 38, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 4, 0, 0, 0, 108, 111, 111, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 41, 0, 0, 0, 5, 0, 0, 0, 108, 111, 111, 112, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0, 0, 0, 0, 0};

  // bitonic_sort 2^20
  //static const u8 BOOK_BUF[] = {19, 0, 0, 0, 0, 0, 0, 0, 109, 97, 105, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 89, 0, 0, 0, 4, 0, 0, 0, 11, 18, 0, 0, 12, 0, 0, 0, 65, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 100, 111, 119, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 60, 0, 0, 0, 20, 0, 0, 0, 44, 0, 0, 0, 28, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 52, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 68, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 2, 0, 0, 0, 100, 111, 119, 110, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 13, 0, 0, 0, 8, 0, 0, 0, 4, 0, 0, 0, 25, 0, 0, 128, 60, 0, 0, 0, 25, 0, 0, 128, 84, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 36, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 45, 0, 0, 0, 52, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 48, 0, 0, 0, 56, 0, 0, 0, 0, 0, 0, 0, 68, 0, 0, 0, 32, 0, 0, 0, 76, 0, 0, 0, 16, 0, 0, 0, 48, 0, 0, 0, 8, 0, 0, 0, 92, 0, 0, 0, 40, 0, 0, 0, 100, 0, 0, 0, 24, 0, 0, 0, 56, 0, 0, 0, 3, 0, 0, 0, 102, 108, 111, 119, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 60, 0, 0, 0, 20, 0, 0, 0, 44, 0, 0, 0, 28, 0, 0, 0, 33, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 52, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 68, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 4, 0, 0, 0, 102, 108, 111, 119, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 14, 0, 0, 0, 8, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0, 60, 0, 0, 0, 129, 0, 0, 0, 84, 0, 0, 0, 13, 0, 0, 0, 28, 0, 0, 0, 22, 0, 0, 0, 8, 0, 0, 0, 35, 1, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 44, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 53, 0, 0, 0, 48, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 0, 0, 0, 0, 68, 0, 0, 0, 32, 0, 0, 0, 76, 0, 0, 0, 56, 0, 0, 0, 48, 0, 0, 0, 8, 0, 0, 0, 92, 0, 0, 0, 40, 0, 0, 0, 100, 0, 0, 0, 16, 0, 0, 0, 108, 0, 0, 0, 24, 0, 0, 0, 56, 0, 0, 0, 5, 0, 0, 0, 103, 101, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 103, 101, 110, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 12, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 41, 0, 0, 128, 68, 0, 0, 0, 41, 0, 0, 128, 84, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 29, 0, 0, 0, 60, 0, 0, 0, 38, 0, 0, 0, 54, 0, 0, 0, 59, 2, 0, 0, 46, 0, 0, 0, 35, 1, 0, 0, 16, 0, 0, 0, 59, 2, 0, 0, 24, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 0, 0, 0, 0, 76, 0, 0, 0, 16, 0, 0, 0, 32, 0, 0, 0, 8, 0, 0, 0, 92, 0, 0, 0, 24, 0, 0, 0, 40, 0, 0, 0, 7, 0, 0, 0, 109, 97, 105, 110, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 41, 0, 0, 0, 4, 0, 0, 0, 11, 18, 0, 0, 12, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 109, 97, 105, 110, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 73, 0, 0, 0, 4, 0, 0, 0, 11, 18, 0, 0, 12, 0, 0, 0, 11, 0, 0, 0, 20, 0, 0, 0, 57, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 115, 111, 114, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 60, 0, 0, 0, 20, 0, 0, 0, 44, 0, 0, 0, 28, 0, 0, 0, 81, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 52, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 68, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 10, 0, 0, 0, 115, 111, 114, 116, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 17, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 25, 0, 0, 0, 60, 0, 0, 0, 73, 0, 0, 128, 92, 0, 0, 0, 73, 0, 0, 128, 116, 0, 0, 0, 13, 0, 0, 0, 36, 0, 0, 0, 22, 0, 0, 0, 29, 0, 0, 0, 35, 1, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 44, 0, 0, 0, 52, 0, 0, 0, 24, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 68, 0, 0, 0, 40, 0, 0, 0, 76, 0, 0, 0, 84, 0, 0, 0, 48, 0, 0, 0, 56, 0, 0, 0, 64, 0, 0, 0, 8, 0, 0, 0, 100, 0, 0, 0, 11, 0, 0, 0, 108, 0, 0, 0, 24, 0, 0, 0, 56, 0, 0, 0, 16, 0, 0, 0, 124, 0, 0, 0, 11, 1, 0, 0, 132, 0, 0, 0, 32, 0, 0, 0, 64, 0, 0, 0, 11, 0, 0, 0, 115, 117, 109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 115, 117, 109, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 10, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 89, 0, 0, 128, 36, 0, 0, 0, 89, 0, 0, 128, 68, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 32, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 0, 0, 0, 0, 44, 0, 0, 0, 16, 0, 0, 0, 54, 0, 0, 0, 3, 4, 0, 0, 62, 0, 0, 0, 40, 0, 0, 0, 32, 0, 0, 0, 8, 0, 0, 0, 76, 0, 0, 0, 24, 0, 0, 0, 40, 0, 0, 0, 13, 0, 0, 0, 115, 119, 97, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 44, 0, 0, 0, 20, 0, 0, 0, 28, 0, 0, 0, 113, 0, 0, 0, 121, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 8, 0, 0, 0, 52, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 14, 0, 0, 0, 115, 119, 97, 112, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 115, 119, 97, 112, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 119, 97, 114, 112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 52, 0, 0, 0, 20, 0, 0, 0, 28, 0, 0, 0, 137, 0, 0, 0, 145, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0, 8, 0, 0, 0, 44, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 60, 0, 0, 0, 8, 0, 0, 0, 68, 0, 0, 0, 0, 0, 0, 0, 24, 0, 0, 0, 17, 0, 0, 0, 119, 97, 114, 112, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 12, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 105, 0, 0, 0, 76, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 29, 0, 0, 0, 52, 0, 0, 0, 38, 0, 0, 0, 24, 0, 0, 0, 3, 15, 0, 0, 46, 0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 62, 0, 0, 0, 40, 0, 0, 0, 3, 18, 0, 0, 70, 0, 0, 0, 16, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 84, 0, 0, 0, 24, 0, 0, 0, 92, 0, 0, 0, 8, 0, 0, 0, 40, 0, 0, 0, 18, 0, 0, 0, 119, 97, 114, 112, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 21, 0, 0, 0, 12, 0, 0, 0, 4, 0, 0, 0, 129, 0, 0, 128, 92, 0, 0, 0, 129, 0, 0, 128, 132, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 28, 0, 0, 0, 36, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 44, 0, 0, 0, 52, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 61, 0, 0, 0, 68, 0, 0, 0, 48, 0, 0, 0, 56, 0, 0, 0, 76, 0, 0, 0, 84, 0, 0, 0, 64, 0, 0, 0, 72, 0, 0, 0, 80, 0, 0, 0, 88, 0, 0, 0, 8, 0, 0, 0, 100, 0, 0, 0, 56, 0, 0, 0, 108, 0, 0, 0, 40, 0, 0, 0, 116, 0, 0, 0, 24, 0, 0, 0, 124, 0, 0, 0, 72, 0, 0, 0, 88, 0, 0, 0, 0, 0, 0, 0, 140, 0, 0, 0, 48, 0, 0, 0, 148, 0, 0, 0, 32, 0, 0, 0, 156, 0, 0, 0, 16, 0, 0, 0, 164, 0, 0, 0, 64, 0, 0, 0, 80, 0, 0, 0};

static const u8 BOOK_BUF[] = {21, 0, 0, 0, 0, 0, 0, 0, 109, 97, 105, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 105, 0, 0, 0, 4, 0, 0, 0, 81, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 20, 0, 0, 0, 11, 0, 16, 0, 8, 0, 0, 0, 1, 0, 0, 0, 80, 97, 114, 84, 114, 101, 101, 47, 76, 101, 97, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 11, 1, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 2, 0, 0, 0, 80, 97, 114, 84, 114, 101, 101, 47, 76, 101, 97, 102, 47, 116, 97, 103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 1, 0, 0, 3, 0, 0, 0, 80, 97, 114, 84, 114, 101, 101, 47, 78, 111, 100, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 28, 0, 0, 0, 16, 0, 0, 0, 11, 0, 0, 0, 36, 0, 0, 0, 0, 0, 0, 0, 44, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 4, 0, 0, 0, 80, 97, 114, 84, 114, 101, 101, 47, 78, 111, 100, 101, 47, 116, 97, 103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 5, 0, 0, 0, 102, 50, 52, 47, 116, 111, 95, 117, 50, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 3, 1, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 106, 115, 104, 97, 114, 107, 95, 103, 114, 105, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 22, 0, 0, 0, 12, 0, 0, 0, 4, 0, 0, 0, 57, 0, 0, 0, 92, 0, 0, 0, 41, 0, 0, 0, 164, 0, 0, 0, 41, 0, 0, 0, 172, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 16, 0, 0, 0, 28, 0, 0, 0, 24, 0, 0, 0, 36, 0, 0, 0, 32, 0, 0, 0, 44, 0, 0, 0, 40, 0, 0, 0, 52, 0, 0, 0, 61, 0, 0, 0, 84, 0, 0, 0, 70, 0, 0, 0, 64, 0, 0, 0, 3, 7, 0, 0, 78, 0, 0, 0, 48, 0, 0, 0, 56, 0, 0, 0, 48, 0, 0, 0, 72, 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 80, 0, 0, 0, 108, 0, 0, 0, 40, 0, 0, 0, 116, 0, 0, 0, 24, 0, 0, 0, 124, 0, 0, 0, 16, 0, 0, 0, 132, 0, 0, 0, 8, 0, 0, 0, 140, 0, 0, 0, 32, 0, 0, 0, 148, 0, 0, 0, 11, 0, 0, 0, 156, 0, 0, 0, 88, 0, 0, 0, 72, 0, 0, 0, 64, 0, 0, 0, 80, 0, 0, 0, 56, 0, 0, 0, 88, 0, 0, 0, 7, 0, 0, 0, 106, 115, 104, 97, 114, 107, 95, 103, 114, 105, 100, 95, 95, 98, 101, 110, 100, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 0, 11, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 16, 0, 0, 0, 28, 0, 0, 0, 24, 0, 0, 0, 36, 0, 0, 0, 32, 0, 0, 0, 44, 0, 0, 0, 40, 0, 0, 0, 52, 0, 0, 0, 48, 0, 0, 0, 60, 0, 0, 0, 69, 0, 0, 0, 76, 0, 0, 0, 56, 0, 0, 0, 64, 0, 0, 0, 85, 0, 0, 0, 80, 0, 0, 0, 94, 0, 0, 0, 72, 0, 0, 0, 3, 5, 0, 0, 102, 0, 0, 0, 56, 0, 0, 0, 110, 0, 0, 0, 115, 1, 0, 0, 119, 0, 0, 0, 124, 0, 0, 0, 132, 0, 0, 0, 65, 0, 0, 0, 73, 0, 0, 0, 0, 0, 0, 0, 140, 0, 0, 0, 8, 0, 0, 0, 148, 0, 0, 0, 16, 0, 0, 0, 156, 0, 0, 0, 24, 0, 0, 0, 164, 0, 0, 0, 32, 0, 0, 0, 172, 0, 0, 0, 40, 0, 0, 0, 180, 0, 0, 0, 48, 0, 0, 0, 188, 0, 0, 0, 64, 0, 0, 0, 196, 0, 0, 0, 72, 0, 0, 0, 80, 0, 0, 0, 8, 0, 0, 0, 106, 115, 104, 97, 114, 107, 95, 103, 114, 105, 100, 95, 95, 98, 101, 110, 100, 48, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 53, 0, 0, 0, 19, 0, 0, 0, 4, 0, 0, 0, 137, 0, 0, 0, 236, 0, 0, 0, 161, 0, 0, 0, 252, 0, 0, 0, 161, 0, 0, 0, 84, 1, 0, 0, 14, 0, 0, 0, 28, 0, 0, 0, 3, 4, 0, 0, 22, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 37, 0, 0, 0, 44, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 53, 0, 0, 0, 92, 0, 0, 0, 32, 0, 0, 0, 61, 0, 0, 0, 70, 0, 0, 0, 77, 0, 0, 0, 75, 0, 0, 64, 40, 0, 0, 0, 48, 0, 0, 0, 86, 0, 0, 0, 75, 0, 0, 64, 56, 0, 0, 0, 101, 0, 0, 0, 116, 0, 0, 0, 110, 0, 0, 0, 72, 0, 0, 0, 75, 0, 0, 64, 64, 0, 0, 0, 125, 0, 0, 0, 132, 0, 0, 0, 80, 0, 0, 0, 88, 0, 0, 0, 142, 0, 0, 0, 156, 0, 0, 0, 3, 4, 0, 0, 150, 0, 0, 0, 96, 0, 0, 0, 104, 0, 0, 0, 165, 0, 0, 0, 180, 0, 0, 0, 174, 0, 0, 0, 120, 0, 0, 0, 75, 0, 0, 64, 112, 0, 0, 0, 189, 0, 0, 0, 228, 0, 0, 0, 198, 0, 0, 0, 214, 0, 0, 0, 3, 10, 0, 0, 206, 0, 0, 0, 16, 0, 0, 0, 128, 0, 0, 0, 3, 8, 0, 0, 222, 0, 0, 0, 24, 0, 0, 0, 136, 0, 0, 0, 2, 0, 0, 0, 144, 0, 0, 0, 8, 0, 0, 0, 244, 0, 0, 0, 104, 0, 0, 0, 144, 0, 0, 0, 128, 0, 0, 0, 6, 1, 0, 0, 3, 7, 0, 0, 14, 1, 0, 0, 32, 0, 0, 0, 22, 1, 0, 0, 3, 4, 0, 0, 30, 1, 0, 0, 40, 0, 0, 0, 38, 1, 0, 0, 3, 5, 0, 0, 46, 1, 0, 0, 64, 0, 0, 0, 54, 1, 0, 0, 3, 7, 0, 0, 62, 1, 0, 0, 80, 0, 0, 0, 70, 1, 0, 0, 3, 8, 0, 0, 78, 1, 0, 0, 72, 0, 0, 0, 0, 0, 0, 0, 136, 0, 0, 0, 94, 1, 0, 0, 3, 7, 0, 0, 102, 1, 0, 0, 48, 0, 0, 0, 110, 1, 0, 0, 3, 4, 0, 0, 118, 1, 0, 0, 56, 0, 0, 0, 126, 1, 0, 0, 3, 5, 0, 0, 134, 1, 0, 0, 112, 0, 0, 0, 142, 1, 0, 0, 3, 7, 0, 0, 150, 1, 0, 0, 88, 0, 0, 0, 158, 1, 0, 0, 3, 8, 0, 0, 166, 1, 0, 0, 120, 0, 0, 0, 96, 0, 0, 0, 9, 0, 0, 0, 106, 115, 104, 97, 114, 107, 95, 103, 114, 105, 100, 95, 95, 98, 101, 110, 100, 48, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 46, 0, 0, 0, 22, 0, 0, 0, 4, 0, 0, 0, 57, 0, 0, 128, 228, 0, 0, 0, 57, 0, 0, 128, 44, 1, 0, 0, 2, 0, 0, 0, 12, 0, 0, 0, 21, 0, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 37, 0, 0, 0, 44, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 53, 0, 0, 0, 60, 0, 0, 0, 32, 0, 0, 0, 40, 0, 0, 0, 69, 0, 0, 0, 76, 0, 0, 0, 48, 0, 0, 0, 56, 0, 0, 0, 85, 0, 0, 0, 92, 0, 0, 0, 64, 0, 0, 0, 72, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 80, 0, 0, 0, 88, 0, 0, 0, 117, 0, 0, 0, 124, 0, 0, 0, 96, 0, 0, 0, 104, 0, 0, 0, 133, 0, 0, 0, 196, 0, 0, 0, 112, 0, 0, 0, 141, 0, 0, 0, 150, 0, 0, 0, 174, 0, 0, 0, 3, 4, 0, 0, 158, 0, 0, 0, 120, 0, 0, 0, 166, 0, 0, 0, 75, 2, 0, 0, 128, 0, 0, 0, 3, 4, 0, 0, 182, 0, 0, 0, 136, 0, 0, 0, 190, 0, 0, 0, 75, 2, 0, 0, 144, 0, 0, 0, 205, 0, 0, 0, 220, 0, 0, 0, 120, 0, 0, 0, 213, 0, 0, 0, 136, 0, 0, 0, 152, 0, 0, 0, 160, 0, 0, 0, 168, 0, 0, 0, 0, 0, 0, 0, 236, 0, 0, 0, 16, 0, 0, 0, 244, 0, 0, 0, 32, 0, 0, 0, 252, 0, 0, 0, 48, 0, 0, 0, 4, 1, 0, 0, 64, 0, 0, 0, 12, 1, 0, 0, 80, 0, 0, 0, 20, 1, 0, 0, 96, 0, 0, 0, 28, 1, 0, 0, 112, 0, 0, 0, 36, 1, 0, 0, 128, 0, 0, 0, 160, 0, 0, 0, 8, 0, 0, 0, 52, 1, 0, 0, 24, 0, 0, 0, 60, 1, 0, 0, 40, 0, 0, 0, 68, 1, 0, 0, 56, 0, 0, 0, 76, 1, 0, 0, 72, 0, 0, 0, 84, 1, 0, 0, 88, 0, 0, 0, 92, 1, 0, 0, 104, 0, 0, 0, 100, 1, 0, 0, 144, 0, 0, 0, 108, 1, 0, 0, 152, 0, 0, 0, 168, 0, 0, 0, 10, 0, 0, 0, 109, 97, 105, 110, 95, 95, 98, 101, 110, 100, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 29, 0, 0, 0, 24, 0, 0, 0, 38, 0, 0, 0, 16, 0, 0, 0, 3, 5, 0, 0, 46, 0, 0, 0, 0, 0, 0, 0, 54, 0, 0, 0, 115, 1, 0, 0, 63, 0, 0, 0, 68, 0, 0, 0, 76, 0, 0, 0, 89, 0, 0, 0, 97, 0, 0, 0, 8, 0, 0, 0, 84, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 11, 0, 0, 0, 109, 97, 105, 110, 95, 95, 98, 101, 110, 100, 48, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 15, 0, 0, 0, 7, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0, 44, 0, 0, 0, 41, 0, 0, 0, 52, 0, 0, 0, 137, 0, 0, 0, 60, 0, 0, 0, 161, 0, 0, 0, 76, 0, 0, 0, 161, 0, 0, 0, 100, 0, 0, 0, 13, 0, 0, 0, 36, 0, 0, 0, 22, 0, 0, 0, 30, 0, 0, 0, 91, 64, 0, 0, 0, 0, 0, 0, 75, 64, 0, 0, 8, 0, 0, 0, 2, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 32, 0, 0, 0, 24, 0, 0, 0, 40, 0, 0, 0, 68, 0, 0, 0, 48, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 86, 0, 0, 0, 75, 0, 0, 66, 94, 0, 0, 0, 51, 0, 0, 64, 40, 0, 0, 0, 8, 0, 0, 0, 110, 0, 0, 0, 75, 0, 0, 66, 118, 0, 0, 0, 51, 0, 128, 63, 48, 0, 0, 0, 12, 0, 0, 0, 109, 97, 105, 110, 95, 95, 98, 101, 110, 100, 48, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 19, 0, 0, 0, 9, 0, 0, 0, 4, 0, 0, 0, 25, 0, 0, 0, 108, 0, 0, 0, 81, 0, 0, 128, 124, 0, 0, 0, 81, 0, 0, 128, 140, 0, 0, 0, 2, 0, 0, 0, 12, 0, 0, 0, 21, 0, 0, 0, 84, 0, 0, 0, 0, 0, 0, 0, 29, 0, 0, 0, 38, 0, 0, 0, 62, 0, 0, 0, 3, 4, 0, 0, 46, 0, 0, 0, 8, 0, 0, 0, 54, 0, 0, 0, 75, 2, 0, 0, 16, 0, 0, 0, 3, 4, 0, 0, 70, 0, 0, 0, 24, 0, 0, 0, 78, 0, 0, 0, 75, 2, 0, 0, 32, 0, 0, 0, 93, 0, 0, 0, 48, 0, 0, 0, 8, 0, 0, 0, 101, 0, 0, 0, 24, 0, 0, 0, 40, 0, 0, 0, 56, 0, 0, 0, 116, 0, 0, 0, 64, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 132, 0, 0, 0, 16, 0, 0, 0, 56, 0, 0, 0, 32, 0, 0, 0, 148, 0, 0, 0, 40, 0, 0, 0, 64, 0, 0, 0, 13, 0, 0, 0, 109, 97, 105, 110, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 115, 117, 109, 95, 116, 114, 101, 101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 113, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 109, 97, 105, 110, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 115, 117, 109, 95, 116, 114, 101, 101, 95, 95, 102, 111, 108, 100, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 129, 0, 0, 0, 0, 0, 0, 0, 15, 0, 0, 0, 109, 97, 105, 110, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 115, 117, 109, 95, 116, 114, 101, 101, 95, 95, 102, 111, 108, 100, 48, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 6, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 113, 0, 0, 128, 20, 0, 0, 0, 113, 0, 0, 128, 44, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 0, 3, 4, 0, 0, 38, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 8, 0, 0, 0, 24, 0, 0, 0, 16, 0, 0, 0, 109, 97, 105, 110, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 115, 117, 109, 95, 116, 114, 101, 101, 95, 95, 102, 111, 108, 100, 48, 95, 95, 67, 49, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 15, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 8, 0, 0, 0, 121, 0, 0, 0, 28, 0, 0, 0, 2, 0, 0, 0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 109, 97, 110, 100, 101, 108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 7, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 145, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 28, 0, 0, 0, 8, 0, 0, 0, 36, 0, 0, 0, 27, 0, 0, 0, 44, 0, 0, 0, 27, 0, 0, 0, 52, 0, 0, 0, 27, 0, 0, 0, 16, 0, 0, 0, 18, 0, 0, 0, 109, 97, 110, 100, 101, 108, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 114, 101, 99, 54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 37, 0, 0, 0, 13, 0, 0, 0, 4, 0, 0, 0, 150, 0, 0, 0, 99, 0, 0, 0, 38, 1, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 8, 0, 0, 0, 20, 0, 0, 0, 29, 0, 0, 0, 44, 0, 0, 0, 38, 0, 0, 0, 24, 0, 0, 0, 123, 0, 128, 67, 16, 0, 0, 0, 53, 0, 0, 0, 108, 0, 0, 0, 62, 0, 0, 0, 101, 0, 0, 0, 3, 7, 0, 0, 70, 0, 0, 0, 32, 0, 0, 0, 78, 0, 0, 0, 3, 4, 0, 0, 86, 0, 0, 0, 40, 0, 0, 0, 94, 0, 0, 0, 123, 0, 128, 64, 48, 0, 0, 0, 32, 0, 0, 0, 56, 0, 0, 0, 117, 0, 0, 0, 80, 0, 0, 0, 126, 0, 0, 0, 141, 0, 0, 0, 3, 7, 0, 0, 134, 0, 0, 0, 64, 0, 0, 0, 40, 0, 0, 0, 64, 0, 0, 0, 72, 0, 0, 0, 16, 0, 0, 0, 158, 0, 0, 0, 3, 4, 0, 0, 166, 0, 0, 0, 88, 0, 0, 0, 174, 0, 0, 0, 107, 0, 0, 0, 182, 0, 0, 0, 107, 0, 0, 0, 191, 0, 0, 0, 196, 0, 0, 0, 252, 0, 0, 0, 153, 0, 0, 0, 204, 0, 0, 0, 2, 0, 0, 0, 212, 0, 0, 0, 2, 0, 0, 0, 220, 0, 0, 0, 2, 0, 0, 0, 228, 0, 0, 0, 96, 0, 0, 0, 236, 0, 0, 0, 2, 0, 0, 0, 244, 0, 0, 0, 2, 0, 0, 0, 96, 0, 0, 0, 0, 0, 0, 0, 4, 1, 0, 0, 8, 0, 0, 0, 12, 1, 0, 0, 24, 0, 0, 0, 20, 1, 0, 0, 56, 0, 0, 0, 28, 1, 0, 0, 72, 0, 0, 0, 80, 0, 0, 0, 48, 0, 0, 0, 88, 0, 0, 0, 19, 0, 0, 0, 109, 97, 110, 100, 101, 108, 95, 95, 108, 111, 99, 97, 108, 95, 48, 95, 114, 101, 99, 54, 95, 95, 67, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 30, 0, 0, 0, 13, 0, 0, 0, 4, 0, 0, 0, 145, 0, 0, 0, 164, 0, 0, 0, 206, 0, 0, 0, 59, 0, 0, 64, 13, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 29, 0, 0, 0, 36, 0, 0, 0, 16, 0, 0, 0, 24, 0, 0, 0, 46, 0, 0, 0, 52, 0, 0, 0, 35, 0, 128, 63, 32, 0, 0, 0, 61, 0, 0, 0, 124, 0, 0, 0, 70, 0, 0, 0, 117, 0, 0, 0, 3, 7, 0, 0, 78, 0, 0, 0, 40, 0, 0, 0, 86, 0, 0, 0, 3, 5, 0, 0, 94, 0, 0, 0, 48, 0, 0, 0, 102, 0, 0, 0, 3, 4, 0, 0, 110, 0, 0, 0, 8, 0, 0, 0, 56, 0, 0, 0, 40, 0, 0, 0, 64, 0, 0, 0, 133, 0, 0, 0, 88, 0, 0, 0, 142, 0, 0, 0, 157, 0, 0, 0, 3, 7, 0, 0, 150, 0, 0, 0, 72, 0, 0, 0, 48, 0, 0, 0, 72, 0, 0, 0, 80, 0, 0, 0, 0, 0, 0, 0, 172, 0, 0, 0, 16, 0, 0, 0, 180, 0, 0, 0, 32, 0, 0, 0, 188, 0, 0, 0, 56, 0, 0, 0, 196, 0, 0, 0, 96, 0, 0, 0, 88, 0, 0, 0, 64, 0, 0, 0, 214, 0, 0, 0, 3, 7, 0, 0, 222, 0, 0, 0, 80, 0, 0, 0, 230, 0, 0, 0, 3, 4, 0, 0, 238, 0, 0, 0, 24, 0, 0, 0, 96, 0, 0, 0, 20, 0, 0, 0, 117, 50, 52, 47, 116, 111, 95, 102, 50, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0};

#ifdef IO
void do_run_io(Net* net, Book* book, Port port);
#endif

// Main
// ----

void hvm_c(u32* book_buffer) {
  // Creates static TMs
  alloc_static_tms();

  // Loads the Book
  Book* book = NULL;
  if (book_buffer) {
    book = (Book*)malloc(sizeof(Book));
    if (!book_load(book, book_buffer)) {
      fprintf(stderr, "failed to load book\n");

      return;
    }
  }

  // GMem
  Net *net = net_new();

  // Starts the timer
  u64 start = time64();

  // Creates an initial redex that calls main
  boot_redex(net, new_pair(new_port(REF, 0), ROOT));

  #ifdef IO
  do_run_io(net, book, ROOT);
  #else
  normalize(net, book);
  #endif

  // Prints the result
  printf("Result: ");
  pretty_print_port(net, book, enter(net, ROOT));
  printf("\n");

  // Stops the timer
  double duration = (time64() - start) / 1000000000.0; // seconds

  // Prints interactions and time
  u64 itrs = atomic_load(&net->itrs);
  printf("- ITRS: %" PRIu64 "\n", itrs);
  printf("- TIME: %.2fs\n", duration);
  printf("- MIPS: %.2f\n", (double)itrs / duration / 1000000.0);

  // Frees everything
  free_static_tms();
  free(net);
  free(book);
}

#ifdef WITH_MAIN
int main() {
  hvm_c((u32*)BOOK_BUF);
  return 0;
}
#endif


/* --- JShark HVM2 WASM bridge (auto-generated) --- */
typedef int64_t jshark_hvm2_i64;

#define JSHARK_MANDEL_MAX_ITER 256

static jshark_hvm2_i64 jshark_mandel_iter(double cr, double ci) {
  int n = 0;
  double zr = 0.0;
  double zi = 0.0;
  while (n < JSHARK_MANDEL_MAX_ITER && (zr * zr + zi * zi) < 4.0) {
    double nzr = zr * zr - zi * zi + cr;
    double nzi = 2.0 * zr * zi + ci;
    zr = nzr;
    zi = nzi;
    n++;
  }
  return (jshark_hvm2_i64)n;
}

static double jshark_i64_to_f64(jshark_hvm2_i64 x) {
  union { jshark_hvm2_i64 i; double d; } u;
  u.i = x;
  return u.d;
}

/* Batched grid: one JS->WASM call per frame instead of one per pixel,
 * so kernel timing reflects compute, not boundary-crossing overhead. */
#define JSHARK_GRID_CAP (1 << 17)
static int32_t jshark_grid_buf[JSHARK_GRID_CAP];

/* SIMD128 quad kernel: four pixels via two interleaved f64x2 vectors.
 * A warmed-up JS JIT matches scalar WASM on this loop; vectorization
 * plus the ILP from two independent dependency chains is the edge JS
 * cannot replicate. Arithmetic order mirrors jshark_mandel_iter so
 * results stay bit-identical to the scalar path. */
#ifdef __wasm_simd128__
#include <wasm_simd128.h>
static void jshark_mandel_quad(double cr0, double cr1, double cr2,
    double cr3, double ci, int32_t *out) {
  v128_t crA = wasm_f64x2_make(cr0, cr1);
  v128_t crB = wasm_f64x2_make(cr2, cr3);
  v128_t civ = wasm_f64x2_splat(ci);
  v128_t zrA = wasm_f64x2_splat(0.0);
  v128_t ziA = zrA;
  v128_t zrB = zrA;
  v128_t ziB = zrA;
  v128_t four = wasm_f64x2_splat(4.0);
  v128_t two = wasm_f64x2_splat(2.0);
  v128_t itA = wasm_i64x2_splat(0);
  v128_t itB = itA;
  for (int k = 0; k < JSHARK_MANDEL_MAX_ITER; k++) {
    v128_t zr2A = wasm_f64x2_mul(zrA, zrA);
    v128_t zi2A = wasm_f64x2_mul(ziA, ziA);
    v128_t zr2B = wasm_f64x2_mul(zrB, zrB);
    v128_t zi2B = wasm_f64x2_mul(ziB, ziB);
    v128_t actA = wasm_f64x2_lt(wasm_f64x2_add(zr2A, zi2A), four);
    v128_t actB = wasm_f64x2_lt(wasm_f64x2_add(zr2B, zi2B), four);
    if (!wasm_v128_any_true(wasm_v128_or(actA, actB))) break;
    /* active lanes are all-ones (-1); subtracting increments them */
    itA = wasm_i64x2_sub(itA, actA);
    itB = wasm_i64x2_sub(itB, actB);
    v128_t nzrA = wasm_f64x2_add(wasm_f64x2_sub(zr2A, zi2A), crA);
    v128_t nziA = wasm_f64x2_add(
        wasm_f64x2_mul(wasm_f64x2_mul(two, zrA), ziA), civ);
    v128_t nzrB = wasm_f64x2_add(wasm_f64x2_sub(zr2B, zi2B), crB);
    v128_t nziB = wasm_f64x2_add(
        wasm_f64x2_mul(wasm_f64x2_mul(two, zrB), ziB), civ);
    zrA = wasm_v128_bitselect(nzrA, zrA, actA);
    ziA = wasm_v128_bitselect(nziA, ziA, actA);
    zrB = wasm_v128_bitselect(nzrB, zrB, actB);
    ziB = wasm_v128_bitselect(nziB, ziB, actB);
  }
  out[0] = (int32_t)wasm_i64x2_extract_lane(itA, 0);
  out[1] = (int32_t)wasm_i64x2_extract_lane(itA, 1);
  out[2] = (int32_t)wasm_i64x2_extract_lane(itB, 0);
  out[3] = (int32_t)wasm_i64x2_extract_lane(itB, 1);
}
#endif

/* --- true HVM2 execution ---
 * Runs the Bend-compiled book itself (interaction-net reduction), not
 * the C fast path above. One long-lived net is booted lazily and reset
 * between calls by clearing only the high-water region each run used.
 * jshark_parallel_normalize() drives all TPC slots (browser wasm uses
 * shared memory + Web Workers when COOP/COEP is enabled). */
static Book* jshark_hvm2_book = NULL;
static Net* jshark_hvm2_net = NULL;
static int jshark_hvm2_last_k = 0;
static Book jshark_hvm2_book_storage;
static Net jshark_hvm2_net_storage;

static int jshark_hvm2_boot(void) {
  if (jshark_hvm2_book) { return jshark_hvm2_net != NULL; }
  alloc_static_tms();
  jshark_hvm2_book = &jshark_hvm2_book_storage;
  memset(jshark_hvm2_book, 0, sizeof(Book));
  if (!book_load(jshark_hvm2_book, (u32*)BOOK_BUF)) {
    jshark_hvm2_last_k = -11;
    jshark_hvm2_book = NULL;
    return 0;
  }
  jshark_hvm2_net = &jshark_hvm2_net_storage;
  memset(jshark_hvm2_net, 0, sizeof(Net));
  return 1;
}

static u32 jshark_hvm2_def_id(const char* name) {
  Book* book = jshark_hvm2_book;
  if (!book) { return 0xFFFFFFFF; }
  for (u32 i = 0; i < 32; ++i) {
    const char* dn = book->defs_buf[i].name;
    if (dn[0] == 0) { continue; }
    u32 j = 0;
    while (name[j] != 0 && dn[j] != 0 && name[j] == dn[j]) { j++; }
    if (name[j] == 0 && dn[j] == 0) { return i; }
  }
  return 0xFFFFFFFF;
}

static void jshark_hvm2_reset(void) {
  Net* net = jshark_hvm2_net;
  /* HVM alloc is tid*(G_NODE_LEN/TPC) + nput%part — clear each slice. */
  u32 part = G_NODE_LEN / TPC;
  if (part == 0) { part = G_NODE_LEN; }
  for (u32 ti = 0; ti < TPC; ++ti) {
    u32 n = tm[ti]->nput + 16;
    u32 v = tm[ti]->vput + 16;
    if (n > part) { n = part; }
    if (v > part) { v = part; }
    memset((void*)(net->node_buf + (u64)ti * part), 0,
        sizeof(net->node_buf[0]) * (u64)n);
    memset((void*)(net->vars_buf + (u64)ti * part), 0,
        sizeof(net->vars_buf[0]) * (u64)v);
  }
  memset((void*)net->rbag_buf, 0, sizeof(net->rbag_buf));
  vars_create(net, get_val(ROOT), 0);
  atomic_store(&net->itrs, 0);
  atomic_store(&net->idle, 0);
  for (u32 ti = 0; ti < TPC; ++ti) {
    TM* t = tm[ti];
    t->itrs = 0;
    t->nput = 1;
    t->vput = 1;
    t->rput = 0;
    t->hput = 0;
    t->sidx = 0;
  }
}

/* Applies an 8-ary def to f24 args and normalizes:
 *   @def ~ (a0 (a1 ... (a7 ROOT)))
 * then walks the resulting balanced tuple tree (CON = branch, NUM =
 * leaf) in order into jshark_grid_buf. Returns leaves written, or -1. */
__attribute__((import_module("jshark"), import_name("spawn_eval")))
void jshark_import_spawn_eval(u32 tid, u32 net_ptr, u32 book_ptr);

__attribute__((import_module("jshark"), import_name("wait_evals")))
void jshark_import_wait_evals(u32 count);

__attribute__((import_module("jshark"), import_name("eval_done")))
void jshark_import_eval_done(void);

__attribute__((import_module("jshark"), import_name("reset_evals")))
void jshark_import_reset_evals(void);

__attribute__((import_module("jshark"), import_name("live_threads")))
u32 jshark_import_live_threads(void);

static void jshark_parallel_normalize(Net* net, Book* book, u32 budget);

/* Do not call HVM evaluator(): it resets idle and sync_threads()
 * for all TPC slots, so a late worker deadlocks the barrier. */
static volatile u32 jshark_eval_cancel = 0;
static u32 jshark_shared_budget = 0;

static int jshark_bags_empty(Net* net) {
  u32 ti;
  for (ti = 0; ti < TPC; ++ti) {
    if (rbag_len(net, tm[ti]) > 0) { return 0; }
  }
  return 1;
}

static void jshark_steal_eval(Net* net, TM* t, Book* book, u32 budget) {
  u32 miss = 0;
  u32 part = G_RBAG_LEN / TPC;
  if (part == 0) { part = 1; }
  while (budget-- > 0) {
    Pair got;
    u32 sid;
    u32 idx;
    if (jshark_eval_cancel) { return; }
    if (rbag_len(net, t) > 0) {
      miss = 0;
      if (!interact(net, t, book)) {
        jshark_hvm2_last_k = -13;
        return;
      }
      continue;
    }
    sid = (t->tid + 1) % TPC;
    idx = sid * part + (t->sidx % part);
    t->sidx++;
    got = atomic_exchange_explicit(
        &net->rbag_buf[idx], 0, memory_order_relaxed);
    if (got != 0) {
      push_redex(net, t, got);
      miss = 0;
      continue;
    }
    miss++;
    if (miss >= part * TPC && jshark_bags_empty(net)) { return; }
    if (miss >= part * TPC) { miss = 0; }
  }
  jshark_hvm2_last_k = -14;
}

static void jshark_wasm_normalize(Net* net, Book* book, u32 budget) {
  jshark_steal_eval(net, tm[0], book, budget);
}

static u32 jshark_norm_budget(int cells) {
  u32 cap = (u32)(cells > 0 ? cells : 0);
  u32 b = cap * 8192u + 65536u;
  if (b < 200000u) { b = 200000u; }
  if (b > 50000000u) { b = 50000000u; }
  return b;
}

/* One net per frame. 4096 leaves matches Bend main's 64×64 tree;
 * larger canvases downsample, then nearest-neighbor expand so the
 * JS blit contract (bxN×byN) stays unchanged. */
#define JSHARK_HVM2_MAX_CELLS 4096
static int32_t jshark_hvm2_scratch[JSHARK_HVM2_MAX_CELLS];

static void jshark_hvm2_fit_grid(int nx, int ny, int* fnx, int* fny) {
  int x = nx;
  int y = ny;
  while ((long)x * (long)y > JSHARK_HVM2_MAX_CELLS && (x > 1 || y > 1)) {
    if (x > 1) { x = (x * 7) / 8; if (x < 1) { x = 1; } }
    if (y > 1) { y = (y * 7) / 8; if (y < 1) { y = 1; } }
  }
  while ((long)(x + 1) * (long)y <= JSHARK_HVM2_MAX_CELLS && x < nx) {
    x++;
  }
  while ((long)x * (long)(y + 1) <= JSHARK_HVM2_MAX_CELLS && y < ny) {
    y++;
  }
  *fnx = x;
  *fny = y;
}

static void jshark_hvm2_upsample(int fnx, int fny, int nx, int ny) {
  int n;
  int i;
  int by;
  if (fnx == nx && fny == ny) { return; }
  n = fnx * fny;
  for (i = 0; i < n; i++) {
    jshark_hvm2_scratch[i] = jshark_grid_buf[i];
  }
  for (by = 0; by < ny; by++) {
    int sy = by * fny / ny;
    int bx;
    for (bx = 0; bx < nx; bx++) {
      int sx = bx * fnx / nx;
      jshark_grid_buf[by * nx + bx] = jshark_hvm2_scratch[sy * fnx + sx];
    }
  }
}

static int jshark_hvm2_run_grid(u32 fid, const double* args, int cap) {
  Net* net = jshark_hvm2_net;
  jshark_hvm2_last_k = 0;
  jshark_hvm2_reset();
  vars_create(net, get_val(ROOT), NONE);
  for (int i = 0; i < 8; ++i) {
    Port cont = (i == 7) ? ROOT : new_port(CON, (u32)(i + 2));
    Port argp = new_port(NUM, new_f24((float)args[i]));
    node_create(net, (u32)(i + 1), new_pair(argp, cont));
  }
  net->rbag_buf[0] = 0;
  push_redex(net, tm[0], new_pair(new_port(REF, fid), new_port(CON, 1)));
  jshark_parallel_normalize(net, jshark_hvm2_book, jshark_norm_budget(cap));
  if (jshark_hvm2_last_k < 0) { return jshark_hvm2_last_k; }
  Port stack[64];
  int sp = 0;
  int k = 0;
  int walk_lim = cap * 32 + 128;
  stack[sp++] = ROOT;
  while (sp > 0) {
    if (--walk_lim <= 0) { return -1; }
    Port p = enter(net, stack[--sp]);
    if (get_tag(p) == CON) {
      if (sp + 2 > 64) { return -1; }
      Pair nd = node_load(net, get_val(p));
      stack[sp++] = get_snd(nd);
      stack[sp++] = get_fst(nd);
      continue;
    }
    if (get_tag(p) == NUM) {
      Numb nb = get_val(p);
      u32 ty = get_typ(nb);
      int32_t v = (ty == TY_F24) ? (int32_t)get_f24(nb)
                                 : (int32_t)get_u24(nb);
      if (k >= cap) { return -1; }
      jshark_grid_buf[k++] = v;
      continue;
    }
    return -1;
  }
  return k;
}

static void jshark_parallel_normalize(Net* net, Book* book, u32 budget) {
  u32 live = jshark_import_live_threads();
  u32 cap;
  u32 spawned = 0;
  u32 t;
  jshark_eval_cancel = 0;
  jshark_shared_budget = budget;
  if (live <= 1) {
    jshark_wasm_normalize(net, book, budget);
    return;
  }
  cap = live < (u32)TPC ? live : (u32)TPC;
  jshark_import_reset_evals();
  for (t = 1; t < cap; ++t) {
    jshark_import_spawn_eval(t, (u32)(uintptr_t)net, (u32)(uintptr_t)book);
    spawned++;
  }
  jshark_steal_eval(net, tm[0], book, budget);
  jshark_import_wait_evals(spawned);
}

__attribute__((export_name("jshark_worker_eval")))
void jshark_worker_eval(u32 tid, u32 net_ptr, u32 book_ptr) {
  jshark_steal_eval((Net*)(uintptr_t)net_ptr, tm[tid],
      (Book*)(uintptr_t)book_ptr, jshark_shared_budget);
  jshark_import_eval_done();
}

__attribute__((export_name("jshark_cancel_eval")))
void jshark_export_cancel_eval(void) { jshark_eval_cancel = 1; }

__attribute__((export_name("jshark_tpc")))
u32 jshark_export_tpc(void) { return (u32)TPC; }

__attribute__((export_name("mandel")))
jshark_hvm2_i64 jshark_export_mandel(jshark_hvm2_i64 a0, jshark_hvm2_i64 a1) {
  return jshark_mandel_iter(jshark_i64_to_f64(a0), jshark_i64_to_f64(a1));
}

__attribute__((export_name("mandel_f64")))
double jshark_export_mandel_f64(double a0, double a1) {
  return (double)jshark_mandel_iter(a0, a1);
}

__attribute__((export_name("mandel_grid")))
int32_t jshark_export_mandel_grid(double centerRe, double centerIm, double scale,
    double w, double h, double blk, double bxN, double byN) {
  int nx = (int)bxN;
  int ny = (int)byN;
  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) { return 0; }
  double half = blk * 0.5;
  double invW = 1.0 / w;
  double invH = 1.0 / h;
  double halfW = w * 0.5;
  double halfH = h * 0.5;
  for (int by = 0; by < ny; by++) {
    double ci = centerIm + ((double)by * blk + half - halfH) * scale * invH;
    int32_t *row = &jshark_grid_buf[by * nx];
    int bx = 0;
#ifdef __wasm_simd128__
    for (; bx + 3 < nx; bx += 4) {
      double cr0 = centerRe + ((double)bx * blk + half - halfW) * scale * invW;
      double cr1 =
          centerRe + ((double)(bx + 1) * blk + half - halfW) * scale * invW;
      double cr2 =
          centerRe + ((double)(bx + 2) * blk + half - halfW) * scale * invW;
      double cr3 =
          centerRe + ((double)(bx + 3) * blk + half - halfW) * scale * invW;
      jshark_mandel_quad(cr0, cr1, cr2, cr3, ci, &row[bx]);
    }
#endif
    for (; bx < nx; bx++) {
      double cr = centerRe + ((double)bx * blk + half - halfW) * scale * invW;
      row[bx] = (int32_t)jshark_mandel_iter(cr, ci);
    }
  }
  return (int32_t)(uintptr_t)jshark_grid_buf;
}

/* Same grid contract, but computed by HVM2 reducing the Bend-compiled
 * jshark_grid def: interaction-net execution end to end (f24 math). */
__attribute__((export_name("mandel_hvm2_grid")))
int32_t jshark_export_mandel_hvm2_grid(double centerRe, double centerIm, double scale,
    double w, double h, double blk, double bxN, double byN) {
  int nx = (int)bxN;
  int ny = (int)byN;
  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) {
    jshark_hvm2_last_k = -4;
    return 0;
  }
  if (!jshark_hvm2_boot()) {
    if (jshark_hvm2_last_k == 0) { jshark_hvm2_last_k = -1; }
    return 0;
  }
  u32 fid = jshark_hvm2_def_id("jshark_grid");
  if (fid == 0xFFFFFFFF) {
    jshark_hvm2_last_k = -2;
    return 0;
  }
  int fnx = 0;
  int fny = 0;
  int cells;
  int k;
  double blk2;
  double args[8];
  (void)blk;
  jshark_hvm2_fit_grid(nx, ny, &fnx, &fny);
  cells = fnx * fny;
  blk2 = w / (double)fnx;
  {
    double bh = h / (double)fny;
    if (bh < blk2) { blk2 = bh; }
  }
  args[0] = centerRe;
  args[1] = centerIm;
  args[2] = scale;
  args[3] = w;
  args[4] = h;
  args[5] = blk2;
  args[6] = (double)fnx;
  args[7] = (double)fny;
  k = jshark_hvm2_run_grid(fid, args, cells);
  if (k != cells) {
    jshark_hvm2_last_k = k;
    return 0;
  }
  jshark_hvm2_upsample(fnx, fny, nx, ny);
  jshark_hvm2_last_k = nx * ny;
  return (int32_t)(uintptr_t)jshark_grid_buf;
}

__attribute__((export_name("jshark_hvm2_last_k")))
int32_t jshark_export_hvm2_last_k(void) {
  return (int32_t)jshark_hvm2_last_k;
}


