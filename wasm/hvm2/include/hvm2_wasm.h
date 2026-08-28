/* Optional WASM shims when targeting freestanding; unused on wasm32-wasi. */
#pragma once

#ifndef PRIu64
#define PRIu64 "llu"
#endif
