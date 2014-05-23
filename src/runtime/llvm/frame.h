#ifndef __FRAME_H
#define __FRAME_H 1

#include "types.h"

typedef struct _frame_t {
  struct _frame_t* parent;
  value_t* values[16];
} frame_t;

frame_t* new_frame( frame_t* parent );
frame_t* free_frame( frame_t* f );
void set_binding( frame_t* f, const int frameindex, const int varindex, value_t* value );
value_t* get_binding( const frame_t* f, const int frameindex, const int varindex );

#endif
