#ifndef __FRAME_H
#define __FRAME_H 1

#include "types.h"

#define MAX_FRAME_VALUES (16)

typedef struct _frame_t {
  struct _frame_t* parent;
  value_t* values[MAX_FRAME_VALUES];
} frame_t;

frame_t* new_frame( frame_t* parent );
frame_t* free_frame( frame_t* f );
void set_binding( frame_t* f, const int frameindex, const int varindex, value_t* value );
value_t* get_binding( const frame_t* f, const int frameindex, const int varindex );
void dump_frame( const frame_t* f );

#endif
