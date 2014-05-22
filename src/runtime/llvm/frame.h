#ifndef __FRAME_H
#define __FRAME_H 1

#include "types.h"

struct frame {
  struct frame* parent;
  value_t values[16];
};

struct frame* new_frame( struct frame* parent );
struct frame* delete_frame( struct frame* f );
void set_binding( struct frame* f, const int frameindex, const int varindex, struct value_t value );
struct value_t get_binding( struct frame* f, const int frameindex, const int varindex );

#endif
