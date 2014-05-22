#ifndef __FRAME_H
#define __FRAME_H 1

struct frame {
  struct frame* parent;
  int values[16];
};

struct frame* new_frame( struct frame* parent );
struct frame* delete_frame( struct frame* f );
void set_binding( struct frame* f, const int frameindex, const int varindex, const int value );
int get_binding( struct frame* f, const int frameindex, const int varindex );

#endif
