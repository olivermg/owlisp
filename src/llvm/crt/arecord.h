#ifndef __ARECORD_H
#define __ARECORD_H 1

#include "frame.h"

typedef struct _arecord_t {
  void* static_link;
  void* dynamic_link;
  frame_t* local_variables;
} arecord_t;

arecord_t* new_arecord();
void free_arecord( arecord_t* ar );
void set_static_link( arecord_t* ar, void* static_link );
void* get_static_link( arecord_t* ar );
void set_dynamic_link( arecord_t* ar, void* dynamic_link );
void* get_dynamic_link( arecord_t* ar );
void set_local_variables( arecord_t* ar, frame_t* local_variables );
frame_t* get_local_variables( arecord_t* ar );

#endif
