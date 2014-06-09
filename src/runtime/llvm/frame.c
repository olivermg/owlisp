#include <stdlib.h>
#include <stdio.h>

#include "frame.h"
#include "types.h"
#include "error.h"

frame_t* new_frame( frame_t* parent )
{
  frame_t* newframe = calloc( 1, sizeof( frame_t ) );
  newframe->parent = parent;

  return newframe;
}

frame_t* free_frame( frame_t* f )
{
  frame_t* parent = NULL;

  if ( f ) {
    parent = f->parent;
    free( f );
  }

  return parent;
}

static frame_t* find_frame( const frame_t* f, const int frameindex )
{
  frame_t* found = (frame_t*)f;

  for ( int i = 0; i < frameindex; i++ ) {
    found = found->parent;

    if ( NULL == found ) {
      fatal_error( "frame not found" );
    }
  }

  return found;
}

void set_binding( frame_t* f, const int frameindex, const int varindex, value_t* value )
{
  frame_t* correctframe = find_frame( f, frameindex );

  if ( varindex < 0 && varindex >= sizeof( correctframe->values ) ) {
    fatal_error( "binding not found in frame" );
  }

  correctframe->values[varindex] = value;
}

value_t* get_binding( const frame_t* f, const int frameindex, const int varindex )
{
  frame_t* correctframe = find_frame( f, frameindex );

  if ( varindex < 0 && varindex >= sizeof( correctframe->values ) ) {
    fatal_error( "binding not found in frame" );
  }

  return correctframe->values[varindex];
}

void dump_frame( const frame_t* f )
{
  if ( f ) {
    fprintf( stderr, "FRAME: %p\n", f );
    for ( int i = 0; i < MAX_FRAME_VALUES; i++ ) {
      dump_value( f->values[i] );
    }
    dump_frame( f->parent );
  }
}
