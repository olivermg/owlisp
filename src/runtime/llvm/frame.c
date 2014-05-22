#include <stdlib.h>

#include "frame.h"
#include "error.h"

struct frame* new_frame( struct frame* parent )
{
  struct frame* newframe = calloc( 1, sizeof( struct frame ) );
  newframe->parent = parent;

  return newframe;
}

struct frame* delete_frame( struct frame* f )
{
  struct frame* parent = NULL;

  if ( f ) {
    parent = f->parent;
    free( f );
  }

  return parent;
}

static struct frame* find_frame( struct frame* f, const int frameindex )
{
  struct frame* found = f;

  for ( int i = 0; i < frameindex; i++ ) {
    found = found->parent;

    if ( NULL == found ) {
      fatal_error( "frame not found" );
    }
  }

  return found;
}

void set_binding( struct frame* f, const int frameindex, const int varindex, const int value )
{
  struct frame* correctframe = find_frame( f, frameindex );

  if ( varindex < 0 && varindex >= sizeof( correctframe->values ) ) {
    fatal_error( "binding not found in frame" );
  }

  correctframe->values[varindex] = value;
}

int get_binding( struct frame* f, const int frameindex, const int varindex )
{
  struct frame* correctframe = find_frame( f, frameindex );

  if ( varindex < 0 && varindex >= sizeof( correctframe->values ) ) {
    fatal_error( "binding not found in frame" );
  }

  return correctframe->values[varindex];
}
