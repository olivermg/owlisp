#include <stdlib.h>

#include "frame.h"

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

static struct frame* find_frame( struct frame* f, int frameindex )
{
  struct frame* found = f;

  for ( int i = 0; i < frameindex; i++ ) {
    if ( NULL != found ) {
      found = found->parent;
    } else {
      // TODO: throw some error, since requested frame does not exist
    }
  }

  return found;
}

void set_binding( struct frame* f, int frameindex, int varindex, int value )
{
  struct frame* correctframe = find_frame( f, frameindex );

  if ( varindex >= 0 && varindex < sizeof( correctframe->values ) ) {
    correctframe->values[varindex] = value;
  } else {
    // TODO: invalid address - throw some error in this case
  }
}

int get_binding( struct frame* f, int frameindex, int varindex )
{
  int value = 0;

  struct frame* correctframe = find_frame( f, frameindex );

  if ( varindex >= 0 && varindex < sizeof( correctframe->values ) ) {
    value = correctframe->values[varindex];
  } else {
    // TODO: throw error
  }

  return value;
}
