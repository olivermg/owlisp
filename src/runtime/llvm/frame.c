#include <stdlib.h>

#include "frame.h"

struct frame* new_frame( struct frame* parent )
{
	struct frame* newframe = calloc( 1, sizeof( struct frame ) );
	newframe->parent = parent;

	return newframe;
}

struct frame* delete_frame( struct frame* theframe )
{
	struct frame* parent = NULL;

	if ( theframe ) {
		parent = theframe->parent;
		free( theframe );
	}

	return parent;
}

