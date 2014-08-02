#ifndef __GC_H
#define __GC_H 1

#include <stdlib.h>

void* gc_malloc( size_t size );
void* gc_calloc( size_t n, size_t size );
void* gc_realloc( void* ptr, size_t size );
void gc_free( void* ptr );

#endif
