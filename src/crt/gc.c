#include "gc.h"

void* gc_malloc( size_t size )
{
  return malloc( size );
}

void* gc_calloc( size_t n, size_t size )
{
  return calloc( n, size );
}

void* gc_realloc( void* ptr, size_t size )
{
  return realloc( ptr, size );
}

void gc_free( void* ptr )
{
  free( ptr );
}
