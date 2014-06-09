#include <stdlib.h>
#include <stdio.h>

#include "types.h"

value_t* new_value_int( int val )
{
  value_t* new_val = calloc( 1, sizeof( value_t ) );
  new_val->type = TYPE_INT;
  new_val->value = val;

  return new_val;
}

void free_value( value_t* value )
{
  free( value );
}

unsigned char values_equal( const value_t* value1, const value_t* value2 )
{
  unsigned char equal = 0;

  if ( value1->type == value2->type ) { // TODO: for now, do only strong typing
    equal = value1->value == value2->value;
  }

  return equal;
}

void dump_value( value_t* value )
{
  if ( value ) {
    fprintf( stderr, "(p%p,t%d,v%d)\n", value, value->type, value->value );
  } else {
    fprintf( stderr, "(undef)\n" );
  }
}
