#include <stdlib.h>
#include <stdio.h>

#include "error.h"
#include "types.h"

value_t* new_value( type_t type, union value_container_t val )
{
  value_t* new_val = calloc( 1, sizeof( value_t ) );
  new_val->type = type;
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
    switch ( value1->type ) {
    case TYPE_NUMBER:
      equal = value1->value.number == value2->value.number;
      break;
    case TYPE_CHARACTER:
      break;
    case TYPE_SYMBOL:
      break;
    case TYPE_LIST:
      break;
    case TYPE_ARRAY:
      break;
    case TYPE_HASHTABLE:
      break;
    case TYPE_READTABLE:
      break;
    case TYPE_PACKAGE:
      break;
    case TYPE_PATHNAME:
      break;
    case TYPE_STREAM:
      break;
    case TYPE_RANDOMSTATE:
      break;
    case TYPE_STRUCTURE:
      break;
    case TYPE_FUNCTION:
      break;
    case TYPE_CONDITION:
      break;
    case TYPE_CLASS:
      break;
    case TYPE_METHOD:
      break;
    case TYPE_GENERICFUNCTION:
      break;
    default:
      fatal_error( "cannot compare values (unknown type)!" );
      break;
    }
  }

  return equal;
}

unsigned char is_value_true( const value_t* value )
{
  if ( value->value.number ) {
    return 1;
  } else {
    return 0;
  }
}

void dump_value( value_t* value )
{
  if ( value ) {
    fprintf( stderr, "(p%p,t%d,v%d)\n", value, value->type, value->value );
  } else {
    fprintf( stderr, "(undef)\n" );
  }
}
