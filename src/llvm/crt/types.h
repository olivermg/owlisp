#ifndef __TYPES_H
#define __TYPES_H 1

typedef unsigned char type_t;

#define TYPE_INT      0x01
#define TYPE_FLOAT    0x02
#define TYPE_STRING   0x03
#define TYPE_CONS     0x04
#define TYPE_PATH     0x05
#define TYPE_ARRAY    0x06

typedef struct _value_t {
  type_t type;
  int value;
} value_t;

value_t* new_value_int( int val );
void free_value( value_t* value );
unsigned char values_equal( const value_t* value1, const value_t* value2 );
unsigned char is_value_true( const value_t* value );
void dump_value( value_t* value );

#endif
