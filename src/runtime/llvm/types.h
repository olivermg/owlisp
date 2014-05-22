#ifndef __TYPES_H
#define __TYPES_H 1

typedef short type_t;

#define TYPE_INT      0x01
#define TYPE_FLOAT    0x02
#define TYPE_STRING   0x03
#define TYPE_CONS     0x04
#define TYPE_PATH     0x05
#define TYPE_ARRAY    0x06

struct value_t {
  type_t type;
  int value;
};

struct value_t* new_value( int val );

#endif
