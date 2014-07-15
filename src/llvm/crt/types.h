#ifndef __TYPES_H
#define __TYPES_H 1

typedef unsigned char type_t;

#define TYPE_NUMBER            0x01
#define TYPE_CHARACTER         0x02
#define TYPE_SYMBOL            0x03
#define TYPE_LIST              0x04
#define TYPE_ARRAY             0x05
#define TYPE_HASHTABLE         0x06
#define TYPE_READTABLE         0x07
#define TYPE_PACKAGE           0x08
#define TYPE_PATHNAME          0x09
#define TYPE_STREAM            0x10
#define TYPE_RANDOMSTATE       0x11
#define TYPE_STRUCTURE         0x12
#define TYPE_FUNCTION          0x13
#define TYPE_CONDITION         0x14
#define TYPE_CLASS             0x15
#define TYPE_METHOD            0x16
#define TYPE_GENERICFUNCTION   0x17

union value_container_t {
  long number;
  unsigned char* character;
  unsigned char* symbol;
  void* list;
  void* array;
  void* hashtable;
  void* readtable;
  unsigned char* package;
  unsigned char* pathname;
  long stream;
  long randomstate;
  void* structure;
  void* function;
  void* condition;
  void* class;
  void* method;
  void* genericfunction;
};

typedef struct _value_t {
  type_t type;
  union value_container_t value;
} value_t;

value_t* new_value( type_t type, union value_container_t val );
void free_value( value_t* value );
unsigned char values_equal( const value_t* value1, const value_t* value2 );
unsigned char is_value_true( const value_t* value );
void dump_value( value_t* value );

#endif
