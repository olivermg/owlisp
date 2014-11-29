#include "math_basic.h"

Object* add_obj(Object* o1, Object* o2)
{
  // TODO: type safety
  Integer* i1 = (Integer*)o1;
  Integer* i2 = (Integer*)o2;

  return newint( i1->value + i2->value );
}
