#include <owlisp/list.h>

Object* car_obj(Object* list)
{
  // TODO: type safety

  return ((List*)list)->value;
}

Object* cdr_obj(Object* list)
{
  // TODO: type safety

  return (Object*)((List*)list)->next;
}
