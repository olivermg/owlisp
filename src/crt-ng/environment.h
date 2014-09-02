#ifndef __ENVIRONMENT_H
#define __ENVIRONMENT_H 1

#include "binding.h"

typedef struct _environment_t {
  struct _environment_t* parent;
  binding_t bindings[];
  int bindingscount;
} environment_t;

environment_t* new_environment( environment_t* parent, binding_t bindings[], int bindingscount );
void free_environment( environment_t* environment );
void free_all_environments( environment_t* environment );

#endif
