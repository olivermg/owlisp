#ifndef __PROCEDURE_H
#define __PROCEDURE_H 1

#include "environment.h"

typedef struct _procedure_t {
  char* parameters[];
  void (code*)();
  environment_t environment;
} procedure_t;

#endif
