#ifndef __PROCEDURE_H
#define __PROCEDURE_H 1

#include "environment.h"

typedef struct _procedure_t {
  char** parameters;
  int parameterscount;
  void (*code)();
  environment_t* environment;
} procedure_t;

procedure_t* new_procedure( char* parameters[], int parameterscount, void (*code)(), environment_t* environment );
void invoke_procedure( procedure_t* procedure );
void free_procedure( procedure_t* procedure );

#endif
