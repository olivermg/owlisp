#ifndef __TRAMPOLINE_H
#define __TRAMPOLINE_H

#include <owlisp/classes.h>

#define CALL(fn,env) { trampoline_return_t _tr = { CALL, .call = { fn, env }; return _tr; }
#define RETURN(rv)   { trampoline_return_t _tr = { RETURN, .retval = rv }; return _tr; }

typedef enum {
  CALL,
  RETURN
} trampoline_action_t;

struct _trampoline_return_t {
  trampoline_action_t action;
  union {
    struct {
      proc_p fn;
      Env* env;
    } call;
    Object* retval;
  };
};

#endif
