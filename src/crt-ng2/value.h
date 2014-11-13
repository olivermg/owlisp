#ifndef __VALUE_H
#define __VALUE_H

typedef union _value_t {
	int i;
	union _value_t (*p)(union _value_t*);
} value_t;

typedef value_t value_t_list[8];

#endif
