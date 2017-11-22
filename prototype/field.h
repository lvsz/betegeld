#ifndef _FIELD_H_
#define _FIELD_H_

#include <stdlib.h>
#include <time.h>

#include "window.h"
#include "types.h"
#include "defaults.h"

char *new_field();
char field_ref(char *, int, int);
void free_field(char *);

#endif /* _FIELD_H_ */

