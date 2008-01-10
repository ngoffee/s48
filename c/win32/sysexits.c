/* Copyright (c) 1993-2008 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include "scheme48.h"

void
s48_init_sysexits(void)
{
  /* For SRFI 22, probably useless on Windows */
  s48_define_exported_binding("EX_SOFTWARE", s48_enter_integer(70L));
}
