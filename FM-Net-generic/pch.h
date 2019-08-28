#pragma once

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

#include <cstring>
#include <cassert>

#define u32 uint32_t

#if defined NDEBUG || defined _NDEBUG

#define myprint(...) ((void)0)
#define STATIC static
#define QUAL cfg::template

#else

#define myprint printf
#define STATIC
#define QUAL net.template

#endif
