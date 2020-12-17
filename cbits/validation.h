#ifndef __VALIDATION_H__
#define __VALIDATION_H__

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
#endif
bool run_utf8_validation
  ( uint8_t * v
  , uint64_t off
  , uint64_t len
  );

#endif /* __VALIDATION_H__ */
