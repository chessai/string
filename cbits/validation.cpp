#include <simdjson.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
#endif
bool
run_utf8_validation
  ( const char * v
  , uint64_t off
  , uint64_t len
  )
{
  return simdjson::validate_utf8(v + off, len);
}
