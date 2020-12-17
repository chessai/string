#include <simdjson.h>
#include <stdint.h>

extern "C" bool run_utf8_validation
  ( const char * v
  , uint64_t off
  , uint64_t len
  )
{
  return simdjson::validate_utf8(v + off, len);
}
