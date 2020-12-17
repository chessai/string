#include <stdint.h>

void
text_encode_utf8
  ( uint8_t **destp
  , const uint16_t *src
  , size_t srcoff
  , size_t srclen
  );
