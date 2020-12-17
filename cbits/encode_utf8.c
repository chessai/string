/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <stdint.h>
#include <string.h>
#include "encode_utf8.h"

void
text_encode_utf8
  ( uint8_t **destp
  , const uint16_t *src
  , size_t srcoff
  , size_t srclen
  )
{
  const uint16_t *srcend;
  uint8_t *dest = *destp;

  src += srcoff;
  srcend = src + srclen;

 ascii:
#if defined(__x86_64__)
  while (srcend - src >= 4) {
    uint64_t w = *((uint64_t *) src);

    if (w & 0xFF80FF80FF80FF80ULL) {
      if (!(w & 0x000000000000FF80ULL)) {
        *dest++ = w & 0xFFFF;
        src++;
        if (!(w & 0x00000000FF800000ULL)) {
          *dest++ = (w >> 16) & 0xFFFF;
          src++;
          if (!(w & 0x0000FF8000000000ULL)) {
            *dest++ = (w >> 32) & 0xFFFF;
            src++;
          }
        }
      }
      break;
    }
    *dest++ = w & 0xFFFF;
    *dest++ = (w >> 16) & 0xFFFF;
    *dest++ = (w >> 32) & 0xFFFF;
    *dest++ = w >> 48;
    src += 4;
  }
#endif

#if defined(__i386__)
  while (srcend - src >= 2) {
    uint32_t w = *((uint32_t *) src);

    if (w & 0xFF80FF80)
      break;
    *dest++ = w & 0xFFFF;
    *dest++ = w >> 16;
    src += 2;
  }
#endif

  while (src < srcend) {
    uint16_t w = *src++;

    if (w <= 0x7F) {
      *dest++ = w;
      /* An ASCII byte is likely to begin a run of ASCII bytes.
         Falling back into the fast path really helps performance. */
      goto ascii;
    }
    else if (w <= 0x7FF) {
      *dest++ = (w >> 6) | 0xC0;
      *dest++ = (w & 0x3f) | 0x80;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      *dest++ = (w >> 12) | 0xE0;
      *dest++ = ((w >> 6) & 0x3F) | 0x80;
      *dest++ = (w & 0x3F) | 0x80;
    } else {
      uint32_t c = ((((uint32_t) w) - 0xD800) << 10) +
        (((uint32_t) *src++) - 0xDC00) + 0x10000;
      *dest++ = (c >> 18) | 0xF0;
      *dest++ = ((c >> 12) & 0x3F) | 0x80;
      *dest++ = ((c >> 6) & 0x3F) | 0x80;
      *dest++ = (c & 0x3F) | 0x80;
    }
  }

  *destp = dest;
}
