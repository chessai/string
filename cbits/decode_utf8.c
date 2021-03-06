/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <stdint.h>
#include <stddef.h>

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  /*
   * The first part of the table maps bytes to character classes that
   * to reduce the size of the transition table and create bitmasks.
   */
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  /*
   * The second part is a transition table that maps a combination of
   * a state of the automaton and a character class to a state.
   */
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

static inline uint32_t
decode
  ( uint32_t *state
  , uint32_t *codep
  , uint32_t byte
  )
{
  uint32_t type = utf8d[byte];

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  return *state = utf8d[256 + *state + type];
}

#if defined(__GNUC__) || defined(__clang__)
static inline uint8_t const *
text_decode_utf8_int
  ( uint16_t *const dest
  , size_t *destoff
  , const uint8_t **src
  , const uint8_t *srcend
  , uint32_t *codepoint0
  , uint32_t *state0
  ) __attribute((always_inline));
#endif

static inline uint8_t const *
text_decode_utf8_int
  ( uint16_t *const dest
  , size_t *destoff
  , const uint8_t **src
  , const uint8_t *srcend
  , uint32_t *codepoint0
  , uint32_t *state0
  )
{
  uint16_t *d = dest + *destoff;
  const uint8_t *s = *src, *last = *src;
  uint32_t state = *state0;
  uint32_t codepoint = *codepoint0;

  while (s < srcend) {
#if defined(__i386__) || defined(__x86_64__)
    /*
     * This code will only work on a little-endian system that
     * supports unaligned loads.
     *
     * It gives a substantial speed win on data that is purely or
     * partly ASCII (e.g. HTML), at only a slight cost on purely
     * non-ASCII text.
     */

    if (state == UTF8_ACCEPT) {
      while (s < srcend - 4) {
        codepoint = *((uint32_t *) s);
        if ((codepoint & 0x80808080) != 0)
          break;
        s += 4;

        /*
         * Tried 32-bit stores here, but the extra bit-twiddling
         * slowed the code down.
         */

        *d++ = (uint16_t) (codepoint & 0xff);
        *d++ = (uint16_t) ((codepoint >> 8) & 0xff);
        *d++ = (uint16_t) ((codepoint >> 16) & 0xff);
        *d++ = (uint16_t) ((codepoint >> 24) & 0xff);
      }
      last = s;
    }
#endif

    if (decode(&state, &codepoint, *s++) != UTF8_ACCEPT) {
      if (state != UTF8_REJECT)
        continue;
      break;
    }

    if (codepoint <= 0xffff)
      *d++ = (uint16_t) codepoint;
    else {
      *d++ = (uint16_t) (0xD7C0 + (codepoint >> 10));
      *d++ = (uint16_t) (0xDC00 + (codepoint & 0x3FF));
    }
    last = s;
  }

  *destoff = d - dest;
  *codepoint0 = codepoint;
  *state0 = state;
  *src = last;

  return s;
}

/*
 * Helper to decode buffer and discard final decoder state
 */
const uint8_t *
text_decode_utf8
  ( uint16_t *const dest
  , size_t *destoff
  , const uint8_t *src
  , const uint8_t *const srcend
  )
{
  uint32_t codepoint;
  uint32_t state = UTF8_ACCEPT;
  text_decode_utf8_int(dest, destoff, &src, srcend,
                          &codepoint, &state);
  return src;
}
