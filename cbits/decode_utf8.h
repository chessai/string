/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <stdint.h>
#include <stddef.h>

const uint8_t *
  ( uint16_t *const dest
  , size_t *destoff
  , const uint8_t *src
  , const uint8_t *const srcend
  );

