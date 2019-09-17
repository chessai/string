--nextChar :: String -> Int -> (# Int#, Char# #)
--writeChar :: String -> Char ->

data String
  (Eq, Semigroup, Monoid)

  Folds(foldl,foldl',foldr,foldr',foldMap,foldMap')
  ConversionsFrom(fromAscii,fromUtf8,fromUtf16,fromUtf32
    ,fromAsciiLossy,fromUtf8Lossy,fromUtf16Lossy,fromUtf32Lossy
    ,fromByteString,fromLazyByteString,fromText
    ,fromByteArray,fromPrimArray)
  ConversionsTo(toAscii,toUtf8,toUtf16,toUtf32
    ,toByteString,toLazyByteString,toText
    ,toByteArray,toPrimArray)
  Length(byteLength,codepointLength,graphemeLength,null)

  -- Convert a 'String' to a 'Slice', without copying.
  toSlice :: String -> Int -> Int -> Maybe Slice

  -- Convert a 'String' to a 'Slice', without copying.
  unsafeToSlice :: String -> Int -> Int -> Slice

  -- Apply a function that acts on slices to a string, without copying.
  asSlice :: String -> (Slice -> a) -> a

  -- Apply a function that acts on bytearrays to a string.
  asBytes :: String -> (ByteArray -> a) -> a

  -- Copy the string to a mutable buffer and apply the function.
  asMutableString :: String -> (MutableString s -> m a) -> m a

  -- Copy the string to a mutable sliced buffer and apply the function.
  asMutableSlice :: String -> (MutableSlice s -> m a) -> m a

  -- Copy the string to a mutable bytearray and apply the function.
  asMutableBytes :: String -> (MutableByteArray s -> m a) -> m a


data MutableString s

data Slice
  (Eq, Semigroup, Monoid)

  Folds(foldl,foldl',foldr,foldr',foldMap,foldMap')
  ConversionsFrom(fromAscii,fromUtf8,fromUtf16,fromUtf32
    ,fromAsciiLossy,fromUtf8Lossy,fromUtf16Lossy,fromUtf32Lossy
    ,fromByteString,fromLazyByteString,fromText
    ,fromByteArray,fromPrimArray)
  ConversionsTo(toAscii,toUtf8,toUtf16,toUtf32
    ,toByteString,toLazyByteString,toText
    ,toByteArray,toPrimArray)

data MutableSlice s

data Builder
  (Semigroup, Monoid)

  build :: Builder -> String

  Builders(word{|8|16|32|64},int{|8|16|32|64},char,float,double
    ,bytearray,fixed)

