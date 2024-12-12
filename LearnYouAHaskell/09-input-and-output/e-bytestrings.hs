-- due to their lazy nature and the myriad of functions defined to use them strings are very helpful when used as file streams and many other things.
-- however... its slow, due to its nature as a list of promises that must be acted upon right at the point they are needed

-- this isn't a problem exept in cases where we are reading big files and manipulating them...
-- enter ! bytestrings - a sort of list where each element is a byte

-- bytestrings can be strict or lazy

-- Data.ByteString - contains strict bytestrings with no laziness at all, so no infinite bytestrings and if you evaluate the first byte you hav to evaluate all of it.
-- no thunks but fills up memory as al read in at once

-- Data.ByteString.Lazy - contains lazy bytestrings, although they aren't as lazy as lists
-- in lists each element is a thunk, but in lazy bytestrings the bytes are split into 32 KiB chunks which then give a thunk of the next chunk.

-- consequently they can act like streams when reading or writing but just with 32 KiB chunks.
-- this is good because it doesn't fill the memory and the chunks

-- a lot of the functions defined in Data.List are defined for ByteStrings in Data.ByteString.Lazy with ByteString instead of [a] and Word8 instead of a.
-- because they function the same its good practice to bring bytestrings into a project with qualified import.

-- ! pack :: [Word8] -> ByteString - this takes a normal list and returns a bytestring making it less lazy

-- Word8 is literaly a byte long number 0-255 which can represent any data. 
-- numbers like 5, being polymorphic value constructors, can also be Word8's

-- B.pack [99,97,110] -> Chunk "can" Empty
-- numbers like 336 when cast to Word8's are also evaluated mod 255 so 336 -> 80

-- ! unpack :: ByteString -> [Word8] - this takes a bytestring and converts it into a list of bytes

-- ! fromChunks - takes a list of strict bytestrings and converts it to a lazy bytestring
-- ! toChunks - takes a lazy bytestring and converts it to a list of strict ones

-- B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.Pack [46,47,48]] -> Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
-- good for when you have a bunch of small strict bytestrings you want to process efficiently without making one large strict bytestring

-- ! cons - bytestring's :, note that its lazy and so will make a new chunk even if the header chunk of the list it is put in front of isn't full
-- ! cons' - the strict version that doesn't make a new chunk, better if inserting a lot of bytes at the beginning of a bytestring
-- ! empty - makes an empty bytestring

-- it has got IO actions implemented for it aswell!

-- almost anything implemented with strings can be swapped out with bytestrings and the right imports
-- consequently it can be a notable performance boost for string munching programs
-- only do this if you know that the performance is not what is desired as using strings is more convenient