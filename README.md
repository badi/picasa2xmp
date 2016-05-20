# picasa2xmp
Write Picasa metadata to XMP for import into other image management programs

This came about when I needed to migrate from Google's [Picasa] to an alternative.
The challenge was that Picasa had its own way of storing image metadata that was could not be imported by other image collection software.
After some research, I discovered that [xmp] is a standard way of encoding the metadata directly into the image files.

This tool (`picasa2xmp`) thus reads the image metadata stored by Picasa and writes them as xmp tags into the files.
As a result, I was able to migrate my collection with minimal effort.

# Requirements

- [Haskell (GHC) + Stack](http://docs.haskellstack.org/en/stable/README/)

# Tested on

I've only tested this on my system, but the xmp Wikipedia page provides a [list of software][xmp supported] that "should" be supported.
I ended using [Adobe Lightroom].
Your mileage may vary.

Tested with GHC 7.10


# Licensing

This software is made available under the BSD3 license.
See the LICENSE file.

[picasa]: https://en.wikipedia.org/wiki/Picasa
[xmp]: https://en.wikipedia.org/wiki/Extensible_Metadata_Platform
[Adobe Lightroom]: https://en.wikipedia.org/wiki/Adobe_Photoshop_Lightroom
[xmp supported]: https://en.wikipedia.org/wiki/Extensible_Metadata_Platform#Support_and_acceptance
