# The Rehasking of thelostlambda.xyz
This is a full rewrite of the project in low-level Haskell.
The aim is to build a simpler program that is easier to extend and maintain. The
first goal is just to reach feature pairity with the Phoenix version. After we
get to that point, support for blogging, photo upload, and an improved interface
are targets.

## Comments
As the purpose for this rewrite is largely didactic, all code should be
extensively commented.

## Libraries & Stack
  - Stack
  - GHC
  - network (Network.Socket)
  - bytestring (Data.ByteString)
  - directory (System.Directory)

## TODO
  - Markdown to HTML for writing blog posts. The Markdown will be stored, and
  then converted and wrapped when the blog page is requested.
  - Comment everything better

## Misc
I have no better place to put this, so for the time being, here is the command I've been using to create thumbnails for the full size photos on the photos page:```
mogrify -path ./thumb -sampling-factor 4:2:0 -strip -quality 85 -interlace Plane -colorspace RGB -thumbnail 100000@\> *.JPG
```
