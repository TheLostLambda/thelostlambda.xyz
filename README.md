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