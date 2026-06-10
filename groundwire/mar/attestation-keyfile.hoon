::  mar/attestation-keyfile.hoon
::
::  Mark for poking one's OWN attestation chain into %urb-watcher (the same
::  self-attestation packet format; see sur/self-attestation), e.g. from dojo:
::  :urb-watcher &attestation-keyfile [...]
::
/-  sa=self-attestation
|_  =self-attestation:sa
++  grab
  |%
  ++  noun  self-attestation:sa
  --
++  grow
  |%
  ++  noun  self-attestation
  --
++  grad  %noun
--
