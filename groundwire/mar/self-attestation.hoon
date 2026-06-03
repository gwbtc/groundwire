::  mar/self-attestation.hoon
::
::  Mark for poking %urb-handler with a confidential-comet self-attestation
::  packet, e.g. from dojo: :urb-handler &self-attestation [...]
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
