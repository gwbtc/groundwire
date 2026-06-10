::  mar/self-attestation.hoon
::
::  Mark for poking %urb-watcher with a confidential-comet self-attestation
::  packet (a remote ship's, relayed by Ames), e.g. from dojo:
::  :urb-watcher &self-attestation [...]
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
