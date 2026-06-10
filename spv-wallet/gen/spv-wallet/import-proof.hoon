::  :spv-wallet|import-proof +/path/to/proof.json
::
::  Ingest a confidential-comet proof.json emitted by Desktop Causeway and
::  stash it in %spv-wallet's Ames self-attestation state.
::
::  Today this is a stub: the agent stores the raw atom; runtime code that
::  consumes proofs on peer contact is still pending. See
::  docs/CONFIDENTIAL-COMETS.md in the causeway/ package for the full proof
::  format and the runtime ingest TODO.
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [proof=@t ~]  ::  raw JSON bytes as a cord
        ~
    ==
=/  id=@ta  (cat 3 'cw_import_' (scot %uv eny))
[%fiber-poke [id causeway-import-proof+!>(proof)]]
