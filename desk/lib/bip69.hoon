/-  psbt
|%
++  output-lte
  |=  [a=out:tx:psbt b=out:tx:psbt]
  ?.  =(value.a value.b)
    (lth value.a value.b)
  (lte dat.script-pubkey.a dat.script-pubkey.b)
::
++  input-lte
  |=  [a=in:tx:psbt b=in:tx:psbt]
  ?.  =(dat.txid.prevout.a dat.txid.prevout.b)
    (lth dat.txid.prevout.a dat.txid.prevout.b)
  (lte idx.prevout.a idx.prevout.b)
::
++  sort-outputs
  |=  os=(list out:tx:psbt)
  (sort os output-lte)
::
++  sort-inputs
  |=  is=(list in:tx:psbt)
  (sort is input-lte)
--
