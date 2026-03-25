::  draft.hoon - Draft transaction types and utilities
::
::  Types and functions for working with draft transactions before signing.
::  Handles fee estimation, address type detection, and output building.
::
/+  fees=tx-fees, taproot
|%
::  Types
::
+$  utxo-input
  $:  txid=@t
      vout=@ud
      amount=@ud
      =spend:fees
  ==
::
+$  output
  $:  address=@t
      amount=@ud
  ==
::
+$  change-config
  $:  fee-rate=@ud
      address=@t
  ==
::
+$  select-mode  ?(%random %largest-first)
::
+$  transaction
  $:  inputs=(list utxo-input)
      outputs=(list output)
      change=(unit change-config)
      auto-select=(unit select-mode)
      created=@da
      modified=@da
  ==
::  +sum-inputs: Sum total amount of inputs
::
++  sum-inputs
  |=  inputs=(list utxo-input)
  ^-  @ud
  %+  roll  inputs
  |=  [in=utxo-input sum=@ud]
  (add sum amount.in)
::  +sum-outputs: Sum total amount of outputs
::
++  sum-outputs
  |=  outputs=(list output)
  ^-  @ud
  %+  roll  outputs
  |=  [out=output sum=@ud]
  (add sum amount.out)
::  +calculate-vbytes: Calculate vbytes for a draft transaction
::
::  Sums overhead + inputs + outputs + change (if configured).
::
++  calculate-vbytes
  |=  draft=transaction
  ^-  @ud
  ::  Sum input vbytes based on spend type
  ::
  =/  inputs-vb=@ud
    %+  roll  inputs.draft
    |=  [in=utxo-input sum=@ud]
    (add sum (input-vbytes:fees spend.in))
  ::  Sum output vbytes based on address type
  ::
  =/  outputs-vb=@ud
    %+  roll  outputs.draft
    |=  [out=output sum=@ud]
    (add sum (output-vbytes:fees (address-to-spend address.out)))
  ::  Add change output if configured
  ::
  =/  change-vb=@ud
    ?~  change.draft  0
    (output-vbytes:fees (address-to-spend address.u.change.draft))
  ::  Total: overhead + inputs + outputs + change
  ::
  ;:  add
    overhead-vbytes:fees
    inputs-vb
    outputs-vb
    change-vb
  ==
::  Address format detection
::
++  is-p2wpkh-address
  |=  addr=@t
  ^-  ?
  =/  prefix=tape  (scag 6 (trip addr))
  ?|  =("bc1q" (scag 4 prefix))
      =("tb1q" (scag 4 prefix))
      =("bcrt1q" prefix)
  ==
::
++  is-p2sh-address
  |=  addr=@t
  ^-  ?
  =/  first=tape  (scag 1 (trip addr))
  ?|  =("3" first)
      =("2" first)
  ==
::
++  is-p2pkh-address
  |=  addr=@t
  ^-  ?
  =/  first=tape  (scag 1 (trip addr))
  ?|  =("1" first)
      =("m" first)
      =("n" first)
  ==
::
++  address-to-spend
  |=  addr=@t
  ^-  spend:fees
  ?:  (is-taproot-address:taproot addr)  %p2tr
  ?:  (is-p2wpkh-address addr)           %p2wpkh
  ?:  (is-p2sh-address addr)             %p2sh-p2wpkh
  ?>  (is-p2pkh-address addr)            %p2pkh
::  +incorporate-change: Add change output to draft if applicable
::
::  Crashes if insufficient funds (caller should validate first).
::  Absorbs dust change into fee silently.
::
++  incorporate-change
  |=  draft=transaction
  ^-  (list output)
  ::  No change configured - return outputs as-is
  ::
  ?~  change.draft
    outputs.draft
  ::  Sum up inputs and outputs
  ::
  =/  total-inputs=@ud   (sum-inputs inputs.draft)
  =/  total-outputs=@ud  (sum-outputs outputs.draft)
  ::  Calculate transaction size and fee
  ::
  =/  vbytes=@ud  (calculate-vbytes draft)
  =/  fee=@ud     (calculate-fee:fees vbytes fee-rate.u.change.draft)
  ::  Calculate change amount
  ::
  =/  result=change-result:fees
    (calculate-change-result:fees total-inputs total-outputs fee)
  ::  Handle result
  ::
  ?-  -.result
    %insufficient  !!                 ::  caller bug - should validate first
    %dust          outputs.draft      ::  absorb into fee
    %ok            (snoc outputs.draft [address.u.change.draft amount.result])
  ==
--
