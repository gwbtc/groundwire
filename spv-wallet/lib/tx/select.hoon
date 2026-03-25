::  select.hoon - UTXO selection algorithms
::
::  Provides strategies for automatically selecting UTXOs to fund a transaction.
::  All selectors return ~ if insufficient funds, or a list of selected UTXOs.
::
/+  fees=tx-fees
|%
::  Minimal UTXO type for selection
::
+$  selectable
  $:  txid=@t
      vout=@ud
      amount=@ud
      =spend:fees
  ==
::  +calculate-selection-fee: Calculate fee for a set of inputs
::
::  Used internally by selection algorithms to calculate running fee.
::
++  calculate-selection-fee
  |=  [inputs=(list selectable) output-vbytes=@ud fee-rate=@ud]
  ^-  @ud
  ::  Sum input vbytes
  ::
  =/  input-vbytes=@ud
    %+  roll  inputs
    |=  [in=selectable sum=@ud]
    (add sum (input-vbytes:fees spend.in))
  ::  Calculate total vbytes
  ::
  =/  vbytes=@ud
    ;:  add
      overhead-vbytes:fees
      input-vbytes
      output-vbytes
    ==
  ::  Calculate fee
  ::
  (calculate-fee:fees vbytes fee-rate)
::  +accumulate: Core accumulator - picks from front until target met
::
::  Takes a pre-ordered list and selects from the front until target + fee
::  is covered. Returns ~ if insufficient funds.
::
++  accumulate
  |=  [ordered=(list selectable) target=@ud output-vbytes=@ud fee-rate=@ud]
  ^-  (unit (list selectable))
  ::  No UTXOs available
  ::
  ?:  =(~ ordered)  ~
  ::  Initialize state
  ::
  =|  selected=(list selectable)
  =|  total=@ud
  ::  Accumulate until target met
  ::
  |-
  ::  Calculate fee based on current selection
  ::
  =/  fee=@ud     (calculate-selection-fee selected output-vbytes fee-rate)
  =/  needed=@ud  (add target fee)
  ::  Success: have enough
  ::
  ?:  (gte total needed)
    `selected
  ::  Failure: nothing left to pick
  ::
  ?:  =(~ ordered)  ~
  ::  Pick next UTXO from front
  ::
  =/  pick=selectable  (snag 0 ordered)
  ::  Add to selection and continue
  ::
  %=  $
    selected  (snoc selected pick)
    total     (add total amount.pick)
    ordered   (slag 1 ordered)
  ==
::  +shuffle: Randomly reorder a list (Fisher-Yates)
::
++  shuffle
  |=  [l=(list selectable) eny=@uvJ]
  ^-  (list selectable)
  ::  Initialize RNG and state
  ::
  =/  rng  ~(. og eny)
  =/  remaining=(list selectable)  l
  =/  result=(list selectable)  ~
  ::  Pick random elements until none remain
  ::
  |-  ^-  (list selectable)
  ?~  remaining  result
  ::  Pick random index and extract element
  ::
  =^  idx  rng  (rads:rng (lent remaining))
  =/  pick=selectable
    (snag idx `(list selectable)`remaining)
  =/  new-remaining=(list selectable)
    (oust [idx 1] `(list selectable)`remaining)
  ::  Add to result and continue
  ::
  $(result (snoc result pick), remaining new-remaining)
::  +random: Random selection - shuffle and accumulate
::
++  random
  |=  [utxos=(list selectable) target=@ud output-vbytes=@ud fee-rate=@ud eny=@uvJ]
  ^-  (unit (list selectable))
  (accumulate (shuffle utxos eny) target output-vbytes fee-rate)
::  +largest-first: Select largest UTXOs first
::
++  largest-first
  |=  [utxos=(list selectable) target=@ud output-vbytes=@ud fee-rate=@ud]
  ^-  (unit (list selectable))
  =/  sorted=(list selectable)
    %+  sort  utxos
    |=([a=selectable b=selectable] (gth amount.a amount.b))
  (accumulate sorted target output-vbytes fee-rate)
::  +smallest-first: Select smallest UTXOs first (consolidation)
::
++  smallest-first
  |=  [utxos=(list selectable) target=@ud output-vbytes=@ud fee-rate=@ud]
  ^-  (unit (list selectable))
  =/  sorted=(list selectable)
    %+  sort  utxos
    |=([a=selectable b=selectable] (lth amount.a amount.b))
  (accumulate sorted target output-vbytes fee-rate)
--
