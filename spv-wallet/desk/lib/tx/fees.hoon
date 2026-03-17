::  fees.hoon - Bitcoin transaction fee estimation
::
::  Virtual byte (vbyte) constants and fee calculations.
::  Based on BIP-141 SegWit weight calculations.
::
|%
+$  spend  ?(%p2pkh %p2wpkh %p2sh-p2wpkh %p2tr)
::  Transaction overhead: version (4) + locktime (4) + marker/flag (2) +
::  ~1 varint
::
++  overhead-vbytes  11
::
::  Dust threshold in satoshis
::
::  Outputs below this are uneconomical to spend. ~3x the cost to spend
::  a P2WPKH input at 1 sat/vB (3 * 68 = 204).
::
++  dust-threshold  204
::
::  Input vbytes by script type
::  P2WPKH: 68, P2PKH: 148, P2TR: 58, P2SH-P2WPKH: 91
::
++  input-vbytes
  |=  =spend
  ^-  @ud
  ?-  spend
    %p2pkh        148
    %p2wpkh       68
    %p2sh-p2wpkh  91
    %p2tr         58
  ==
::
::  Output vbytes by script type
::  P2WPKH: 31, P2PKH: 34, P2TR: 43, P2SH-P2WPKH: 32
::
++  output-vbytes
  |=  =spend
  ^-  @ud
  ?-  spend
    %p2pkh        34
    %p2wpkh       31
    %p2sh-p2wpkh  32
    %p2tr         43
  ==
::  Calculate fee from vbytes and fee rate (sat/vB)
::
::  Hook for future fee policy: min/max bounds, RBF bumping, rounding, etc.
::
++  calculate-fee
  |=  [vbytes=@ud fee-rate=@ud]
  ^-  @ud
  (mul vbytes fee-rate)
::  Change calculation result
::
+$  change-result
  $%  [%ok amount=@ud]
      [%insufficient shortfall=@ud]  :: need this many more sats
      [%dust amount=@ud]             :: change exists but below threshold
  ==
::  +calculate-change-result: Calculate change with detailed result
::
++  calculate-change-result
  |=  [total-inputs=@ud total-outputs=@ud fee=@ud]
  ^-  change-result
  ::  Calculate amount needed (outputs + fee)
  ::
  =/  needed=@ud  (add total-outputs fee)
  ::  Check if inputs cover needed amount
  ::
  ?:  (lth total-inputs needed)
    [%insufficient (sub needed total-inputs)]
  ::  Calculate leftover as change
  ::
  =/  change=@ud  (sub total-inputs needed)
  ::  Check if change is above dust threshold
  ::
  ?:  (gte change dust-threshold)
    [%ok change]
  [%dust change]
::  Calculate change amount
::
::  Returns ~ if change would be negative or dust
::
++  calculate-change
  |=  [total-inputs=@ud total-outputs=@ud fee=@ud]
  ^-  (unit @ud)
  =/  result=change-result  (calculate-change-result total-inputs total-outputs fee)
  ?-  -.result
    %dust          ~
    %insufficient  ~
    %ok            `amount.result
  ==
--
