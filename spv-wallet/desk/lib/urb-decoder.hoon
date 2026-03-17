::  lib/urb-decoder.hoon
::
::  Decode URB-formatted tapscripts from raw bytes or witness data.
::  Detects URB envelope and parses to structured sotx types.
::
/-  urb, ord, btc=bitcoin
/+  bscr=btc-script, ue=urb-encoder, bcu=bitcoin-utils, taproot
|%
::  +decode: attempt to decode raw script bytes as URB tapscript
::
::  Returns ~ if not URB-formatted, otherwise list of parsed sotx
::
++  decode
  |=  raw=hexb:btc
  ^-  (unit (list raw-sotx:urb))
  ::  1. Parse raw bytes to script structure
  ::
  =/  script=(unit script:bscr)  (de:bscr [wid.raw dat.raw])
  ?~  script  ~
  ::  2. Extract URB envelope (returns ~ if not URB format)
  ::
  =/  unvs=(list @)  (unv:de:ue u.script)
  ?~  unvs  ~
  ::  3. Parse each UNV to raw-sotx
  ::
  =/  parsed=(list (list raw-sotx:urb))
    (turn unvs parse-roll:ue)
  ::  4. Flatten and return
  ::
  `(zing parsed)
::
::  +is-urb: quick check if script bytes are URB-formatted
::
++  is-urb
  |=  raw=hexb:btc
  ^-  ?
  =/  script=(unit script:bscr)  (de:bscr [wid.raw dat.raw])
  ?~  script  %.n
  !=(~ (unv:de:ue u.script))
::
::  +decode-witness: decode URB from taproot witness stack
::
::  In a script-path spend, the witness is:
::    [signature, ...custom-witness, script, control-block]
::  The script is second-to-last item.
::
++  decode-witness
  |=  witness=(list hexb:btc)
  ^-  (unit (list raw-sotx:urb))
  ::  Need at least 2 items: script + control block
  ::
  ?:  (lth (lent witness) 2)  ~
  ::  Script is second-to-last
  ::
  =/  wit-list  (flop witness)
  ?~  wit-list  ~
  ?~  t.wit-list  ~
  =/  script-bytes=hexb:btc  i.t.wit-list
  (decode script-bytes)
::
::  +decode-tapleaf: decode URB from ptst tapleaf
::
++  decode-tapleaf
  |=  leaf=tapleaf:taproot
  ^-  (unit (list raw-sotx:urb))
  (decode script.leaf)
::
::  ============================================
::  Rendering functions for human-readable output
::  ============================================
::
::  +render-sotx: render raw-sotx to tape
::
++  render-sotx
  |=  sot=raw-sotx:urb
  ^-  tape
  =/  ship-str=tape  (scow %p ship.sot.sot)
  =/  sig-str=tape
    ?~  sig.sot.sot  "unsigned"
    "signed"
  =/  op-str=tape  (render-skim +.sot.sot)
  "{ship-str} ({sig-str}): {op-str}"
::
::  +render-skim: render skim-sotx operation to tape
::
++  render-skim
  |=  sot=skim-sotx:urb
  ^-  tape
  ?-    -.sot
      %batch
    =/  count=tape  (scow %ud (lent bat.sot))
    "batch of {count} operations"
  ::
      %spawn
    =/  spkh=tape  ((x-co:co 64) spkh.to.sot)
    =/  off=tape  (scow %ud off.to.sot)
    "spawn at spkh={spkh} off={off}"
  ::
      %keys
    =/  breach=tape  ?:(breach.sot "breach" "rotate")
    "keys ({breach})"
  ::
      %escape
    =/  parent=tape  (scow %p parent.sot)
    "escape to {parent}"
  ::
      %cancel-escape
    =/  parent=tape  (scow %p parent.sot)
    "cancel-escape from {parent}"
  ::
      %adopt
    =/  who=tape  (scow %p ship.sot)
    "adopt {who}"
  ::
      %reject
    =/  who=tape  (scow %p ship.sot)
    "reject {who}"
  ::
      %detach
    =/  who=tape  (scow %p ship.sot)
    "detach {who}"
  ::
      %fief
    ?~  fief.sot  "clear fief"
    ?-  -.u.fief.sot
      %turf  "set fief turf"
      %if    "set fief ipv4"
      %is    "set fief ipv6"
    ==
  ::
      %set-mang
    ?~  mang.sot  "clear management proxy"
    ?-  -.u.mang.sot
      %sont  "set management proxy (sont)"
      %pass  "set management proxy (pass)"
    ==
  ==
::
::  +render-skim-raw: render full skim-sotx structure as manx
::
++  render-skim-raw
  |=  sot=skim-sotx:urb
  ^-  manx
  ?-    -.sot
      %batch
    ;div.fc.g2
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %batch ({(scow %ud (lent bat.sot))} ops)
      ==
      ;*  =/  idx=@ud  0
          %+  turn  bat.sot
          |=  op=single:skim-sotx:urb
          =.  idx  +(idx)
          ;div.p2.br1(style "background: rgba(100, 200, 150, 0.05); border: 1px solid rgba(100, 200, 150, 0.15); margin-left: 8px;")
            ;div.f3.s-2(style "opacity: 0.4; margin-bottom: 4px;"): batch[{(scow %ud (dec idx))}]
            ;+  ^$(sot op)
          ==
    ==
  ::
      %spawn
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %spawn
      ==
      ;div
        ;div.f3.s-2(style "opacity: 0.5;"): pass=
        ;div.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap;"): {((x-co:co 128) pass.sot)}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): fief=
        ;span.mono.f3.s-2: {?~(fief.sot "~" <u.fief.sot>)}
      ==
      ;div
        ;div.f3.s-2(style "opacity: 0.5;"): to.spkh=
        ;div.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap;"): {((x-co:co 64) spkh.to.sot)}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): to.vout=
        ;span.mono.f3.s-2: {?~(vout.to.sot "~" (scow %ud u.vout.to.sot))}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): to.off=
        ;span.mono.f3.s-2: {(scow %ud off.to.sot)}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): to.tej=
        ;span.mono.f3.s-2: {(scow %ud tej.to.sot)}
      ==
    ==
  ::
      %keys
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %keys
      ==
      ;div
        ;div.f3.s-2(style "opacity: 0.5;"): pass=
        ;div.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap;"): {((x-co:co 128) pass.sot)}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): breach=
        ;span.mono.f3.s-2: {?:(breach.sot "%.y" "%.n")}
      ==
    ==
  ::
      %escape
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %escape
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): parent=
        ;span.mono.f3.s-1: {(scow %p parent.sot)}
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): sig=
        ;span.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap; display: block;"): {?~(sig.sot "~" ((x-co:co 128) u.sig.sot))}
      ==
    ==
  ::
      %cancel-escape
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %cancel-escape
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): parent=
        ;span.mono.f3.s-2: {(scow %p parent.sot)}
      ==
    ==
  ::
      %adopt
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %adopt
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): ship=
        ;span.mono.f3.s-2: {(scow %p ship.sot)}
      ==
    ==
  ::
      %reject
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %reject
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): ship=
        ;span.mono.f3.s-2: {(scow %p ship.sot)}
      ==
    ==
  ::
      %detach
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %detach
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): ship=
        ;span.mono.f3.s-2: {(scow %p ship.sot)}
      ==
    ==
  ::
      %fief
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %fief
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): fief=
        ;span.mono.f3.s-2: {?~(fief.sot "~" <u.fief.sot>)}
      ==
    ==
  ::
      %set-mang
    ;div.fc.g1
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): skim-sotx=
        ;span.mono.f3.s-1: %set-mang
      ==
      ;div
        ;span.f3.s-2(style "opacity: 0.5;"): mang=
        ;span.mono.f3.s-2: {?~(mang.sot "~" <u.mang.sot>)}
      ==
    ==
  ==
::
::  +sotx-to-json: convert raw-sotx to JSON for web UI
::
++  sotx-to-json
  |=  sot=raw-sotx:urb
  ^-  json
  =/  ship-str=@t  (scot %p ship.sot.sot)
  =/  signed=?  !=(~ sig.sot.sot)
  :-  %o
  %-  ~(gas by *(map @t json))
  :~  ['ship' s+ship-str]
      ['signed' b+signed]
      ['operation' (skim-to-json +.sot.sot)]
  ==
::
::  +skim-to-json: convert skim-sotx to JSON
::
++  skim-to-json
  |=  sot=skim-sotx:urb
  ^-  json
  :-  %o
  %-  ~(gas by *(map @t json))
  ?-    -.sot
      %batch
    :~  ['type' s+'batch']
        ['count' (numb:enjs:format (lent bat.sot))]
        ['operations' a+(turn bat.sot skim-to-json)]
    ==
  ::
      %spawn
    :~  ['type' s+'spawn']
        ['spkh' s+(crip ((x-co:co 64) spkh.to.sot))]
        ['off' (numb:enjs:format off.to.sot)]
        ['tej' (numb:enjs:format tej.to.sot)]
        ['vout' ?~(vout.to.sot ~ (numb:enjs:format u.vout.to.sot))]
    ==
  ::
      %keys
    :~  ['type' s+'keys']
        ['breach' b+breach.sot]
    ==
  ::
      %escape
    :~  ['type' s+'escape']
        ['parent' s+(scot %p parent.sot)]
    ==
  ::
      %cancel-escape
    :~  ['type' s+'cancel-escape']
        ['parent' s+(scot %p parent.sot)]
    ==
  ::
      %adopt
    :~  ['type' s+'adopt']
        ['ship' s+(scot %p ship.sot)]
    ==
  ::
      %reject
    :~  ['type' s+'reject']
        ['ship' s+(scot %p ship.sot)]
    ==
  ::
      %detach
    :~  ['type' s+'detach']
        ['ship' s+(scot %p ship.sot)]
    ==
  ::
      %fief
    :~  ['type' s+'fief']
        :-  'fief'
        ?~  fief.sot  ~
        :-  %o
        %-  ~(gas by *(map @t json))
        ?-  -.u.fief.sot
          %turf  ~[['kind' s+'turf']]
          %if    ~[['kind' s+'ipv4'] ['ip' s+(scot %if p.u.fief.sot)]]
          %is    ~[['kind' s+'ipv6'] ['ip' s+(scot %is p.u.fief.sot)]]
        ==
    ==
  ::
      %set-mang
    :~  ['type' s+'set-mang']
        :-  'mang'
        ?~  mang.sot  ~
        :-  %o
        %-  ~(gas by *(map @t json))
        ?-  -.u.mang.sot
          %sont  ~[['kind' s+'sont']]
          %pass  ~[['kind' s+'pass']]
        ==
    ==
  ==
::
::  +decode-to-json: convenience function - decode and render to JSON
::
++  decode-to-json
  |=  raw=hexb:btc
  ^-  (unit json)
  =/  decoded=(unit (list raw-sotx:urb))  (decode raw)
  ?~  decoded  ~
  `a+(turn u.decoded sotx-to-json)
--
