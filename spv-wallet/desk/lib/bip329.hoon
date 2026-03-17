/-  *hd-path
/+  json-utils, html-utils, sailbox
=,  dejs:format
|%
::  BIP-329 Wallet Labels Export Format
::  https://github.com/bitcoin/bips/blob/master/bip-0329.mediawiki
::
+$  label-type  ?(%tx %addr %pubkey %input %output %xpub)
::
+$  script-type  ?(%wpkh %wsh %tr %sh %pkh %pk)
::
+$  label-entry
  $:  type=label-type
      ref=@t
      label=@t
      origin=(unit parsed-origin)
      spendable=(unit ?)
  ==
::
+$  labels
  $:  tx=(map @t (set label-entry))
      addr=(map @t (set label-entry))
      output=(map @t (set label-entry))
      input=(map @t (set label-entry))
      pubkey=(map @t (set label-entry))
      xpub=(map @t (set label-entry))
  ==
::
+$  parsed-origin
  $:  type=script-type
      fingerprint=@ux
      path=(list seg)
  ==
::
::  Serialization
::
++  origin-to-cord
  |=  po=parsed-origin
  ^-  @t
  ::  Format fingerprint as 8-digit hex with leading zeros
  =/  fp-hex=tape  (hexn:sailbox fingerprint.po)
  =/  fp-str=tape  (runt [(sub 8 (lent fp-hex)) '0'] fp-hex)
  =/  path-str=tape
    %-  zing
    %+  join  "/"
    %+  turn  path.po
    |=  =seg
    (weld (numb:sailbox q.seg) "'")
  (crip "{(trip type.po)}([{fp-str}/{path-str}])")
::
::  Parsing
::
++  origin-rule
  ::  Parser rule for BIP-329 origin descriptor strings
  ::  Format: script-type([fingerprint/seg'/seg'/seg'])
  ::  Example: (rust (trip 'wpkh([d34db33f/84\'/0\'/0\'])') origin-rule)
  ::  Returns: [%wpkh 0xd34d.b33f ~[[%.y 84] [%.y 0] [%.y 0]]]
  %+  cook
    |=  [st=script-type fp=@ux segs=(list @ud)]
    ^-  parsed-origin
    [st fp (turn segs |=(n=@ud [%.y n]))]
  ;~  plug
    ::  Parse script type
    ;~  pose
      (cold %wpkh (jest 'wpkh'))
      (cold %wsh (jest 'wsh'))
      (cold %tr (jest 'tr'))
      (cold %sh (jest 'sh'))
      (cold %pkh (jest 'pkh'))
      (cold %pk (jest 'pk'))
    ==
    ::  Parse bracketed fingerprint and path
    %+  ifix  [(jest '([') (jest '])')]
    ;~  plug
      ::  Parse 8-digit hex fingerprint
      %+  cook
        |=(a=tape `@ux`(rash (crip a) hex))
      (stun [8 8] ;~(pose (shim '0' '9') (shim 'a' 'f') (shim 'A' 'F')))
      ::  Parse /84'/0'/0' style hardened path segments
      ;~(pfix fas (more fas ;~(sfix dem (just '\''))))
    ==
  ==
::
++  parse-entry
  |=  jon=json
  ^-  (unit label-entry)
  =/  jo  ~(. jo:json-utils jon)
  %-  mole  |.
  ?>  ?&  (has:jo /type)
          (has:jo /ref)
          (has:jo /label)
      ==
  =/  origin-str=(unit @t)  (deg:jo /origin so)
  =/  parsed-origin=(unit parsed-origin)
    ?~  origin-str
      ~
    (rust (trip u.origin-str) origin-rule)
  :*  ;;(label-type (dog:jo /type so))
      (dog:jo /ref so)
      (dog:jo /label so)
      parsed-origin
      (deg:jo /spendable bo)
  ==
::
++  parse-jsonl
  |=  lines=wain
  ^-  (list label-entry)
  %+  murn  lines
  |=  line=@t
  ^-  (unit label-entry)
  ?~  jon=(de:json:html line)
    ~
  (parse-entry u.jon)
::
::  Serialization
::
++  entry-to-json
  |=  entry=label-entry
  ^-  json
  %-  pairs:enjs:format
  ;:  welp
    :~  ['type' s+(scot %tas type.entry)]
        ['ref' s+ref.entry]
        ['label' s+label.entry]
    ==
    ?~(origin.entry ~ ~[['origin' s+(origin-to-cord u.origin.entry)]])
    ?~(spendable.entry ~ ~[['spendable' b+u.spendable.entry]])
  ==
::
++  entries-to-jsonl
  |=  entries=(list label-entry)
  ^-  wain
  %+  turn  entries
  |=  entry=label-entry
  (en:json:html (entry-to-json entry))
::
::  +la: CRUD core for labels structure
::  Usage: (~(get la my-labels) %output 'txid:0')
::
++  la
  |_  =labels
  ::  Get all label entries for a ref of a specific type
  ::
  ++  get
    |=  [typ=label-type ref=@t]
    ^-  (set label-entry)
    =/  type-map=(map @t (set label-entry))
      ?-  typ
        %tx      tx.labels
        %addr    addr.labels
        %output  output.labels
        %input   input.labels
        %pubkey  pubkey.labels
        %xpub    xpub.labels
      ==
    (fall (~(get by type-map) ref) ~)
  ::  Get just the label text strings for a ref
  ::
  ++  texts
    |=  [typ=label-type ref=@t]
    ^-  (list @t)
    =/  entries=(set label-entry)  (get typ ref)
    %+  sort
      (turn ~(tap in entries) |=(e=label-entry label.e))
    |=([a=@t b=@t] (aor a b))
  ::  Check if an output is frozen (spendable=%.n)
  ::
  ++  frozen
    |=  ref=@t
    ^-  ?
    =/  entries=(set label-entry)  (get %output ref)
    %+  lien  ~(tap in entries)
    |=(e=label-entry =([~ %.n] spendable.e))
  ::  Add or update a label entry
  ::  If entry with same type+ref+label exists, it's replaced
  ::
  ++  put
    |=  entry=label-entry
    ^-  ^labels
    =/  type-map=(map @t (set label-entry))
      ?-  type.entry
        %tx      tx.labels
        %addr    addr.labels
        %output  output.labels
        %input   input.labels
        %pubkey  pubkey.labels
        %xpub    xpub.labels
      ==
    =/  existing=(set label-entry)
      (fall (~(get by type-map) ref.entry) ~)
    ::  Remove any entry with same label text, then add new one
    =/  filtered=(set label-entry)
      %-  sy
      %+  skip  ~(tap in existing)
      |=(e=label-entry =(label.e label.entry))
    =/  updated=(set label-entry)
      (~(put in filtered) entry)
    =/  new-type-map=(map @t (set label-entry))
      (~(put by type-map) ref.entry updated)
    ?-  type.entry
      %tx      labels(tx new-type-map)
      %addr    labels(addr new-type-map)
      %output  labels(output new-type-map)
      %input   labels(input new-type-map)
      %pubkey  labels(pubkey new-type-map)
      %xpub    labels(xpub new-type-map)
    ==
  ::  Remove a specific label entry (by type+ref+label text)
  ::
  ++  del
    |=  [typ=label-type ref=@t lbl=@t]
    ^-  ^labels
    =/  type-map=(map @t (set label-entry))
      ?-  typ
        %tx      tx.labels
        %addr    addr.labels
        %output  output.labels
        %input   input.labels
        %pubkey  pubkey.labels
        %xpub    xpub.labels
      ==
    =/  existing=(set label-entry)
      (fall (~(get by type-map) ref) ~)
    =/  filtered=(set label-entry)
      %-  sy
      %+  skip  ~(tap in existing)
      |=(e=label-entry =(label.e lbl))
    =/  new-type-map=(map @t (set label-entry))
      ?:  =(~ filtered)
        (~(del by type-map) ref)
      (~(put by type-map) ref filtered)
    ?-  typ
      %tx      labels(tx new-type-map)
      %addr    labels(addr new-type-map)
      %output  labels(output new-type-map)
      %input   labels(input new-type-map)
      %pubkey  labels(pubkey new-type-map)
      %xpub    labels(xpub new-type-map)
    ==
  ::  Delete all labels for a ref
  ::
  ++  del-all
    |=  [typ=label-type ref=@t]
    ^-  ^labels
    =/  type-map=(map @t (set label-entry))
      ?-  typ
        %tx      tx.labels
        %addr    addr.labels
        %output  output.labels
        %input   input.labels
        %pubkey  pubkey.labels
        %xpub    xpub.labels
      ==
    =/  new-type-map=(map @t (set label-entry))
      (~(del by type-map) ref)
    ?-  typ
      %tx      labels(tx new-type-map)
      %addr    labels(addr new-type-map)
      %output  labels(output new-type-map)
      %input   labels(input new-type-map)
      %pubkey  labels(pubkey new-type-map)
      %xpub    labels(xpub new-type-map)
    ==
  ::  Freeze an output (set spendable=%.n)
  ::
  ++  freeze
    |=  ref=@t
    ^-  ^labels
    =/  existing=(set label-entry)  (get %output ref)
    ?:  =(~ existing)
      ::  No existing labels - create one just for spendable
      (put [%output ref '' ~ `%.n])
    ::  Update spendable on all existing entries
    =/  updated=(list label-entry)
      (turn ~(tap in existing) |=(e=label-entry e(spendable `%.n)))
    =/  new-set=(set label-entry)  (sy updated)
    labels(output (~(put by output.labels) ref new-set))
  ::  Thaw an output (set spendable=%.y or ~)
  ::
  ++  thaw
    |=  ref=@t
    ^-  ^labels
    =/  existing=(set label-entry)  (get %output ref)
    ?:  =(~ existing)
      labels
    ::  Update spendable on all existing entries
    ::  Remove entries that only existed for spendable (empty label)
    =/  updated=(list label-entry)
      %+  murn  ~(tap in existing)
      |=  e=label-entry
      ?:  =('' label.e)
        ~  :: Remove empty-label entries that were just for freezing
      `e(spendable ~)
    ?:  =(~ updated)
      labels(output (~(del by output.labels) ref))
    labels(output (~(put by output.labels) ref (sy updated)))
  ::  Set labels for a ref (replaces all existing)
  ::
  ++  set-texts
    |=  [typ=label-type ref=@t txts=(list @t)]
    ^-  ^labels
    =/  type-map=(map @t (set label-entry))
      ?-  typ
        %tx      tx.labels
        %addr    addr.labels
        %output  output.labels
        %input   input.labels
        %pubkey  pubkey.labels
        %xpub    xpub.labels
      ==
    ::  Preserve spendable from existing entries if output type
    =/  existing-spendable=(unit ?)
      ?.  =(%output typ)  ~
      =/  existing=(set label-entry)
        (fall (~(get by type-map) ref) ~)
      ?:  =(~ existing)  ~
      spendable:(head ~(tap in existing))
    =/  new-set=(set label-entry)
      ?:  =(~ txts)
        ::  If no texts but we have spendable, keep one entry for it
        ?~  existing-spendable  ~
        (sy [typ ref '' ~ existing-spendable]~)
      %-  sy
      %+  turn  txts
      |=(t=@t `label-entry`[typ ref t ~ existing-spendable])
    =/  new-type-map=(map @t (set label-entry))
      ?:  =(~ new-set)
        (~(del by type-map) ref)
      (~(put by type-map) ref new-set)
    ?-  typ
      %tx      labels(tx new-type-map)
      %addr    labels(addr new-type-map)
      %output  labels(output new-type-map)
      %input   labels(input new-type-map)
      %pubkey  labels(pubkey new-type-map)
      %xpub    labels(xpub new-type-map)
    ==
  ::  Export all labels as a flat list
  ::
  ++  export
    ^-  (list label-entry)
    %-  zing
    ^-  (list (list label-entry))
    :~  (zing (turn ~(val by tx.labels) |=(s=(set label-entry) ~(tap in s))))
        (zing (turn ~(val by addr.labels) |=(s=(set label-entry) ~(tap in s))))
        (zing (turn ~(val by output.labels) |=(s=(set label-entry) ~(tap in s))))
        (zing (turn ~(val by input.labels) |=(s=(set label-entry) ~(tap in s))))
        (zing (turn ~(val by pubkey.labels) |=(s=(set label-entry) ~(tap in s))))
        (zing (turn ~(val by xpub.labels) |=(s=(set label-entry) ~(tap in s))))
    ==
  ::  Import labels from a list (merges with existing)
  ::
  ++  import
    |=  entries=(list label-entry)
    ^-  ^labels
    %+  roll  entries
    |=  [entry=label-entry acc=_labels]
    (~(put la acc) entry)
  --
--
