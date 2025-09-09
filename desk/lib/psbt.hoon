::  psbt.hoon -- A more complete implementation of BIP-174
::
::  V2 notes: better distinguish types, methods for PSBT noun representation, PSBT bytecode, and transaction bytecode
::
/-  *psbt
/+  bc=bitcoin, der
|%
+$  key-value  [typ=@ key=hexb:bc val=hexb:bc]
+$  map        (list key-value)
::  +read-bytes: take n bytes, drop n bytes
::
++  read-bytes
  |=  [n=@ b=hexb:bc]
  ^-  (pair hexb:bc hexb:bc)
  [(take:byt:bcu:bc n b) (drop:byt:bcu:bc n b)]
::
++  tx
  =<  tx
  |%
  +$  tx  [id=txid data]
  ::
  +$  data
    $:  is=(list input)
        os=(list output)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  ::  included: whether tx is in the mempool or blockchain
  ::
  ::
  +$  input
    $:  =txid
        pos=@ud
        sequence=hexb:bc
        script-sig=(unit hexb:bc)
        pubkey=(unit hexb:bc)
        value=@ud
    ==
  ::
  +$  output
    $:  script-pubkey=hexb:bc
        value=sats
    ==
  ::
  +$  witness    (list hexb:bc)
  --

::  +read-compact-size: decode CompactSize
::    bug in bitcoin-utils
::
++  read-compact-size
  |=  b=hexb:bc
  ^-  [a=@ rest=hexb:bc]
  =^  s  b  (read-bytes 1 b)
  ?:  (lth +.s 0xfd)  [+.s b]
  ~|  %invalid-compact-size
  =/  len=bloq
    ?+  +.s  !!
      %0xfd  1
      %0xfe  2
      %0xff  3
    ==
  =^  k  b  (read-bytes (bex len) b)
  :_  b
  dat:(flip:byt:bcu:bc k)
::  +encode-compact-size
::    bug in bitcoin-utils for a=0
::
++  encode-compact-size
  =,  bcu:bc
  |=  a=@
  ^-  hexb:bc
  =+  n=(met 3 a)
  ?:  =(n 0)     1^a
  ?:  =(n 1)     1^a
  ?:  =(n 2)     (cat:byt ~[1^0xfd (flip:byt 2^a)])
  ?:  (lte n 4)  (cat:byt ~[1^0xfe (flip:byt 4^a)])
  ?:  (lte n 8)  (cat:byt ~[1^0xff (flip:byt 8^a)])
  ~|(%invalid-compact-size !!)
::  +full-key: build full key from type and key bytes
::
++  full-key
  |=  [typ=@ key=hexb:bc]
  ^-  hexb:bc
  =/  size=hexb:bc  (encode-compact-size typ)
  :: ~&  >  "size {<size>}"
  :: ~&  >  "key {<key>}"
  %-  cat:byt:bcu:bc
  ~[size key]
::  +next-key-value: parse next key-value pair from map
::
++  next-key-value
  |=  b=hexb:bc
  ^-  (pair (unit key-value) hexb:bc)
  =^  ksz  b    (read-compact-size b)
  ?:  =(ksz 0)
    [~ b]
  =^  key  b    (read-bytes ksz b)
  =^  typ  key  (read-compact-size key)
  =^  vsz  b    (read-compact-size b)
  =^  val  b    (read-bytes vsz b)
  [`[typ=typ key=key val=val] b]
::  +read-inputs: read n inputs from byts
::
++  read-inputs
  |=  [n=@ b=hexb:bc]
  ^-  (pair (list in:tx) hexb:bc)
  =|  acc=(list in:tx)
  |-
  ?:  =(n 0)  [acc b]
  =^  a  b  (read-input b)
  $(acc (snoc acc a), n (dec n))
::  +read-outputs: read n outputs from byts
::
++  read-outputs
  |=  [n=@ b=hexb:bc]
  ^-  (pair (list out:tx) hexb:bc)
  =|  acc=(list out:tx)
  |-
  ?:  =(n 0)  [acc b]
  =^  a  b  (read-output b)
  $(acc (snoc acc a), n (dec n))
::  +parse-witness: decode witness stack
::
++  parse-witness
  |=  b=hexb:bc
  ^-  (pair (list hexb:bc) hexb:bc)
  =|  acc=witness
  =+  i=0
  =^  n  b  (read-compact-size b)
  |-
  ?:  =(i n)  [acc b]
  =^  wid  b  (read-compact-size b)
  =^  dat  b  (read-bytes wid b)
  $(acc (snoc acc dat), i +(i))
::  +read-witness: read witness into input
::
++  read-witness
  |=  [input=in:tx b=hexb:bc]
  ^-  (pair in:tx hexb:bc)
  =^  witness  b  (parse-witness b)
  :_  b
  input(script-witness ?~(witness ~ `witness))
::  +read-input: read input from byts
::
++  read-input
  |=  b=hexb:bc
  ^-  (pair in:tx hexb:bc)
  =|  input=in:tx
  =^  prevout-hash  b  (read-bytes 32 b)
  =+  prevout-txid=dat:(flip:byt:bcu:bc prevout-hash)
  =^  prevout-n     b  (read-bytes 4 b)
  =+  idx=dat:(flip:byt:bcu:bc prevout-n)
  =^  script-len    b  (read-compact-size b)
  =^  script-sig    b  (read-bytes script-len b)
  =^  sequence      b  (read-bytes 4 b)
  =+  nsequence=dat:(flip:byt:bcu:bc sequence)
  :_  b
  %=  input
    prevout     [txid=prevout-txid idx=idx]
    script-sig  ?:(=(0 script-len) ~ `script-sig)
    nsequence   nsequence
  ==
::  +read-output: read output from byts
::
++  read-output
  |=  b=hexb:bc
  ^-  (pair out:tx hexb:bc)
  =|  output=out:tx
  =^  value  b       (read-bytes 8 b)
  =+  amount=dat:(flip:byt:bcu:bc value)
  =^  script-len  b  (read-compact-size b)
  =^  script-pub  b  (read-bytes script-len b)
  :_  b
  %=  output
    value          amount
    script-pubkey  script-pub
  ==
::  +decode-tx: decode unsigned transaction
::
++  decode-tx
  |=  b=hexb:bc
  ^-  tx:tx
  =|  =tx:tx
  =^  version  b  (read-bytes 4 b)
  =+  nversion=dat:(flip:byt:bcu:bc version)
  =^  n-vin     b  (read-compact-size b)
  =+  ^=  is-segwit  =(n-vin 0)
  =^  n-vin     b
    ?.  is-segwit  [n-vin b]
    =^  marker  b  (read-bytes 1 b)
    ~|  %psbt-invalid-marker
    ?<  =(b 1^0x1)
    (read-compact-size b)
  =^  vin=(list in:^tx)  b
    (read-inputs n-vin b)
  =^  n-vout   b  (read-compact-size b)
  =^  outputs  b  (read-outputs n-vout b)
  =^  inputs   b
    ?.  is-segwit  [vin b]
    (spin vin b read-witness)
  =^  locktime  b  (read-bytes 4 b)
  =+  nlocktime=dat:(flip:byt:bcu:bc locktime)
  ?.  =(wid.b 0)
    ~|(%psbt-invalid-tx !!)
  %=  tx
    vin        inputs
    vout       outputs
    nversion   nversion
    nlocktime  nlocktime
  ==
::  +encode-input: encode input to byts
::
++  encode-input
  |=  =in:tx
  ^-  hexb:bc
  %-  cat:byt:bcu:bc
  %-  zing
  :~  ~[(flip:byt:bcu:bc 256^txid.prevout.in)]
      ~[(flip:byt:bcu:bc 4^idx.prevout.in)]
      ?^  script-sig.in
        :~  (encode-compact-size wid.u.script-sig.in)
            u.script-sig.in
        ==
      ~[1^0x0]
      ~[(flip:byt:bcu:bc 4^nsequence.in)]
  ==
::  +encode-output: encode output to byts
::
++  encode-output
  |=  =out:tx
  ^-  hexb:bc
  %-  cat:byt:bcu:bc
  :~  (flip:byt:bcu:bc 8^value.out)
      (encode-compact-size wid.script-pubkey.out)
      script-pubkey.out
  ==
::  +encode-witness-script: pack witness script to byts
::
++  encode-witness-script
  |=  w=witness
  ^-  hexb:bc
  %-  cat:byt:bcu:bc
  :-  (encode-compact-size (lent w))
  %-  zing
  %+  turn  w
  |=  e=hexb:bc
  ?:  =(wid.e 0)
    ~[1^0x0]
  ~[(encode-compact-size wid.e) e]
::  +encode-witness: encode input's script-witness
::
++  encode-witness
  |=  =in:tx
  ^-  hexb:bc
  ?~  script-witness.in
    0^0x0
  (encode-witness-script u.script-witness.in)
::  +encode-tx: encode unsigned transaction
::
++  encode-tx
  |=  =tx:tx
  ^-  hexb:bc
  =+  ^=  is-segwit
      %+  lien  vin.tx
      |=  i=in:^tx
      ?=(^ script-witness.i)
  %-  cat:byt:bcu:bc
  %-  zing
  :~  ~[(flip:byt:bcu:bc 4^nversion.tx)]
      ?:  is-segwit
        ~[1^0x0 1^0x1]
      ~
      ~[(encode-compact-size (lent vin.tx))]
      (turn vin.tx encode-input)
      ~[(encode-compact-size (lent vout.tx))]
      (turn vout.tx encode-output)
      ?:  is-segwit
        (turn vin.tx encode-witness)
      ~
      ~[(flip:byt:bcu:bc 4^nlocktime.tx)]
  ==
::  +txid: compute txid for unsigned transaction
::
++  txid
  |=  =tx
  ^-  hexb:bc
  =+  ^=  strip-witness
      |=  =in:^tx
      in(script-witness ~)
  %-  wtxid
  tx(vin (turn vin.tx strip-witness))
::  +wtxid: txid including witness data
::
++  wtxid
  |=  =tx:tx
  ^-  hexb:bc
  %-  flip:byt:bcu:bc
  %-  dsha256:bcu:bc
  (encode-tx tx)
::  +unsigned-tx: get the unsigned transaction in global map
::
++  unsigned-tx
  |=  g=map
  ^-  tx:tx
  ~|  %psbt-no-unsigned-tx
  |-
  =+  (head g)
  ?:  =(dat.key unsigned-tx:global)
    (decode-tx val)
  $(g (tail g))
::  +parse: decode psbt maps from byts
::
++  parse
  |=  b=hexb:bc
  ^-  (list map)
  =^  magc=hexb:bc  b  (read-bytes 5 b)
  ?.  =(dat.magc magic)
    ~|([%psbt-bad-magic dat.magc] !!)
  =|  acc=(list map)
  =|  m=map
  |-
  ?:  =(wid.b 0)
    (snoc acc m)
  =^  kv  b  (next-key-value b)
  ?~  kv
    $(acc (snoc acc m), m ~)
  $(m (snoc m +.kv))
::  +de: decode psbt from byts
::
++  de
  |=  b=hexb:bc
  |^  ^-  psbt
  =+  maps=(parse b)
  =+  globals=(head maps)
  =+  unsigned=(unsigned-tx globals)
  =+  partial-tx=(from-unsigned-tx:create unsigned)
  =+  n-is=(lent vin.unsigned)
  =+  n-os=(lent vout.unsigned)
  =+  input-maps=(scag n-is (tail maps))
  =+  output-maps=(scag n-os (slag n-is (tail maps)))
  %=  partial-tx
    inputs
      %^    populate-fields
          inputs.partial-tx
        input-maps
      populate-input
  ::
    outputs
      %^    populate-fields
          outputs.partial-tx
        output-maps
      populate-output
  ==
  ++  populate-fields
    |*  [as=(list) maps=(list map) f=$-([key-value *] *)]
    ^+  as
    =+  acc=`_as`~
    ~|  %psbt-invalid
    |-
    ?~  as
      ?>  ?=(~ maps)
      acc
    =+  d=(head maps)
    =+  ^=  a
        %+  roll  d
        |:  [kv=*key-value i=(head as)]
        (f kv i)
    %=  $
      acc   (snoc acc a)
      as    (tail as)
      maps  (tail maps)
    ==
  ::
  ++  populate-input
    |=  [kv=key-value i=input]
    ^-  input
    =+  kv
    ?:  =(typ non-witness-utxo:in)
      ?>  =(wid.key 0)
      i(non-witness-utxo `(decode-tx val))
    ?:  =(typ witness-utxo:in)
      i(witness-utxo `(decode-output val))
    ?:  =(typ partial-sig:in)
      i(partial-sigs (~(put by partial-sigs.i) key val))
    ?:  =(typ sighash:in)
      i(sighash `dat:(flip:byt:bcu:bc val))
    ?:  =(typ redeemscript:in)
      i(redeem-script `val)
    ?:  =(typ witnessscript:in)
      i(witness-script `val)
    ?:  =(typ bip32-derivation:in)
      i(hd-keypaths (decode-hd-keypaths hd-keypaths.i key val))
    ?:  =(typ scriptsig:in)
      i(final-script-sig `val)
    ?:  =(typ scriptwitness:in)
      i(final-script-witness `-:(parse-witness val))
    i(unknown (~(put by unknown.i) (full-key typ key) val))
  ::
  ++  populate-output
    |=  [kv=key-value o=output]
    ^-  output
    =+  kv
    ?:  =(typ redeemscript:out)
      o(redeem-script `val)
    ?:  =(typ witnessscript:out)
      o(witness-script `val)
    ?:  =(typ bip32-derivation:out)
      o(hd-keypaths (decode-hd-keypaths hd-keypaths.o key val))
    o(unknown (~(put by unknown.o) (full-key typ key) val))
  ::
  ++  decode-output
    |=  b=hexb:bc
    ^-  out:tx
    -:(read-output b)
  ::
  ++  decode-hd-keypaths
    |=  [d=(^map pubkey keyinfo) key=hexb:bc val=hexb:bc]
    ^-  (^map pubkey keyinfo)
    ~|  %psbt-duplicate-key
    ?<  (~(has by d) key)
    =^  fprint  val  (read-bytes 4 val)
    =+  ^=  path
        =|  acc=(list @u)
        |-
        ?:  =(wid.val 0)
          acc
        =^  n  val  (read-bytes 4 val)
        $(acc (snoc acc `@u`dat:(flip:byt:bcu:bc n)))
    %+  ~(put by d)
      key
    [fprint=fprint path=path]
  --
::  +encode-hd-keypaths: generate psbt map for bip32-keypaths
::
++  encode-hd-keypaths
  |=  [typ=@ d=(^map pubkey keyinfo)]
  ^-  map
  %-  ~(rep by d)
  |=  [[=pubkey =keyinfo] acc=map]
  =+  keyinfo
  :_  acc
  :*  typ=typ
      key=pubkey
      ^=  val
      %-  cat:byt:bcu:bc
      :-  fprint
      %+  turn  path
      |=  n=@u
      (flip:byt:bcu:bc 4^`@ux`n)
  ==
::  +encode-key-value: serialize key-value to bytes
::
++  encode-key-value
  |=  =key-value
  ^-  hexb:bc
  =+  key-value
  =.  key  (full-key typ key)
  %-  cat:byt:bcu:bc
  :~  (encode-compact-size wid.key)
      key
      (encode-compact-size wid.val)
      val
  ==
::  +encode-map: serialize key-value map to bytes
::
++  encode-map
  |=  =map
  ^-  hexb:bc
  %-  cat:byt:bcu:bc
  %-  zing
  ~[(turn map encode-key-value) [1^0x0]~]
::  +en: encode psbt to byts
::
++  en
  |=  =psbt
  |^  ^-  hexb:bc
  %-  cat:byt:bcu:bc
  %-  zing
  :~  [5^magic]~
      [(encode-map global-map)]~
      (turn input-maps encode-map)
      (turn output-maps encode-map)
  ==
  ++  global-map
    ^-  map
    :-  :*  typ=unsigned-tx:global
            key=0^0x0
            val=(encode-tx (extract-unsigned psbt))
        ==
    %-  ~(rep by unknown.psbt)
    |=  [[k=key v=value] acc=map]
    =^  t  k  (read-compact-size k)
    [[t k v] acc]
  ::
  ++  input-maps
    ^-  (list map)
    %+  turn  inputs.psbt
    |=  =input
    ~(section-map txin input)
  ::
  ++  output-maps
    ^-  (list map)
    %+  turn  outputs.psbt
    |=  =output
    ~(section-map txout output)
  --
::  +txin: input-related utilities
::
++  txin
  |_  =input
  ::
  ++  from-input
    |=  [i=in:tx strip-witness=?]
    ^-  ^input
    =|  r=^input
    %=  r
      prevout         prevout.i
      nsequence       nsequence.i
      script-sig      ?:(strip-witness ~ script-sig.i)
      script-witness  ?:(strip-witness ~ script-witness.i)
    ==
  ::
  ++  to-input
    ^-  in:tx
    =|  r=in:tx
    %=  r
      prevout         prevout.input
      nsequence       nsequence.input
      script-sig      final-script-sig.input
      script-witness  final-script-witness.input
    ==
  ::
  ++  value
    ^-  (unit sats)
    ?^  trusted-value.input
      trusted-value.input
    ?^  non-witness-utxo.input
      =+  out-idx=idx.prevout.input
      =+  ^=  out
          %+  snag
            out-idx
          vout.u.non-witness-utxo.input
      `value.out
    ?^  witness-utxo.input
      `value.u.witness-utxo.input
    ~
  ::
  ++  is-coinbase-input
    ^-  ?
    %~  is-coinbase
      outpoint
    prevout.input
  ::
  ++  is-complete
    ^-  ?
    =+  sigs=~(val by partial-sigs.input)
    =+  s=(lent sigs)
    ?|  ?&  ?=(^ final-script-sig.input)
            ?=(^ final-script-witness.input)
        ==
        is-coinbase-input
        ?&  ?=(^ final-script-sig.input)
            ?!  is-segwit
        ==
        ::  TODO match template patterns
        ?&  ?=(^ (find [script-type.input]~ ~[%p2pk %p2pkh %p2wpkh]))
            (gte s 1)
        ==
        ?&  ?=(^ (find [script-type.input]~ ~[%p2sh %p2wsh]))
            (gte s num-sigs.input)
        ==
    ==
  ::
  ++  is-segwit
    ^-  ?
    :: ?=(^ witness-script.input)
    %.y
  ::
  ++  input-script
    ^-  (unit hexb:bc)
    ::  TODO
    ~
  ::
  ++  signature-list
    ^-  (list signature)
    %+  reel  pubkeys.input
    |=  [pub=pubkey sigs=(list signature)]
    =+  sig=(~(get by partial-sigs.input) pub)
    ?~  sig  [0^0x0 sigs]
    [u.sig sigs]
  ::
  ++  script-witness
    ^-  witness
    ?^  final-script-witness.input
      u.final-script-witness.input
    ?:  is-coinbase-input
      [0^0x0]~
    ?.  is-segwit
      [(encode-compact-size 0)]~
    ?^  (find [script-type.input]~ ~[%p2wsh])
      %-  zing
      :~  [0^0x0]~
          signature-list
          [(need witness-script.input)]~
      ==
    ?:  ?=(%p2wpkh script-type.input)
      =+  wit=(head ~(tap by partial-sigs.input))
      :: ~&  "PUB AND SIG"
      :: ~&  wit
      :~  +.wit
          -.wit
      ==
    [(encode-compact-size 0)]~
  ::
  ++  finalize
    ^-  ^input
    ?:  ?&  ?=(^ final-script-sig.input)
            ?=(^ final-script-witness.input)
        ==
      input
    ?:  is-complete
      :: ~&  >>>  "FINALIZING"
      %=  input
        final-script-sig      input-script
        final-script-witness  `script-witness
      ==
    input
  ::
  ++  section-map
    ^-  map
    %-  zing
    :~
      ?~  witness-utxo.input
        ~
      :_  ~
      :*  typ=witness-utxo:in
          key=0^0x0
          val=(encode-output u.witness-utxo.input)
      ==
      ::
      ?~  non-witness-utxo.input
        ~
      :_  ~
      :*  typ=non-witness-utxo:in
          key=0^0x0
          val=(encode-tx u.non-witness-utxo.input)
      ==
      ::
      ^-  (list key-value)
      %-  ~(rep by partial-sigs.input)
      |=  [[=pubkey =signature] acc=map]
      [[partial-sig:in pubkey signature] acc]
      ::
      ?~  sighash.input
        ~
      :_  ~
      :*  typ=sighash:in
          key=0^0x0
          val=1^u.sighash.input
      ==
      ::
      ?~  redeem-script.input
        ~
      :_  ~
      :*  typ=redeemscript:in
          key=0^0x0
          val=u.redeem-script.input
      ==
      ::
      ?~  witness-script.input
        ~
      :_  ~
      :*  typ=witnessscript:in
          key=0^0x0
          val=u.witness-script.input
      ==
      ::
      ^-  (list key-value)
      %+  encode-hd-keypaths
        bip32-derivation:in
      hd-keypaths.input
      ::
      ?~  final-script-sig.input
        ~
      :_  ~
      :*  typ=scriptsig:in
          key=0^0x0
          val=u.final-script-sig.input
      ==
      ::
      ?~  final-script-witness.input
        ~
      :_  ~
      :*  typ=scriptwitness:in
          key=0^0x0
          val=(encode-witness-script u.final-script-witness.input)
      ==
    ==
  --
::  +txout: output-related utilities
::
++  txout
  |_  o=output
  ::
  ++  from-output
    |=  o=out:tx
    ^-  output
    =|  r=output
    %=  r
      script-pubkey  script-pubkey.o
      value          value.o
    ==
  ::
  ++  to-output
    ^-  out:tx
    =|  r=out:tx
    %=  r
      value          value.o
      script-pubkey  script-pubkey.o
    ==
  ::
  ++  en
    ^-  hexb:bc
    (encode-output to-output)
  ::
  ++  section-map
    ^-  map
    %-  zing
    :~
      ?~  redeem-script.o
        ~
      :_  ~
      :*  typ=redeemscript:out
          key=0^0x0
          val=u.redeem-script.o
      ==
      ::
      ?~  witness-script.o
        ~
      :_  ~
      :*  typ=witnessscript:out
          key=0^0x0
          val=u.witness-script.o
      ==
      ::
      ^-  (list key-value)
      %+  encode-hd-keypaths
        bip32-derivation:out
      hd-keypaths.o
      ::
      ^-  (list key-value)
      %-  ~(rep by unknown.o)
      |=  [[=key =value] acc=map]
      =^  typ  key  (read-compact-size key)
      [[typ key value] acc]
    ==
  --
::  +outpoint: outpoint-related utilities
::
++  outpoint
  |_  [txid=@ux pos=@ud]
  ::
  ++  en
    ^-  hexb:bc
    %-  cat:byt:bcu:bc
    :~  (flip:byt:bcu:bc 256^txid)
        (flip:byt:bcu:bc 4^pos)
    ==
  ::
  ++  is-coinbase
    ^-  ?
    =(txid 32^(fil 3 32 0x0))
  --
::  +create: creator role
::
++  create
  |_  tx=psbt
  ::
  ++  from-byts  de
  ++  to-byts    (en tx)
  ::
  ++  from-base64
    |=  =cord
    ^-  (unit psbt)
    %+  bind  (de:base64:mimes:html cord)
    |=  =byts
    %-  from-byts
    (flip:byt:bcu:bc byts)
  ::
  ++  to-base64
    %-  en:base64:mimes:html
    (flip:byt:bcu:bc to-byts)
  ::
  ++  from-base16
    |=  =cord
    ^-  (unit psbt)
    %+  bind  (de:base16:mimes:html cord)
    from-byts
  ::
  ++  from-unsigned-tx
    |=  t=tx:^tx
    ^-  psbt
    =+  ^=  inputs
        ^-  (list input)
        %+  turn  vin.t
        |=  i=in:^tx
        (from-input:txin i %.y)
    =+  ^=  outputs
        ^-  (list output)
        (turn vout.t from-output:txout)
    %=  tx
      inputs     inputs
      outputs    outputs
      nversion   nversion.t
      nlocktime  nlocktime.t
      vin        vin.t
      vout       vout.t
    ==
  --
::  +update: updater role
::
++  update
  |_  tx=psbt
  ::
  ++  add-input
    |=  =input
    ^-  psbt
    tx(inputs (snoc inputs.tx input))
  ::
  ++  add-output
    |=  =output
    ^-  psbt
    tx(outputs (snoc outputs.tx output))
  ::
  ++  add-non-witness-utxo
    |=  [i=@u utxo=tx:^tx]
    ^-  psbt
    =+  txin=(snag i inputs.tx)
    =.  non-witness-utxo.txin  `utxo
    tx(inputs (snap inputs.tx i txin))
  ::
  ++  add-witness-utxo
    |=  [i=@u utxo=out:^tx]
    ^-  psbt
    =+  txin=(snag i inputs.tx)
    =.  witness-utxo.txin  `utxo
    tx(inputs (snap inputs.tx i txin))
  ::
  ++  add-redeem-script
    |=  [i=@u script=hexb:bc]
    ^-  psbt
    =+  txin=(snag i inputs.tx)
    =.  redeem-script.txin  `script
    tx(inputs (snap inputs.tx i txin))
  ::
  ++  add-witness-script
    |=  [i=@u script=hexb:bc]
    ^-  psbt
    =+  txin=(snag i inputs.tx)
    =.  witness-script.txin  `script
    tx(inputs (snap inputs.tx i txin))
  ::
  ++  add-signature
    |=  [i=@u =pubkey =signature]
    =+  txin=(snag i inputs.tx)
    =.  txin
      %=  txin
        partial-sigs  (~(put by partial-sigs.txin) pubkey signature)
      ==
    tx(inputs (snap inputs.tx i txin))
  --
::  +sign: signer role
::
++  sign
  |_  tx=psbt
  ::
  +$  shared-fields
    $:  hash-prevouts=hexb:bc
        hash-sequence=hexb:bc
        hash-outputs=hexb:bc
    ==
  ::
  ++  digest-fields
    |^
    ^-  shared-fields
    :*  hash-prevouts=hash-prevouts
        hash-sequence=hash-sequence
        hash-outputs=hash-outputs
    ==
    ++  hash-prevouts
      ^-  hexb:bc
      %-  dsha256:bcu:bc
      %-  cat:byt:bcu:bc
      %+  turn  inputs.tx
      |=  =input
      ^-  hexb:bc
      %-  cat:byt:bcu:bc
      :~  (flip:byt:bcu:bc 256^txid.prevout.input)
          (flip:byt:bcu:bc 4^idx.prevout.input)
      ==
    ::
    ++  hash-sequence
      ^-  hexb:bc
      %-  dsha256:bcu:bc
      %-  cat:byt:bcu:bc
      %+  turn  inputs.tx
      |=  =input
      ^-  hexb:bc
      (flip:byt:bcu:bc 4^nsequence.input)
    ::
    ++  hash-outputs
      ^-  hexb:bc
      %-  dsha256:bcu:bc
      %-  cat:byt:bcu:bc
      %+  turn  outputs.tx
      |=  =output
      ^-  hexb:bc
      ~(en txout output)
    --
  ::
  ++  preimage-script
    |=  i=input
    ^-  hexb:bc
    ?^  witness-script.i
      u.witness-script.i
    ?:  ?&  ?!(~(is-segwit txin i))
            ?=(^ redeem-script.i)
        ==
      u.redeem-script.i
    ~|(%psbt-unknown-script !!)
  ::
  ++  tx-data
    ^-  data:tx
    =|  =data:tx
    =.  is.data
      %+  turn  inputs.tx
      |=  i=input
      =|  =input:tx
      %=  input
        txid        txid.prevout.i
        pos         idx.prevout.i
        sequence    (flip:byt:bcu:bc 4^nsequence.i)
        value       (need ~(value txin i))
        script-sig  script-sig.i
      ==
    =.  os.data
      %+  turn  outputs.tx
      |=  o=output
      =|  =output:tx
      %=  output
        script-pubkey  script-pubkey.o
        value          value.o
      ==
    %=  data
      locktime  nlocktime.tx
      nversion  nversion.tx
    ==
  ::
  ++  witness-preimage
    |=  [i=@ud shared=(unit shared-fields)]
    |^  ^-  hexb:bc
    :: missing other sighash flags
    ?.  =(sig-hash all:sighash)
      ~|(%psbt-sighash-unsupported !!)
    =?  shared  ?=(~ shared)  `digest-fields
    ?>  ?=(^ shared)
    =/  hash-values=(list hexb:bc)
    :~  nversion
        hash-prevouts.u.shared
        hash-sequence.u.shared
        outpoint
        script-code
        value
        nsequence
        hash-outputs.u.shared
        nlocktime
        nhashtyp
    ==
    (cat:byt:bcu:bc hash-values)
    ++  txin
      ^-  input
      (snag i inputs.tx)
    ::
    ++  sig-hash
      ^-  @ux
      (fall sighash:txin all:sighash)
    ::
    ++  nhashtyp
      ^-  hexb:bc
      (flip:byt:bcu:bc 4^sig-hash)
    ::
    ++  nsequence
      ^-  hexb:bc
      (flip:byt:bcu:bc 4^nsequence:txin)
    ::
    ++  value
      ^-  hexb:bc
      (flip:byt:bcu:bc 8^(need ~(value ^txin txin)))
    ::
    ++  nversion
      ^-  hexb:bc
      (flip:byt:bcu:bc 4^nversion.tx)
    ::
    ++  nlocktime
      ^-  hexb:bc
      (flip:byt:bcu:bc 4^nlocktime.tx)
    ::
    ++  outpoint
      ^-  hexb:bc
      ~(en ^outpoint prevout:txin)
    ::
    ++  script-code
      ^-  hexb:bc
      =+  script=(preimage-script txin)
      %-  cat:byt:bcu:bc
      ~[(encode-compact-size wid.script) script]
    --
  ::
  ++  non-witness-preimage
    |=  [i=@ud shared=(unit shared-fields)]
    =+  txin=(snag i inputs.tx)
    =+  sig-hash=(fall sighash.txin all:sighash)
    ?.  =(sig-hash all:sighash)
      ~|(%psbt-sighash-unsupported !!)
    =+  nhashtyp=(flip:byt:bcu:bc 4^sig-hash)
    %-  cat:byt:bcu:bc
    ~[(basic-encode:txu:bc tx-data) nhashtyp]
  ::
  ++  sign-transaction
    =,  secp256k1:secp:crypto
    |=  [hash=hexb:bc =privkey]
    ^-  hexb:bc
    =+  (ecdsa-raw-sign `@uvI`dat.hash dat.privkey)
    %-  flip:byt:bcu:bc
    %-  en:der
    :-  %seq
    :~  [%int r]
        [%int s]
    ==
  ::  +one:sign: sign one input
  ::
  ++  one
    |=  [i=@ud =privkey shared=(unit shared-fields)]
    ^-  hexb:bc
    =+  input=(snag i inputs.tx)
    =+  sig-hash=(fall sighash.input all:sighash)
    =+  ^=  hash
        %-  dsha256:bcu:bc
        ?:  ~(is-segwit txin input)
          (witness-preimage i shared)
        (non-witness-preimage i shared)
    :: ~&  >  "hashed elements"
    :: ~&  hash
    %-  cat:byt:bcu:bc
    :~  (sign-transaction hash privkey)
        1^sig-hash
    ==
  ::  +all:sign: sign all inputs for keys
  ::
  ++  all
    |=  keys=(^map pubkey privkey)
    ^-  psbt
    =+  shared=`digest-fields
    =|  inputs=(list input)
    =+  i=0
    =+  n=(lent inputs.tx)
    |-
    ?:  =(i n)
      tx(inputs inputs)
    =+  input=(snag i inputs.tx)
    =.  input
      %+  roll  pubkeys.input
      |:  [pub=*pubkey input=input]
      ?.  (~(has by keys) pub)
        input
      =+  prv=(~(got by keys) pub)
      =+  sig=(one i prv shared)
      input(partial-sigs (~(put by partial-sigs.input) pub sig))
    $(inputs (snoc inputs input), i +(i))
  --
::  +combine: merge multiple PSBTs
::
++  combine
  |_  tx=psbt
  ::  +one:combine: merge one PSBT with another
  ::
  ++  one
    |=  ty=psbt
    ^-  psbt
    ?<  =(tx ty)
    ~|(%unimplemented !!)
  ::  +all:combine: merge PSBT with a list of PSBTs
  ::
  ++  all
    |=  txs=(list psbt)
    ^-  psbt
    =+  this=tx
    |-
    ?~  txs  this
    $(this (one (head txs)), txs (tail txs))
  --
::  +finalize: finalize all PSBT inputs
::
++  finalize
  |=  tx=psbt
  ^-  psbt
  tx(inputs (turn inputs.tx |=(=input ~(finalize txin input))))
::  +is-complete: have all inputs been signed?
::
++  is-complete
  |=  =psbt
  ^-  ?
  %+  levy  inputs.psbt
  |=  =input
  ~(is-complete txin input)
::  +extract-unsigned: generate unsigned-transaction from psbt data
::
++  extract-unsigned
  |=  =psbt
  ^-  tx:tx
  =|  =tx:tx
  =+  ^=  input-txin
    |=  =input
    ~(to-input txin input)
  =+  ^=  output-txout
    |=  =output
    ~(to-output txout output)
  %=  tx
    vin        (turn inputs.psbt input-txin)
    vout       (turn outputs.psbt output-txout)
    nversion   nversion.psbt
    nlocktime  nlocktime.psbt
  ==
::  +extract: create a network-serialized bitcoin transaction
::
++  extract
  |=  =psbt
  ^-  hexb:bc
  =.  psbt  (finalize psbt)
  :: ~&  "FINALIZED PSBT"
  :: ~&  psbt
  ?:  (is-complete psbt)
    :: ~&  "IS COMPLETE"
    (encode-tx (extract-unsigned psbt))
  (en psbt)
::  +estimated-size: return an estimated virtual tx size in vbytes
::
++  estimated-size
  |=  =psbt
  ^-  @
  %-  virtual-size-from-weight
  %-  estimated-weight
    psbt
::  +estimated-total-size: return an estimated total transaction size in bytes
::
++  estimated-total-size
  |=  =psbt
  ^-  @
  wid:(encode-tx (extract-unsigned psbt))
::
++  virtual-size-from-weight
  |=  weight=@
  ^-  @
  %+  add  (div weight 4)
  ?:((gth (mod weight 4) 0) 1 0)
::  +estimated-witness-size: return an estimate of witness size in bytes
::
++  estimated-witness-size
  |=  =psbt
  ^-  @
  %+  roll  inputs.psbt
  |=  [=input acc=@]
  %+  add  acc
  wid:(encode-witness ~(to-input txin input))
::  +estiamted-base-size: return an estimated base transaction size in bytes
::
++  estimated-base-size
  |=  =psbt
  ^-  @
  %+  sub
    (estimated-total-size psbt)
  (estimated-witness-size psbt)
::  +estimated-weight: return an estimate of transaction weight
::
++  estimated-weight
  |=  =psbt
  ^-  @
  =+  total=(estimated-total-size psbt)
  =+  base=(estimated-base-size psbt)
  (add (mul 3 base) total)
--
