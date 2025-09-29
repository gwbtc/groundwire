/-  ord, urb
/+  der, scr=btc-script, bc=bitcoin, b173=bip-b173
|%
+$  point
  $+  urb-point
  point:secp256k1:secp:crypto
::
+$  outpoint
  $+  urb-outpoint
  [=txid:ord =pos:ord]
::
+$  keypair
  $+  urb-keypair
  [pub=point priv=@]
::
+$  pubkey
  $+  urb-pubkey
  @
::
++  sighash
  |%
  ++  default                   0x0
  ++  all                       0x1
  ++  none                      0x2
  ++  single                    0x3
  ++  anyonecanpay              0x80
  --
++  tx
  =<  tx
  |%
  +$  in
    $+  urb-tx-in
    $:  prevout=outpoint
        nsequence=$~(0xffff.ffff @ux)
        script-witness=(list octs)
    ==
  ::
  +$  out
    $+  urb-tx-out
    $:  script-pubkey=octs
        value=sats:ord
    ==
  ::
  +$  tx
    $+  urb-tx
    $:  vin=(list in)
        vout=(list out)
        nversion=$~(0x2 @)
        nlocktime=@
        inputs=(list input)
        outputs=(list output)
    ==
  ::
  +$  input
    $+  urb-tx-input
    $:  =in
        utxo=output
        sig-hash=$~(0x0 @ux)
    ==
  +$  output
    $+  urb-tx-output
    $:  out
        spend-script=(unit script:scr)
        internal-keys=keypair
    ==
  --
+$  input  input:tx
+$  output  output:tx
::
++  raws
  |=  [eny=@ bits=@]
  ^-  [@ @]
  [- +>-]:(~(raws og eny) bits)
::
++  build
  |_  t=tx
  ::
  ++  add-input-1
    |=  $:  $:  prev=outpoint
                from=output
            ==
            sigh=(unit @ux)
            nseq=(unit @ux)
        ==
    (add-input prev from sigh nseq)
  ::
  ++  add-input
    |=  $:  prev=outpoint
            from=output
            sigh=(unit @ux)
            nseq=(unit @ux)
        ==
    ^-  tx
    =|  n=input
    =.  n
      %_  n
        utxo               from
        prevout.in         prev
        sig-hash           (fall sigh sig-hash.n)
        nsequence.in       (fall nseq nsequence.in.n)
      ==
    t(inputs (snoc inputs.t n))
  ::
  ++  add-output-1
    |=  out=output
    ^-  tx
    t(outputs (snoc outputs.t out), vout (snoc vout.t -:out))
  ::
  ++  add-output
    |=  $:  val=sats:ord
            int-key=keypair
            scr=(unit script:scr)
        ==
    ^-  tx
    =/  o=out:tx  [~(scriptpubkey p2tr `x.pub.int-key scr ~) val]
    =|  =output
    =.  output
      %_  output
        -                  o
        internal-keys      int-key
        spend-script       scr
      ==
    t(outputs (snoc outputs.t output), vout (snoc vout.t o))
  ::
  ++  finalize-input
    |=  [i=@ eny=@]
    ^-  [tx _eny]
    =/  n=input  (snag i inputs.t)
    ?.  =(~ script-witness.in.n)
      [t eny]
    ?~  spend-script.utxo.n
      =/  tpriv=@
        ~(tweak-privkey p2tr `x.pub.internal-keys.utxo.n spend-script.utxo.n `priv.internal-keys.utxo.n)
      =/  twpub=keypair  ~(tweak-keypair p2tr `x.pub.internal-keys.utxo.n ~ `priv.internal-keys.utxo.n)
      =/  address=cord  (need (encode-taproot:b173 %regtest 32^x.pub.twpub))
      =^  sig  eny  (sign-input t i tpriv eny 0)
      =.  script-witness.in.n  ~[sig]
      [t(inputs (snap inputs.t i `input`n)) eny]

    =/  address=cord  (need (encode-taproot:b173 %regtest 32^x.pub.internal-keys.utxo.n))
    =.  script-witness.in.n
      ~(scriptspend p2tr `x.pub.internal-keys.utxo.n spend-script.utxo.n ~)
    =^  sig  eny  (sign-input t i priv.internal-keys.utxo.n eny 1)
    =.  script-witness.in.n  [sig script-witness.in.n]
    [t(inputs (snap inputs.t i `input`n)) eny]
  ::
  ++  finalize
    |=  eny=@
    ^-  [tx _eny]
    =|  i=@
    |-
    ?:  =((lent inputs.t) i)
      t^eny
    =/  n=input  (snag i inputs.t)
      ::  todo: add checks - value (fees), sighash_single, valid nseq and nlock
    ?.  =(~ script-witness.in.n)
      $(i +(i), t t(vin (snoc vin.t in.n)))
    =^  t  eny  (finalize-input i eny)
    $(i +(i), t t(vin (snoc vin.t in:(snag i inputs.t))))
  --
::
++  encode
  |%
  ++  txid
    |=  t=tx
    ^-  @ux
    =<  +
    %-  flipb
    %-  dsha256:bcu:bc
    %-  catb  %-  zing
    ^-  (list (list octs))
    :~  ~[(flipb 4^nversion.t)]
        ~[(encode-compact-size (lent vin.t))]
        (turn vin.t input)
        ~[(encode-compact-size (lent vout.t))]
        (turn vout.t output)
        ~[(flipb 4^nlocktime.t)]
    ==
  ::
  ++  txn
    |=  t=tx
    ^-  octs
    %-  catb  %-  zing
    ^-  (list (list octs))
    :~  ~[(flipb 4^nversion.t)]
        ~[1^0x0 1^0x1]
        ~[(encode-compact-size (lent vin.t))]
        (turn vin.t input)
        ~[(encode-compact-size (lent vout.t))]
        (turn vout.t output)
        (witness vin.t)
        ~[(flipb 4^nlocktime.t)]
    ==
  ::
  ++  input
    |=  =in:tx
    ^-  octs
    %-  catb
    :~  (flipb (to-octs txid.prevout.in))
        (flipb 4^pos.prevout.in)
        1^0x0
        (flipb 4^nsequence.in)
    ==
  ::
  ++  output
    |=  =out:tx
    ^-  octs
    %-  catb
    :~  (flipb 8^value.out)
        (encode-compact-size p.script-pubkey.out)
        script-pubkey.out
    ==
  ::
  ++  witness
    |=  v=(list in:tx)
    ^-  (list octs)
    %+  turn  v
    |=  =in:tx
    ^-  octs
    ?~  script-witness.in
      0^0x0
    %-  catb
    :-  (encode-compact-size (lent script-witness.in))
    %-  zing  %+  turn  script-witness.in
    |=  o=octs
    ^-  (list octs)
    ?:  =(0 p.o)
      ~[1^0x0]
    ~[(encode-compact-size p.o) o]
  --
::
::  taproot spending and construction methods
++  sign-input
  |=  [t=tx i=@ priv=@ eny=@ ext=@]
  ^-  [octs _eny]
  =+  n=(snag i inputs.t)
  =/  neone=?  =(anyonecanpay:sighash (dis sig-hash.n 0x80))
  =/  single=?  =(single:sighash (dis sig-hash.n 3))
  =/  none=?  =(none:sighash (dis sig-hash.n 3))
  |^
  =/  preimage=octs  (catb ~[1^0x0 taproot-preimage])
  =/  msghash=@uvI
    %+  tagged-hash:schnorr:secp256k1:secp:crypto  'TapSighash'
    preimage
  =^  sed  eny  (raws eny 256)
  =/  sig=octs
    %-  to-octs
    (sign:schnorr:secp256k1:secp:crypto priv msghash sed)
  =?  sig  !=(default:sighash sig-hash.n)
    (catb ~[sig 1^sig-hash.n])
  sig^eny
  ::
  ++  taproot-preimage
    ^-  octs
    %-  catb
    :-  1^sig-hash.n
    ;:  welp
      tx-fields
      input-fields
      output-fields
      ext-fields
    ==
  ::
  ++  tx-fields
    |^  ^-  (list octs)
    =/  fields=(list octs)
      :~  (flipb 4^nversion.t)
          (flipb 4^nlocktime.t)
      ==
    =?  fields  !neone
      %+  welp  fields
      :~  sha-prevouts
          sha-amounts
          sha-scriptpubkeys
          sha-sequences
      ==
    =?  fields  &(!single !none)
      (snoc fields sha-outputs)
    fields
    ::
    ++  sha-prevouts
      ^-  octs
      =/  preimage=octs
        (catb (turn inputs.t |=(np=input (encode-outpoint prevout.in.np))))
      (flipb 32^(shay (flipb preimage)))
    ::
    ++  sha-amounts
      ^-  octs
      =/  preimage=octs
        (catb (turn inputs.t |=(np=input (flipb 8^value.utxo.np))))
      (flipb 32^(shay (flipb preimage)))
    ::
    ++  sha-scriptpubkeys
      ^-  octs
      =/  preimage=octs
        %-  catb
        (turn inputs.t |=(np=input (encode-scriptpubkey script-pubkey.utxo.np)))
      (flipb 32^(shay (flipb preimage)))
    ::
    ++  sha-sequences
      ^-  octs
      =/  preimage=octs
        (catb (turn inputs.t |=(np=input (flipb 4^nsequence.in.np))))
      (flipb 32^(shay (flipb preimage)))
    ::
    ++  sha-outputs
      ^-  octs
      =/  preimage=octs
        %-  catb
        %+  turn  outputs.t
        |=  =output
        (output:encode -.output)
      (flipb 32^(shay (flipb preimage)))
    --
  ::
  ++  input-fields
    ::  assume no annex
    ::  assume known ext_flag as of BIP342 (0 for keyspends, 1 for scriptspends)
    ^-  (list octs)
    =/  enc-out=octs  (encode-outpoint prevout.in.n)
    =/  fields=(list octs)
      ?.  neone
        ~[(flipb 4^i)]
      :~  (catb ~[(encode-compact-size p.enc-out) enc-out])
          (flipb 8^value.utxo.n)
          (catb ~[(encode-compact-size 35) script-pubkey.utxo.n])
          (flipb 4^nsequence.in.n)
      ==
    ?:  =(1 ext)
      (into fields 0 1^0x2)
    ?:  =(0 ext)
      (into fields 0 1^0x0)
    !!
  ::
  ++  output-fields
    ^-  (list octs)
    ?.  single
      ~
    :_  ~
    %-  flipb  %-  to-octs
    (shay (output:encode -:(snag i outputs.t)))
  ::
  ++  ext-fields
    ::  assume no OP_CODESEPARATORs
    ^-  (list octs)
    ?:  =(0 ext)
      ~
    ?:  =(1 ext)
      :~  (to-octs ~(tapleaf-hash p2tr `x.pub.internal-keys.utxo.n spend-script.utxo.n ~))
          1^0x0
          4^0xffff.ffff
      ==
    !!
  --
::
++  p2tr
  ::  current: single tapleaf
  =,  secp256k1:secp:crypto
  |_  [p=(unit pubkey) s=(unit script:scr) sec=(unit @)]
  ++  tweak-keypair
    ^-  keypair
    =/  tweaked-seckey=@  tweak-privkey
    :: populated the pubkey sample to match the privkey in case not provided
    [q:tweak-pubkey tweaked-seckey]
  ::
  ++  scriptpubkey
    (catb ~[1^0x51 1^0x20 (to-octs x.q:tweak-pubkey)])
  ::
  ++  tweak-pubkey
    ^-  (pair @ point)
    =/  pt=point  (need (lift-x:schnorr (need p)))
    =/  t=@I  (tweak pt)
    =/  tweaked=point
      (add-points pt (mul-point-scalar g.domain.curve t))
    =/  parity=@  ?:  =(0 (mod y.tweaked 2))  0  1
    [p=parity q=tweaked]
  ::
  ++  tweak-privkey
    ^-  @
    ?~  sec  !!
    =/  pt=point  (mul-point-scalar g.domain.curve u.sec)
    ?:  &(=(^ p) !=(p `x.pt))  ~|(%non-matching-keys !!)
    =.  p  `x.pt
    =/  t=@I  (tweak pt)
    =/  priv=@
      ?:  =(0 (mod y.pt 2))
        u.sec
      (sub n.domain.curve u.sec)
    (mod (add priv t) n.domain.curve)
  ::
  ++  tweak
    |=  =point
    ^-  @I
    ?~  s
      (tagged-hash:schnorr 'TapTweak' (to-octs x.point))
    %+  tagged-hash:schnorr  'TapTweak'
    (catb ~[(to-octs x.point) (to-octs tapleaf-hash)])
  ::
  ++  tapleaf-hash
    ^-  @I
    =/  scrbyt=octs  (en:scr (need s))
    %+  tagged-hash:schnorr  'TapLeaf'
    (catb ~[1^0xc0 (encode-compact-size p.scrbyt) scrbyt])
  ::
  ++  scriptspend
    :: pre-signing (no script inputs, only ser-script and control block)
    :: |=  sin=(list octs)
    ^-  (list octs)
    =/  sscr=octs  (en:scr (need s))
    =+  cbyt=(add 0xc0 p:tweak-pubkey)
    =/  control-block=octs
      %-  catb
      :~  (to-octs cbyt)
          (to-octs (need p))
          :: additional merkle hashes for multileaf taptree would go here - irrelevant for inscriptions rn
      ==
    :: %+  welp  (flop sin)  :: assume script inputs provided in regular exec order
    :~  sscr
        control-block
    ==
  ::
  :: ++  nums-point
  ::   ^-  point
  ::   =/  pt=point
  ::     %-  need
  ::     %-  lift-x:schnorr
  ::     0x5092.9b74.c1a0.4954.b78b.4b60.35e9.7a5e.078a.5a0f.28ec.96d5.47bf.ee9a.ce80.3ac0
  ::   (add-points pt (mul-point-scalar g.domain.curve x.pt))
  --
::
:: consolidated bitcoin utils
:: hex
++  catb
  |=  dats=(list octs)
  ^-  octs
  :-  (roll (turn dats |=(o=octs -.o)) add)
  (can 3 (flop dats))
::
++  to-octs
  |=  dat=@
  ^-  octs
  [(met 3 dat) dat]
::
++  flipb
  |=  o=octs
  ^-  octs
  [p.o (rev 3 o)]
::
++   splitb
  |=  [n=@ o=octs]
  ^-  [octs octs]
  =+  rest=(sub p.o n)
  :-  [n (rsh [3 (sub p.o n)] q.o)]
  [rest (end [3 rest] q.o)]
::
:: encoding
++  encode-compact-size
  |=  a=@ 
  ^-  octs
  =+  n=(met 3 a)
  ?:  =(n 0)     1^a
  ?:  =(n 1)     1^a
  ?:  =(n 2)     (catb ~[1^0xfd (flipb 2^a)])
  ?:  (lte n 4)  (catb ~[1^0xfe (flipb 4^a)])
  ?:  (lte n 8)  (catb ~[1^0xff (flipb 8^a)])
  ~|(%invalid-compact-size !!)
::
++  read-compact-size
  |=  b=octs
  ^-  [a=@ rest=octs]
  =^  s  b  (splitb 1 b)
  ?:  (lth +.s 0xfd)  [+.s b]
  ~|  %invalid-compact-size
  =/  len=bloq
    ?+  +.s  !!
      %0xfd  1
      %0xfe  2
      %0xff  3
    ==
  =^  k  b  (splitb (bex len) b)
  :_  b
  q:(flipb k)
::
++  encode-outpoint
  |=  o=outpoint
  ^-  octs
  %-  catb
  :~  (flipb 32^txid.o)
      (flipb 4^pos.o)
  ==
::
++  encode-scriptpubkey
  |=  spk=octs
  (catb ~[(encode-compact-size p.spk) spk])
--
