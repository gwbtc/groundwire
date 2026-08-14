::  tests/lib/gw-btc-pass.hoon
::
::  Golden values are pinned by /vectors/gw-kelvin-9.json and were
::  cross-checked against an independent python implementation of the
::  BIP-340 tagged hashes.  Any change here is a protocol change.
::
/-  ord, sa=self-attestation
/+  *test, cc=gw-btc-pass, taproot, btc=bitcoin, bcu=bitcoin-utils
|%
++  spawn  ^-  sont:ord  [txid=0x1234.5678.9abc.def0 vout=1 off=0]
++  seed   0xdead.beef
++  blind  (make-blind:cc seed)
++  dat    (make-dat:cc spawn blind)
++  snap   ^-  snapshot:sa  [life=1 rift=0 key=0xabcd sponsor=~ fief=~]
++  key
  |=  [sed=@ xtr=@]
  =<  pub:ex
  %:  pit:nu:cric:crypto
      512  (shaz sed)
      %c   dat
      xtr
  ==
::
++  test-golden-blind
  %+  expect-eq
    !>  0xf0de.dc6a.72ec.b8c1.6b5d.af25.2b8d.b53b.2510.4f32.c2d9.e9bc.3459.db71.6535.ef2e
    !>  blind
::
++  test-golden-spawn-commit
  %+  expect-eq
    !>  0x134a.ab80.f09d.97d8.0efc.2573.570e.2767.279a.ad7b.43ea.2740.18a7.570d.26fc.57e0
    !>  (spawn-commit:cc spawn blind)
::
++  test-golden-dat
  %+  expect-eq
    !>  0x2.6955.701e.13b2.fb01.df84.ae6a.e1c4.ece4.f355.af68.7d44.e803.14ea.e1a4.df8a.fc12.4637.4622.d776.77c0
    !>  `@ux`dat
::
++  test-golden-state-commit
  %+  expect-eq
    !>  0xc31d.0c5c.e2b3.5a94.79ce.af7b.284a.fac8.e955.324c.0b40.59f6.bdc8.b182.22aa.ba75
    !>  (state-commit:cc snap)
::
++  test-golden-state-leaf
  %+  expect-eq
    !>  ^-  tapleaf:taproot
        :+  0xc0  37
        0x6a.0267.7720.c31d.0c5c.e2b3.5a94.79ce.af7b.284a.fac8.e955.324c.0b40.59f6.bdc8.b182.22aa.ba75
    !>  (state-leaf:cc (state-commit:cc snap))
::
++  test-golden-state-key
  ::  P = 02 || secp generator x; Q = +state-key over the basic snapshot
  =/  p  0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
  %+  expect-eq
    !>  0xc136.d880.9683.7f8d.167e.6141.555a.6e1d.0068.e24b.7df6.a973.7cea.44c8.29ab.1e7b
    !>  (state-key:cc p snap)
::
++  test-dat-roundtrip
  %+  expect-eq
    !>(`(unit [dom=@tas kel=@ud d=@ux])``[%gw-btc 9 (spawn-commit:cc spawn blind)])
    !>((parse-dat:cc dat))
::
++  test-dat-rejects-trailing
  %+  expect-eq
    !>(*(unit [dom=@tas kel=@ud d=@ux]))
    !>((parse-dat:cc (can 0 ~[[(met 0 dat) dat] [8 0xab]])))
::
++  test-verify-dat
  ;:  weld
    (expect-eq !>(%.y) !>((verify-dat:cc dat [spawn 0 blind])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [spawn 0 +(blind)])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [[0x1234.5678.9abc.def0 2 0] 0 blind])))
  ==
::
++  test-pass-roundtrip
  =/  pas  (key 'gw-btc-pass' 0x1234)
  %+  expect-eq
    !>  ^-  (unit [dom=@tas kel=@ud d=@ux xtr=@])
        `[%gw-btc 9 (spawn-commit:cc spawn blind) 0x1234]
    !>((parse-pass:cc pas))
::
++  test-same-key-ignores-xtr
  %+  expect-eq
    !>(%.y)
    !>((same-key:cc (key 'same' 0x1) (key 'same' 0x2)))
::
++  test-same-key-rejects-other-key
  %+  expect-eq
    !>(%.n)
    !>((same-key:cc (key 'one' 0x1) (key 'two' 0x1)))
::
++  test-same-key-rejects-malformed-pass
  ;:  weld
    (expect-eq !>(%.n) !>((same-key:cc 42 (key 'valid' 0x1))))
    (expect-eq !>(%.n) !>((same-key:cc (key 'valid' 0x1) 42)))
  ==
::
++  test-publication-roundtrip-small
  =/  payload=hexb:btc  [4 0xcafe.babe]
  %+  expect-eq
    !>(`(unit [kel=@ud payload=hexb:btc])``[9 payload])
    !>((parse-publication:cc (publication-script:cc payload)))
::
++  test-publication-roundtrip-pushdata
  =/  payload=hexb:btc  [100 (fil 3 100 0xab)]
  %+  expect-eq
    !>(`(unit [kel=@ud payload=hexb:btc])``[9 payload])
    !>((parse-publication:cc (publication-script:cc payload)))
::
++  test-publication-rejects-garbage
  %+  expect-eq
    !>(*(unit [kel=@ud payload=hexb:btc]))
    !>((parse-publication:cc [5 0x6a.6a6a.6a6a]))
::  ------------------------------------------------------------------
::  Publication payload BYTE ORDER.
::
::  The payload is the jam's ordinary LITTLE-endian byte dump (+jam-octs),
::  the same convention as every hash preimage in this lib and as
::  Causeway's jam_bytes.  The shipped encoder/decoder instead used the raw
::  jam atom as a big-endian byte string; that pair was self-consistent and
::  inconsistent with everything else, so the desk could neither write a
::  publication Causeway could read nor read a real one off the chain.
::
::  +test-publication-real-onchain is the one that matters: it decodes the
::  ACTUAL mainnet OP_RETURN of the public comet
::  ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd (spawn
::  ec5c1fbe... in block 961.059) and re-derives the name, the spawn
::  satpoint and the sat output's P2TR key from it alone.
::  ------------------------------------------------------------------
++  test-publication-payload-is-jam-octs
  =/  pub  ^-(publication:sa [(key 'pubkey' 0) [0x2 snap ~]])
  =/  script  (make-publication:cc pub)
  =/  env  (parse-publication:cc script)
  ;:  weld
    (expect !>(?=(^ env)))
    ::  the payload IS +jam-octs, not the raw jam atom read big-endian
    (expect-eq !>((jam-octs:cc pub)) !>(payload:(need env)))
    ::  ... and reading it back reproduces the publication exactly
    (expect-eq !>(`(unit publication:sa)``pub) !>((read-publication:cc script)))
  ==
::
++  c3-onchain-opret
  ^-  hexb:btc
  :-  263
  0x6a.0375.7262.0109.4cfe.01e0.d7b1.e431.8330.b6a4.cbf6.1189.b748.431a.
    2f0d.b8d5.2c9d.5a33.a360.b913.daec.f231.3fac.2b1c.523e.5eb8.af73.6889.
    af70.cab7.8248.c063.3675.d362.06af.effe.b27a.194b.986f.008b.80ef.ecae.
    458c.6e8c.2405.5937.5259.bd9d.df4e.c3e0.104e.c2b4.931a.7bcc.f74d.2dea.
    253c.ed83.3454.eaa3.6900.1418.b843.cfe7.6c4d.bfa4.06af.b441.8126.649f.
    5346.aba7.318c.f0e1.1467.2d5d.7f4f.09b7.3803.20c0.150e.291f.2fdc.d739.
    b4c4.5738.e55b.4124.e031.9bba.6931.83d7.777f.59bd.8c25.cc77.9a05.e09f.
    8a37.559b.eece.5d7e.3f66.0661.aad9.6486.3c4f.6444.b4e1.3f7c.de42.6863.
    5981.461e.9b00.0448.208c.b04b.69dd.4170.9a02.b357.5877.1ba7.b840.3e86.
    f2d1.1450.f26b.623b.bd79.5d07
::
++  test-publication-real-onchain
  =/  pub  (read-publication:cc c3-onchain-opret)
  ?~  pub
    (expect-eq !>('decodes') !>('SHIPPED DECODER CANNOT READ A REAL PUBLICATION'))
  =*  o    opening.u.pub
  =/  cic  (com:nu:cric:crypto pass.u.pub)
  ?.  ?=(%c suite.+<.cic)
    (expect-eq !>('suite-c') !>('published pass is not suite-C'))
  ;:  weld
    ::  the published pass fingerprints to the comet's real @p
    %+  expect-eq
      !>  `@p`~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd
      !>  `@p`fig:ex:cic
    ::  the blind-opening opens that pass's own hiding dat commitment
    (expect !>(?=(^ blind-opening.o)))
    (expect !>((verify-dat:cc dat.tw.pub.+<.cic (need blind-opening.o))))
    ::  ... to the real spawn satpoint 72340acb...:1
    %+  expect-eq
      !>  ^-  sont:ord
          :+  0x7234.0acb.1b42.16f3.e1ff.0da2.2322.79e4.3326.cd53.0833.31fb.f2ee.7774.daa9.bc54
            1
          0
      !>  spawn:(need blind-opening.o)
    ::  the published snapshot is life 1 / rift 0 with the messaging key
    %+  expect-eq
      !>  ^-  snapshot:sa
          :*  life=1
              rift=0
              key=0xdf30.9632.f565.fddf.5e0c.c5a6.ea6c.c780.9105.6f94.e15f.12d0.e75f.70bc.7ca4.3857
              sponsor=~
              fief=~
          ==
      !>  snapshot.o
    ::  and it recomputes the sat output's on-chain P2TR key exactly
    %+  expect-eq
      !>  0xca2.828c.764a.0f3e.76e3.3df0.7be9.8703.06ec.5650.af8e.cae8.8f6f.0c64.2ec8.0bee
      !>  (state-key:cc internal-key.o snapshot.o)
  ==
::  ------------------------------------------------------------------
::  OP_PUSHDATA2 -- the publication that does not fit in one length byte
::
::  OP_PUSHDATA1 (0x4c) carries a SINGLE length byte and therefore stops
::  at 255, but +max-publication is 1.024 and every fief-carrying
::  publication measures 265-269 bytes.  Before +push-data learned
::  OP_PUSHDATA2 (0x4d, TWO length bytes, LITTLE-ENDIAN), `[1 wid]` with
::  wid > 255 packed to wid mod 256 and this lib emitted a silently
::  corrupt script -- no crash, no parse, an unreadable OP_RETURN on
::  chain.  These arms pin the encoding at every boundary and make the
::  inexpressible cases loud.
::  ------------------------------------------------------------------
::
::  +push-hdr: the 3 bytes after the 7-byte OP_RETURN envelope
++  push-hdr
  |=  n=@ud
  ^-  @ux
  =/  script  (publication-script:cc [n (fil 3 n 0xab)])
  dat:(take:byt:bcu 3 (drop:byt:bcu 7 script))
::
::  the minimal push opcode at each boundary, per Bitcoin's own rules
++  test-publication-push-opcodes
  ;:  weld
    ::  1..75: a direct push -- the opcode IS the length
    (expect-eq !>(0x3.abab) !>((push-hdr 3)))
    (expect-eq !>(0x4b.abab) !>((push-hdr 75)))
    ::  76..255: OP_PUSHDATA1, one length byte
    (expect-eq !>(0x4c.4cab) !>((push-hdr 76)))
    (expect-eq !>(0x4c.feab) !>((push-hdr 254)))
    (expect-eq !>(0x4c.ffab) !>((push-hdr 255)))
    ::  256..: OP_PUSHDATA2, two LITTLE-ENDIAN length bytes.
    ::  256 = 0x0100 -> 00 01;  269 = 0x010d -> 0d 01;  512 = 0x0200 -> 00 02
    (expect-eq !>(0x4d.0001) !>((push-hdr 256)))
    (expect-eq !>(0x4d.0d01) !>((push-hdr 269)))
    (expect-eq !>(0x4d.0002) !>((push-hdr 512)))
    (expect-eq !>(0x4d.0004) !>((push-hdr 1.024)))
  ==
::
::  encode -> parse -> compare, across the 255/256 boundary the bug lived on
++  test-publication-roundtrip-boundaries
  %-  zing
  %+  turn  `(list @ud)`~[1 75 76 77 254 255 256 269 511 512 1.023 1.024]
  |=  n=@ud
  ^-  tang
  =/  payload=hexb:btc  [n (fil 3 n 0xab)]
  %+  expect-eq
    !>(`(unit [kel=@ud payload=hexb:btc])``[9 payload])
    !>((parse-publication:cc (publication-script:cc payload)))
::
::  what the encoder cannot express it must CRASH on, never emit
++  test-publication-refuses-the-inexpressible
  ;:  weld
    ::  one byte over the spec's cap
    %-  expect-fail  |.
    (publication-script:cc [1.025 (fil 3 1.025 0xab)])
    ::  wider than OP_PUSHDATA2 can name
    %-  expect-fail  |.
    (push-data:cc [65.536 (fil 3 65.536 0xab)])
    ::  a `dat` too wide for its declared `wid` -- the other way to emit a
    ::  script whose length field lies about its payload
    %-  expect-fail  |.
    (publication-script:cc [1 0x12c])
  ==
::
::  a leading byte that is not a push this codec emits is refused, not
::  read as a length: 0x4d now MEANS OP_PUSHDATA2, and 0x4e/0xff never
::  meant anything
++  test-publication-rejects-non-push-opcodes
  =/  bad
    |=  [opc=@ux n=@ud]
    ^-  hexb:btc
    %-  cat:byt:bcu
    ~[[5 0x6a.0375.7262] [1 0x1] [1 9] [1 opc] [n (fil 3 n 0xab)]]
  ;:  weld
    (expect-eq !>(*(unit [kel=@ud payload=hexb:btc])) !>((parse-publication:cc (bad 0x4e 78))))
    (expect-eq !>(*(unit [kel=@ud payload=hexb:btc])) !>((parse-publication:cc (bad 0xff 255))))
    ::  0x4d with a two-byte length that does not match the tail
    (expect-eq !>(*(unit [kel=@ud payload=hexb:btc])) !>((parse-publication:cc (bad 0x4d 77))))
  ==
::  THE SPLIT.  "Not a Groundwire output" and "a Groundwire output we
::  cannot read" must be DIFFERENT answers.  They were the same one:
::  the whole of +parse-publication sat inside a +mole, envelope match
::  included, so a script that IS ours with a broken push came back ~
::  and vanished into +read-publication's hot filter alongside the
::  millions of scripts that simply are not ours.  A comet past the
::  ~17-hop ceiling that published anyway produced zero log lines on
::  every watcher on the network, and its transaction was not retained.
::
::  The unit is ~ in both cases -- that is not what changed and is not
::  what this pins.  What it pins is that +publication-envelope
::  SEPARATES them, which is what lets +read-publication announce the
::  first and stay silent about the second.
::
++  test-publication-envelope-tells-ours-from-unreadable
  =/  bad
    |=  [opc=@ux n=@ud]
    ^-  hexb:btc
    %-  cat:byt:bcu
    ~[[5 0x6a.0375.7262] [1 0x1] [1 9] [1 opc] [n (fil 3 n 0xab)]]
  ::  a 7-byte OP_RETURN that is not ours: right length, wrong tag
  =/  alien  `hexb:btc`[7 0x6a.0311.2233.4455]
  ;:  weld
    ::  OURS, and unreadable.  0x4e is not a push opcode this codec
    ::  emits; 0x4d 77 is a length that lies about its own tail.
    (expect !>((publication-envelope:cc (bad 0x4e 78))))
    (expect !>((publication-envelope:cc (bad 0x4d 77))))
    ::  Both on the READ path too, because they fail at different points
    ::  and only the read path announces.  0x4e is refused by the opcode
    ::  test; 0x4d 77 gets past it and dies on `=(len wid.rst)`, so a
    ::  regression could plausibly close one route and not the other.
    (expect-eq !>(*(unit publication:sa)) !>((read-publication:cc (bad 0x4e 78))))
    (expect-eq !>(*(unit publication:sa)) !>((read-publication:cc (bad 0x4d 77))))
    ::  NOT ours: the envelope says no, and the silence is correct.
    (expect !>(!(publication-envelope:cc alien)))
    (expect-eq !>(*(unit publication:sa)) !>((read-publication:cc alien)))
    ::  The filter is TOTAL.  Every output of every transaction in every
    ::  block reaches it, including scripts far too short to slice -- so
    ::  it must answer, not crash.
    (expect !>(!(publication-envelope:cc `hexb:btc`[3 0x6a.0375])))
    (expect !>(!(publication-envelope:cc `hexb:btc`[0 0x0])))
  ==
::  ------------------------------------------------------------------
::  Golden vector "pushdata2-fief" (/vectors/gw-kelvin-9.json).
::
::  A realistic public spawn: a real 108-byte suite-%c pass, a snapshot
::  committing a fief, a full blind-opening.  269 bytes of payload, so
::  the script carries OP_PUSHDATA2 `4d 0d 01`.  Causeway desktop
::  (Python) and Causeway web (TS) pin the SAME bytes from the SAME JSON
::  -- that agreement is the point of the vector.
::  ------------------------------------------------------------------
++  v2-pass
  ^-  pass
  0x2aac.0e0b.79f6.b4fc.f9da.c3f6.cf75.f5ba.fdb4.c907.33ed.a811.
    7d62.eeb8.9440.e15a.4918.dd18.8b5d.d9df.0112.007e.5662.ba9c.
    7b41.343c.da61.5d33.8992.f009.596a.b8a2.8912.084b.ffd7.93ab.
    d02b.ae4d.45ed.4517.a917.38d2.eeab.1876.aded.7707.3533.e061.
    c872.748e.b161.1c02.e4a6.4063
::
++  v2-snapshot
  ^-  snapshot:sa
  :*  life=1
      rift=0
      key=0x7e56.62ba.9c7b.4134.3cda.615d.3389.92f0.
             0959.6ab8.a289.1208.4bff.d793.abd0.2bae
      sponsor=~
      fief=`[%if .64.227.13.22 35.353]
  ==
::
++  v2-internal-key
  ^-  @ux
  0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
    029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
::
++  v2-publication
  ^-  publication:sa
  :-  v2-pass
  :*  internal-key=v2-internal-key
      snapshot=v2-snapshot
      :-  ~
      :+  :+  txid=0xa1b2.c3d4.e5f6.0718.293a.4b5c.6d7e.8f90.
                       a1b2.c3d4.e5f6.0718.293a.4b5c.6d7e.8f90
            vout=1
          off=0
        start-height=961.059
      blind=0x345b.c3c5.fc0e.b35b.ee8d.ce5d.e886.b130.
               6e79.67e4.e111.b009.ab9e.13d2.2850.6127
  ==
::
++  v2-payload
  ^-  hexb:btc
  :-  269
  0x1.a0d7.3120.5372.018e.b058.473a.39e4.30f0.999a.83bb.f656.3b8c.
    5577.699c.8bd4.8ba2.f6a2.26d7.15e8.d5c9.ebff.2504.8944.515c.
    b5ac.0478.c9c4.99ae.306d.1e9a.a03d.4e5d.312b.3f00.8980.efec.
    ae45.8c6e.8c24.ad70.204a.5c77.b1be.08d4.f699.8364.da7e.ddfa.
    ba67.fb61.ed7c.7e5a.fbbc.0507.5635.000a.605e.e05b.6c05.ca67.
    65a3.38b7.6cf3.6f0a.1c2c.1c3a.578a.8156.b1ee.72e7.fb99.f9e6.
    599c.01f8.775d.815e.9dbc.fe5f.4290.4814.c555.cb4a.8097.4c9c.
    e90a.d3e6.a109.dae3.d415.b3f2.6706.3ecd.1cf0.2d1a.c681.40c8.
    50cc.0220.00e4.a35f.1bd7.924e.0ac6.817d.39f5.b06c.28e4.a35f.
    1bd7.924e.0ac6.817d.39f5.b06c.686c.401a.5175.00fd.2761.5028.
    d213.9eab.09b0.11e1.e467.796e.30b1.86e8.5dce.8dee.5bb3.0efc.
    c5c3.5b34
::
++  v2-script
  ^-  hexb:btc
  :-  279
  0x6a.0375.7262.0109.4d0d.0101.a0d7.3120.5372.018e.b058.473a.
    39e4.30f0.999a.83bb.f656.3b8c.5577.699c.8bd4.8ba2.f6a2.26d7.
    15e8.d5c9.ebff.2504.8944.515c.b5ac.0478.c9c4.99ae.306d.1e9a.
    a03d.4e5d.312b.3f00.8980.efec.ae45.8c6e.8c24.ad70.204a.5c77.
    b1be.08d4.f699.8364.da7e.ddfa.ba67.fb61.ed7c.7e5a.fbbc.0507.
    5635.000a.605e.e05b.6c05.ca67.65a3.38b7.6cf3.6f0a.1c2c.1c3a.
    578a.8156.b1ee.72e7.fb99.f9e6.599c.01f8.775d.815e.9dbc.fe5f.
    4290.4814.c555.cb4a.8097.4c9c.e90a.d3e6.a109.dae3.d415.b3f2.
    6706.3ecd.1cf0.2d1a.c681.40c8.50cc.0220.00e4.a35f.1bd7.924e.
    0ac6.817d.39f5.b06c.28e4.a35f.1bd7.924e.0ac6.817d.39f5.b06c.
    686c.401a.5175.00fd.2761.5028.d213.9eab.09b0.11e1.e467.796e.
    30b1.86e8.5dce.8dee.5bb3.0efc.c5c3.5b34
::
++  test-golden-pushdata2-publication
  =/  script  (make-publication:cc v2-publication)
  ;:  weld
    ::  the payload is 269 bytes -- past OP_PUSHDATA1's ceiling
    (expect-eq !>(v2-payload) !>((jam-octs:cc v2-publication)))
    ::  ... and the script is byte-identical to the shared vector
    (expect-eq !>(v2-script) !>(script))
    ::  ... and reads back to exactly the publication we encoded
    (expect-eq !>(`(unit publication:sa)``v2-publication) !>((read-publication:cc script)))
    ::  the fief-carrying snapshot's taproot output key, also pinned in
    ::  the vector, so a fief silently dropped from the jam would show
    %+  expect-eq
      !>  0x6c60.baeb.b2f8.880a.20ee.d197.7ef2.2eae.
             b93c.f147.f287.661c.12f8.1f83.7240.0d41
      !>  (state-key:cc v2-internal-key v2-snapshot)
  ==
::
::  ... and the "basic" vector, whose 69-byte payload takes the DIRECT
::  push (0x45).  Pinned here too so the <=75 path cannot drift while
::  OP_PUSHDATA2 is added above; Causeway desktop and web assert the same
::  op_return_script bytes from the same JSON.
++  test-golden-basic-publication-script
  =/  pub=publication:sa
    :-  0xdead.beef.cafe
    :*  internal-key=0x2.cafe
        snapshot=`snapshot:sa`[life=2 rift=0 key=0xabcd sponsor=`~zod fief=~]
        :-  ~
        :+  [txid=0x1234.5678.9abc.def0 vout=1 off=0]
          start-height=778.000
        blind=0xf0de.dc6a.72ec.b8c1.6b5d.af25.2b8d.b53b.
                 2510.4f32.c2d9.e9bc.3459.db71.6535.ef2e
    ==
  %+  expect-eq
    !>  ^-  hexb:btc
        :-  77
        0x6a.0375.7262.0109.4501.427f.e577.df56.ef80.e2af.6c21.3320.
          34af.969a.05d8.e1bd.7935.f1ac.6864.6c40.82f8.5e00.02b8.bcd7.
          94c5.6d67.d1f0.a667.0bcb.3c41.94ec.d436.ae94.bc76.ad05.e3b2.
          cba9.717b.c303
    !>  (make-publication:cc pub)
::
::  ------------------------------------------------------------------
::  Golden vector "full-packet" (/vectors/gw-kelvin-9.json).
::
::  THE VECTOR THAT MOVED THE CAP.  A kelvin-9 publication is the
::  comet's whole attestation packet -- the pass it hands a peer over
::  ames, custody log and all -- plus the opening for the hop the
::  carrying transaction performs, which the payload cannot name
::  because that transaction's txid does not exist until it is signed.
::
::  Six carried hops (the same 108-byte suite-%c pass as pushdata2-fief,
::  re-encoded around them) jam to 392 bytes of xtr, a 500-byte pass and
::  a 588-byte payload.  That is past the old 512-byte cap and inside
::  the new 1.024 -- which is the whole argument for the change,
::  measured rather than asserted.  Causeway desktop (Python) and
::  Causeway web (TS) pin these same bytes from this same JSON.
::  ------------------------------------------------------------------
++  fp-key
  ^-  @
  0x7e56.62ba.9c7b.4134.3cda.615d.3389.92f0.
    0959.6ab8.a289.1208.4bff.d793.abd0.2bae
::
++  fp-ikey
  ^-  @ux
  0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
    029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
::
++  fp-spawn
  ^-  sont:ord
  :+  txid=0xa1b2.c3d4.e5f6.0718.293a.4b5c.6d7e.8f90.
              a1b2.c3d4.e5f6.0718.293a.4b5c.6d7e.8f90
    vout=1
  off=0
::
++  fp-blind
  ^-  @ux
  0x345b.c3c5.fc0e.b35b.ee8d.ce5d.e886.b130.
    6e79.67e4.e111.b009.ab9e.13d2.2850.6127
::
++  fp-snap0
  ^-  snapshot:sa
  [life=1 rift=0 key=fp-key sponsor=~ fief=`[%if .64.227.13.22 35.353]]
++  fp-snap6
  ^-  snapshot:sa
  [life=4 rift=1 key=fp-key sponsor=`~zod fief=`[%if .64.227.13.22 35.353]]
::  entry 0 opens the hiding dat commitment; the terminal opening -- the
::  hop this transaction performs -- never may.
::
++  fp-open0
  ^-  opening:sa
  [fp-ikey fp-snap0 `[fp-spawn start-height=961.055 fp-blind]]
++  fp-open6  ^-(opening:sa [fp-ikey fp-snap6 ~])
::
++  fp-log
  ^-  custody-log:sa
  :~  [0x1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111 961.059 `fp-open0]
      [0x2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222.2222 961.104 ~]
      [0x3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333 961.240 ~]
      [0x4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444 961.388 ~]
      [0x5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555.5555 961.512 ~]
      [0x6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666 961.744 ~]
  ==
::  the 108-byte pass BEFORE the log rides in it (pushdata2-fief's pass)
::
++  fp-pass-empty
  ^-  pass
  0x2aac.0e0b.79f6.b4fc.f9da.c3f6.cf75.f5ba.fdb4.c907.33ed.a811.
    7d62.eeb8.9440.e15a.4918.dd18.8b5d.d9df.0112.007e.5662.ba9c.
    7b41.343c.da61.5d33.8992.f009.596a.b8a2.8912.084b.ffd7.93ab.
    d02b.ae4d.45ed.4517.a917.38d2.eeab.1876.aded.7707.3533.e061.
    c872.748e.b161.1c02.e4a6.4063
::
++  fp-script
  ^-  hexb:btc
  :-  598
  0x6a03.7572.6201.094d.4c02.0180.991f.0332.2517.e008.8b75.a493.
    430e.039f.a939.b86b.6fb5.c358.7597.c6b9.48bd.286a.2f6a.725d.
    815e.9dbc.fe5f.4290.4814.c555.cb4a.8097.4c9c.e90a.d3e6.a109.
    dae3.d415.b3f2.0390.08f8.ceee.5ac4.e8c6.48d2.0a07.a2c4.7517.
    eb8b.406d.9f39.48a6.edd7.adaf.7bb6.1fd6.cee7.a7b5.cf5b.7060.
    550b.c07e.4444.4444.4444.4444.4444.4444.4444.4444.4444.4444.
    4444.4444.4444.4444.4444.440c.4823.aa9e.0150.00f3.02df.622b.
    503e.2b1b.c5b9.659b.7f53.e060.e1d0.b952.0cb4.8a75.973b.dfcf.
    cc37.cfe2.0cc0.bfeb.0af4.eae4.f5ff.1282.44a2.28ae.5a56.02bc.
    64e2.4c57.9836.0f4d.d01e.a7ae.9895.3f33.f069.e680.6fd1.300e.
    0442.8662.1600.0120.1ffd.dab8.9674.5230.0eec.cba9.8765.4321.
    1ffd.dab8.9674.5230.0eec.cba9.8765.4363.03d2.87aa.03e8.3f09.
    8342.919e.f05c.4d80.8d08.273f.cb73.8389.3544.ef72.6e74.df9a.
    75e0.2f1e.dea2.0b40.bf88.8888.8888.8888.8888.8888.8888.8888.
    8888.8888.8888.8888.8888.8888.8888.8888.1890.a054.dd02.d03f.
    3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.
    3333.3333.3333.3307.246c.55b7.00fc.1311.1111.1111.1111.1111.
    1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.0312.
    dbaa.5b00.feab.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.
    aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aa01.097d.d52d.00ff.6666.6666.
    6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.6666.
    6666.66e6.8004.cdea.1a00.0530.2ff0.2db6.02e5.b3b2.519c.5bb6.
    f937.050e.160e.9d2b.c540.ab58.77b9.f3fd.cc7c.f32c.cc38.00ff.
    ae2b.d0ab.93d7.ff4b.0812.89a2.b86a.5909.f092.8933.5d61.da3c.
    3441.7b9c.ba62.56fe.d20c.7c9a.39e0.5b34.8c03.8190.a128
::
::  A seven-hop packet, published on chain, byte for byte.
::
++  test-golden-full-packet-publication
  =/  xtr  (jam fp-log)
  =/  full  (need (with-xtr:cc fp-pass-empty xtr))
  =/  pub   `publication:sa`[full fp-open6]
  =/  payload  (jam-octs:cc pub)
  =/  script   (make-publication:cc pub)
  ;:  weld
    ::  the measurements the cap argument rests on
    ::
    (expect-eq !>(392) !>((met 3 xtr)))
    (expect-eq !>(108) !>((met 3 fp-pass-empty)))
    (expect-eq !>(500) !>((met 3 full)))
    (expect-eq !>(588) !>(wid.payload))
    ::  ... which is exactly what 512 could not carry, and 1.024 can
    ::
    (expect !>((gth wid.payload 512)))
    (expect !>((lte wid.payload max-publication:cc)))
    ::  the script, byte for byte, shared with Python and TS
    ::
    (expect-eq !>(fp-script) !>(script))
    ::  it reads back to exactly the publication we encoded
    ::
    (expect-eq !>(`(unit publication:sa)``pub) !>((read-publication:cc script)))
    ::  and the log rode in without touching the NAME: xtr is outside
    ::  the key tweak, so the comet is the same comet
    ::
    %+  expect-eq
      !>(fig:ex:(com:nu:cric:crypto fp-pass-empty))
      !>(fig:ex:(com:nu:cric:crypto full))
    (expect !>((same-key:cc fp-pass-empty full)))
  ==
::
::  The cap is a LOUD refusal, at the new boundary.
::
++  test-publication-cap-is-the-packet-bound
  ;:  weld
    (expect-eq !>(1.024) !>(max-publication:cc))
    ::  1.024 exactly is accepted; 1.025 crashes with a named reason
    ::
    (expect !>(?=(^ (publication-script:cc [1.024 (fil 3 1.024 0xab)]))))
    (expect-fail |.((publication-script:cc [1.025 (fil 3 1.025 0xab)])))
  ==
::
::  ---------------------------------------------------------------------
::  +with-xtr -- the pass a %anew refresh emits
::
::  The custody log grows; the NAME does not.  xtr is excluded from the
::  key tweak, so re-encoding a pass around a longer log must leave `fig`
::  (and the messaging key, and dat) untouched.  The identity case is the
::  one that keeps this in step with the kernel's own encoder: if
::  +with-xtr and +pub:ex:cric ever disagree by one bit, a refreshed pass
::  stops hashing to our @p and ames drops it.
::  ---------------------------------------------------------------------
::
++  xtr-a  (jam ~[[0xdead 900.001 ~]])
++  xtr-b  (jam ~[[0xdead 900.001 ~] [0xbeef 900.050 ~]])
::
::  re-encoding with the SAME xtr must reproduce the pass byte-for-byte
++  test-with-xtr-is-identity-on-the-same-log
  =/  p  (key 'anew-probe' xtr-a)
  %+  expect-eq
    !>  `(unit pass)`(some p)
    !>  (with-xtr:cc p xtr-a)
::
::  a LONGER log leaves the name, the messaging key and dat unchanged
++  test-with-xtr-preserves-the-identity
  =/  p    (key 'anew-probe' xtr-a)
  =/  p2   (need (with-xtr:cc p xtr-b))
  =/  m    (need (parse-pass:cc p))
  =/  m2   (need (parse-pass:cc p2))
  ;:  weld
    (expect-eq !>(fig:ex:(com:nu:cric:crypto p)) !>(fig:ex:(com:nu:cric:crypto p2)))
    (expect-eq !>(d.m) !>(d.m2))
    (expect-eq !>(dom.m) !>(dom.m2))
    (expect-eq !>(kel.m) !>(kel.m2))
    ::  ... and the new log really is in there
    (expect-eq !>(xtr-b) !>(xtr.m2))
    ::  the messaging half is untouched
    %+  expect-eq
      !>  `@`cry.pub.+<:(com:nu:cric:crypto p)
      !>  `@`cry.pub.+<:(com:nu:cric:crypto p2)
    ::  and it is not the pass we started from
    (expect !>(!=(p p2)))
  ==
::
::  an empty log round-trips to the pass minted with no xtr at all
++  test-with-xtr-empty-matches-a-bare-pass
  %+  expect-eq
    !>  `(unit pass)`(some (key 'anew-probe' 0))
    !>  (with-xtr:cc (key 'anew-probe' xtr-a) 0)
::
::  a pass that is not suite-%c (a vanilla ship's, or junk) is refused
::  rather than mangled -- %anew then answers with silence.
++  test-with-xtr-refuses-a-non-suite-c-pass
  =/  vanilla  pub:ex:(pit:nu:cric:crypto 512 (shaz 'b-probe') %b ~)
  ;:  weld
    (expect-eq !>(`(unit pass)`~) !>((with-xtr:cc vanilla xtr-a)))
    (expect-eq !>(`(unit pass)`~) !>((with-xtr:cc `pass`0x0 xtr-a)))
  ==
--
