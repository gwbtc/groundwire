::  script.hoon
::
/+  bc=bitcoin-utils
|%
++  script
  =<  script-label
  |%
  +$  script  (list op)
  +$  script-label  $+(script script)
  --
::
+$  op
  $@  $?  %op-nop
          %op-if
          %op-notif
          %op-else
          %op-endif
          %op-verify
          %op-return
      ::
          %op-toaltstack
          %op-fromaltstack
          %op-ifdup
          %op-depth
          %op-drop
          %op-dup
          %op-nip
          %op-over
          %op-pick
          %op-roll
          %op-rot
          %op-swap
          %op-tuck
          %op-2drop
          %op-2dup
          %op-3dup
          %op-2over
          %op-2rot
          %op-2swap
      ::
          %op-cat
          %op-substr
          %op-left
          %op-right
          %op-size
      ::
          %op-invert
          %op-and
          %op-or
          %op-xor
          %op-equal
          %op-equalverify
      ::
          %op-1add
          %op-1sub
          %op-2mul
          %op-2div
          %op-negate
          %op-abs
          %op-not
          %op-0notequal
          %op-add
          %op-sub
          %op-mul
          %op-div
          %op-mod
          %op-lshift
          %op-rshift
          %op-booland
          %op-boolor
          %op-numequal
          %op-numequalverify
          %op-numnotequal
          %op-lessthan
          %op-greaterthan
          %op-lessthanorequal
          %op-greaterthanorequal
          %op-min
          %op-max
          %op-within
      ::
          %op-ripemd160
          %op-sha1
          %op-sha256
          %op-hash160
          %op-hash256
          %op-codeseparator
          %op-checksig
          %op-checksigverify
          %op-checkmultisig
          %op-checkmultisigverify
      ::
          %op-checklocktimeverify
          %op-checksequenceverify
      ::
          %op-pubkeyhash
          %op-pubkey
          %op-invalidopcode
      ::
          %op-reserved
          %op-ver
          %op-verif
          %op-vernotif
          %op-reserved1
          %op-reserved2
      ::
          %op-nop1
          %op-nop4
          %op-nop5
          %op-nop6
          %op-nop7
          %op-nop8
          %op-nop9
          %op-nop10
      ==
   $:  %op-push
       $%  [p=%num octs=[p=%1 q=@]]
           [p=?(~ %1 %2 %4) =octs]
       ==
   ==
::
++  en
  |=  =script
  |^  ^-  octs
  =-  [r p]
  %-  fax:plot  :-  bloq=3
  %-  flop
  (turn script encode-op)
  ::
  ++  encode-op
    |=  =op
    ^-  plat:plot
    ?:  ?=(^ op)
      ?-  p.op
          %num
        :-  1
        ?:  =(q.octs.op 0x81)  0x4f
        ?:  =(0 q.octs.op)  0
        ?>  (lte q.octs.op 0x10)
        (add 0x50 q.octs.op)
      ::
          *  [%s ~]^(encode-pushdata +.op)
      ==
    ::
    ?-  op
      %op-reserved             1^0x50
    ::
      %op-nop                  1^0x61
      %op-ver                  1^0x62
      %op-if                   1^0x63
      %op-notif                1^0x64
      %op-verif                1^0x65
      %op-vernotif             1^0x66
      %op-else                 1^0x67
      %op-endif                1^0x68
      %op-verify               1^0x69
      %op-return               1^0x6a
      %op-toaltstack           1^0x6b
      %op-fromaltstack         1^0x6c
      %op-2drop                1^0x6d
      %op-2dup                 1^0x6e
      %op-3dup                 1^0x6f
      %op-2over                1^0x70
      %op-2rot                 1^0x71
      %op-2swap                1^0x72
      %op-ifdup                1^0x73
      %op-depth                1^0x74
      %op-drop                 1^0x75
      %op-dup                  1^0x76
      %op-nip                  1^0x77
      %op-over                 1^0x78
      %op-pick                 1^0x79
      %op-roll                 1^0x7a
      %op-rot                  1^0x7b
      %op-swap                 1^0x7c
      %op-tuck                 1^0x7d
      ::
      %op-cat                  1^0x7e
      %op-substr               1^0x7f
      %op-left                 1^0x80
      %op-right                1^0x81
      %op-size                 1^0x82
      %op-invert               1^0x83
      %op-and                  1^0x84
      %op-or                   1^0x85
      %op-xor                  1^0x86
      %op-equal                1^0x87
      %op-equalverify          1^0x88
      %op-reserved1            1^0x89
      %op-reserved2            1^0x8a
      ::
      %op-1add                 1^0x8b
      %op-1sub                 1^0x8c
      %op-2mul                 1^0x8d
      %op-2div                 1^0x8e
      %op-negate               1^0x8f
      %op-abs                  1^0x90
      %op-not                  1^0x91
      %op-0notequal            1^0x92
      %op-add                  1^0x93
      %op-sub                  1^0x94
      %op-mul                  1^0x95
      %op-div                  1^0x96
      %op-mod                  1^0x97
      %op-lshift               1^0x98
      %op-rshift               1^0x99
      %op-booland              1^0x9a
      %op-boolor               1^0x9b
      %op-numequal             1^0x9c
      %op-numequalverify       1^0x9d
      %op-numnotequal          1^0x9e
      %op-lessthan             1^0x9f
      %op-greaterthan          1^0xa0
      %op-lessthanorequal      1^0xa1
      %op-greaterthanorequal   1^0xa2
      %op-min                  1^0xa3
      %op-max                  1^0xa4
      %op-within               1^0xa5
      ::
      %op-ripemd160            1^0xa6
      %op-sha1                 1^0xa7
      %op-sha256               1^0xa8
      %op-hash160              1^0xa9
      %op-hash256              1^0xaa
      %op-codeseparator        1^0xab
      %op-checksig             1^0xac
      %op-checksigverify       1^0xad
      %op-checkmultisig        1^0xae
      %op-checkmultisigverify  1^0xaf
      ::
      %op-checklocktimeverify  1^0xb1
      %op-checksequenceverify  1^0xb2
      ::
      %op-pubkeyhash           1^0xfd
      %op-pubkey               1^0xfe
      %op-invalidopcode        1^0xff
      ::
      %op-nop1                 1^0xb0
      %op-nop4                 1^0xb3
      %op-nop5                 1^0xb4
      %op-nop6                 1^0xb5
      %op-nop7                 1^0xb6
      %op-nop8                 1^0xb7
      %op-nop9                 1^0xb8
      %op-nop10                1^0xb9
    ==
  ::
  ++  encode-pushdata
    |=  [a=?(~ %1 %2 %4) b=byts]
    ^-  plot
    :-  bloq=3
    %-  flop
    ?>  (lte (met 3 +.b) dat.b)
    ~|  %push-data-too-big-for-opcode
    ?-    a
        ~
      ?>  (lte wid.b 0x4b)
      ~[[1 wid.b] wid.b^(rev 3 wid.b dat.b)]
    ::
        %1
      ?>  (lte wid.b 0xff)
      ~[1^0x4c [1 wid.b] wid.b^(rev 3 wid.b dat.b)]
    ::
        %2
      ?>  (lte wid.b 0xffff)
      ~[1^0x4d [2 (rev 3 2 wid.b)] wid.b^(rev 3 wid.b dat.b)]
    ::
        %4
      ?>  (lte wid.b 0xffff.ffff)
      ~[1^0x4e [4 (rev 3 4 wid.b)] wid.b^(rev 3 wid.b dat.b)]
    ==
  --
::
++  de
  |=  a=octs
  ^-  (unit script)
  ?:  =(p.a 0)  ~ :: `~
  =/  n  (dec p.a)
  |-  ^-  (unit script)
  =/  op  (cut 3 [n 1] q.a)
  =-  ?~  -  ~
      ?:  =(n.u 0)  `op.u^~
      ?~  rest=%_($ n (dec n.u))  ~
      `op.u^u.rest
  ^-  (unit [=^op n=@])
  ?:  &(!=(0 op) (lte op 0x4b)) :: push next `op` bytes
    ?:  (lth n op)  ~
    =.  n  (sub n op)
    =/  dat  (rev 3 op (cut 3 [n op] q.a))
    `[%op-push ~ op dat]^n
  ?:  ?=(%0x4c op) :: op_pushdata1
    ?:  =(n 0)  ~
    =.  n  (dec n)
    =/  len  (cut 3 [n 1] q.a)
    ?:  (lth n len)  ~
    =.  n  (sub n len)
    =/  dat  (rev 3 len (cut 3 [n len] q.a))
    `[%op-push %1 len dat]^n
  ?:  ?=(%0x4d op) :: op_pushdata2
    ?:  (lth n 2)  ~
    =.  n  (sub n 2)
    =/  len  (rev 3 2 (cut 3 [n 2] q.a))
    ?:  (lth n len)  ~
    =.  n  (sub n len)
    =/  dat  (rev 3 len (cut 3 [n len] q.a))
    `[%op-push %2 len dat]^n
  ?:  ?=(%0x4e op) :: op_pushdata4
    ?:  (lth n 4)  ~
    =.  n  (sub n 4)
    =/  len  (rev 3 4 (cut 3 [n 4] q.a))
    ?:  (lth n len)  ~
    =.  n  (sub n len)
    =/  dat  (rev 3 len (cut 3 [n len] q.a))
    `[%op-push %4 len dat]^n
  =-  ?~(- ~ `u^n)
  ^-  (unit ^op)
  ?:  &((lth 0x50 op) (lte op 0x60))  :: op_1..op_16
    `[%op-push %num %1 (sub op 0x50)]
  ?+  op  ~
    %0     `[%op-push %num %1 0]
    %0x4f  `[%op-push %num %1 0x81]
    %0x50  `%op-reserved
  ::
    %0x61  `%op-nop
    %0x62  `%op-ver
    %0x63  `%op-if
    %0x64  `%op-notif
    %0x65  `%op-verif
    %0x66  `%op-vernotif
    %0x67  `%op-else
    %0x68  `%op-endif
    %0x69  `%op-verify
    %0x6a  `%op-return
    %0x6b  `%op-toaltstack
    %0x6c  `%op-fromaltstack
    %0x6d  `%op-2drop
    %0x6e  `%op-2dup
    %0x6f  `%op-3dup
    %0x70  `%op-2over
    %0x71  `%op-2rot
    %0x72  `%op-2swap
    %0x73  `%op-ifdup
    %0x74  `%op-depth
    %0x75  `%op-drop
    %0x76  `%op-dup
    %0x77  `%op-nip
    %0x78  `%op-over
    %0x79  `%op-pick
    %0x7a  `%op-roll
    %0x7b  `%op-rot
    %0x7c  `%op-swap
    %0x7d  `%op-tuck
    ::
    %0x7e  `%op-cat
    %0x7f  `%op-substr
    %0x80  `%op-left
    %0x81  `%op-right
    %0x82  `%op-size
    %0x83  `%op-invert
    %0x84  `%op-and
    %0x85  `%op-or
    %0x86  `%op-xor
    %0x87  `%op-equal
    %0x88  `%op-equalverify
    %0x89  `%op-reserved1
    %0x8a  `%op-reserved2
    ::
    %0x8b  `%op-1add
    %0x8c  `%op-1sub
    %0x8d  `%op-2mul
    %0x8e  `%op-2div
    %0x8f  `%op-negate
    %0x90  `%op-abs
    %0x91  `%op-not
    %0x92  `%op-0notequal
    %0x93  `%op-add
    %0x94  `%op-sub
    %0x95  `%op-mul
    %0x96  `%op-div
    %0x97  `%op-mod
    %0x98  `%op-lshift
    %0x99  `%op-rshift
    %0x9a  `%op-booland
    %0x9b  `%op-boolor
    %0x9c  `%op-numequal
    %0x9d  `%op-numequalverify
    %0x9e  `%op-numnotequal
    %0x9f  `%op-lessthan
    %0xa0  `%op-greaterthan
    %0xa1  `%op-lessthanorequal
    %0xa2  `%op-greaterthanorequal
    %0xa3  `%op-min
    %0xa4  `%op-max
    %0xa5  `%op-within
    ::
    %0xa6  `%op-ripemd160
    %0xa7  `%op-sha1
    %0xa8  `%op-sha256
    %0xa9  `%op-hash160
    %0xaa  `%op-hash256
    %0xab  `%op-codeseparator
    %0xac  `%op-checksig
    %0xad  `%op-checksigverify
    %0xae  `%op-checkmultisig
    %0xaf  `%op-checkmultisigverify
    ::
    %0xb1  `%op-checklocktimeverify
    %0xb2  `%op-checksequenceverify
    ::
    %0xfd  `%op-pubkeyhash
    %0xfe  `%op-pubkey
    %0xff  `%op-invalidopcode
    ::
    %0xb0  `%op-nop1
    %0xb3  `%op-nop4
    %0xb4  `%op-nop5
    %0xb5  `%op-nop6
    %0xb6  `%op-nop7
    %0xb7  `%op-nop8
    %0xb8  `%op-nop9
    %0xb9  `%op-nop10
  ==
--
