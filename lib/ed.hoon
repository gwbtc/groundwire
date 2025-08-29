=,  ames
=,  number
|%
++  ted
    =>  ed:crypto
    |%
    ::                                                  ::  ++sign:ed:crypto
    ++  sign-tw                                         ::  certify
      ~/  %sign-tw
      |=  [m=@ se=@ tw=@]  ^-  @
      =+  sk=(suck-tw se tw)
      =+  pk=(cut 0 [b b] sk)
      =+  h-sk=(shal (rsh [0 3] b) sk)
      =+  h-tw=(shal (rsh [0 3] b) tw)
      =+  ^=  a-sk
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h-sk))
      =+  ^=  a-tw
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h-tw))
      =+  a=(~(sit fo l) (add a-tw a-sk))
      =+  ^=  r
          =+  hm=(cut 0 [b b] (mod (add h-sk h-tw) (bex 512)))
          =+  ^=  i
              %+  can  0
              :~  [b hm]
                  [(met 0 m) m]
              ==
          (shaz i)
      =+  rr=(scam bb r)
      =+  ^=  ss
          =+  er=(etch rr)
          =+  ^=  ha
              %+  can  0
              :~  [b er]
                  [b pk]
                  [(met 0 m) m]
              ==
          (~(sit fo l) (add r (mul (shaz ha) a)))
      (can 0 ~[[b (etch rr)] [b ss]])
    ::
    ++  puck-tw                                         ::  public key
      ~/  %puck-tw
      |=  [sk=@I tw=@I]  ^-  @
      ?:  (gth (met 3 sk) 32)  !!
      =+  h-sk=(shal (rsh [0 3] b) sk)
      =+  h-tw=(shal (rsh [0 3] b) tw)
      =+  ^=  a-sk
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h-sk))
      =+  ^=  a-tw
          %+  add
            (bex (sub b 2))
          (lsh [0 3] (cut 0 [3 (sub b 5)] h-tw))
      =+  aa=(scam bb (~(sit fo l) (add a-sk a-tw)))
      (etch aa)
    ::                                                  ::  ++suck:ed:crypto
    ++  suck-tw                                         ::  keypair from seed
      |=  [se=@I tw=@I]  ^-  @uJ
      =+  pu=(puck-tw se tw)
      (can 0 ~[[b se] [b pu]])
    --
--
