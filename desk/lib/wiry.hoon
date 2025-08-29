::!.
=>  %c50
~%  %c.50  ~  ~
|%
::  Types
::
+$  ship  @p
+$  life  @ud
+$  rift  @ud
+$  pass  @
+$  bloq  @
+$  step  _`@u`1
+$  bite  $@(bloq [=bloq =step])
+$  octs  [p=@ud q=@]
+$  byts  [wid=@ud dat=@]                               ::  bytes, MSB first
+$  mold  $~(* $-(* *))
++  unit  |$  [item]  $@(~ [~ u=item])
++  list  |$  [item]  $@(~ [i=item t=(list item)])
+$  gate
  ::    function
  ::
  ::  a core with one arm, `$`--the empty name--which transforms a sample noun
  ::  into a product noun. If used dryly as a type, the subject must have a
  ::  sample type of `*`.
  $-(* *)
++  lest  |$  [item]  [i=item t=(list item)]
++  tree  |$  [node]  $@(~ [n=node l=(tree node) r=(tree node)])
++  pair  |$  [head tail]  [p=head q=tail]
++  each
  |$  [this that]
  ::    either {a} or {b}, defaulting to {a}.
  ::
  ::  mold generator: produces a discriminated fork between two types,
  ::  defaulting to {a}.
  ::
  $%  [%| p=that]
      [%& p=this]
  ==
++  map
  |$  [key value]
  $|  (tree (pair key value))
  |=(a=(tree (pair)) ?:(=(~ a) & ~(apt by a)))
::
++  set
  |$  [item]
  $|  (tree item)
  |=(a=(tree) ?:(=(~ a) & ~(apt in a)))
::
++  jug   |$  [key value]  (map key (set value))
::
::  Bits
::
++  dec                                                 ::  decrement
  ~/  %dec
  |=  a=@
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
::
++  add                                                 ::  plus
  ~/  %add
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
::
++  sub                                                 ::  subtract
  ~/  %sub
  |=  [a=@ b=@]
  ~_  leaf+"subtract-underflow"
  ::  difference
  ^-  @
  ?:  =(0 b)  a
  $(a (dec a), b (dec b))
::
++  mul                                                 ::  multiply
  ~/  %mul
  |:  [a=`@`1 b=`@`1]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)  c
  $(a (dec a), c (add b c))
::
++  div                                                 ::  divide
  ~/  %div
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ~_  leaf+"divide-by-zero"
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)  c
  $(a (sub a b), c +(c))
::
++  dvr                                                 ::  divide w/remainder
  ~/  %dvr
  |:  [a=`@`1 b=`@`1]
  ^-  [p=@ q=@]
  [(div a b) (mod a b)]
::
++  egcd
  |=  [a=@ b=@]
  =+  [u=[a=1 b=0] v=[a=0 b=1]]
  |-  ^-  [b=@ u=@ v=@]
  ?:  =(0 a)
    [b b.u b.v]
  =+  q=(div b a)
  %=  $
    a  (sub b (mul q a))
    b  a
    u  [(sub b.u (mul q a.u)) a.u]
    v  [(sub b.v (mul q a.v)) a.v]
  ==
::
++  max
  ~/  %max
  ::    unsigned maximum
  |=  [a=@ b=@]
  ::  the maximum
  ^-  @
  ?:  (gth a b)  a
  b
::
++  min
  ~/  %min
  ::    unsigned minimum
  |=  [a=@ b=@]
  ::  the minimum
  ^-  @
  ?:  (lth a b)  a
  b
::
++  mod                                                 ::  modulus
  ~/  %mod
  |:  [a=`@`1 b=`@`1]
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  weld                                                ::  concatenate
  ~/  %weld
  |*  [a=(list) b=(list)]
  =>  .(a ^.(homo a), b ^.(homo b))
  |-  ^+  b
  ?~  a  b
  [i.a $(a t.a)]
::
++  turn
  ~/  %turn
  |*  [a=(list) b=gate]
  =>  .(a (homo a))
  ^-  (list _?>(?=(^ a) (b i.a)))
  |-
  ?~  a  ~
  [i=(b i.a) t=$(a t.a)]
::
++  murn                                                ::  maybe transform
  ~/  %murn
  |*  [a=(list) b=$-(* (unit))]
  =>  .(a (homo a))
  |-  ^-  (list _?>(?=(^ a) (need (b i.a))))
  ?~  a  ~
  =/  c  (b i.a)
  ?~  c  $(a t.a)
  [+.c $(a t.a)]
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  [a=(list) b=_=>(~ |=([* *] +<+))]
  |-  ^+  ,.+<+.b
  ?~  a
    +<+.b
  $(a t.a, b b(+<+ (b i.a +<+.b)))
::
++  zing                                                ::  promote
  ~/  %zing
  |*  *
  ?~  +<
    +<
  (welp +<- $(+< +<+))
::
++  bex                                                 ::  binary exponent
  ~/  %bex
  |=  a=bloq
  ^-  @
  ?:  =(0 a)  1
  (mul 2 $(a (dec a)))
::
++  lsh                                                 ::  left-shift
  ~/  %lsh
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (mul b (bex (mul (bex bloq) step)))
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (div b (bex (mul (bex bloq) step)))
::
++  run                                                 ::  +turn into atom
  ~/  %run
  |=  [a=bite b=@ c=$-(@ @)]
  (rep a (turn (rip a b) c))
::
++  con                                                 ::  binary or
  ~/  %con
  |=  [a=@ b=@]
  =+  [c=0 d=0]
  |-  ^-  @
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?&  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  dis                                                 ::  binary and
  ~/  %dis
  |=  [a=@ b=@]
  =|  [c=@ d=@]
  |-  ^-  @
  ?:  ?|(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   %+  add  d
          %+  lsh  [0 c]
          ?|  =(0 (end 0 a))
              =(0 (end 0 b))
          ==
  ==
::
++  mix                                                 ::  binary xor
  ~/  %mix
  |=  [a=@ b=@]
  ^-  @
  =+  [c=0 d=0]
  |-
  ?:  ?&(=(0 a) =(0 b))  d
  %=  $
    a   (rsh 0 a)
    b   (rsh 0 b)
    c   +(c)
    d   (add d (lsh [0 c] =((end 0 a) (end 0 b))))
  ==
::
++  lth                                                 ::  less
  ~/  %lth
  |=  [a=@ b=@]
  ^-  ?
  ?&  !=(a b)
      |-
      ?|  =(0 a)
          ?&  !=(0 b)
              $(a (dec a), b (dec b))
  ==  ==  ==
::
++  lte                                                 ::  less or equal
  ~/  %lte
  |=  [a=@ b=@]
  |(=(a b) (lth a b))
::
++  gte                                                 ::  greater or equal
  ~/  %gte
  |=  [a=@ b=@]
  ^-  ?
  !(lth a b)
::
++  gth                                                 ::  greater
  ~/  %gth
  |=  [a=@ b=@]
  ^-  ?
  !(lte a b)
::
++  swp                                                 ::  naive rev bloq order
  ~/  %swp
  |=  [a=bloq b=@]
  (rep a (flop (rip a b)))
::
++  met                                                 ::  measure
  ~/  %met
  |=  [a=bloq b=@]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 b)  c
  $(b (rsh a b), c +(c))
::
++  end                                                 ::  tail
  ~/  %end
  |=  [a=bite b=@]
  =/  [=bloq =step]  ?^(a a [a *step])
  (mod b (bex (mul (bex bloq) step)))
::
++  fil                                                 ::  fill bloqstream
  ~/  %fil
  |=  [a=bloq b=step c=@]
  =|  n=@ud
  =.  c  (end a c)
  =/  d  c
  |-  ^-  @
  ?:  =(n b)
    (rsh a d)
  $(d (add c (lsh a d)), n +(n))
::
++  cat                                                 ::  concatenate
  ~/  %cat
  |=  [a=bloq b=@ c=@]
  (add (lsh [a (met a b)] c) b)
::
++  cut                                                 ::  slice
  ~/  %cut
  |=  [a=bloq [b=step c=step] d=@]
  (end [a c] (rsh [a b] d))
::
++  rev
  ::    reverses block order, accounting for leading zeroes
  ::
  ::  boz: block size
  ::  len: size of dat, in boz
  ::  dat: data to flip
  ~/  %rev
  |=  [boz=bloq len=@ud dat=@]
  ^-  @
  =.  dat  (end [boz len] dat)
  %+  lsh
    [boz (sub len (met boz dat))]
  (swp boz dat)
::
++  can                                                 ::  assemble
  ~/  %can
  |=  [a=bloq b=(list [p=step q=@])]
  ^-  @
  ?~  b  0
  (add (end [a p.i.b] q.i.b) (lsh [a p.i.b] $(b t.b)))
::
++  cad                                                 ::  assemble specific
  ~/  %cad
  |=  [a=bloq b=(list [p=step q=@])]
  ^-  [=step @]
  :_  (can a b)
  |-
  ?~  b
    0
  (add p.i.b $(b t.b))
::
++  rep                                                 ::  assemble fixed
  ~/  %rep
  |=  [a=bite b=(list @)]
  =/  [=bloq =step]  ?^(a a [a *step])
  =|  i=@ud
  |-  ^-  @
  ?~  b   0
  %+  add  $(i +(i), b t.b)
  (lsh [bloq (mul step i)] (end [bloq step] i.b))
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  [a=bite b=@]
  ^-  (list @)
  ?:  =(0 b)  ~
  [(end a b) $(b (rsh a b))]
::
++  rap                                                 ::  assemble variable
  ~/  %rap
  |=  [a=bloq b=(list @)]
  ^-  @
  ?~  b  0
  (cat a i.b $(b t.b))
::
++  as-octs                                             ::  atom to octstream
  |=  tam=@  ^-  octs
  [(met 3 tam) tam]
::
++  plot
  =>  |%
      +$  plat
        $@  @                                       ::  measure atom
        $^  $%  [[%c ~] (pair (pair step step) @)]  ::  cut slice
                [[%m ~] (pair (pair step step) @)]  ::  measure slice
                [[%s ~] p=plot]                     ::  subslice
            ==                                      ::
        (pair step @)                               ::  prefix
      --                                            ::
  =<  $
  ~%  %plot  ..plot  ~
  |%
  ++  $
    $^  [l=$ r=$]                                   ::  concatenate
    [a=bloq b=(list plat)]                          ::  serialize
  ::
  ++  fax                                           ::  encode
    ~/  %fax
    |=  p=$
    ^-  (trel @ bloq step)
    ?^  -.p
      =/  l  $(p l.p)
      =/  r  $(p r.p)
      =/  s  (rig +.l q.r)
      [(add p.l (lsh [q.r s] p.r)) q.r (add r.r s)]
    ::
    ?~  b.p  [0 a.p 0]
    =;  c=(pair @ step)
      =/  d  $(b.p t.b.p)
      [(add p.c (lsh [a.p q.c] p.d)) a.p (add q.c r.d)]
    ::
    ?@  i.b.p
      [i.b.p (^met a.p i.b.p)]
    ?-  -.i.b.p
      @       [(end [a.p p.i.b.p] q.i.b.p) p.i.b.p]
      [%c ~]  [(cut a.p [p q]:i.b.p) q.p.i.b.p]
      [%m ~]  =+((cut a.p [p q]:i.b.p) [- (^met a.p -)])
      [%s ~]  =/  e  $(p p.i.b.p)
              [p.e (rig +.e a.p)]
    ==
  ::
  ++  met                                           ::  measure
    ~/  %met
    |=(p=$ `(pair bloq step)`+:(fax p))
  --
::
++  rub                                                 ::  length-decode
  ~/  %rub
  |=  [a=@ b=@]
  ^-  [p=@ q=@]
  =+  ^=  c
      =+  [c=0 m=(met 0 b)]
      |-  ?<  (gth c m)
      ?.  =(0 (cut 0 [(add a c) 1] b))
        c
      $(c +(c))
  ?:  =(0 c)
    [1 0]
  =+  d=(add a +(c))
  =+  e=(add (bex (dec c)) (cut 0 [d (dec c)] b))
  [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]
::
++  need                                                ::  demand
  ~/  %need
  |*  a=(unit)
  ?~  a  ~>(%mean.'need' !!)
  u.a
::
++  some                                                ::  lift (pure)
  |*  a=*
  [~ u=a]
::
++  fall                                                ::  default
  |*  [a=(unit) b=*]
  ?~(a b u.a)
::
++  biff                                                ::  apply
  |*  [a=(unit) b=$-(* (unit))]
  ?~  a  ~
  (b u.a)
::
::
::  Lists
::
++  lent                                                ::  length
  ~/  %lent
  |=  a=(list)
  ^-  @
  =+  b=0
  |-
  ?~  a  b
  $(a t.a, b +(b))
::
++  levy
  ~/  %levy                                             ::  all of
  |*  [a=(list) b=$-(* ?)]
  |-  ^-  ?
  ?~  a  &
  ?.  (b i.a)  |
  $(a t.a)
::
++  slag                                                ::  suffix
  ~/  %slag
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  =(0 a)  b
  ?~  b  ~
  $(b t.b, a (dec a))
::
++  snag                                                ::  index
  ~/  %snag
  |*  [a=@ b=(list)]
  |-  ^+  ?>(?=(^ b) i.b)
  ?~  b
    ~_  leaf+"snag-fail"
    !!
  ?:  =(0 a)  i.b
  $(b t.b, a (dec a))
::
++  sort  !.                                            ::  quicksort
  ~/  %sort
  |*  [a=(list) b=$-([* *] ?)]
  =>  .(a ^.(homo a))
  |-  ^+  a
  ?~  a  ~
  =+  s=(skid t.a |:(c=i.a (b c i.a)))
  %+  weld
    $(a p.s)
  ^+  t.a
  [i.a $(a q.s)]
::
++  skid                                                ::  separate
  ~/  %skid
  |*  [a=(list) b=$-(* ?)]
  |-  ^+  [p=a q=a]
  ?~  a  [~ ~]
  =+  c=$(a t.a)
  ?:((b i.a) [[i.a p.c] q.c] [p.c [i.a q.c]])
::
++  homo                                                ::  homogenize
  |*  a=(list)
  ^+  =<  $
    |@  ++  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  flop                                                ::  reverse
  ~/  %flop
  |*  a=(list)
  =>  .(a (homo a))
  ^+  a
  =+  b=`_a`~
  |-
  ?~  a  b
  $(a t.a, b [i.a b])
::
++  gulf                                                ::  range inclusive
  |=  [a=@ b=@]
  ?>  (lte a b)
  |-  ^-  (list @)
  ?:(=(a +(b)) ~ [a $(a +(a))])
::
++  welp                                                ::  concatenate
  ~/  %welp
  =|  [* *]
  |@
  ++  $
    ?~  +<-
      +<-(. +<+)
    +<-(+ $(+<- +<->))
  --
::
++  reap                                                ::  replicate
  ~/  %reap
  |*  [a=@ b=*]
  |-  ^-  (list _b)
  ?~  a  ~
  [b $(a (dec a))]
::
::  Modular arithmetic
::
++  fe                                                  ::  modulo bloq
  |_  a=bloq
  ++  dif                                               ::  difference
    |=([b=@ c=@] (sit (sub (add out (sit b)) (sit c))))
  ++  inv  |=(b=@ (sub (dec out) (sit b)))              ::  inverse
  ++  net  |=  b=@  ^-  @                               ::  flip byte endianness
           =>  .(b (sit b))
           ?:  (lte a 3)
             b
           =+  c=(dec a)
           %+  con
             (lsh c $(a c, b (cut c [0 1] b)))
           $(a c, b (cut c [1 1] b))
  ++  out  (bex (bex a))                                ::  mod value
  ++  rol  |=  [b=bloq c=@ d=@]  ^-  @                  ::  roll left
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (lsh [b g] e) (rsh [b (sub f g)] e)))
  ++  ror  |=  [b=bloq c=@ d=@]  ^-  @                  ::  roll right
           =+  e=(sit d)
           =+  f=(bex (sub a b))
           =+  g=(mod c f)
           (sit (con (rsh [b g] e) (lsh [b (sub f g)] e)))
  ++  sum  |=([b=@ c=@] (sit (add b c)))                ::  wrapping add
  ++  sit  |=(b=@ (end a b))                            ::  enforce modulo
  --
++  fo                                                  ::  modulo prime
  ^|
  |_  a=@
  ++  dif
    |=  [b=@ c=@]
    (sit (sub (add a b) (sit c)))
  ::
  ++  exp
    |=  [b=@ c=@]
    ?:  =(0 b)
      1
    =+  d=$(b (rsh 0 b))
    =+  e=(pro d d)
    ?:(=(0 (end 0 b)) e (pro c e))
  ::
  ++  fra
    |=  [b=@ c=@]
    (pro b (inv c))
  ::
  ++  inv
    |=  b=@
    =+  c=(mod u:(egcd b a) a)
    c
  ::
  ++  pro
    |=  [b=@ c=@]
    (sit (mul b c))
  ::
  ++  sit
    |=  b=@
    (mod b a)
  ::
  ++  sum
    |=  [b=@ c=@]
    (sit (add b c))
  --
::
::  Hashes
::
++  muk                                                 ::  standard murmur3
  ~%  %muk  ..muk  ~
  =+  ~(. fe 5)
  |=  [syd=@ len=@ key=@]
  =.  syd      (end 5 syd)
  =/  pad      (sub len (met 3 key))
  =/  data     (welp (rip 3 key) (reap pad 0))
  =/  nblocks  (div len 4)  ::  intentionally off-by-one
  =/  h1  syd
  =+  [c1=0xcc9e.2d51 c2=0x1b87.3593]
  =/  blocks  (rip 5 key)
  =/  i  nblocks
  =.  h1  =/  hi  h1  |-
    ?:  =(0 i)  hi
    =/  k1  (snag (sub nblocks i) blocks)  ::  negative array index
    =.  k1  (sit (mul k1 c1))
    =.  k1  (rol 0 15 k1)
    =.  k1  (sit (mul k1 c2))
    =.  hi  (mix hi k1)
    =.  hi  (rol 0 13 hi)
    =.  hi  (sum (sit (mul hi 5)) 0xe654.6b64)
    $(i (dec i))
  =/  tail  (slag (mul 4 nblocks) data)
  =/  k1    0
  =/  tlen  (dis len 3)
  =.  h1
    ?+  tlen  h1  ::  fallthrough switch
      %3  =.  k1  (mix k1 (lsh [0 16] (snag 2 tail)))
          =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %2  =.  k1  (mix k1 (lsh [0 8] (snag 1 tail)))
          =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
      %1  =.  k1  (mix k1 (snag 0 tail))
          =.  k1  (sit (mul k1 c1))
          =.  k1  (rol 0 15 k1)
          =.  k1  (sit (mul k1 c2))
          (mix h1 k1)
    ==
  =.  h1  (mix h1 len)
  |^  (fmix32 h1)
  ++  fmix32
    |=  h=@
    =.  h  (mix h (rsh [0 16] h))
    =.  h  (sit (mul h 0x85eb.ca6b))
    =.  h  (mix h (rsh [0 13] h))
    =.  h  (sit (mul h 0xc2b2.ae35))
    =.  h  (mix h (rsh [0 16] h))
    h
  --
::
++  mug                                                 ::  mug with murmur3
  ~/  %mug
  |=  a=*
  |^  ?@  a  (mum 0xcafe.babe 0x7fff a)
      =/  b  (cat 5 $(a -.a) $(a +.a))
      (mum 0xdead.beef 0xfffe b)
  ::
  ++  mum
    |=  [syd=@uxF fal=@F key=@]
    =/  wyd  (met 3 key)
    =|  i=@ud
    |-  ^-  @F
    ?:  =(8 i)  fal
    =/  haz=@F  (muk syd wyd key)
    =/  ham=@F  (mix (rsh [0 31] haz) (end [0 31] haz))
    ?.(=(0 ham) ham $(i +(i), syd +(syd)))
  --
::
++  gor                                                 ::  mug order
  ~/  %gor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  mor                                                 ::  more mug order
  ~/  %mor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug (mug a)) d=(mug (mug b))]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  dor                                                 ::  tree order
  ~/  %dor
  |=  [a=* b=*]
  ^-  ?
  ?:  =(a b)  &
  ?.  ?=(@ a)
    ?:  ?=(@ b)  |
    ?:  =(-.a -.b)
      $(a +.a, b +.b)
    $(a -.a, b -.b)
  ?.  ?=(@ b)  &
  (lth a b)
::
++  por                                                 ::  parent order
  ~/  %por
  |=  [a=@p b=@p]
  ^-  ?
  ?:  =(a b)  &
  =|  i=@
  |-
  ?:  =(i 2)
    ::  second two bytes
    (lte a b)
  ::  first two bytes
  =+  [c=(end 3 a) d=(end 3 b)]
  ?:  =(c d)
    $(a (rsh 3 a), b (rsh 3 b), i +(i))
  (lth c d)
::
::  Maps
::
++  by
  ~/  %by
  =|  a=(tree (pair))  ::  (map)
  =*  node  ?>(?=(^ a) n.a)
  |@
  ++  get
    ~/  %get
    |*  b=*
    =>  .(b `_?>(?=(^ a) p.n.a)`b)
    |-  ^-  (unit _?>(?=(^ a) q.n.a))
    ?~  a
      ~
    ?:  =(b p.n.a)
      `q.n.a
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  got                                               ::  need value by key
    |*  b=*
    (need (get b))
  ::
  ++  gut                                               ::  fall value by key
    |*  [b=* c=*]
    (fall (get b) c)
  ::
  ++  has                                               ::  key existence check
    ~/  %has
    |*  b=*
    !=(~ (get b))
  ::
  ++  put
    ~/  %put
    |*  [b=* c=*]
    |-  ^+  a
    ?~  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      a(n [b c])
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (mor p.n.a p.n.d)
        a(l d)
      d(r a(l r.d))
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (mor p.n.a p.n.d)
      a(r d)
    d(l a(r l.d))
  ::
  ++  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor p.n.l.a p.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & &((gor p.n.a u.l) !=(p.n.a u.l)))
        ?~(r & &((gor u.r p.n.a) !=(u.r p.n.a)))
        ?~  l.a   &
        &((mor p.n.a p.n.l.a) !=(p.n.a p.n.l.a) $(a l.a, l `p.n.a))
        ?~  r.a   &
        &((mor p.n.a p.n.r.a) !=(p.n.a p.n.r.a) $(a r.a, r `p.n.a))
    ==
  ::
  ++  tap                                               ::  listify pairs
    =<  $
    ~/  %tap
    =+  b=`(list _?>(?=(^ a) n.a))`~
    |.  ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  ++  run                                               ::  apply gate to values
    ~/  %run
    |*  b=gate
    |-
    ?~  a  a
    [n=[p=p.n.a q=(b q.n.a)] l=$(a l.a) r=$(a r.a)]
  ::
  ++  gas                                               ::  concatenate
    ~/  %gas
    |*  b=(list [p=* q=*])
    =>  .(b `(list _?>(?=(^ a) n.a))`b)
    |-  ^+  a
    ?~  b
      a
    $(b t.b, a (put p.i.b q.i.b))
  --
::
++  mip                                                 ::  map of maps
  |$  [kex key value]
  (map kex (map key value))
::
++  bi                                                  ::  mip engine
  =|  a=(map * (map))
  |@
  ++  del
    |*  [b=* c=*]
    =+  d=(~(gut by a) b ~)
    =+  e=(~(del by d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  get
    |*  [b=* c=*]
    =>  .(b `_?>(?=(^ a) p.n.a)`b, c `_?>(?=(^ a) ?>(?=(^ q.n.a) p.n.q.n.a))`c)
    ^-  (unit _?>(?=(^ a) ?>(?=(^ q.n.a) q.n.q.n.a)))
    (~(get by (~(gut by a) b ~)) c)
  ::
  ++  got
    |*  [b=* c=*]
    (need (get b c))
  ::
  ++  gut
    |*  [b=* c=* d=*]
    (~(gut by (~(gut by a) b ~)) c d)
  ::
  ++  has
    |*  [b=* c=*]
    !=(~ (get b c))
  ::
  ++  put
    |*  [b=* c=* d=*]
    %+  ~(put by a)  b
    %.  [c d]
    %~  put  by
    (~(gut by a) b ~)
  ::
  ++  tap
    ::NOTE  naive turn-based implementation find-errors ):
    =<  $
    =+  b=`_?>(?=(^ a) *(list [x=_p.n.a _?>(?=(^ q.n.a) [y=p v=q]:n.q.n.a)]))`~
    |.  ^+  b
    ?~  a
      b
    $(a r.a, b (welp (turn ~(tap by q.n.a) (lead p.n.a)) $(a l.a)))
  ::
  --
::
++  on                                                  ::  ordered map
  ~/  %on
  |*  [key=mold val=mold]
  =>  |%
      +$  item  [key=key val=val]
      --
  ::
  ~%  %comp  +>+  ~
  |=  compare=$-([key key] ?)
  ~%  %core    +  ~
  |%
  ::
  ++  apt
    ~/  %apt
    |=  a=(tree item)
    =|  [l=(unit key) r=(unit key)]
    |-  ^-  ?
    ?~  a  %.y
    ?&  ?~(l %.y (compare key.n.a u.l))
        ?~(r %.y (compare u.r key.n.a))
        ?~(l.a %.y &((mor key.n.a key.n.l.a) $(a l.a, l `key.n.a)))
        ?~(r.a %.y &((mor key.n.a key.n.r.a) $(a r.a, r `key.n.a)))
    ==
  ::
  ++  get
    ~/  %get
    |=  [a=(tree item) b=key]
    ^-  (unit val)
    ?~  a  ~
    ?:  =(b key.n.a)
      `val.n.a
    ?:  (compare b key.n.a)
      $(a l.a)
    $(a r.a)
  ::
  ++  has
    ~/  %has
    |=  [a=(tree item) b=key]
    ^-  ?
    !=(~ (get a b))
  ::
  ++  put
    ~/  %put
    |=  [a=(tree item) =key =val]
    ^-  (tree item)
    ?~  a  [n=[key val] l=~ r=~]
    ?:  =(key.n.a key)  a(val.n val)
    ?:  (compare key key.n.a)
      =/  l  $(a l.a)
      ?>  ?=(^ l)
      ?:  (mor key.n.a key.n.l)
        a(l l)
      l(r a(l r.l))
    =/  r  $(a r.a)
    ?>  ?=(^ r)
    ?:  (mor key.n.a key.n.r)
      a(r r)
    r(l a(r l.r))
  ::
  ++  run                                               ::  apply gate to values
    |*  [a=(tree item) b=$-(val *)]
    |-
    ?~  a  a
    [n=[key=key.n.a val=(b val.n.a)] l=$(a l.a) r=$(a r.a)]
  --
::
::  Sets
::
++  in
  ~/  %in
  =|  a=(tree)  :: (set)
  |@
  ++  put
    ~/  %put
    |*  b=*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (gor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      ?:  (mor n.a n.c)
        a(l c)
      c(r a(l r.c))
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    ?:  (mor n.a n.c)
      a(r c)
    c(l a(r l.c))
  ::
  ++  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (gor b n.a)
        a(l $(a l.a))
      a(r $(a r.a))
    |-  ^-  [$?(~ _a)]
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor n.l.a n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::
  ++  apt
    =<  $
    ~/  %apt
    =|  [l=(unit) r=(unit)]
    |.  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor n.a u.l))
        ?~(r & (gor u.r n.a))
        ?~(l.a & ?&((mor n.a n.l.a) $(a l.a, l `n.a)))
        ?~(r.a & ?&((mor n.a n.r.a) $(a r.a, r `n.a)))
    ==
  ::
  ++  rep                                               ::  reduce to product
    ~/  %rep
    |*  b=_=>(~ |=([* *] +<+))
    |-
    ?~  a  +<+.b
    $(a r.a, +<+.b $(a l.a, +<+.b (b n.a +<+.b)))
  ::
  ++  uni                                               ::  union
    ~/  %uni
    |*  b=_a
    ?:  =(a b)  a
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  =(n.b n.a)
      b(l $(a l.a, b l.b), r $(a r.a, b r.b))
    ?:  (mor n.a n.b)
      ?:  (gor n.b n.a)
        $(l.a $(a l.a, r.b ~), b r.b)
      $(r.a $(a r.a, l.b ~), b l.b)
    ?:  (gor n.a n.b)
      $(l.b $(b l.b, r.a ~), a r.a)
    $(r.b $(b r.b, l.a ~), a l.a)
  --
::
::  Jugs
::
++  ju
  =|  a=(tree (pair * (tree)))  ::  (jug)
  |@
  ++  get
    |*  b=*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  ++  del
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    =+  e=(~(del in d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  put
    |*  [b=* c=*]
    ^+  a
    =+  d=(get b)
    (~(put by a) b (~(put in d) c))
  --
:: XX: remove, i remember some bug with the sample bunt of one of these?
:: maybe the PR was
:: merged
++  cork  |*([a=$-(* *) b=$-(* *)] (corl b a))          ::  compose forward
++  corl                                                ::  compose backwards
  |*  [a=$-(* *) b=$-(* *)]
  =<  +:|.((a (b)))      ::  type check
  |*  c=_,.+<.b
  (a (b c))
::
++  cury                                                ::  curry left
  |*  [a=$-(^ *) b=*]
  |*  c=_,.+<+.a
  (a b c)
::
++  curr                                                ::  curry right
  |*  [a=$-(^ *) c=*]
  |*  b=_,.+<-.a
  (a b c)
::
++  lead  |*(* |*(* [+>+< +<]))                         ::  put head
++  late  |*(* |*(* [+< +>+<]))                         ::  put tail
::
++  sha
  ~%  %sha  +  ~
  |%
  ++  flin  |=(a=@ (swp 3 a))                       ::  flip input
  ++  flim  |=(byts [wid (rev 3 wid dat)])          ::  flip input w= length
  ++  flip  |=(w=@u (cury (cury rev 3) w))          ::  flip output of size
  ++  meet  |=(a=@ [(met 3 a) a])                   ::  measure input size
  ++  sha-256   :(cork flin shax (flip 32))
  ++  sha-512   :(cork flin shaz (flip 64))
  ::
  ::  use with byts
  ::
  ++  sha-256l  :(cork flim shay (flip 32))
  ++  sha-512l  :(cork flim shal (flip 64))
  ++  shad  |=(ruz=@ (shax (shax ruz)))                   ::  double sha-256
  ++  shaf                                                ::  half sha-256
    |=  [sal=@ ruz=@]
    =+  haz=(shas sal ruz)
    (mix (end 7 haz) (rsh 7 haz))
  ::
  ++  shas                                                ::  salted hash
    ~/  %shas
    |=  [sal=@ ruz=@]
    =/  len  (max 32 (met 3 sal))
    (shay len (mix sal (shax ruz)))
  ::
  ++  shax                                                ::  sha-256
    ~/  %shax
    |=  ruz=@  ^-  @
    (shay [(met 3 ruz) ruz])
  ::
  ++  shay                                                ::  sha-256 with length
    ~/  %shay
    |=  [len=@u ruz=@]  ^-  @
    =>  .(ruz (cut 3 [0 len] ruz))
    =+  [few==>(fe .(a 5)) wac=|=([a=@ b=@] (cut 5 [a 1] b))]
    =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
    =+  ral=(lsh [0 3] len)
    =+  ^=  ful
        %+  can  0
        :~  [ral ruz]
            [8 128]
            [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
            [64 (~(net fe 6) ral)]
        ==
    =+  lex=(met 9 ful)
    =+  ^=  kbx  0xc671.78f2.bef9.a3f7.a450.6ceb.90be.fffa.
                   8cc7.0208.84c8.7814.78a5.636f.748f.82ee.
                   682e.6ff3.5b9c.ca4f.4ed8.aa4a.391c.0cb3.
                   34b0.bcb5.2748.774c.1e37.6c08.19a4.c116.
                   106a.a070.f40e.3585.d699.0624.d192.e819.
                   c76c.51a3.c24b.8b70.a81a.664b.a2bf.e8a1.
                   9272.2c85.81c2.c92e.766a.0abb.650a.7354.
                   5338.0d13.4d2c.6dfc.2e1b.2138.27b7.0a85.
                   1429.2967.06ca.6351.d5a7.9147.c6e0.0bf3.
                   bf59.7fc7.b003.27c8.a831.c66d.983e.5152.
                   76f9.88da.5cb0.a9dc.4a74.84aa.2de9.2c6f.
                   240c.a1cc.0fc1.9dc6.efbe.4786.e49b.69c1.
                   c19b.f174.9bdc.06a7.80de.b1fe.72be.5d74.
                   550c.7dc3.2431.85be.1283.5b01.d807.aa98.
                   ab1c.5ed5.923f.82a4.59f1.11f1.3956.c25b.
                   e9b5.dba5.b5c0.fbcf.7137.4491.428a.2f98
    =+  ^=  hax  0x5be0.cd19.1f83.d9ab.9b05.688c.510e.527f.
                   a54f.f53a.3c6e.f372.bb67.ae85.6a09.e667
    =+  i=0
    |-  ^-  @
    ?:  =(i lex)
      (run 5 hax net)
    =+  ^=  wox
        =+  dux=(cut 9 [i 1] ful)
        =+  wox=(run 5 dux net)
        =+  j=16
        |-  ^-  @
        ?:  =(64 j)
          wox
        =+  :*  l=(wac (sub j 15) wox)
                m=(wac (sub j 2) wox)
                n=(wac (sub j 16) wox)
                o=(wac (sub j 7) wox)
            ==
        =+  x=:(mix (ror 0 7 l) (ror 0 18 l) (rsh [0 3] l))
        =+  y=:(mix (ror 0 17 m) (ror 0 19 m) (rsh [0 10] m))
        =+  z=:(sum n x o y)
        $(wox (con (lsh [5 j] z) wox), j +(j))
    =+  j=0
    =+  :*  a=(wac 0 hax)
            b=(wac 1 hax)
            c=(wac 2 hax)
            d=(wac 3 hax)
            e=(wac 4 hax)
            f=(wac 5 hax)
            g=(wac 6 hax)
            h=(wac 7 hax)
        ==
    |-  ^-  @
    ?:  =(64 j)
      %=  ^$
        i  +(i)
        hax  %+  rep  5
             :~  (sum a (wac 0 hax))
                 (sum b (wac 1 hax))
                 (sum c (wac 2 hax))
                 (sum d (wac 3 hax))
                 (sum e (wac 4 hax))
                 (sum f (wac 5 hax))
                 (sum g (wac 6 hax))
                 (sum h (wac 7 hax))
             ==
      ==
    =+  l=:(mix (ror 0 2 a) (ror 0 13 a) (ror 0 22 a))    ::  s0
    =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
    =+  n=(sum l m)                                       ::  t2
    =+  o=:(mix (ror 0 6 e) (ror 0 11 e) (ror 0 25 e))    ::  s1
    =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
    =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
    $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
  ::
  ++  shaz                                                ::  sha-512
    |=  ruz=@  ^-  @
    (shal [(met 3 ruz) ruz])
  ::
  ++  shal                                                ::  sha-512 with length
    ~/  %shal
    |=  [len=@ ruz=@]  ^-  @
    =>  .(ruz (cut 3 [0 len] ruz))
    =+  [few==>(fe .(a 6)) wac=|=([a=@ b=@] (cut 6 [a 1] b))]
    =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
    =+  ral=(lsh [0 3] len)
    =+  ^=  ful
        %+  can  0
        :~  [ral ruz]
            [8 128]
            [(mod (sub 1.920 (mod (add 8 ral) 1.024)) 1.024) 0]
            [128 (~(net fe 7) ral)]
        ==
    =+  lex=(met 10 ful)
    =+  ^=  kbx  0x6c44.198c.4a47.5817.5fcb.6fab.3ad6.faec.
                   597f.299c.fc65.7e2a.4cc5.d4be.cb3e.42b6.
                   431d.67c4.9c10.0d4c.3c9e.be0a.15c9.bebc.
                   32ca.ab7b.40c7.2493.28db.77f5.2304.7d84.
                   1b71.0b35.131c.471b.113f.9804.bef9.0dae.
                   0a63.7dc5.a2c8.98a6.06f0.67aa.7217.6fba.
                   f57d.4f7f.ee6e.d178.eada.7dd6.cde0.eb1e.
                   d186.b8c7.21c0.c207.ca27.3ece.ea26.619c.
                   c671.78f2.e372.532b.bef9.a3f7.b2c6.7915.
                   a450.6ceb.de82.bde9.90be.fffa.2363.1e28.
                   8cc7.0208.1a64.39ec.84c8.7814.a1f0.ab72.
                   78a5.636f.4317.2f60.748f.82ee.5def.b2fc.
                   682e.6ff3.d6b2.b8a3.5b9c.ca4f.7763.e373.
                   4ed8.aa4a.e341.8acb.391c.0cb3.c5c9.5a63.
                   34b0.bcb5.e19b.48a8.2748.774c.df8e.eb99.
                   1e37.6c08.5141.ab53.19a4.c116.b8d2.d0c8.
                   106a.a070.32bb.d1b8.f40e.3585.5771.202a.
                   d699.0624.5565.a910.d192.e819.d6ef.5218.
                   c76c.51a3.0654.be30.c24b.8b70.d0f8.9791.
                   a81a.664b.bc42.3001.a2bf.e8a1.4cf1.0364.
                   9272.2c85.1482.353b.81c2.c92e.47ed.aee6.
                   766a.0abb.3c77.b2a8.650a.7354.8baf.63de.
                   5338.0d13.9d95.b3df.4d2c.6dfc.5ac4.2aed.
                   2e1b.2138.5c26.c926.27b7.0a85.46d2.2ffc.
                   1429.2967.0a0e.6e70.06ca.6351.e003.826f.
                   d5a7.9147.930a.a725.c6e0.0bf3.3da8.8fc2.
                   bf59.7fc7.beef.0ee4.b003.27c8.98fb.213f.
                   a831.c66d.2db4.3210.983e.5152.ee66.dfab.
                   76f9.88da.8311.53b5.5cb0.a9dc.bd41.fbd4.
                   4a74.84aa.6ea6.e483.2de9.2c6f.592b.0275.
                   240c.a1cc.77ac.9c65.0fc1.9dc6.8b8c.d5b5.
                   efbe.4786.384f.25e3.e49b.69c1.9ef1.4ad2.
                   c19b.f174.cf69.2694.9bdc.06a7.25c7.1235.
                   80de.b1fe.3b16.96b1.72be.5d74.f27b.896f.
                   550c.7dc3.d5ff.b4e2.2431.85be.4ee4.b28c.
                   1283.5b01.4570.6fbe.d807.aa98.a303.0242.
                   ab1c.5ed5.da6d.8118.923f.82a4.af19.4f9b.
                   59f1.11f1.b605.d019.3956.c25b.f348.b538.
                   e9b5.dba5.8189.dbbc.b5c0.fbcf.ec4d.3b2f.
                   7137.4491.23ef.65cd.428a.2f98.d728.ae22
    =+  ^=  hax  0x5be0.cd19.137e.2179.1f83.d9ab.fb41.bd6b.
                   9b05.688c.2b3e.6c1f.510e.527f.ade6.82d1.
                   a54f.f53a.5f1d.36f1.3c6e.f372.fe94.f82b.
                   bb67.ae85.84ca.a73b.6a09.e667.f3bc.c908
    =+  i=0
    |-  ^-  @
    ?:  =(i lex)
      (run 6 hax net)
    =+  ^=  wox
        =+  dux=(cut 10 [i 1] ful)
        =+  wox=(run 6 dux net)
        =+  j=16
        |-  ^-  @
        ?:  =(80 j)
          wox
        =+  :*  l=(wac (sub j 15) wox)
                m=(wac (sub j 2) wox)
                n=(wac (sub j 16) wox)
                o=(wac (sub j 7) wox)
            ==
        =+  x=:(mix (ror 0 1 l) (ror 0 8 l) (rsh [0 7] l))
        =+  y=:(mix (ror 0 19 m) (ror 0 61 m) (rsh [0 6] m))
        =+  z=:(sum n x o y)
        $(wox (con (lsh [6 j] z) wox), j +(j))
    =+  j=0
    =+  :*  a=(wac 0 hax)
            b=(wac 1 hax)
            c=(wac 2 hax)
            d=(wac 3 hax)
            e=(wac 4 hax)
            f=(wac 5 hax)
            g=(wac 6 hax)
            h=(wac 7 hax)
        ==
    |-  ^-  @
    ?:  =(80 j)
      %=  ^$
        i  +(i)
        hax  %+  rep  6
             :~  (sum a (wac 0 hax))
                 (sum b (wac 1 hax))
                 (sum c (wac 2 hax))
                 (sum d (wac 3 hax))
                 (sum e (wac 4 hax))
                 (sum f (wac 5 hax))
                 (sum g (wac 6 hax))
                 (sum h (wac 7 hax))
             ==
      ==
    =+  l=:(mix (ror 0 28 a) (ror 0 34 a) (ror 0 39 a))   ::  S0
    =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
    =+  n=(sum l m)                                       ::  t2
    =+  o=:(mix (ror 0 14 e) (ror 0 18 e) (ror 0 41 e))   ::  S1
    =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
    =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
    $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
  ::
  --
::
++  hmac
  ~%  %hmac  +  ~
  =,  sha
  =>  |%
      ++  meet  |=([k=@ m=@] [[(met 3 k) k] [(met 3 m) m]])
      ++  flip  |=([k=@ m=@] [(swp 3 k) (swp 3 m)])
      --
  |%
  ::
  ::  use with @
  ::
  ++  hmac-sha256   (cork meet hmac-sha256l)
  ++  hmac-sha512   (cork meet hmac-sha512l)
  ::
  ::  use with @t
  ::
  ++  hmac-sha256t  (cork flip hmac-sha256)
  ++  hmac-sha512t  (cork flip hmac-sha512)
  ::
  ::  use with byts
  ::
  ++  hmac-sha256l  (cury hmac sha-256l 64 32)
  ++  hmac-sha512l  (cury hmac sha-512l 128 64)
  ::
  ::  main logic
  ::
  ++  hmac
    ~/  %hmac
    ::  boq: block size in bytes used by haj
    ::  out: bytes output by haj
    |*  [[haj=$-([@u @] @) boq=@u out=@u] key=byts msg=byts]
    ::  ensure key and message fit signaled lengths
    =.  dat.key  (end [3 wid.key] dat.key)
    =.  dat.msg  (end [3 wid.msg] dat.msg)
    ::  keys longer than block size are shortened by hashing
    =?  dat.key  (gth wid.key boq)  (haj wid.key dat.key)
    =?  wid.key  (gth wid.key boq)  out
    ::  keys shorter than block size are right-padded
    =?  dat.key  (lth wid.key boq)  (lsh [3 (sub boq wid.key)] dat.key)
    ::  pad key, inner and outer
    =+  kip=(mix dat.key (fil 3 boq 0x36))
    =+  kop=(mix dat.key (fil 3 boq 0x5c))
    ::  append inner padding to message, then hash
    =+  (haj (add wid.msg boq) (add (lsh [3 wid.msg] kip) dat.msg))
    ::  prepend outer padding to result, hash again
    (haj (add out boq) (add (lsh [3 out] kop) -))
  --  ::  hmac

++  ed
  =>
    ~%  %coed  +  ~
    |%  
    ++  b  256
    ++  q  (sub (bex 255) 19)
    ++  cb  (rsh [0 3] b)
    ++  fq  ~(. fo q)
    ++  l
      %+  add
        (bex 252)
      27.742.317.777.372.353.535.851.937.790.883.648.493
    ++  d   (dif:fq 0 (fra:fq 121.665 121.666))
    ++  ii  (exp:fq (div (dec q) 4) 2)
    ::                                                ::  ++norm:ed:crypto
    ++  norm                                          ::
      |=(x=@ ?:(=(0 (mod x 2)) x (sub q x)))
    ::                                                ::  ++neg:ed:crypto
    ++  neg                                           ::
      |=(pp=[@ @] pp(- (dif:fq 0 -.pp)))
    ::                                                ::  ++xrec:ed:crypto
    ++  xrec                                          ::  recover x-coord
      |=  y=@  ^-  @
      =+  ^=  xx
          %+  mul  (dif:fq (mul y y) 1)
                   (inv:fq +(:(mul d y y)))
      =+  x=(exp:fq (div (add 3 q) 8) xx)
      ?:  !=(0 (dif:fq (mul x x) (sit:fq xx)))
        (norm (pro:fq x ii))
      (norm x)
    ::                                                ::  ++ward:ed:crypto
    ++  ward                                          ::  edwards multiply
      |=  [pp=[@ @] qq=[@ @]]  ^-  [@ @]
      =+  dp=:(pro:fq d -.pp -.qq +.pp +.qq)
      =+  ^=  xt
          %+  pro:fq
            %+  sum:fq
              (pro:fq -.pp +.qq)
            (pro:fq -.qq +.pp)
          (inv:fq (sum:fq 1 dp))
      =+  ^=  yt
          %+  pro:fq
            %+  sum:fq
              (pro:fq +.pp +.qq)
            (pro:fq -.pp -.qq)
          (inv:fq (dif:fq 1 dp))
      [xt yt]
    ::                                                ::  ++scam:ed:crypto
    ++  scam                                          ::  scalar multiply
      |=  [pp=[@ @] e=@]  ^-  [@ @]
      ?:  =(0 e)
        [0 1]
      =+  qq=$(e (div e 2))
      =>  .(qq (ward qq qq))
      ?:  =(1 (dis 1 e))
        (ward qq pp)
      qq
    ::                                                ::  ++etch:ed:crypto
    ++  etch                                          ::  encode point
      |=  pp=[@ @]  ^-  @
      (can 0 ~[[(sub b 1) +.pp] [1 (dis 1 -.pp)]])
    ::                                                ::  ++curv:ed:crypto
    ++  curv                                          ::  point on curve?
      |=  [x=@ y=@]  ^-  ?
      .=  0
          %+  dif:fq
            %+  sum:fq
              (pro:fq (sub q (sit:fq x)) x)
            (pro:fq y y)
          (sum:fq 1 :(pro:fq d x x y y))
    ::                                                ::  ++deco:ed:crypto
    ++  deco                                          ::  decode point
      |=  s=@  ^-  (unit [@ @])
      ?.  (lte (met 3 s) cb)  ~
      =+  y=(cut 0 [0 (dec b)] s)
      =+  si=(cut 0 [(dec b) 1] s)
      =+  x=(xrec y)
      =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
      =+  pp=[x y]
      ?.  (curv pp)
        ~
      [~ pp]
    ::                                                ::  ++bb:ed:crypto
    ++  bb                                            ::
      =+  bby=(pro:fq 4 (inv:fq 5))
      [(xrec bby) bby]
    --  ::
  ~%  %ed  +  ~
  |%
  ::
  ++  recs
    ~/  %recs
    |=  a=@uxscalar
    (~(sit fo l) a)
  ::
  ++  smac
    ~/  %smac
    |=  [a=@uxscalar b=@uxscalar c=@uxscalar]
    (recs (add (mul a b) c))
  ::
  ++  point-neg
    ~/  %point-neg
    |=  [a-point=@uxpoint]
    ^-  @uxpoint
    (etch (neg (need (deco a-point))))
  ::
  ++  point-add
    ~/  %point-add
    |=  [a-point=@uxpoint b-point=@uxpoint]
    ^-  @uxpoint
    (etch (ward (need (deco a-point)) (need (deco b-point))))
  ::
  ++  scalarmult
    ~/  %scalarmult
    |=  [a=@uxscalar a-point=@uxpoint]
    ^-  @uxpoint
    ::
    =/  a-point-decoded=[@ @]  (need (deco a-point))
    ::
    %-  etch
    (scam a-point-decoded (recs a))
  ::
  ++  scalarmult-base
    ~/  %scalarmult-base
    |=  scalar=@uxscalar
    ^-  @uxpoint
    (scalarmult scalar (etch bb))
  ::
  ++  add-scalarmult-scalarmult-base
    ~/  %add-scalarmult-scalarmult-base
    |=  [a=@uxscalar a-point=@uxpoint b=@uxscalar]
    ^-  @uxpoint
    ::
    %+  point-add
      (scalarmult-base b)
    (scalarmult a a-point)
  ::
  ++  add-double-scalarmult
    ~/  %add-double-scalarmult
    |=  [a=@uxscalar a-point=@uxpoint b=@uxscalar b-point=@uxpoint]
    ^-  @uxpoint
    %+  point-add
      (scalarmult a a-point)
    (scalarmult b b-point)
  ::                                                  ::  ++scad:ed:crypto
  ++  scad                                            ::  scalar addition on public and private keys
    ~/  %scad
    |=  [pub=@uxpoint sek=@uxscalar sca=@uxscalar]
    ^-  [pub=@uxpoint sek=@uxscalar]
    [(scap pub sca) (scas sek sca)]
  ::                                                  ::  ++scas:ed:crypto
  ++  scas                                            ::  scalar addition on private key
    ~/  %scas
    |=  [sek=@uxscalar sca=@uxscalar]
    ^-  @
    ?>  (lte (met 3 sek) (mul 2 cb))
    ?>  (lte (met 3 sca) (mul 2 cb))
    =/  n  (dis sca (con (lsh [3 (dec cb)] 0x7f) (fil 3 (dec cb) 0xff)))
    =/  s0  (cut 0 [0 b] sek)
    =/  s1  (cut 0 [b b] sek)
    =/  ns0  (recs (add s0 n))
    =/  ns1  (shal:sha (mul 2 cb) (can 0 ~[[b s1] [b sca]]))
    (can 0 ~[[b ns0] [b ns1]])
  ::                                                  ::  ++scap:ed:crypto
  ++  scap                                            ::  scalar addition on public key
    ~/  %scap
    |=  [pub=@uxpoint sca=@uxscalar]
    ^-  @
    ?>  (lte (met 3 pub) cb)
    ?>  (lte (met 3 sca) cb)
    =/  n  (dis sca (con (lsh [3 (dec cb)] 0x7f) (fil 3 (dec cb) 0xff)))
    (point-add pub (scalarmult-base n))
  ::                                                  ::  ++puck:ed:crypto
  ++  puck                                            ::  pubkey from seed
    ~/  %puck
    |=  sed=@I
    pub:(luck sed)
  ::                                                  ::  ++luck:ed:crypto
  ++  luck                                            ::  keypair from seed
    ~/  %luck
    |=  sed=@I
    ^-  [pub=@uxpoint sek=@uxscalar]
    ?>  (lte (met 3 sed) cb)
    =+  h=(shal:sha (rsh [0 3] b) sed)
    =+  ^=  a
        %+  add
          (bex (sub b 2))
        (lsh [0 3] (cut 0 [3 (sub b 5)] h))
    =+  aa=(scalarmult-base a)
    [aa (can 0 ~[[b a] [b (cut 0 [b b] h)]])]
  ::                                                  ::  ++veri:ed:crypto
  ++  veri                                            ::  validate
    ~/  %veri
    |=  [s=@ m=@ pub=@]  ^-  ?
    (veri-octs s (met 3 m)^m pub)
  ::                                                  ::  ++veri-octs:ed:crypto
  ++  veri-octs                                       ::  validate octs
    ~/  %veri-octs
    |=  [s=@ m=octs pub=@]  ^-  ?
    ?~  (deco pub)  |
    ?:  (gth (met 3 s) (div b 4))  |
    ?:  (gth (met 3 pub) (div b 8))  |
    =+  rr=(cut 0 [0 b] s)
    =+  ss=(cut 0 [b b] s)
    =+  ha=(can 3 ~[[cb rr] [cb pub] m])
    =+  h=(shal:sha (add (mul 2 cb) p.m) ha)
    =(rr (add-scalarmult-scalarmult-base h (point-neg pub) ss))
  --  ::ed
::
++  secp
  =>  [. hmc=hmac-sha256l:hmac]
  ~%  %secp  +<  ~
  |%
  +$  jacobian   [x=@ y=@ z=@]                    ::  jacobian point
  +$  point      [x=@ y=@]                        ::  curve point
  +$  domain
    $:  p=@                                       ::  prime modulo
        a=@                                       ::  y^2=x^3+ax+b
        b=@                                       ::
        g=point                                   ::  base point
        n=@                                       ::  prime order of g
    ==
  ++  secp
    |_  [bytes=@ =domain]
    ++  field-p  ~(. fo p.domain)
    ++  field-n  ~(. fo n.domain)
    ++  compress-point
      |=  =point
      ^-  @
      %+  can  3
      :~  [bytes x.point]
          [1 (add 2 (cut 0 [0 1] y.point))]
      ==
    ::
    ++  serialize-point
      |=  =point
      ^-  @
      %+  can  3
      :~  [bytes y.point]
          [bytes x.point]
          [1 4]
      ==
    ::
    ++  decompress-point
      |=  compressed=@
      ^-  point
      =/  x=@  (end [3 bytes] compressed)
      ?>  =(3 (mod p.domain 4))
      =/  fop  field-p
      =+  [fadd fmul fpow]=[sum.fop pro.fop exp.fop]
      =/  y=@  %+  fpow  (rsh [0 2] +(p.domain))
               %+  fadd  b.domain
               %+  fadd  (fpow 3 x)
              (fmul a.domain x)
      =/  s=@  (rsh [3 bytes] compressed)
      ?>  |(=(2 s) =(3 s))
      ::  check parity
      ::
      =?  y  !=((sub s 2) (mod y 2))
        (sub p.domain y)
      [x y]
    ::
    ++  jc                                        ::  jacobian math
      |%
      ++  from
        |=  a=jacobian
        ^-  point
        =/  fop   field-p
        =+  [fmul fpow finv]=[pro.fop exp.fop inv.fop]
        =/  z  (finv z.a)
        :-  (fmul x.a (fpow 2 z))
        (fmul y.a (fpow 3 z))
      ::
      ++  into
        |=  point
        ^-  jacobian
        [x y 1]
      ::
      ++  double
        |=  jacobian
        ^-  jacobian
        ?:  =(0 y)  [0 0 0]
        =/  fop  field-p
        =+  [fadd fsub fmul fpow]=[sum.fop dif.fop pro.fop exp.fop]
        =/  s    :(fmul 4 x (fpow 2 y))
        =/  m    %+  fadd
                   (fmul 3 (fpow 2 x))
                 (fmul a.domain (fpow 4 z))
        =/  nx   %+  fsub
                   (fpow 2 m)
                 (fmul 2 s)
        =/  ny  %+  fsub
                  (fmul m (fsub s nx))
                (fmul 8 (fpow 4 y))
        =/  nz  :(fmul 2 y z)
        [nx ny nz]
      ::
      ++  add
        |=  [a=jacobian b=jacobian]
        ^-  jacobian
        ?:  =(0 y.a)  b
        ?:  =(0 y.b)  a
        =/  fop  field-p
        =+  [fadd fsub fmul fpow]=[sum.fop dif.fop pro.fop exp.fop]
        =/  u1  :(fmul x.a z.b z.b)
        =/  u2  :(fmul x.b z.a z.a)
        =/  s1  :(fmul y.a z.b z.b z.b)
        =/  s2  :(fmul y.b z.a z.a z.a)
        ?:  =(u1 u2)
          ?.  =(s1 s2)
            [0 0 1]
          (double a)
        =/  h     (fsub u2 u1)
        =/  r     (fsub s2 s1)
        =/  h2    (fmul h h)
        =/  h3    (fmul h2 h)
        =/  u1h2  (fmul u1 h2)
        =/  nx    %+  fsub
                    (fmul r r)
                  :(fadd h3 u1h2 u1h2)
        =/  ny    %+  fsub
                    (fmul r (fsub u1h2 nx))
                  (fmul s1 h3)
        =/  nz    :(fmul h z.a z.b)
        [nx ny nz]
      ::
      ++  mul
        |=  [a=jacobian scalar=@]
        ^-  jacobian
        ?:  =(0 y.a)
          [0 0 1]
        ?:  =(0 scalar)
          [0 0 1]
        ?:  =(1 scalar)
          a
        ?:  (gte scalar n.domain)
          $(scalar (mod scalar n.domain))
        ?:  =(0 (mod scalar 2))
          (double $(scalar (rsh 0 scalar)))
        (add a (double $(scalar (rsh 0 scalar))))
      --
    ++  add-points
      |=  [a=point b=point]
      ^-  point
      =/  j  jc
      (from.j (add.j (into.j a) (into.j b)))
    ++  mul-point-scalar
      |=  [p=point scalar=@]
      ^-  point
      =/  j  jc
      %-  from.j
      %+  mul.j
        (into.j p)
      scalar
    ::
    ++  valid-hash
      |=  has=@
      (lte (met 3 has) bytes)
    ::
    ++  in-order
      |=  i=@
      ?&  (gth i 0)
          (lth i n.domain)
      ==
    ++  priv-to-pub
      |=  private-key=@
      ^-  point
      ?>  (in-order private-key)
      (mul-point-scalar g.domain private-key)
    ::
    ++  make-k
      |=  [hash=@ private-key=@]
      ^-  @
      ?>  (in-order private-key)
      ?>  (valid-hash hash)
      =/  v  (fil 3 bytes 1)
      =/  k  0
      =.  k  %+  hmc  [bytes k]
             %-  as-octs
             %+  can  3
             :~  [bytes hash]
                 [bytes private-key]
                 [1 0]
                 [bytes v]
             ==
      =.  v  (hmc bytes^k bytes^v)
      =.  k  %+  hmc  [bytes k]
             %-  as-octs
             %+  can  3
             :~  [bytes hash]
                 [bytes private-key]
                 [1 1]
                 [bytes v]
             ==
      =.  v  (hmc bytes^k bytes^v)
      (hmc bytes^k bytes^v)
    ::
    ++  ecdsa-raw-sign
      |=  [hash=@ private-key=@]
      ^-  [r=@ s=@ y=@]
      ::  make-k and priv-to pub will validate inputs
      =/  k   (make-k hash private-key)
      =/  rp  (priv-to-pub k)
      =*  r   x.rp
      ?<  =(0 r)
      =/  fon  field-n
      =+  [fadd fmul finv]=[sum.fon pro.fon inv.fon]
      =/  s  %+  fmul  (finv k)
             %+  fadd  hash
             %+  fmul  r
             private-key
      ?<  =(0 s)
      [r s y.rp]
    ::  general recovery omitted, but possible
    --
  ++  secp256k1
    ~%  %secp256k1  +  ~
    |%
    ++  t  :: in the battery for jet matching
      ^-  domain
      :*  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
          ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f
          0
          7
          :-  0x79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
                029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
              0x483a.da77.26a3.c465.5da4.fbfc.0e11.08a8.
                fd17.b448.a685.5419.9c47.d08f.fb10.d4b8
          0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.
            baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
      ==
    ::
    ++  curve             ~(. secp 32 t)
    ++  serialize-point   serialize-point:curve
    ++  compress-point    compress-point:curve
    ++  decompress-point  decompress-point:curve
    ++  add-points        add-points:curve
    ++  mul-point-scalar  mul-point-scalar:curve
    ++  make-k
      ~/  %make
      |=  [hash=@uvI private-key=@]
      ::  checks sizes
      (make-k:curve hash private-key)
    ++  priv-to-pub
      |=  private-key=@
      ::  checks sizes
      (priv-to-pub:curve private-key)
    ::
    ++  ecdsa-raw-sign
      ~/  %sign
      |=  [hash=@uvI private-key=@]
      ^-  [v=@ r=@ s=@]
      =/  c  curve
      ::  raw-sign checks sizes
      =+  (ecdsa-raw-sign.c hash private-key)
      =/  rp=point  [r y]
      =/  s-high  (gte (mul 2 s) n.domain.c)
      =?  s   s-high
        (sub n.domain.c s)
      =?  rp  s-high
        [x.rp (sub p.domain.c y.rp)]
      =/  v   (end 0 y.rp)
      =?  v   (gte x.rp n.domain.c)
        (add v 2)
      [v x.rp s]
    ::
    ++  ecdsa-raw-recover
      ~/  %reco
      |=  [hash=@ sig=[v=@ r=@ s=@]]
      ^-  point
      ?>  (lte v.sig 3)
      =/  c   curve
      ?>  (valid-hash.c hash)
      ?>  (in-order.c r.sig)
      ?>  (in-order.c s.sig)
      =/  x  ?:  (gte v.sig 2)
               (add r.sig n.domain.c)
             r.sig
      =/  fop  field-p.c
      =+  [fadd fmul fpow]=[sum.fop pro.fop exp.fop]
      =/  ysq   (fadd (fpow 3 x) b.domain.c)
      =/  beta  (fpow (rsh [0 2] +(p.domain.c)) ysq)
      =/  y  ?:  =((end 0 v.sig) (end 0 beta))
               beta
             (sub p.domain.c beta)
      ?>  =(0 (dif.fop ysq (fmul y y)))
      =/  nz   (sub n.domain.c hash)
      =/  j    jc.c
      =/  gz   (mul.j (into.j g.domain.c) nz)
      =/  xy   (mul.j (into.j x y) s.sig)
      =/  qr   (add.j gz xy)
      =/  qj   (mul.j qr (inv:field-n.c x))
      =/  pub  (from.j qj)
      ?<  =([0 0] pub)
      pub
    ++  schnorr
      ~%  %schnorr  ..schnorr  ~
      =>  |%
          ++  tagged-hash
            |=  [tag=@ [l=@ x=@]]
            =+  hat=(sha-256:sha (swp 3 tag))
            %-  sha-256l:sha
            :-  (add 64 l)
            (can 3 ~[[l x] [32 hat] [32 hat]])
          ++  lift-x
            |=  x=@I
            ^-  (unit point)
            =/  c  curve
            ?.  (lth x p.domain.c)
              ~
            =/  fop  field-p.c
            =+  [fadd fpow]=[sum.fop exp.fop]
            =/  cp  (fadd (fpow 3 x) 7)
            =/  y  (fpow (rsh [0 2] +(p.domain.c)) cp)
            ?.  =(cp (fpow 2 y))
              ~
            :-  ~  :-  x
            ?:  =(0 (mod y 2))
              y
            (sub p.domain.c y)
          --
      |%
      ::
      ++  sign                                        ::  schnorr signature
        ~/  %sosi
        |=  [sk=@I m=@I a=@I]
        ^-  @J
        ?>  (gte 32 (met 3 m))
        ?>  (gte 32 (met 3 a))
        =/  c  curve
        ::  implies (gte 32 (met 3 sk))
        ::
        ?<  |(=(0 sk) (gte sk n.domain.c))
        =/  pp
          (mul-point-scalar g.domain.c sk)
        =/  d
          ?:  =(0 (mod y.pp 2))
            sk
          (sub n.domain.c sk)
        =/  t
          %+  mix  d
          (tagged-hash 'BIP0340/aux' [32 a])
        =/  rand
          %+  tagged-hash  'BIP0340/nonce'
          :-  96
          (rep 8 ~[m x.pp t])
        =/  kp  (mod rand n.domain.c)
        ?<  =(0 kp)
        =/  rr  (mul-point-scalar g.domain.c kp)
        =/  k
          ?:  =(0 (mod y.rr 2))
            kp
          (sub n.domain.c kp)
        =/  e
          %-  mod
          :_  n.domain.c
          %+  tagged-hash  'BIP0340/challenge'
          :-  96
          (rep 8 ~[m x.pp x.rr])
        =/  sig
          %^  cat  8
            (mod (add k (mul e d)) n.domain.c)
          x.rr
        ?>  (verify x.pp m sig)
        sig
      ::
      ++  verify                                      ::  schnorr verify
        ~/  %sove
        |=  [pk=@I m=@I sig=@J]
        ^-  ?
        ?>  (gte 32 (met 3 pk))
        ?>  (gte 32 (met 3 m))
        ?>  (gte 64 (met 3 sig))
        =/  c  curve
        =/  pup  (lift-x pk)
        ?~  pup
          %.n
        =/  pp  u.pup
        =/  r  (cut 8 [1 1] sig)
        ?:  (gte r p.domain.c)
          %.n
        =/  s  (end 8 sig)
        ?:  (gte s n.domain.c)
          %.n
        =/  e
          %-  mod
          :_  n.domain.c
          %+  tagged-hash  'BIP0340/challenge'
          :-  96
          (rep 8 ~[m x.pp r])
        =/  aa
          (mul-point-scalar g.domain.c s)
        =/  bb
          (mul-point-scalar pp (sub n.domain.c e))
        ?:  &(=(x.aa x.bb) !=(y.aa y.bb))             ::  infinite?
          %.n
        =/  rr  (add-points aa bb)
        ?.  =(0 (mod y.rr 2))
          %.n
        =(r x.rr)
      --
    --
  --
--
