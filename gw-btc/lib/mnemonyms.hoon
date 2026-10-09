::
::  helper core
|%
+$  nym    @t
+$  word   @t
+$  hex    @ux
+$  width  @ud
::
++  split-by-dots
  ::  XX refactor to use Hoon's built-in parsing
  |=  t=tape
  ^-  (list tape)
  =|  cur=tape
  =|  acc=(list tape)
  |-
  ?~  t  (flop [(flop cur) acc])
  ?:  =('.' i.t)
    $(t t.t, cur ~, acc [(flop cur) acc])
  $(t t.t, cur [i.t cur])
::
++  count-bytes
  |=  =width
  ^-  @ud
  (div width 8)
::
++  count-cs-lent
  |=  =width
  ^-  @ud
  (div width 32)
::
++  count-total-bits
  |=  =width
  ^-  @ud
  (add width (count-cs-lent width))
::
++  count-total-words
  |=  bits=@ud
  ^-  @ud
  (div bits 11)
::
::  sha256 of big-endian entropy bytes;
::  shay returns hash with byte[0] as LSB
++  sha256-hash
  |=  [=hex =width]
  (shay [(count-bytes width) (rev 3 (count-bytes width) hex)])
::
::  top .cs-lent bits of (sha256-hash (LSB of hex))
++  derive-checksum
  |=  [=hex =width]
  ^-  @ux
  (rsh [0 (sub 8 (count-cs-lent width))] (end [3 1] (sha256-hash hex width)))
::
::  entropy bits followed by checksum bits
++  concat-eny-cs
  |=  [=hex =width]
  ^-  @ux
  (add (lsh [0 (count-cs-lent width)] hex) (derive-checksum hex width))
::
++  get-idx
  |=  [tot=@ud com=@ux k=@ud]
  ^-  @ud
  ::  0x7ff = 2.047
  (dis (rsh [0 (mul 11 (sub (dec tot) k))] com) 0x7ff)
--
::
::  mnemonym core
|%
++  me
  |_  [tweaked=? =width wordlist=(list word)]
  ::
  ::  urbit ship core
  ++  ship
    |%
    ++  en
      |=  =nym
      ^-  ^ship
      `@p`(en:hex nym)
    ::
    ++  de
      |=  =^ship
      ^-  nym
      ?.  =(%pawn (clan:title ship))
        ~|  %fallen-clan
        !!
      (de:hex `@ux`ship)
    --
  ::
  ++  come
    |=  [=bowl:gall who=@pG]
    ^-  @pH
    ::  non-comet ships are unverifable,
    ::  more in-practice than in-theory
    ~|  %unverifiable
    ?<  tweaked
    ~|  %wrong-bitwidth
    ?>  =(128 width)
    ?:  =(%pawn (clan:title who))
      ~|(%already-a-comet !!)
    =/  lyf=@ud
      .^(@ud %j /(scot %p our.bowl)/life/(scot %da now.bowl)/(scot %p who))
    =/  key=(unit [crypto-suite=@ud =pass])
      .^  (unit [crypto-suite=@ud =pass])
          %j
          /(scot %p our.bowl)/puby/(scot %da now.bowl)/(scot %p who)/(scot %ud lyf)
      ==
    ?~  key
      ~|(%key-not-found !!)
    `@pH`fig:ex:(com:nu:cric:crypto pass.u.key)
  ::
  ::  generate a random unverified nym from given eny
  ++  gene
    |=  eny=@uvJ
    ^-  nym
    ~|  %cant-generate-a-verified-nym
    ?<  tweaked
    (de:hex (~(rad og eny) (bex width)))
  ::
  ::  abridge a nym e.g. ..abet...escape
  ++  brig
    |=  =nym
    ^-  ^nym
    =/  nym-tape=tape  (trip nym)
    ?.  ?=(^ nym-tape)  !!
    ?.  ?=(^ t.nym-tape)  !!
    ?>  .=  tweaked
        ?&  ?!  ?&  =('.' i.nym-tape)
                    =('.' i.t.nym-tape)
                ==
            =('.' i.nym-tape)
        ==
    =/  word-tapes=(list tape)
      %-  split-by-dots
        ?:  ?&  =('.' i.nym-tape)
                =('.' i.t.nym-tape)
            ==
          t.t.nym-tape
        ?.  =('.' i.nym-tape)
          !!
        t.nym-tape
    ?:  (lte (lent word-tapes) 2)
      nym
    =/  first=tape  (snag 0 word-tapes)
    =/  last=tape   (snag (dec (lent word-tapes)) word-tapes)
    %-  crip
    %-  zing
    :~  ?:(tweaked "." "..")
        first
        "..."
        last
    ==
  ::
  ::  foreshorten a nym e.g. ..abet.baboon..denounce.escape
  ++  fore
    |=  =nym
    ^-  ^nym
    =/  nym-tape=tape  (trip nym)
    ?.  ?=(^ nym-tape)  !!
    ?.  ?=(^ t.nym-tape)  !!
    ?>  .=  tweaked
        ?&  ?!  ?&  =('.' i.nym-tape)
                    =('.' i.t.nym-tape)
                ==
            =('.' i.nym-tape)
        ==
    =/  word-tapes=(list tape)
      %-  split-by-dots
        ?:  ?&  =('.' i.nym-tape)
                =('.' i.t.nym-tape)
            ==
          t.t.nym-tape
        ?.  =('.' i.nym-tape)
          !!
        t.nym-tape
    ?:  (lte (lent word-tapes) 4)
      nym
    %-  crip
    %-  zing
    :~  ?:(tweaked "." "..")
        (snag 0 word-tapes)
        "."
        (snag 1 word-tapes)
        ".."
        (snag (sub (lent word-tapes) 2) word-tapes)
        "."
        (snag (dec (lent word-tapes)) word-tapes)
    ==
  ::
  ::  hexidecimal core
  ++  hex
    |%
    ::
    ::  hex to nym
    ++  de
      |=  =^hex
      ^-  nym
      =/  combined=@ux  (concat-eny-cs hex width)
      =/  total=@ud     (count-total-words (count-total-bits width))
      ::  get words
      =/  words=(list tape)
        %-  turn
        :_  ::  get index from wordlist
            |=  idx=@ud
            %-  trip
            %+  snag
              idx
            wordlist
        ^-  (list @ud)
        ::  drop leading zero indices
        =/  indices=(list @ud)
          ::  extract 11-bit indices
          %-  flop
          %+  roll
            (gulf 0 (dec total))
          |=  [k=@ud acc=(list @ud)]
          [(get-idx total combined k) acc]
        |-
        ?~  indices
          indices
        ?:  =(0 i.indices)
          $(indices t.indices)
        indices
      ::
      ::  format string
      %-  crip
      %+  welp
        ?:(tweaked "." "..")
      ?~  words
        ""
      %-  zing
      :-  i.words
      (turn t.words |=(w=tape (weld "." w)))
    ::
    ::  nym to hex
    ++  en
      |=  =nym
      ^-  ^hex
      =/  nym-tape=tape  (trip nym)
      ::  prove at least two chars exist so type system allows t.t access
      ?.  ?=(^ nym-tape)  !!
      ?.  ?=(^ t.nym-tape)  !!
      =/  word-tapes=(list tape)
        %-  split-by-dots
        ?:  ?&(=('.' i.nym-tape) =('.' i.t.nym-tape))
          t.t.nym-tape
        ?.  =('.' i.nym-tape)  !!
        t.nym-tape
      ::  rebuild combined atom from indices, MSB first
      =/  combined=@ux
        %+  roll
          %+  weld
            (reap (sub (count-total-words (count-total-bits width)) (lent word-tapes)) 0)
          (turn word-tapes |=(wt=tape (need (find ~[(crip wt)] wordlist))))
        |=  [idx=@ud acc=@ux]
        (add (lsh [0 11] acc) idx)
      ?>  .=  (dis combined (dec (lsh [0 (count-cs-lent width)] 1)))
          (derive-checksum `@ux`(rsh [0 (count-cs-lent width)] combined) width)
      (rsh [0 (count-cs-lent width)] combined)
    --
  ::
  ::  finish nym, fail if there's >1 candidate
  ++  fini
    |=  [query=nym nyms=(list nym)]
    ^-  (unit nym)
    =/  nym-tape=tape  (trip query)
    ?.  ?=(^ nym-tape)  ~
    ?.  =('.' i.nym-tape)  ~
    ?.  ?=(^ t.nym-tape)  ~
    =/  iparts=(list tape)
      %-  split-by-dots
        ?:  =('.' i.t.nym-tape)
          t.t.nym-tape
        t.nym-tape
    ?.  ?=(^ iparts)  ~
    |-  ^-  (unit nym)
    ?~  nyms  ~
    =/  cand=nym  i.nyms
    =/  cand-tape=tape  (trip cand)
    =/  match=?
      ?.  ?=(^ cand-tape)  .n
      ?.  =('.' i.cand-tape)  .n
      ?.  ?=(^ t.cand-tape)  .n
      =/  ip=(list tape)  iparts
      =/  cp=(list tape)
        %-  split-by-dots
        ?:  =('.' i.t.cand-tape)
          t.t.cand-tape
        t.cand-tape
      |-  ^-  ?
      ?~  ip  .y
      ?~  cp  .n
      ?~  t.ip
        ::  last word of incomplete: check it is a prefix of the candidate word
        =/  fl  i.ip
        =/  cl  i.cp
        |-  ^-  ?
        ?~  fl  .y
        ?~  cl  .n
        ?.  =(i.fl i.cl)  .n
        $(fl t.fl, cl t.cl)
      ::  non-last word: must match exactly
      ?.  =(i.ip i.cp)  .n
      $(ip t.ip, cp t.cp)
    ?:  match  `cand
    $(nyms t.nyms)
  ::
  ::  validate that a nym is well-formed
  ++  vali
    |=  =nym
    ^-  ?
    =/  nym-tape=tape  (trip nym)
    ?.  ?=(^ nym-tape)  !!
    ?.  ?=(^ t.nym-tape)  !!
    ?>  .=  tweaked
        ?&  ?!  ?&  =('.' i.nym-tape)
                    =('.' i.t.nym-tape)
                ==
            =('.' i.nym-tape)
        ==
    =/  word-tapes=(list tape)
      %-  split-by-dots
        ?:  ?&  =('.' i.nym-tape)
                =('.' i.t.nym-tape)
            ==
          t.t.nym-tape
        ?.  =('.' i.nym-tape)
          !!
        t.nym-tape
    ?>  %+  lte
          (lent word-tapes)
        (count-total-words (count-total-bits width))
    =/  combined=@ux
      %+  roll
        %+  weld
          (reap (sub (count-total-words (count-total-bits width)) (lent word-tapes)) 0)
        (turn word-tapes |=(wt=tape (need (find ~[(crip wt)] wordlist))))
      |=  [idx=@ud acc=@ux]
      (add (lsh [0 11] acc) idx)
    ?>  .=  (dis combined (dec (lsh [0 (count-cs-lent width)] 1)))
        (derive-checksum (rsh [0 (count-cs-lent width)] combined) width)
    .y
  ::
  ::  grow a nym by one word if there's only one candidate
  ++  grow
    |=  =nym
    ^-  (unit ^nym)
    =/  nym-tape=tape  (trip nym)
    ?.  ?=(^ nym-tape)  ~
    ?.  =('.' i.nym-tape)  ~
    ?.  ?=(^ t.nym-tape)  ~
    =/  parts=(list tape)
      %-  split-by-dots
        ?:  =('.' i.t.nym-tape)
          t.t.nym-tape
        t.nym-tape
    ?.  ?=(^ parts)  ~
    =/  fragment=tape
      =/  lst  parts
      |-  ^-  tape
      ?~  t.lst  i.lst
      $(lst t.lst)
    =/  matches=(list word)
      %+  skim  wordlist
      |=  w=word
      =/  wt=tape  (trip w)
      ?:  (gth (lent fragment) (lent wt))  .n
      =((scag (lent fragment) wt) fragment)
    ?.  =(1 (lent matches))  ~
    =/  new-parts=(list tape)
      =/  lst  parts
      |-  ^-  (list tape)
      ?~  t.lst
        ~[(trip (snag 0 matches))]
      [i.lst $(lst t.lst)]
    :-  ~
    %-  crip
    %+  welp
      ?:(=('.' i.t.nym-tape) ".." ".")
    ?~  new-parts  ""
    %-  zing
    :-  i.new-parts
    (turn t.new-parts |=(w=tape (weld "." w)))
  --
--
