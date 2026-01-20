/-  urb, spider
/+  strandio
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [par=@p sed=@uw]
  (need !<((unit [@p @uw]) args))
::
::  run %reg-tester
;<    ~
    bind:m
  %-  poke-our:strandio
  :*  %reg-tester
      %noun
      !>
      :*  sed
          ~
          [%escape par]
  ==  ==
(pure:m !>(~))
