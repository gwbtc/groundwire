/-  urb, spider
/+  strandio
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [por=@ud sed=@uw]
  (need !<((unit [@ud @uw]) args))
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
          [%fief `[%if .127.0.0.1 por]]
  ==  ==
(pure:m !>(~))
