|%
++  tool
  =<  tool
  |%
  +$  name  @t
  +$  desc  @t
  ::
  +$  parameters
    (map name:parameter def:parameter)
  ::
  +$  required
    (list name:parameter)
  ::
  +$  tool
    $+  mcp-tool
    $:  =name
        =desc
        =parameters
        =required
        =thread-builder
    ==
  ::
  +$  thread-builder
    $+  mcp-thread-builder
    $-((map name:parameter argument) shed:khan)
  ::
  +$  argument
    $@  ~
    $%  [%string p=@t]
        [%number p=@ud]
        [%boolean p=?]
        [%array p=(list argument)]
        [%object p=(map @t argument)]
    ==
  ::
  ++  parameter
    |%
    +$  name  @t
    ::
    +$  type
      $+  mcp-parameter-type
      $?  %array
          %boolean
          %number
          %object
          %string
      ==
    ::
    +$  def
      $+  mcp-parameter-definition
      $:  =type
          desc=@t
      ==
    --
  --
::
++  resource
  =<  resource
  |%
  +$  resource
    $+  mcp-resource
    $:  uri=@t
        name=@t
        desc=@t
        mime-type=(unit @t)
        annotations=(unit annotations)
    ==
  ::
  +$  annotations
    $+  mcp-resource-annotations
    $:  audience=(list @t)
        priority=(unit @rs)
        last-modified=(unit @t)
    ==
  --
::
++  prompt
  =<  prompt
  |%
  +$  prompt
    $+  mcp-prompt
    $:  name=@t
        title=@t
        desc=@t
        arguments=(list argument)
        icons=(list icon)
        messages-builder=$-((map name:argument @t) (list message))
    ==
  ::
  ++  argument
     =<  argument
     |%
     +$  name  @t
     ::
     +$  argument
       $+  mcp-prompt-argument
       $:  =name
           desc=@t
           required=?
       ==
     --
  ::
  +$  icon
    $+  mcp-prompt-icon
    $:  src=@t
        mime-type=@t
        sizes=(list @t)
    ==
  ::
  +$  message
    $+  mcp-prompt-message
    $:  =role
        =content
    ==
  ::
  +$  role
    $?  %assistant
        %user
    ==
  ::
  ::  XX support audio, image, resource
  +$  content
    $+  mcp-prompt-message-content
    $:  =type
        text=(unit @t)
    ==
  ::
  +$  type
    $+  mcp-prompt-message-content-type
    $?  %audio
        %image
        %resource
        %text
    ==
  --
--
