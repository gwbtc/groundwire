/-  *spv-wallet
/+  io=sailboxio, html-utils, multipart, tarball, bip39, bip32=bip32-spv, btc=bitcoin,
    txns=transactions
|%
::  Handle test pages (timer, PDF, MP4, mempool tests)
::
++  handle-test-pages
  |=  [site=(list @t) args=key-value-list:kv:html-utils]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  ?+    site  !!
        [%spv-wallet %timer ~]
      =.  io  io(hold &) :: claim the mutex
      ?+  action  !!
          %start
        ;<  state=state-0  bind:m  (get-state-as:io state-0)
        =.  state  state(counter 0)
        ;<  ~  bind:m  (replace:io !>(state))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        |-
        ;<  state=state-0  bind:m  (get-state-as:io state-0)
        ?:  (gte counter.state 5)
          (pure:m ~)
        ;<  ~  bind:m  (replace:io !>(state(counter +(counter.state))))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        ;<  ~  bind:m  (sleep:io ~s1)
        $
      ==
      ::
      [%spv-wallet %pdf-test ~]
    ?+  action  !!
        %clear
      ;<  state=state-0  bind:m  (get-state-as:io state-0)
      =.  pdf.state  ~
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/stream/pdf-test ~ `'pdf-update')
    ==
      [%spv-wallet %mp4-test ~]
    ?+  action  !!
        %clear
      ;<  state=state-0  bind:m  (get-state-as:io state-0)
      =.  mp4.state  ~
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/stream/mp4-test ~ `'mp4-update')
    ==
      [%spv-wallet %mempool-test ~]
    ?+  action  !!
        %fetch
      =/  url=tape  "https://mempool.space/api/v1/fees/recommended"
      ;<  result=json  bind:m  (fetch-json:io url)
      ;<  state=state-0  bind:m  (get-state-as:io state-0)
      =.  mempool-result.state  `result
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/mempool-test ~ `'fetch-result')
    ==
      [%spv-wallet %delete ~]
    ?+  action  !!
        %delete
      =/  pubkey=@ux  (rash (need (get-key:kv:html-utils 'pubkey' args)) hex)
      ;<  state=state-0  bind:m  (get-state-as:io state-0)
      :: Send progress event
      =.  wallets.state  (~(del by wallets.state) pubkey)
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
    ==
  ==
::  Handle tarball file browser actions
::
++  handle-ball-actions
  |=  [ball-path=(list @t) args=key-value-list:kv:html-utils]
  ::  Extract ball path (everything after /spv-wallet/ball/)
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  =/  m  (fiber:io ,~)
  ^-  form:m
  ?+  action  !!
      %delete-file
    =/  filename=@ta  (need (get-key:kv:html-utils 'filename' args))
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =.  ball.state  (~(del ba:tarball ball.state) ball-path filename)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
      %create-folder
    =/  foldername=@ta  (need (get-key:kv:html-utils 'foldername' args))
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  new-path=path  (weld ball-path /[foldername])
    =.  ball.state  (~(mkd ba:tarball ball.state) new-path ~)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
      %create-symlink
    =/  link-name=@ta  (need (get-key:kv:html-utils 'linkname' args))
    =/  target=@t  (need (get-key:kv:html-utils 'target' args))
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    ;<  now=@da  bind:m  get-time:io
    ::  Parse target path into road
    =/  target-road=(unit road:tarball)  (parse-road:tarball target)
    ?~  target-road
      ::  Invalid path, just return
      (pure:m ~)
    ::  Create symlink content with metadata
    =/  symlink-metadata=(map @t @t)
      %-  ~(gas by *(map @t @t))
      :~  ['date-created' (scot %da now)]
          ['mtime' (da-oct:tarball now)]
      ==
    =/  symlink-content=content:tarball
      [%symlink symlink-metadata u.target-road]
    ::  Add symlink to ball at current path
    =.  ball.state  (~(put ba:tarball ball.state) ball-path link-name symlink-content)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
      %delete-folder
    =/  foldername=@ta  (need (get-key:kv:html-utils 'foldername' args))
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  folder-path=path  (weld ball-path /[foldername])
    =.  ball.state  (~(lop ba:tarball ball.state) folder-path)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ==
::  Handle file uploads (PDF, MP4, tarball)
::
++  handle-uploads
  |=  $:  host=@t
          [ext=(unit @ta) site=(list @t)]
          parts=(list [@t part:multipart])
      ==
  =/  m  (fiber:io ,~)
  ^-  form:m
  ?+    site  (pure:m ~)
      [%spv-wallet %pdf-test ~]
    ::  Extract the PDF from the parts
    =/  pdf-part=(unit part:multipart)
      |-  ?~  parts  ~
      ?:  =('pdf' -.i.parts)
        `+.i.parts
      $(parts t.parts)
    ?~  pdf-part
      (pure:m ~)
    ::  Convert part to mime
    =/  pdf-mime=mime
      [/application/pdf [(met 3 body.u.pdf-part) body.u.pdf-part]]
    ::  Store the PDF in state
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =.  pdf.state  `pdf-mime
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/pdf-test ~ `'pdf-update')
      ::
      [%spv-wallet %mp4-test ~]
    ::  Extract the MP4 from the parts
    =/  mp4-part=(unit part:multipart)
      |-  ?~  parts  ~
      ?:  =('mp4' -.i.parts)
        `+.i.parts
      $(parts t.parts)
    ?~  mp4-part
      (pure:m ~)
    ::  Convert part to mime
    =/  mp4-mime=mime
      [/video/mp4 [(met 3 body.u.mp4-part) body.u.mp4-part]]
    ::  Store the MP4 in state
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =.  mp4.state  `mp4-mime
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/mp4-test ~ `'mp4-update')
      ::
      [%spv-wallet %ball *]
    ::  Process all uploaded files using library function
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    ;<  now=@da  bind:m  get-time:io
    =.  ball.state  (from-parts:tarball ball.state t.t.site parts now)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ==
--
