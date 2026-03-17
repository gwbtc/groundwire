/-  s=spv-wallet, urb
/+  sailbox, fi=feather-icons, *ui-layout, *wallet-address, wallet-account, ui-utils, bitcoin, taproot, btc-script, urb-decoder
|%
::  Compute script-hash from address string for indexer lookups
::
++  address-to-script-hash
  |=  addr=@t
  ^-  @ux
  =/  script-pubkey=hexb:bitcoin
    ?:  (is-taproot-address:taproot addr)
      (to-script-pubkey:taproot addr)
    (to-script-pubkey:adr:bitcoin [%bech32 addr])
  (sha-256:sha dat.script-pubkey)
::  Render indexer subscription status and button
::
++  indexer-status-ui
  |=  [pubkey=@ux addr=@t chain=@t index=@ud is-subscribed=?]
  ^-  manx
  ?:  is-subscribed
    ;div(style "display: flex; align-items: center; gap: 12px;")
      ;div.p2.br1(style "display: flex; align-items: center; gap: 6px; background: rgba(100, 200, 100, 0.15); border: 1px solid rgba(100, 200, 100, 0.4);")
        ;div(style "width: 8px; height: 8px; border-radius: 50%; background: rgba(100, 200, 100, 0.8);");
        ;span.f3.s-1(style "color: var(--f2);"): Subscribed
      ==
      ;button.p2.b1.br2.hover.pointer
        =onclick  "unsubscribeFromIndexer('{(hexn:sailbox pubkey)}', '{(trip addr)}')"
        =style  "display: flex; align-items: center; gap: 6px; background: rgba(255, 100, 100, 0.15); border: 1px solid rgba(255, 100, 100, 0.4); color: var(--f3); outline: none;"
        ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'x')
        ==
        ;span.f2: Unsubscribe
      ==
    ==
  ;div(style "display: flex; align-items: center; gap: 12px;")
    ;div.p2.br1(style "display: flex; align-items: center; gap: 6px; background: var(--b2); border: 1px solid var(--b3);")
      ;div(style "width: 8px; height: 8px; border-radius: 50%; background: var(--f3); opacity: 0.4;");
      ;span.f3.s-1(style "color: var(--f3); opacity: 0.7;"): Not subscribed
    ==
    ;button.p2.b1.br2.hover.pointer
      =onclick  "subscribeToIndexer('{(hexn:sailbox pubkey)}', '{(trip addr)}', '{(trip chain)}', '{(scow %ud index)}')"
      =style  "display: flex; align-items: center; gap: 6px; background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); color: var(--f3); outline: none;"
      ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'radio')
      ==
      ;span.f2: Subscribe
    ==
  ==
::  Helper to extract balance stats from address-details
::
++  extract-balance-stats
  |=  addr-details=address-details
  ^-  [funded-sats=@ud spent-sats=@ud balance-sats=@ud tx-count=@ud]
  ?~  info.addr-details
    [0 0 0 0]
  =/  funded=@ud  chain-funded.u.info.addr-details
  =/  spent=@ud  chain-spent.u.info.addr-details
  =/  balance=@ud  (sub funded spent)
  =/  txs=@ud  tx-count.u.info.addr-details
  [funded spent balance txs]
::
++  address-refresh-button
  |=  [pubkey=@ux chain=@t index=@ud refresh-state=(unit [pid=@ta act=?])]
  ^-  manx
  ?~  refresh-state
    ::  No refresh running
    ;div.p3.b2.br2.hover.pointer(onclick "refreshAddress('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})", style "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2);")
      ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'refresh-cw')
      ==
      ;span.f2.bold.f-3: Refresh Address
    ==
  ::  Refresh running - show progress and controls
  =/  [refresh-pid=@ta refresh-act=?]  u.refresh-state
  =/  border-color=tape  ?:(refresh-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
  =/  bg-color=tape  ?:(refresh-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
  =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
  ;div.p3.b2.br2(style container-style)
    ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
      ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
        ;+  ?:  refresh-act
              ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                ;+  (make:fi 'loader')
              ==
            ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'pause-circle')
            ==
      ==
      ;div(style "display: flex; flex-direction: column; gap: 4px;")
        ;div.f2.bold: {?:(refresh-act "Refreshing Address..." "Refresh Paused")}
        ;div.f3.s-1: Fetching latest data from blockchain
      ==
    ==
    ;div(style "display: flex; gap: 4px;")
      ;+  ?:  refresh-act
            ::  Active - show pause button
            ;button.p2.b1.br1.hover.pointer(title "Pause refresh", onclick "pauseRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})", style "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'pause')
              ==
            ==
          ::  Paused - show play button
          ;button.p2.b1.br1.hover.pointer(title "Resume refresh", onclick "resumeRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})", style "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'play')
            ==
          ==
      ;button.p2.b1.br1.hover.pointer(title "Cancel refresh", onclick "cancelRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})", style "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
        ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'x-circle')
        ==
      ==
    ==
  ==
::
++  tx-verify-status-badge
  |=  [txid=@t verification-status=(unit (unit tang))]
  ^-  manx
  ::  Verification status badge (display only) - no class by default
  ?~  verification-status
    ::  Unverified - show question mark
    ;span.p1.br1(id "tx-status-badge-{(trip txid)}", style "background: rgba(150, 150, 150, 0.3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Unverified")
      ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'help-circle')
      ==
    ==
  ?~  u.verification-status
    ::  Verified successfully - show check
    ;span.p1.br1(id "tx-status-badge-{(trip txid)}", style "background: rgba(50, 200, 100, 0.3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "SPV Verified")
      ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'check-circle')
      ==
    ==
  ::  Verification failed with error - show X
  ;span.p1.br1(id "tx-status-badge-{(trip txid)}", style "background: rgba(255, 80, 80, 0.3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Verification Failed")
    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
      ;+  (make:fi 'x-circle')
    ==
  ==
::
++  tx-verify-status-badge-oob
  |=  [txid=@t verification-status=(unit (unit tang))]
  ^-  manx
  =/  badge=manx  (tx-verify-status-badge txid verification-status)
  ::  Add pulse-badge class for SSE animation
  =/  with-animation=manx
    badge(a.g [[%class "pulse-badge"] a.g.badge])
  (hx-swap-oob with-animation)
::
++  tx-verify-badge
  |=  $:  txid=@t
          pubkey=@ux
          tx-verify-proc=(unit [pid=@ta act=?])
          verification-status=(unit (unit tang))
      ==
  ^-  manx
  ::  Show verification status badge and button together
  ;span(id "tx-verify-badge-{(trip txid)}", style "display: flex; align-items: center; gap: 8px; flex-shrink: 0;")
    ::  Verify button (clickable)
    ;+  ?~  tx-verify-proc
          ::  Not verifying - show gray verify button
          ;button.p1.b0.br1.hover.pointer
            =onclick  "verifyTransaction('{(hexn:sailbox pubkey)}', '{(trip txid)}')"
            =title  "Verify transaction"
            =style  "background: rgba(150, 150, 150, 0.15); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'rotate-cw')
            ==
          ==
        =/  [proc-pid=@ta proc-act=?]  u.tx-verify-proc
        ?:  proc-act
          ::  Active - show spinning loader
          ;div.p1.b0.br1(style "background: rgba(100, 150, 255, 0.15); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center;")
            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
              ;+  (make:fi 'loader')
            ==
          ==
        ::  Paused - show pause icon
        ;div.p1.b0.br1(style "background: rgba(150, 150, 150, 0.15); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center;")
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'pause-circle')
          ==
        ==
    ::  Verification status badge (display only)
    ;+  (tx-verify-status-badge txid verification-status)
  ==
::  Render a tapscript sub-row (indented under main address)
::
++  tapscript-row
  |=  $:  index=@ud
          script-name=@t
          details=address-details
          pubkey=@ux
          chain=@t
          now=@da
          tapscript-proc-map=(map @t [pid=@ta act=?])
      ==
  ^-  manx
  ::  Extract balance stats
  =/  [funded-sats=@ud spent-sats=@ud balance-sats=@ud tx-count=@ud]
    (extract-balance-stats details)
  =/  has-txs=?  (gth tx-count 0)
  =/  tapscript-addr=tape  (trip address.details)
  =/  tapscript-addr-cord=@t  address.details
  ::  Check if loading
  =/  is-loading=?  (~(has by tapscript-proc-map) tapscript-addr-cord)
  =/  row-classes=tape
    ?:  is-loading
      "p2 b1 br2 hover"
    ?:(has-txs "p2 b1 br2 hover" "p2 b1 br2 hover empty-address")
  ::  Calculate staleness color (same as main row)
  =/  last-check-text=tape
    ?~  last-check.details
      "Never checked"
    =/  day-only=@da  `@da`(sub u.last-check.details (mod u.last-check.details ~d1))
    (weld "Last checked: " (scow %da day-only))
  =/  bg-color=tape
    ?~  last-check.details
      "rgba(200, 80, 80, 0.2)"
    =/  time-since=@dr  (sub now u.last-check.details)
    ?:  (lth time-since ~h1)
      "var(--b2)"
    ?:  (lth time-since ~h6)
      "rgba(200, 180, 80, 0.15)"
    ?:  (lth time-since ~d1)
      "rgba(220, 140, 80, 0.2)"
    "rgba(200, 80, 80, 0.2)"
  ;div(class row-classes, style "display: flex; justify-content: space-between; align-items: center; gap: 12px; margin-left: 24px; border-left: 2px solid var(--b3); padding-left: 12px;")
    ;div(style "flex: 1; min-width: 0;")
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;span.f3.s-2(style "opacity: 0.6;"): └─
        ;span.f3.s-2.mono(style "color: var(--f2);"): {(trip script-name)}
        ;+  ?~  info.details
              ;span;
            ;div(style "display: flex; gap: 8px;")
              ;span.f3.s-2(style "opacity: 0.8;")
                ; • {(scow %ud tx-count)} txs
              ==
              ;span.f3.s-2(style "opacity: 0.8;")
                ; • {(numb:sailbox balance-sats)} sats
              ==
            ==
      ==
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{tapscript-addr}')"
          =title  "Copy tapscript address"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 20px; height: 20px; justify-content: center; outline: none;"
          ;div(style "width: 10px; height: 10px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
        ;a.mono.f3.s-2(href "/spv-wallet/account/{(hexn:sailbox pubkey)}/tapscript/{(trip chain)}/{(scow %ud index)}/{tapscript-addr}", style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); opacity: 0.8; text-decoration: none;")
          ; {tapscript-addr}
        ==
      ==
    ==
    ::  Buttons - loading state or normal state
    ;div(style "display: flex; gap: 4px;")
      ;+  ?:  is-loading
            ::  Loading state - show spinner/pause-circle, pause/play, and cancel button
            =/  proc-entry=[pid=@ta act=?]  (~(got by tapscript-proc-map) tapscript-addr-cord)
            =/  proc-act=?  act.proc-entry
            ;div(style "display: flex; gap: 4px;")
              ;+  ?:  proc-act
                    ::  Active - show spinning loader
                    ;div.p1.b1.br1(style "background: rgba(100, 150, 255, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center;")
                      ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                        ;+  (make:fi 'loader')
                      ==
                    ==
                  ::  Paused - show pause-circle
                  ;div.p1.b1.br1(style "background: rgba(150, 150, 150, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center;")
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'pause-circle')
                    ==
                  ==
              ;+  ?:  proc-act
                    ::  Active - show pause button
                    ;button.p1.b1.br1.hover.pointer
                      =title  "Pause this refresh"
                      =onclick  "pauseTapscriptRefresh('{(hexn:sailbox pubkey)}', '{tapscript-addr}')"
                      =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                      ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                        ;+  (make:fi 'pause')
                      ==
                    ==
                  ::  Paused - show play button
                  ;button.p1.b1.br1.hover.pointer
                    =title  "Resume this refresh"
                    =onclick  "resumeTapscriptRefresh('{(hexn:sailbox pubkey)}', '{tapscript-addr}')"
                    =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'play')
                    ==
                  ==
              ;button.p1.b1.br1.hover.pointer
                =title  "Cancel this operation"
                =onclick  "cancelTapscriptRefresh('{(hexn:sailbox pubkey)}', '{tapscript-addr}')"
                =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'x-circle')
                ==
              ==
            ==
          ::  Normal state - show refresh and delete buttons
          ;div(style "display: flex; gap: 4px;")
            ;button.p1.b1.br1.hover.pointer
              =title  last-check-text
              =onclick  "refreshTapscript('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)}, '{tapscript-addr}')"
              =style  "background: {bg-color}; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'refresh-cw')
              ==
            ==
            ;button.p1.b1.br1.hover.pointer
              =title  "Delete this tapscript address"
              =onclick  "deleteTapscript('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)}, '{tapscript-addr}')"
              =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'trash-2')
              ==
            ==
          ==
    ==
  ==
::
++  address-row
  |=  [index=@ud leaf-mop=((mop @ud hd-leaf) gth) now=@da pubkey=@ux chain=@t proc-map=(map @ud [pid=@ta act=?]) tapscript-proc-map=(map @t [pid=@ta act=?])]
  ^-  manx
  ::  Look up this index in the mop
  =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
  =/  maybe-details=(unit address-details)
    ?~(leaf ~ `main.u.leaf)
  ::  Check if there's a running process for this address
  =/  is-loading=?  (~(has by proc-map) index)
  ?~  maybe-details
    ::  Underived - no address derived at this index yet
    ;div.p3.b1.br2.empty-address(id "address-row-{(trip chain)}-{(scow %ud index)}", style "display: flex; justify-content: space-between; align-items: center; gap: 12px; opacity: 0.4;")
      ;div(style "flex: 1; min-width: 0;")
        ;div(style "display: flex; align-items: center; gap: 8px;")
          ;span.f3.s-2.mono: Index {(scow %ud index)}
        ==
        ;div.f3.s-1.italic: (underived)
      ==
      ;div(style "display: flex; gap: 4px;")
        ;button.p2.b1.br1.hover.pointer
          =title  "Derive and scan this address"
          =onclick  "refreshAddress('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
          =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
          ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'refresh-cw')
          ==
        ==
      ==
    ==
  ::  Populated slot - show address details
  =/  details=address-details  u.maybe-details
  =/  last-check-text=tape
    ?~  last-check.details
      "Never checked"
    =/  day-only=@da  `@da`(sub u.last-check.details (mod u.last-check.details ~d1))
    (weld "Last checked: " (scow %da day-only))
  ::  Check if paused
  =/  is-paused=?
    ?:  is-loading
      =/  proc-entry=[pid=@ta act=?]  (~(got by proc-map) index)
      =(act.proc-entry %.n)
    %.n
  ::  Calculate staleness color
  =/  bg-color=tape
    ?~  last-check.details
      "rgba(200, 80, 80, 0.2)"
    =/  time-since=@dr  (sub now u.last-check.details)
    ?:  (lth time-since ~h1)
      "var(--b2)"
    ?:  (lth time-since ~h6)
      "rgba(200, 180, 80, 0.15)"
    ?:  (lth time-since ~d1)
      "rgba(220, 140, 80, 0.2)"
    "rgba(200, 80, 80, 0.2)"
  =/  row-style=tape
    ?:  is-paused
      "display: flex; justify-content: space-between; align-items: center; gap: 12px; opacity: 0.6;"
    "display: flex; justify-content: space-between; align-items: center; gap: 12px;"
  ::  Determine if this address has transactions
  =/  has-txs=?
    ?~  info.details  %.n
    (gth tx-count.u.info.details 0)
  ::  Only mark as empty if not loading AND has no transactions
  =/  is-empty=?  &(!is-loading !has-txs)
  =/  row-classes=tape  ?:(is-empty "p3 b1 br2 hover empty-address" "p3 b1 br2 hover")
  ::  Get tapscript addresses from the leaf (keyed by address)
  =/  tapscripts=(list [@t tapscript-details])
    ?~  leaf  ~
    ~(tap by script-trees.u.leaf)
  =/  group-classes=tape  ?:(is-empty "fc g1 empty-address" "fc g1")
  ;div(id "address-group-{(trip chain)}-{(scow %ud index)}", class group-classes)
    ;div(id "address-row-{(trip chain)}-{(scow %ud index)}", class row-classes, style row-style)
    ;div(style "flex: 1; min-width: 0;")
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;span.f3.s-2.mono: Index {(scow %ud index)}
        ::  Show tx count and balance if available
        ;+  ?~  info.details
              ;span;
              =/  tx-count=@ud  tx-count.u.info.details
              =/  balance=@ud
                (sub chain-funded.u.info.details chain-spent.u.info.details)
              ;div(style "display: flex; gap: 8px;")
                ;span.f3.s-2(style "opacity: 0.8;")
                  ; • {(scow %ud tx-count)} txs
                ==
                ;span.f3.s-2(style "opacity: 0.8;")
                  ; • {(numb:sailbox balance)} sats
                ==
              ==
      ==
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{(trip address.details)}')"
          =title  "Copy address"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
        ;a.mono.f2.s-1(href "/spv-wallet/account/{(hexn:sailbox pubkey)}/address/{(trip chain)}/{(scow %ud index)}", style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none;")
          ; {(trip address.details)}
        ==
      ==
    ==
    ;div(style "display: flex; gap: 4px;")
      ;+  ?:  is-loading
            ::  Loading state - show spinner/pause-circle, pause/play, and cancel button
            =/  proc-entry=[pid=@ta act=?]  (~(got by proc-map) index)
            =/  proc-act=?  act.proc-entry
            ;div(style "display: flex; gap: 4px;")
              ;+  ?:  proc-act
                    ::  Active - show spinning loader
                    ;div.p2.b1.br1(style "background: rgba(100, 150, 255, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center;")
                      ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                        ;+  (make:fi 'loader')
                      ==
                    ==
                  ::  Paused - show pause-circle
                  ;div.p2.b1.br1(style "background: rgba(150, 150, 150, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center;")
                    ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'pause-circle')
                    ==
                  ==
              ;+  ?:  proc-act
                    ::  Active - show pause button
                    ;button.p2.b1.br1.hover.pointer
                      =title  "Pause this refresh"
                      =onclick  "pauseRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
                      =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                      ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                        ;+  (make:fi 'pause')
                      ==
                    ==
                  ::  Paused - show play button
                  ;button.p2.b1.br1.hover.pointer
                    =title  "Resume this refresh"
                    =onclick  "resumeRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
                    =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                    ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'play')
                    ==
                  ==
              ;button.p2.b1.br1.hover.pointer
                =title  "Cancel this operation"
                =onclick  "cancelRefresh('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
                =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'x-circle')
                ==
              ==
            ==
          ::  Normal state - show refresh and delete buttons
          ;div(style "display: flex; gap: 4px;")
            ;button.p2.b1.br1.hover.pointer
              =title  last-check-text
              =onclick  "refreshAddress('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
              =style  "background: {bg-color}; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'refresh-cw')
              ==
            ==
            ;button.p2.b1.br1.hover.pointer
              =title  "Delete this address (can be re-derived later)"
              =onclick  "deleteAddress('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)})"
              =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'trash-2')
              ==
            ==
          ==
    ==
    ==  ::  end main row div
    ::  Render tapscript sub-rows
    ;*  %+  turn  tapscripts
        |=  [addr=@t =tapscript-details]
        (tapscript-row index name.tapscript-details address-details.tapscript-details pubkey chain now tapscript-proc-map)
  ==  ::  end container div
::
++  address-row-oob
  |=  [index=@ud leaf-mop=((mop @ud hd-leaf) gth) now=@da pubkey=@ux chain=@t proc-map=(map @ud [pid=@ta act=?]) tapscript-proc-map=(map @t [pid=@ta act=?])]
  ^-  manx
  (hx-swap-oob (address-row index leaf-mop now pubkey chain proc-map tapscript-proc-map))
::
++  address-list
  |=  [leaf-mop=((mop @ud hd-leaf) gth) now=@da pubkey=@ux chain=@t proc-map=(map @ud [pid=@ta act=?]) tapscript-proc-map=(map @t [pid=@ta act=?])]
  ^-  manx
  ::  Find highest index in mop
  =/  leaves-list=(list [@ud hd-leaf])
    (tap:((on @ud hd-leaf) gth) leaf-mop)
  =/  max-index=@ud
    ?~  leaves-list
      0
    =/  indices=(list @ud)  (turn leaves-list head)
    (roll indices max)
  ::  Generate full list from max-index down to 0 (or empty if none)
  =/  descending=(list @ud)
    ?~  leaves-list
      ~
    =/  full-range=(list @ud)  (gulf 0 max-index)
    (flop full-range)
  ;div.fc.g2(style "flex: 1; min-height: 0;")
    ::  Derive Next button (per-chain)
    ;div.p3.b2.br2.hover.pointer(onclick "refreshAddress('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud ?~(leaves-list 0 +(max-index)))})", style "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px dashed var(--b3);")
      ;div(style "font-size: 24px; color: var(--f-3);")
        ; +
      ==
      ;span.f2.bold.f-3: Derive Next Address (Index {(scow %ud ?~(leaves-list 0 +(max-index)))})
    ==
    ::  Scrollable address rows
    ;div.fc.g2(style "flex: 1; min-height: 0; overflow-y: auto;")
      ;*  ?~  descending
            :~  ;div.p4.b1.br2.tc
                  ;div.s0.f2.mb2: No addresses yet
                  ;div.f3.s-1: Click above to derive your first address
                ==
            ==
          %+  turn  descending
          |=  index=@ud
          (address-row index leaf-mop now pubkey chain proc-map tapscript-proc-map)
    ==
  ==
::
++  scan-status-oob
  |=  [pubkey=@ux scan-proc=(unit [pid=@ta act=? scn=account-scan])]
  ^-  manx
  ?~  scan-proc
    ::  No scan running - show Full Scan button
    ;div#scan-status.p3.b2.br2.hover.pointer(hx-swap-oob "true", onclick "fullScan('{(hexn:sailbox pubkey)}', 'receiving')", style "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2);")
      ;div(style "font-size: 24px; color: var(--f-3);")
        ; ↻
      ==
      ;span.f2.bold.f-3: Full Scan
    ==
  ::  Scan running - show progress and cancel button
  =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan-proc
  =/  scan-idx=@ud  idx.scn
  =/  scan-gap=@ud  gap.scn
  =/  border-color=tape  ?:(scan-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
  =/  bg-color=tape  ?:(scan-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
  =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
  ;div#scan-status.p3.b2.br2(hx-swap-oob "true", style container-style)
    ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
      ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
        ;+  ?:  scan-act
              ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                ;+  (make:fi 'loader')
              ==
            ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'pause-circle')
            ==
      ==
      ;div(style "display: flex; flex-direction: column; gap: 4px;")
        ;div.f2.bold: {?-(-.scn %1 "Phase 1: Receiving", %2 "Phase 2: Change")}
        ;div.f3.s-1: Index: {(scow %ud scan-idx)} • Gap: {(scow %ud scan-gap)}/20
      ==
    ==
    ;div(style "display: flex; gap: 4px;")
      ;+  ?:  scan-act
            ::  Active - show pause button
            ;button.p2.b1.br1.hover.pointer
              =title  "Pause full scan"
              =onclick  "pauseScan('{(hexn:sailbox pubkey)}')"
              =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'pause')
              ==
            ==
          ::  Paused - show play button
          ;button.p2.b1.br1.hover.pointer
            =title  "Resume full scan"
            =onclick  "resumeScan('{(hexn:sailbox pubkey)}')"
            =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'play')
            ==
          ==
      ;button.p2.b1.br1.hover.pointer
        =title  "Cancel full scan"
        =onclick  "cancelScan('{(hexn:sailbox pubkey)}')"
        =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
        ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'x-circle')
        ==
      ==
    ==
  ==
::  Handle GET request for address data JSON
::  Returns JSON data for a specific address if available
::
++  handle-address-data-get
  |=  $:  pubkey=@ux
          account-path=@t
          index=@ud
          chain=@t
          wallets=(map @ux wallet)
          accounts=(map @ux account-details)
      ==
  ^-  simple-payload:http
  =/  wallet=(unit wallet)  (~(get by wallets) pubkey)
  ?~  wallet
    [[404 ~] `(as-octs:mimes:html 'wallet not found')]
  ::  Convert to old format for iteration
  =/  wallet-accts-with-details=(map account:hd-path account-details)
    (wallet-accounts-with-details:wallet-account accounts.u.wallet accounts)
  ::  Find matching account
  =/  matching-account=(unit [account:hd-path account-details])
    %-  ~(rep by wallet-accts-with-details)
    |=  [[acct=account:hd-path details=account-details] result=(unit [account:hd-path account-details])]
    ?^  result  result
    =/  path-str=tape  (format-account-path acct)
    ?.  =(path-str (trip account-path))
      ~
    `[acct details]
  ?~  matching-account
    [[404 ~] `(as-octs:mimes:html 'account not found')]
  =/  ac  ~(. ac:wallet-account [+.u.matching-account active-network.+.u.matching-account])
  ::  Get address from appropriate mop
  =/  leaf-mop=((mop @ud hd-leaf) gth)
    ?:  =(chain 'receiving')
      receiving:ac
    change:ac
  =/  address=(unit address-details)
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
    ?~(leaf ~ `main.u.leaf)
  ?~  address
    [[404 ~] `(as-octs:mimes:html 'address not found')]
  ::  JSON endpoint removed - data is now stored in canonical form
  [[410 ~] `(as-octs:mimes:html 'JSON endpoint deprecated - data now in canonical form')]
::  Render address detail page
::
++  address-detail-page
  |=  $:  account-pubkey=@ux
          account-pubkey-hex=@t
          index=@ud
          chain=@t
          wallets=(map @ux wallet)
          accounts=(map @ux account-details)
          indexer-subs=(map @ux indexer-sub:s)
          now=@da
      ==
  ^-  manx
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    ;div.p5: Account not found
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ::  Extract tx-verify proc map for use in transaction list
  =/  tx-verify-proc-map=(map @t [pid=@ta act=?])  tx-verify.proc.u.details
  ::  Get address from appropriate mop
  =/  leaf-mop=((mop @ud hd-leaf) gth)
    ?:  =(chain 'receiving')
      receiving:ac
    change:ac
  =/  maybe-address=(unit address-details)
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
    ?~(leaf ~ `main.u.leaf)
  ?~  maybe-address
    ;div.p5: Address not found
  =/  addr-details=address-details  u.maybe-address
  ::  Extract balance data from canonical info
  =/  [funded-sats=@ud spent-sats=@ud balance-sats=@ud tx-count=@ud]
    (extract-balance-stats addr-details)
  ::  Filter account transactions to only include this address
  =/  this-address=@t  address.addr-details
  =/  all-transactions=(map @t transaction)  transactions:ac
  =/  relevant-transactions=(list [txid=@t tx=transaction])
    =/  filtered=(list [txid=@t tx=transaction])
      %+  murn  ~(tap by all-transactions)
      |=  [txid=@t tx=transaction]
      ^-  (unit [txid=@t tx=transaction])
      ::  Check if address appears in outputs (received)
      =/  has-output=?
        %+  lien  outputs.tx
        |=  out=tx-output
        =(address.out this-address)
      ::  Check if address appears in inputs (spent)
      =/  has-input=?
        %+  lien  inputs.tx
        |=  input=tx-input
        ?~  prevout.input  %.n
        =(address.u.prevout.input this-address)
      ::  Include if address appears in either inputs or outputs
      ?:(|(has-output has-input) `[txid tx] ~)
    ::  Sort by block height: unconfirmed first, then by descending block height
    %+  sort  filtered
    |=  [[txid-a=@t tx-a=transaction] [txid-b=@t tx-b=transaction]]
    ^-  ?
    ?-  tx-status.tx-a
      [%unconfirmed ~]
        ?-  tx-status.tx-b
          [%unconfirmed ~]  %.y  :: maintain order
          [%confirmed *]    %.y  :: unconfirmed comes first
        ==
      [%confirmed *]
        ?-  tx-status.tx-b
          [%unconfirmed ~]       %.n  :: confirmed comes after
          [%confirmed *]         :: compare block heights
            (gth +>:tx-status.tx-a +>:tx-status.tx-b)
        ==
    ==
  ::  Format derivation path - get account path from wallet if available
  =/  derivation-path=tape
    =/  chain-num=tape  ?:(=(chain 'receiving') "0" "1")
    =/  acct-path=(unit tape)
      ?~  wallet.u.details  ~
      =/  wallet-data=(unit wallet)  (~(get by wallets) u.wallet.u.details)
      ?~  wallet-data  ~
      ::  Find the account path from wallet's accounts map
      =/  found=(unit account:hd-path)
        %-  ~(rep by accounts.u.wallet-data)
        |=  [[acct=account:hd-path acct-pub=@ux] result=(unit account:hd-path)]
        ?^  result  result
        ?:  =(acct-pub account-pubkey)  `acct  ~
      ?~  found  ~
      `(format-account-path u.found)
    ?~  acct-path
      "{chain-num}/{(scow %ud index)}"
    "{u.acct-path}/{chain-num}/{(scow %ud index)}"
  ::  Build back link - use account pubkey for URLs
  =/  account-url=tape
    "/spv-wallet/account/{(hexn:sailbox account-pubkey)}"
  =/  chain-display=tape  ?:(=(chain 'receiving') "Receiving" "Change")
  ::  Check if this address has a running refresh process
  =/  proc-map=(map @ud [pid=@ta act=?])
    ?:  =(chain 'receiving')
      receiving.proc.u.details
    change.proc.u.details
  =/  refresh-state=(unit [pid=@ta act=?])  (~(get by proc-map) index)
  =/  is-refreshing=?  ?=(^ refresh-state)
  =/  row-event=@t  (crip "{(trip chain)}-row-update")
  ::  Indexer subscription status
  =/  script-hash=@ux  (address-to-script-hash address.addr-details)
  =/  sub-state=(unit indexer-sub:s)  (~(get by indexer-subs) script-hash)
  =/  is-subscribed=?  ?=(^ sub-state)
  %-  htmx-page
  :^  "{chain-display} Address #{(scow %ud index)}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ::  SSE connection for live updates - uses account pubkey
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/account/{(hexn:sailbox account-pubkey)}", sse-swap "{(trip row-event)},tx-verify-update,indexer-subscribed,indexer-unsubscribed,indexer-update", style "display:none;");
    ::  CSS animations
    ;style
      ; @keyframes pulseBadge {
      ;   0% {
      ;     filter: brightness(2) saturate(1.8);
      ;     transform: scale(1.1);
      ;   }
      ;   100% {
      ;     filter: brightness(1) saturate(1);
      ;     transform: scale(1);
      ;   }
      ; }
      ; .pulse-badge {
      ;   animation: pulseBadge 0.8s cubic-bezier(0.4, 0, 0.2, 1) forwards;
      ; }
    ==
    ::  JavaScript functions - accountPath is PLAIN and gets encoded by JavaScript
    ;script
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Copied to clipboard');
      ;   }).catch(function(err) {
      ;     console.error('Failed to copy: ', err);
      ;   });
      ; }
      ;
      ; function refreshAddress(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=refresh-address&chain=' + encodeURIComponent(chain) + '&index=' + index
      ;   });
      ; }
      ;
      ; function pauseRefresh(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=pause-refresh&chain=' + encodeURIComponent(chain) + '&index=' + index
      ;   });
      ; }
      ;
      ; function resumeRefresh(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=resume-refresh&chain=' + encodeURIComponent(chain) + '&index=' + index
      ;   });
      ; }
      ;
      ; function cancelRefresh(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-refresh&chain=' + encodeURIComponent(chain) + '&index=' + index
      ;   });
      ; }
      ;
      ; function verifyTransaction(pubkey, txid) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=verify-transaction&txid=' + encodeURIComponent(txid)
      ;   });
      ; }
      ;
      ; function subscribeToIndexer(pubkey, address, chain, index) {
      ;   console.log('Subscribe to indexer for address:', address, 'chain:', chain, 'index:', index);
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=subscribe-indexer&address=' + encodeURIComponent(address) + '&chain=' + encodeURIComponent(chain) + '&index=' + encodeURIComponent(index)
      ;   }).then(function(response) {
      ;     console.log('Indexer subscription response:', response.status);
      ;   });
      ; }
      ;
      ; function unsubscribeFromIndexer(pubkey, address) {
      ;   console.log('Unsubscribe from indexer for address:', address);
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=unsubscribe-indexer&address=' + encodeURIComponent(address)
      ;   }).then(function(response) {
      ;     console.log('Indexer unsubscribe response:', response.status);
      ;   });
      ; }
    ==
    ::  Back link
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href "{account-url}", style "color: var(--f3); text-decoration: none;"): ← Back to Account
    ==
    ::  Header section
    ;div.p4.b1.br2
      ;h1.s2.bold.mb2
        ; {chain-display} Address #{(scow %ud index)}
      ==
      ;div.f2.s-1.mb1: Path: {derivation-path}
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;div.f2.s-1.mono(style "word-break: break-all;"): {(trip address.addr-details)}
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{(trip address.addr-details)}')"
          =title  "Copy address"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
      ==
      ::  Network display
      ;div.p2.br1(style "display: inline-flex; align-items: center; gap: 8px; margin-top: 12px; background: var(--b2);")
        ;+  (network-badge:ui-utils active-network.u.details)
        ;span.f2.s-1: {(trip (network-to-cord:ui-utils active-network.u.details))}
      ==
    ==
    ::  Refresh Address button or progress indicator
    ;div(id "address-refresh-status-{(trip chain)}-{(scow %ud index)}")
      ;+  (address-refresh-button account-pubkey chain index refresh-state)
    ==
    ::  Balance stats
    ;div.p4.b2.br2(id "address-balance-{(trip chain)}-{(scow %ud index)}")
      ;h2.s1.bold.mb2: Balance
      ;div(style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 16px;")
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Balance
          ;div.s0.bold.mono: {(scow %ud balance-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Funded
          ;div.f2.mono: {(scow %ud funded-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Spent
          ;div.f2.mono: {(scow %ud spent-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Transactions
          ;div.f2.mono: {(scow %ud tx-count)}
        ==
      ==
    ==
    ::  UTXOs
    ;div.p4.b2.br2(id "address-utxos-{(trip chain)}-{(scow %ud index)}")
      ;h2.s1.bold.mb2: UTXOs ({(scow %ud (lent utxos.addr-details))})
      ;+  ?~  utxos.addr-details
          ;div.f2.s-1(style "opacity: 0.6;"): No UTXOs
        ;div.fc.g2(style "max-height: 300px; overflow-y: auto;")
          ;*  %+  turn  utxos.addr-details
              |=  [txid=@t vout=@ud value=@ud =tx-status]
              ^-  manx
              =/  txid-tape=tape  (trip txid)
              =/  outpoint=tape  "{txid-tape}:{(scow %ud vout)}"
              =/  confirmed=?
                ?-  -.tx-status
                  %confirmed    %.y
                  %unconfirmed  %.n
                ==
              =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
              =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
              ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                ;span.mono.f3.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2);"): {outpoint}
                ;span.mono.f2.bold(style "flex-shrink: 0; color: var(--f1);"): {(scow %ud value)} sats
                ;button.p1.b0.br1.hover.pointer
                  =onclick  "copyToClipboard('{outpoint}')"
                  =title  "Copy outpoint"
                  =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                  ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'copy')
                  ==
                ==
              ==
        ==
    ==
    ::  Indexer subscription
    ;div#indexer-section.p4.b2.br2
      ;h2.s1.bold.mb2: Indexer
      ;div(style "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;")
        ;div.f3.s-1(style "opacity: 0.6;"): Subscribe to local indexer for real-time updates
        ;div#indexer-sub-status
          ;+  (indexer-status-ui account-pubkey address.addr-details chain index is-subscribed)
        ==
      ==
      ::  History from indexer
      ;+  ?~  indexer-history.addr-details
          ;div#indexer-history.f3.s-1(style "opacity: 0.5;"): No indexer history yet
        ;div#indexer-history.fc.g2(style "max-height: 200px; overflow-y: auto;")
          ;*  %+  turn  (flop indexer-history.addr-details)
              |=  [block-hash=@ux evt=sh-tx:indexer]
              ^-  manx
              ?-  -.evt
                  %&
                ::  Fund event - outpoint is [txid vout]
                =/  raw-hex=tape  (hexn:sailbox txid.p.evt)
                =/  txid-hex=tape  (weld (reap (sub 64 (lent raw-hex)) '0') raw-hex)
                ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                  ;span.p1.br1(style "background: rgba(100, 200, 100, 0.3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Fund")
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'arrow-down')
                    ==
                  ==
                  ;span.mono.f3.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2);"): {txid-hex}
                  ;span.mono.f3.s-1(style "color: var(--f3); flex-shrink: 0;"): :{(scow %ud vout.p.evt)}
                  ;button.p1.b0.br1.hover.pointer
                    =onclick  "copyToClipboard('{txid-hex}')"
                    =title  "Copy transaction ID"
                    =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'copy')
                    ==
                  ==
                ==
                  %|
                ::  Spend event - just txid
                =/  raw-hex=tape  (hexn:sailbox p.evt)
                =/  txid-hex=tape  (weld (reap (sub 64 (lent raw-hex)) '0') raw-hex)
                ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                  ;span.p1.br1(style "background: rgba(255, 100, 100, 0.3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Spend")
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'arrow-up')
                    ==
                  ==
                  ;span.mono.f3.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2);"): {txid-hex}
                  ;button.p1.b0.br1.hover.pointer
                    =onclick  "copyToClipboard('{txid-hex}')"
                    =title  "Copy transaction ID"
                    =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'copy')
                    ==
                  ==
                ==
              ==
        ==
    ==
    ::  Transaction history
    ;div(id "address-transactions-{(trip chain)}-{(scow %ud index)}")
      ;+  ?~  relevant-transactions
          ;div.p4.b2.br2
            ;h2.s1.bold.mb2: Transaction History
            ;div.f2.s-1(style "opacity: 0.6;"): No transactions yet
          ==
        ;div.p4.b2.br2
          ;h2.s1.bold.mb2: Transaction History
          ;div.fc.g2(style "max-height: 400px; overflow-y: auto;")
            ;*  %+  turn  relevant-transactions
                |=  [txid=@t tx=transaction]
                ^-  manx
                =/  txid-tape=tape  (trip txid)
                =/  confirmed=?
                  ?-  -.tx-status.tx
                    %confirmed    %.y
                    %unconfirmed  %.n
                  ==
                =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
                =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
                ::  Look up verification status
                =/  verification-lookup=(unit (unit (unit tang)))  (~(get by tx-verification:ac) txid)
                =/  verification-status=(unit (unit tang))
                  ?~  verification-lookup  ~
                  u.verification-lookup
                ::  Check if address is in inputs (spent from this address)
                =/  in-vin=?
                  %+  lien  inputs.tx
                  |=  input=tx-input
                  ?~  prevout.input  %.n
                  =(address.u.prevout.input address.addr-details)
                ::  Check if address is in outputs (received at this address)
                =/  in-vout=?
                  %+  lien  outputs.tx
                  |=  output=tx-output
                  =(address.output address.addr-details)
                ::  Check if this tx has a UTXO (from cached data)
                =/  has-utxo=?
                  ?:  |(!in-vout in-vin)  %.n
                  %+  lien  utxos.addr-details
                  |=  [utxo-txid=@t vout=@ud value=@ud =tx-status]
                  =(utxo-txid txid)
                =/  direction-color=tape
                  ?:  &(in-vin in-vout)  "rgba(150, 150, 200, 0.3)"
                  ?:  in-vin  "rgba(255, 100, 100, 0.3)"
                  ?:  has-utxo  "rgba(255, 200, 50, 0.3)"
                  "rgba(100, 200, 100, 0.3)"
                =/  tx-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/tx/{txid-tape}"
                ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                  ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                  ::  Verification badge with button
                  ;+  =/  tx-verify-proc=(unit [pid=@ta act=?])  (~(get by tx-verify-proc-map) txid)
                      (tx-verify-badge txid account-pubkey tx-verify-proc verification-status)
                  ;+  ?:  &(in-vin in-vout)
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; gap: 2px; width: 36px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent to self")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-down')
                          ==
                        ==
                      ?:  in-vin
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                        ==
                      ?:  has-utxo
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "UTXO")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'star')
                          ==
                        ==
                      ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Received")
                        ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                          ;+  (make:fi 'arrow-down')
                        ==
                      ==
                  ;a.mono.f3.s-1.hover.pointer(href tx-url, style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2); text-decoration: none;"): {txid-tape}
                  ;button.p1.b0.br1.hover.pointer
                    =onclick  "copyToClipboard('{txid-tape}')"
                    =title  "Copy transaction ID"
                    =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'copy')
                    ==
                  ==
                ==
          ==
        ==
    ==
  ==
::
::  Tapscript refresh button (matches address-refresh-button style)
::
++  tapscript-refresh-button
  |=  [pubkey=@ux chain=@t index=@ud tapscript-addr=@t refresh-state=(unit [pid=@ta act=?])]
  ^-  manx
  ?~  refresh-state
    ::  No refresh running
    ;div.p3.b2.br2.hover.pointer(onclick "refreshTapscript('{(hexn:sailbox pubkey)}', '{(trip chain)}', {(scow %ud index)}, '{(trip tapscript-addr)}')", style "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2);")
      ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'refresh-cw')
      ==
      ;span.f2.bold.f-3: Refresh Address
    ==
  ::  Refresh running - show progress and controls
  =/  [refresh-pid=@ta refresh-act=?]  u.refresh-state
  =/  border-color=tape  ?:(refresh-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
  =/  bg-color=tape  ?:(refresh-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
  =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
  ;div.p3.b2.br2(style container-style)
    ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
      ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
        ;+  ?:  refresh-act
              ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                ;+  (make:fi 'loader')
              ==
            ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'pause-circle')
            ==
      ==
      ;div(style "display: flex; flex-direction: column; gap: 4px;")
        ;div.f2.bold: {?:(refresh-act "Refreshing Address..." "Refresh Paused")}
        ;div.f3.s-1: Fetching latest data from blockchain
      ==
    ==
    ;div(style "display: flex; gap: 4px;")
      ;+  ?:  refresh-act
            ::  Active - show pause button
            ;button.p2.b1.br1.hover.pointer(title "Pause refresh", onclick "pauseTapscriptRefresh('{(hexn:sailbox pubkey)}', '{(trip tapscript-addr)}')", style "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'pause')
              ==
            ==
          ::  Paused - show play button
          ;button.p2.b1.br1.hover.pointer(title "Resume refresh", onclick "resumeTapscriptRefresh('{(hexn:sailbox pubkey)}', '{(trip tapscript-addr)}')", style "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'play')
            ==
          ==
      ;button.p2.b1.br1.hover.pointer(title "Cancel refresh", onclick "cancelTapscriptRefresh('{(hexn:sailbox pubkey)}', '{(trip tapscript-addr)}')", style "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;")
        ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'x-circle')
        ==
      ==
    ==
  ==
::
::  Tapscript name display with edit button
::
++  tapscript-name-display
  |=  name=@t
  ^-  manx
  ;div#name-display(style "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;")
    ;h1.s2.bold: Tapscript: {(trip name)}
    ;button.p1.b0.br1.hover.pointer
      =onclick  "toggleEditName()"
      =title  "Edit name"
      =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 28px; height: 28px; justify-content: center; outline: none;"
      ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;")
        ;+  (make:fi 'edit-2')
      ==
    ==
  ==
::
::  Tapscript name edit form
::
++  tapscript-name-edit
  |=  [name=@t pubkey-hex=tape chain-str=tape index-str=tape addr-str=tape]
  ^-  manx
  =/  rename-call=tape
    "renameTapscript('{pubkey-hex}', '{chain-str}', {index-str}, '{addr-str}')"
  =/  keydown-handler=tape
    "if(event.key==='Enter') \{{rename-call}}; if(event.key==='Escape') toggleEditName();"
  ;div#name-edit(style "display: none; align-items: center; gap: 8px; margin-bottom: 8px;")
    ;input
      =id  "name-input"
      =class  "p2 b1 br1"
      =type  "text"
      =value  "{(trip name)}"
      =placeholder  "Enter name"
      =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f0); outline: none; font-size: 1.2rem; font-weight: bold; width: 200px;"
      =onkeydown  "{keydown-handler}"
    ;
    ==
    ;button
      =class  "p2 b1 br1 hover pointer"
      =onclick  "{rename-call}"
      =style  "background: rgba(100, 200, 100, 0.2); border: 1px solid rgba(100, 200, 100, 0.4); color: var(--f2); outline: none;"
      ; Save
    ==
    ;button
      =class  "p2 b1 br1 hover pointer"
      =onclick  "toggleEditName()"
      =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); outline: none;"
      ; Cancel
    ==
  ==
::
::  Tapscript address detail page
::
++  tapscript-detail-page
  |=  $:  account-pubkey=@ux
          account-pubkey-hex=@t
          chain=@t
          index=@ud
          tapscript-addr=@t
          wallets=(map @ux wallet)
          accounts=(map @ux account-details)
          now=@da
      ==
  ^-  manx
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    ;div.p5: Account not found
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ::  Get the leaf containing this tapscript
  =/  leaf-mop=((mop @ud hd-leaf) gth)
    ?:  =(chain 'receiving')  receiving:ac  change:ac
  =/  maybe-leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
  ?~  maybe-leaf
    ;div.p5: Address index not found
  =/  leaf=hd-leaf  u.maybe-leaf
  ::  Look up the tapscript details by address
  =/  maybe-ts=(unit tapscript-details)  (~(get by script-trees.leaf) tapscript-addr)
  ?~  maybe-ts
    ;div.p5: Tapscript address not found
  =/  ts-details=tapscript-details  u.maybe-ts
  =/  addr-details=address-details  address-details.ts-details
  ::  Extract balance data
  =/  [funded-sats=@ud spent-sats=@ud balance-sats=@ud tx-count=@ud]
    (extract-balance-stats addr-details)
  ::  Filter transactions to only include this tapscript address
  =/  all-transactions=(map @t transaction)  transactions:ac
  =/  relevant-transactions=(list [txid=@t tx=transaction])
    =/  filtered=(list [txid=@t tx=transaction])
      %+  murn  ~(tap by all-transactions)
      |=  [txid=@t tx=transaction]
      ^-  (unit [txid=@t tx=transaction])
      =/  has-output=?
        %+  lien  outputs.tx
        |=  out=tx-output
        =(address.out tapscript-addr)
      =/  has-input=?
        %+  lien  inputs.tx
        |=  input=tx-input
        ?~  prevout.input  %.n
        =(address.u.prevout.input tapscript-addr)
      ?:(|(has-output has-input) `[txid tx] ~)
    ::  Sort by block height: unconfirmed first, then descending
    %+  sort  filtered
    |=  [[txid-a=@t tx-a=transaction] [txid-b=@t tx-b=transaction]]
    ^-  ?
    ?-  tx-status.tx-a
      [%unconfirmed ~]
        ?-  tx-status.tx-b
          [%unconfirmed ~]  %.y
          [%confirmed *]    %.y
        ==
      [%confirmed *]
        ?-  tx-status.tx-b
          [%unconfirmed ~]  %.n
          [%confirmed *]    (gth +>:tx-status.tx-a +>:tx-status.tx-b)
        ==
    ==
  ::  Build URLs
  =/  parent-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/address/{(trip chain)}/{(scow %ud index)}"
  =/  chain-display=tape  ?:(=(chain 'receiving') "Receiving" "Change")
  ::  Check if this tapscript has a running refresh process
  =/  tapscript-proc-map=(map @t [pid=@ta act=?])  tapscript.proc.u.details
  =/  refresh-state=(unit [pid=@ta act=?])  (~(get by tapscript-proc-map) tapscript-addr)
  ::  Row event for SSE
  =/  row-event=@t  (crip "{(trip chain)}-row-update")
  %-  htmx-page
  :^  "Tapscript: {(trip name.ts-details)}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ::  SSE connection
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/account/{(hexn:sailbox account-pubkey)}", sse-swap "{(trip row-event)}", style "display:none;");
    ::  CSS animations
    ;style
      ; @keyframes spin {
      ;   from { transform: rotate(0deg); }
      ;   to { transform: rotate(360deg); }
      ; }
    ==
    ::  JavaScript functions
    ;script
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Copied to clipboard');
      ;   }).catch(function(err) {
      ;     console.error('Failed to copy: ', err);
      ;   });
      ; }
      ;
      ; function refreshTapscript(pubkey, chain, index, tapscriptAddr) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=refresh-tapscript&chain=' + encodeURIComponent(chain) + '&index=' + index + '&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
      ;   });
      ; }
      ;
      ; function pauseTapscriptRefresh(pubkey, tapscriptAddr) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=pause-tapscript-refresh&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
      ;   });
      ; }
      ;
      ; function resumeTapscriptRefresh(pubkey, tapscriptAddr) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=resume-tapscript-refresh&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
      ;   });
      ; }
      ;
      ; function cancelTapscriptRefresh(pubkey, tapscriptAddr) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-tapscript-refresh&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
      ;   });
      ; }
      ;
      ; function deleteTapscript(pubkey, chain, index, tapscriptAddr) {
      ;   if (confirm('Delete this tapscript address? You can re-add it later.')) {
      ;     fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;       method: 'POST',
      ;       headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;       body: 'action=delete-tapscript&chain=' + encodeURIComponent(chain) + '&index=' + index + '&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
      ;     }).then(function() {
      ;       window.location.href = '{parent-url}';
      ;     });
      ;   }
      ; }
      ;
      ; function toggleEditName() {
      ;   var display = document.getElementById('name-display');
      ;   var edit = document.getElementById('name-edit');
      ;   if (display.style.display === 'none') {
      ;     display.style.display = 'flex';
      ;     edit.style.display = 'none';
      ;   } else {
      ;     display.style.display = 'none';
      ;     edit.style.display = 'flex';
      ;     document.getElementById('name-input').focus();
      ;   }
      ; }
      ;
      ; function renameTapscript(pubkey, chain, index, tapscriptAddr) {
      ;   var newName = document.getElementById('name-input').value.trim();
      ;   if (!newName) return;
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=rename-tapscript&chain=' + encodeURIComponent(chain) + '&index=' + index + '&tapscript-addr=' + encodeURIComponent(tapscriptAddr) + '&name=' + encodeURIComponent(newName)
      ;   }).then(function() {
      ;     window.location.reload();
      ;   });
      ; }
    ==
    ::  Back link
    ;div(style "margin-bottom: 16px;")
      ;a.hover.pointer(href "/spv-wallet/account/{(hexn:sailbox account-pubkey)}", style "color: var(--f3); text-decoration: none;"): ← Back to Account
    ==
    ::  Header section
    ;div.p4.b1.br2
      ;+  (tapscript-name-display name.ts-details)
      ;+  %:  tapscript-name-edit
            name.ts-details
            (hexn:sailbox account-pubkey)
            (trip chain)
            (scow %ud index)
            (trip tapscript-addr)
          ==
      ;div.f2.s-1.mb1: Parent: {chain-display} #{(scow %ud index)}
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;div.f2.s-1.mono(style "word-break: break-all;"): {(trip tapscript-addr)}
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{(trip tapscript-addr)}')"
          =title  "Copy tapscript address"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
      ==
      ::  Network display
      ;div.p2.br1(style "display: inline-flex; align-items: center; gap: 8px; margin-top: 12px; background: var(--b2);")
        ;+  (network-badge:ui-utils active-network.u.details)
        ;span.f2.s-1: {(trip (network-to-cord:ui-utils active-network.u.details))}
      ==
    ==
    ::  Refresh button
    ;+  (tapscript-refresh-button account-pubkey chain index tapscript-addr refresh-state)
    ::  Balance stats
    ;div.p4.b2.br2
      ;h2.s1.bold.mb2: Balance
      ;div(style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 16px;")
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Balance
          ;div.s0.bold.mono: {(scow %ud balance-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Funded
          ;div.f2.mono: {(scow %ud funded-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Spent
          ;div.f2.mono: {(scow %ud spent-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Transactions
          ;div.f2.mono: {(scow %ud tx-count)}
        ==
      ==
    ==
    ::  UTXOs
    ;div.p4.b2.br2
      ;h2.s1.bold.mb2: UTXOs ({(scow %ud (lent utxos.addr-details))})
      ;+  ?~  utxos.addr-details
          ;div.f2.s-1(style "opacity: 0.6;"): No UTXOs
        ;div.fc.g2(style "max-height: 300px; overflow-y: auto;")
          ;*  %+  turn  utxos.addr-details
              |=  [txid=@t vout=@ud value=@ud =tx-status]
              ^-  manx
              =/  txid-tape=tape  (trip txid)
              =/  outpoint=tape  "{txid-tape}:{(scow %ud vout)}"
              =/  confirmed=?
                ?-  -.tx-status
                  %confirmed    %.y
                  %unconfirmed  %.n
                ==
              =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
              =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
              ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                ;span.mono.f3.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2);"): {outpoint}
                ;span.mono.f2.bold(style "flex-shrink: 0; color: var(--f1);"): {(scow %ud value)} sats
                ;button.p1.b0.br1.hover.pointer
                  =onclick  "copyToClipboard('{outpoint}')"
                  =title  "Copy outpoint"
                  =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                  ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'copy')
                  ==
                ==
              ==
        ==
    ==
    ::  Script tree visualization
    ;div.p4.b2.br2
      ;h2.s1.bold.mb2: Script Tree
      ;+  (render-script-tree-browser ptst.ts-details)
    ==
    ::  Transaction history
    ;+  ?~  relevant-transactions
          ;div.p4.b2.br2
            ;h2.s1.bold.mb2: Transaction History
            ;div.f2.s-1(style "opacity: 0.6;"): No transactions yet
          ==
        ;div.p4.b2.br2
          ;h2.s1.bold.mb2: Transaction History
          ;div.fc.g2(style "max-height: 400px; overflow-y: auto;")
            ;*  %+  turn  relevant-transactions
                |=  [txid=@t tx=transaction]
                ^-  manx
                =/  txid-tape=tape  (trip txid)
                =/  confirmed=?
                  ?-  -.tx-status.tx
                    %confirmed    %.y
                    %unconfirmed  %.n
                  ==
                =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
                =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
                ::  Check direction
                =/  in-vin=?
                  %+  lien  inputs.tx
                  |=  input=tx-input
                  ?~  prevout.input  %.n
                  =(address.u.prevout.input tapscript-addr)
                =/  in-vout=?
                  %+  lien  outputs.tx
                  |=  output=tx-output
                  =(address.output tapscript-addr)
                ::  Check if this tx has a UTXO (from cached data)
                =/  has-utxo=?
                  ?:  |(!in-vout in-vin)  %.n
                  %+  lien  utxos.addr-details
                  |=  [utxo-txid=@t vout=@ud value=@ud =tx-status]
                  =(utxo-txid txid)
                =/  direction-color=tape
                  ?:  &(in-vin in-vout)  "rgba(150, 150, 200, 0.3)"
                  ?:  in-vin  "rgba(255, 100, 100, 0.3)"
                  ?:  has-utxo  "rgba(255, 200, 50, 0.3)"
                  "rgba(100, 200, 100, 0.3)"
                =/  tx-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/tx/{txid-tape}"
                ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                  ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                  ;+  ?:  &(in-vin in-vout)
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; gap: 2px; width: 36px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent to self")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-down')
                          ==
                        ==
                      ?:  in-vin
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                        ==
                      ?:  has-utxo
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "UTXO")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'star')
                          ==
                        ==
                      ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Received")
                        ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                          ;+  (make:fi 'arrow-down')
                        ==
                      ==
                  ;a.mono.f3.s-1.hover.pointer(href tx-url, style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2); text-decoration: none;"): {txid-tape}
                  ;button.p1.b0.br1.hover.pointer
                    =onclick  "copyToClipboard('{txid-tape}')"
                    =title  "Copy transaction ID"
                    =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'copy')
                    ==
                  ==
                ==
          ==
        ==
    ::  Delete button at bottom
    ;div(style "display: flex; justify-content: flex-end;")
      ;button.p2.b1.br1.hover.pointer
        =onclick  "deleteTapscript('{(hexn:sailbox account-pubkey)}', '{(trip chain)}', {(scow %ud index)}, '{(trip tapscript-addr)}')"
        =title  "Delete this tapscript address"
        =style  "background: transparent; border: 1px solid rgba(255, 80, 80, 0.3); color: rgba(255, 80, 80, 0.8); display: flex; align-items: center; gap: 8px; outline: none;"
        ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'trash-2')
        ==
        ;span.f3.s-1: Delete Tapscript
      ==
    ==
  ==
::
::  Render a single op as human-readable text
::
++  render-op
  |=  =op:btc-script
  ^-  tape
  ?:  ?=(^ op)
    ::  Push data
    ?:  ?=(%num p.op)
      ::  Small number push (OP_0 through OP_16, OP_1NEGATE)
      =/  num=@  q.octs.op
      ?:  =(num 0x81)  "OP_1NEGATE"
      ?:  =(num 0)  "OP_0"
      "OP_{<num>}"
    ::  Data push - just show hex, truncate if long
    =/  data-hex=tape  (hexn:sailbox q.octs.op)
    ?:  (lte (lent data-hex) 16)
      "<{data-hex}>"
    "<{(scag 16 data-hex)}...>"
  ::  Simple opcode - convert to uppercase name
  =/  name=tape  (trip op)
  ::  Remove "op-" prefix and uppercase
  ?:  (gte (lent name) 3)
    =/  rest=tape  (slag 3 name)
    (cuss rest)
  (cuss name)
::
::  Disassemble script bytes and render as text
::
++  disassemble-script
  |=  script-bytes=hexb:bitcoin
  ^-  (unit tape)
  =/  parsed=(unit script:btc-script)
    (de:btc-script [wid.script-bytes dat.script-bytes])
  ?~  parsed  ~
  =/  op-list=(list tape)
    (turn u.parsed render-op)
  ?~  op-list  `""
  ::  Join with spaces using roll
  =/  result=tape
    %+  roll  t.op-list
    |=  [item=tape acc=tape]
    (weld acc (weld " " item))
  `(weld i.op-list result)
::
::  Eye icon button for viewing witness data
::
++  witness-eye-btn
  |=  [modal-id=tape has-witness=?]
  ^-  manx
  ?:  !has-witness  ;div;
  ;button.p1.b0.br1.hover.pointer
    =onclick  "document.getElementById('{modal-id}').style.display='flex'"
    =title  "View witness data"
    =style  "background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
      ;+  (make:fi 'eye')
    ==
  ==
::
::  Modal for displaying witness data
::
++  witness-modal
  |=  [modal-id=tape vin-index=@ud witness=(list @t)]
  ^-  manx
  ?:  =(~ witness)  ;div;
  ;div(id modal-id, onclick "if(event.target===this)this.style.display='none'", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
    ;div(style "background: var(--b0); padding: 24px; border-radius: 12px; box-shadow: 0 8px 24px rgba(0,0,0,0.3); max-width: 600px; width: 90%; max-height: 80vh; overflow-y: auto;")
      ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
        ;h3(style "margin: 0; color: var(--f0);"): Witness Data (Input #{(scow %ud vin-index)})
        ;button.p1.b0.br1.hover.pointer
          =onclick  "document.getElementById('{modal-id}').style.display='none'"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 28px; height: 28px; justify-content: center; outline: none;"
          ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'x')
          ==
        ==
      ==
      ;+  (render-witness-content witness)
    ==
  ==
::
::  Render witness data content for a transaction input
::  Detects P2TR script-path spends and shows tapscript disassembly
::
++  render-witness-content
  |=  witness=(list @t)
  ^-  manx
  =/  n=@ud  (lent witness)
  ?:  =(0 n)
    ;div.f3.s-1(style "opacity: 0.5;"): No witness data
  ::  Detect P2TR script-path spend:
  ::  2+ items, last item starts with c0 or c1 (control block)
  =/  last-item=@t  (rear witness)
  =/  last-tape=tape  (trip last-item)
  =/  first-two=tape  (scag 2 last-tape)
  =/  is-script-path=?
    &((gte n 2) |(=("c0" first-two) =("c1" first-two)))
  ?:  is-script-path
    =/  control-block=@t  last-item
    =/  tapscript-hex=@t  (snag (sub n 2) witness)
    =/  script-args=(list @t)  (scag (sub n 2) witness)
    =/  script-hexb=(unit hexb:bitcoin)
      (de:base16:mimes:html tapscript-hex)
    =/  disasm=(unit tape)
      ?~  script-hexb  ~
      (disassemble-script u.script-hexb)
    ::  Attempt URB decode
    =/  urb-decoded=(unit (list raw-sotx:urb))
      ?~  script-hexb  ~
      (decode:urb-decoder u.script-hexb)
    =/  is-urb=?  !=(~ urb-decoded)
    =/  byte-count=@ud  (div (lent (trip tapscript-hex)) 2)
    ;div.fc.g3
      ;div
        ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Type
        ;div(style "display: flex; align-items: center; gap: 8px;")
          ;span.f2: Taproot Script-Path Spend
          ;+  ?:  is-urb
                ;span.p1.br1(style "background: rgba(100, 200, 150, 0.2); border: 1px solid rgba(100, 200, 150, 0.4); color: var(--f2); font-size: 10px;"): URB
              ;div;
        ==
      ==
      ::  Show URB decoded content if present
      ;+  ?~  urb-decoded
            ;div;
          ;div.p3.br2(style "background: rgba(100, 200, 150, 0.1); border: 1px solid rgba(100, 200, 150, 0.3);")
            ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 8px;"): raw-sotx:urb ({(scow %ud (lent u.urb-decoded))})
            ;div.fc.g2
              ;*  %+  turn  u.urb-decoded
                  |=  sot=raw-sotx:urb
                  ;div.p2.br1.fc.g2(style "background: var(--b1);")
                    ;div
                      ;span.f3.s-2(style "opacity: 0.5;"): raw.p=
                      ;span.mono.f3.s-2: {(scow %ud p.raw.sot)}
                    ==
                    ;div
                      ;span.f3.s-2(style "opacity: 0.5;"): ship=
                      ;span.mono.f3.s-1: {(scow %p ship.sot.sot)}
                    ==
                    ;div
                      ;span.f3.s-2(style "opacity: 0.5;"): sig=
                      ;span.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap; display: block;"): {?~(sig.sot.sot "~" ((x-co:co 128) u.sig.sot.sot))}
                    ==
                    ;+  (render-skim-raw:urb-decoder +.sot.sot)
                  ==
            ==
          ==
      ;div
        ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Tapscript Disassembly
        ;+  ?~  disasm
              ;div.f3.s-1(style "opacity: 0.5; font-style: italic;"): (unable to parse)
            ;div.mono.f3.s-1(style "word-break: break-all; line-height: 1.6;"): {u.disasm}
      ==
      ;div
        ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Raw Tapscript ({(scow %ud byte-count)} bytes)
        ;div.mono.f3.s-2(style "word-break: break-all; max-height: 80px; overflow-y: auto; opacity: 0.6;"): {(trip tapscript-hex)}
      ==
      ;div
        ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Control Block
        ;div.mono.f3.s-2(style "word-break: break-all; max-height: 60px; overflow-y: auto; opacity: 0.6;"): {(trip control-block)}
      ==
      ;+  ?~  script-args
            ;div;
          ;div
            ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Script Arguments ({(scow %ud (lent script-args))} items)
            ;div.fc.g1
              ;*  %+  turn  script-args
                  |=  arg=@t
                  ;div.mono.f3.s-2(style "word-break: break-all; opacity: 0.8;"): {(trip arg)}
            ==
          ==
    ==
  ::  Non-script-path witness (key-path or other types)
  ;div.fc.g3
    ;div
      ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Type
      ;div.f2: {?:((lte n 1) "Key-Path Spend" "Witness Spend")}
    ==
    ;div
      ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Witness Stack ({(scow %ud n)} items)
      ;div.fc.g1
        ;*  %+  turn  witness
            |=  item=@t
            ;div.mono.f3.s-2(style "word-break: break-all; opacity: 0.8;"): {(trip item)}
      ==
    ==
  ==
::
::  Tree item: either a leaf or an opaque hash
::
+$  tree-item
  $%  [%leaf hash=@ux =tapleaf:taproot]
      [%opaque hash=@ux]
  ==
::
::  Collect all leaves and opaque nodes from a ptst
::
++  collect-tree-items
  |=  tree=ptst:taproot
  ^-  (list tree-item)
  ?~  tree  ~
  ?-  -.tree
    %leaf    ~[[%leaf (leaf-hash:taproot tapleaf.tree) tapleaf.tree]]
    %opaque  ~[[%opaque hash.tree]]
    %branch  (weld $(tree l.tree) $(tree r.tree))
  ==
::
::  Render script tree browser with dropdown and display area
::
++  render-script-tree-browser
  |=  tree=ptst:taproot
  ^-  manx
  =/  items=(list tree-item)  (collect-tree-items tree)
  ?~  items
    ;div.f3.s-1(style "opacity: 0.5;"): (empty tree)
  ::  Generate unique IDs for each item based on hash
  ;div.fc.g3
    ::  Dropdown selector
    ;div(style "display: flex; align-items: center; gap: 12px;")
      ;label.f3.s-1(for "tree-item-select"): Select item:
      ;select#tree-item-select.p2.b1.br1(onchange "showTreeItem(this.value)", style "background: var(--b2); color: var(--f2); border: 1px solid var(--b3); min-width: 200px;")
        ;*  %+  turn  items
            |=  item=tree-item
            ^-  manx
            =/  hash=@ux  ?-  -.item
                            %leaf    hash.item
                            %opaque  hash.item
                          ==
            =/  hash-hex=tape  (hexn:sailbox hash)
            =/  hash-short=tape
              ?:  (gth (lent hash-hex) 16)
                (weld (scag 16 hash-hex) "...")
              hash-hex
            =/  item-type=tape  ?-(-.item %leaf "Leaf", %opaque "Opaque")
            ;option(value hash-hex): {item-type}: {hash-short}
      ==
    ==
    ::  Display area for selected item
    ;div#tree-item-display.p3.b1.br2(style "background: var(--b1); min-height: 80px;")
      ::  Content divs for each item (first one visible by default)
      ;*  =/  idx=@ud  0
          %+  turn  items
          |=  item=tree-item
          ^-  manx
          =/  hash=@ux  ?-(-.item %leaf hash.item, %opaque hash.item)
          =/  hash-hex=tape  (hexn:sailbox hash)
          =/  display-style=tape
            ?:(=(idx 0) "display: block;" "display: none;")
          =.  idx  +(idx)
          ?-  -.item
              %leaf
            =/  version-hex=tape  (hexn:sailbox version.tapleaf.item)
            =/  script-hex=tape  (hexn:sailbox dat.script.tapleaf.item)
            =/  disasm=(unit tape)  (disassemble-script script.tapleaf.item)
            ::  Attempt URB decode
            =/  urb-decoded=(unit (list raw-sotx:urb))
              (decode-tapleaf:urb-decoder tapleaf.item)
            =/  is-urb=?  !=(~ urb-decoded)
            ;div.tree-item-content(id "item-{hash-hex}", style display-style)
              ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 12px;")
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
                  ;+  (make:fi 'code')
                ==
                ;span.f2.bold: Script Leaf
                ;span.f3.s-1(style "opacity: 0.6;"): (version 0x{version-hex})
                ;+  ?:  is-urb
                      ;span.p1.br1(style "background: rgba(100, 200, 150, 0.2); border: 1px solid rgba(100, 200, 150, 0.4); color: var(--f2); font-size: 10px; margin-left: 8px;"): URB
                    ;div;
              ==
              ;div.fc.g3
                ;div
                  ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Leaf Hash
                  ;div.mono.f3.s-1(style "word-break: break-all;"): {hash-hex}
                ==
                ::  Show URB decoded content if present
                ;+  ?~  urb-decoded
                      ;div;
                    ;div.p3.br2(style "background: rgba(100, 200, 150, 0.1); border: 1px solid rgba(100, 200, 150, 0.3);")
                      ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 8px;"): raw-sotx:urb ({(scow %ud (lent u.urb-decoded))})
                      ;div.fc.g2
                        ;*  %+  turn  u.urb-decoded
                            |=  sot=raw-sotx:urb
                            ;div.p2.br1.fc.g2(style "background: var(--b1);")
                              ;div
                                ;span.f3.s-2(style "opacity: 0.5;"): raw.p=
                                ;span.mono.f3.s-2: {(scow %ud p.raw.sot)}
                              ==
                              ;div
                                ;span.f3.s-2(style "opacity: 0.5;"): ship=
                                ;span.mono.f3.s-1: {(scow %p ship.sot.sot)}
                              ==
                              ;div
                                ;span.f3.s-2(style "opacity: 0.5;"): sig=
                                ;span.mono.f3.s-2(style "overflow-x: auto; white-space: nowrap; display: block;"): {?~(sig.sot.sot "~" ((x-co:co 128) u.sig.sot.sot))}
                              ==
                              ;+  (render-skim-raw:urb-decoder +.sot.sot)
                            ==
                      ==
                    ==
                ;div
                  ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Disassembly
                  ;+  ?~  disasm
                        ;div.f3.s-1(style "opacity: 0.5; font-style: italic;"): (unable to parse)
                      ;div.mono.f3.s-1(style "word-break: break-all; line-height: 1.6;"): {u.disasm}
                ==
                ;div
                  ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Raw Script ({<wid.script.tapleaf.item>} bytes)
                  ;div.mono.f3.s-2(style "word-break: break-all; max-height: 80px; overflow-y: auto; opacity: 0.6;"): {script-hex}
                ==
              ==
            ==
              %opaque
            ;div.tree-item-content(id "item-{hash-hex}", style display-style)
              ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 12px;")
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
                  ;+  (make:fi 'lock')
                ==
                ;span.f2.bold: Opaque Node
              ==
              ;div
                ;div.f3.s-2(style "opacity: 0.6; margin-bottom: 4px;"): Hash
                ;div.mono.f3.s-1(style "word-break: break-all;"): {hash-hex}
              ==
            ==
          ==
    ==
    ::  JavaScript for toggling display
    ;script
      ; function showTreeItem(hashHex) {
      ;   document.querySelectorAll('.tree-item-content').forEach(function(el) {
      ;     el.style.display = 'none';
      ;   });
      ;   var selected = document.getElementById('item-' + hashHex);
      ;   if (selected) {
      ;     selected.style.display = 'block';
      ;   }
      ; }
    ==
  ==
::
::  Render a partial tapscript tree
::
++  render-ptst
  |=  [tree=ptst:taproot depth=@ud]
  ^-  manx
  =/  indent-px=@ud  (mul depth 20)
  ?~  tree
    ;div.f3.s-1(style "opacity: 0.5; padding-left: {(scow %ud indent-px)}px;"): (empty tree)
  ?-  -.tree
      %leaf
    =/  version-hex=tape  (hexn:sailbox version.tapleaf.tree)
    =/  script-hex=tape
      =/  raw=tape  (hexn:sailbox dat.script.tapleaf.tree)
      ?:  (gth (lent raw) 64)
        (weld (scag 64 raw) "...")
      raw
    ;div(style "padding-left: {(scow %ud indent-px)}px;")
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center; color: var(--f3); opacity: 0.6;")
          ;+  (make:fi 'code')
        ==
        ;span.f3.s-1: Leaf (v0x{version-hex})
      ==
      ;div.mono.f3.s-2(style "margin-top: 4px; margin-left: 22px; word-break: break-all; opacity: 0.5;"): {script-hex}
    ==
      %opaque
    =/  hash-hex=tape
      =/  raw=tape  (hexn:sailbox hash.tree)
      ?:  (gth (lent raw) 16)
        (weld (scag 16 raw) "...")
      raw
    ;div(style "padding-left: {(scow %ud indent-px)}px; display: flex; align-items: center; gap: 8px;")
      ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center; color: var(--f3); opacity: 0.6;")
        ;+  (make:fi 'lock')
      ==
      ;span.f3.s-1: Opaque
      ;span.mono.f3.s-2(style "opacity: 0.5;"): {hash-hex}
    ==
      %branch
    ;div(style "padding-left: {(scow %ud indent-px)}px;")
      ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;")
        ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center; color: var(--f3); opacity: 0.6;")
          ;+  (make:fi 'git-branch')
        ==
        ;span.f3.s-1: Branch
      ==
      ;div.fc.g2(style "border-left: 1px solid var(--b3); margin-left: 7px;")
        ;+  (render-ptst l.tree +(depth))
        ;+  (render-ptst r.tree +(depth))
      ==
    ==
  ==
::
++  address-detail-sections
  |=  $:  index=@ud
          leaf-map=((mop @ud hd-leaf) gth)
          account-pubkey=@ux
          chain=@t
          proc-map=(map @ud [pid=@ta act=?])
          tx-verification=(map @t (unit (unit tang)))
          tx-verify-proc-map=(map @t [pid=@ta act=?])
          all-transactions=(map @t transaction)
      ==
  ^-  manx
  ::  Get the refresh state for this specific address
  =/  refresh-state=(unit [@ta ?])
    (~(get by proc-map) index)
  ::  Find the specific address
  =/  maybe-address=(unit address-details)
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-map index)
    ?~(leaf ~ `main.u.leaf)
  ?~  maybe-address
    ;div;
  =/  addr-details=address-details  u.maybe-address
  ::  Extract balance data from canonical info
  =/  [funded-sats=@ud spent-sats=@ud balance-sats=@ud tx-count=@ud]
    (extract-balance-stats addr-details)
  ::  Filter transactions to only include this address
  =/  this-address=@t  address.addr-details
  =/  relevant-transactions=(list [txid=@t tx=transaction])
    =/  filtered=(list [txid=@t tx=transaction])
      %+  murn  ~(tap by all-transactions)
      |=  [txid=@t tx=transaction]
      ^-  (unit [txid=@t tx=transaction])
      ::  Check if address appears in outputs (received)
      =/  has-output=?
        %+  lien  outputs.tx
        |=  out=tx-output
        =(address.out this-address)
      ::  Check if address appears in inputs (spent)
      =/  has-input=?
        %+  lien  inputs.tx
        |=  input=tx-input
        ?~  prevout.input  %.n
        =(address.u.prevout.input this-address)
      ::  Include if address appears in either inputs or outputs
      ?:(|(has-output has-input) `[txid tx] ~)
    ::  Sort by block height: unconfirmed first, then by descending block height
    %+  sort  filtered
    |=  [[txid-a=@t tx-a=transaction] [txid-b=@t tx-b=transaction]]
    ^-  ?
    ?-  tx-status.tx-a
      [%unconfirmed ~]
        ?-  tx-status.tx-b
          [%unconfirmed ~]  %.y  :: maintain order
          [%confirmed *]    %.y  :: unconfirmed comes first
        ==
      [%confirmed *]
        ?-  tx-status.tx-b
          [%unconfirmed ~]       %.n  :: confirmed comes after
          [%confirmed *]         :: compare block heights
            (gth +>:tx-status.tx-a +>:tx-status.tx-b)
        ==
    ==
  ::  Generate OOB updates for all sections
  =/  balance-section=manx
    ;div.p4.b2.br2(id "address-balance-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
      ;h2.s1.bold.mb2: Balance
      ;div(style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 16px;")
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Balance
          ;div.s0.bold.mono: {(scow %ud balance-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Funded
          ;div.f2.mono: {(scow %ud funded-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Spent
          ;div.f2.mono: {(scow %ud spent-sats)} sats
        ==
        ;div
          ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Transactions
          ;div.f2.mono: {(scow %ud tx-count)}
        ==
      ==
    ==
  =/  utxo-section=manx
    ;div.p4.b2.br2(id "address-utxos-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
      ;h2.s1.bold.mb2: UTXOs ({(scow %ud (lent utxos.addr-details))})
      ;+  ?~  utxos.addr-details
          ;div.f2.s-1(style "opacity: 0.6;"): No UTXOs
        ;div.fc.g2(style "max-height: 300px; overflow-y: auto;")
          ;*  %+  turn  utxos.addr-details
              |=  [txid=@t vout=@ud value=@ud =tx-status]
              ^-  manx
              =/  txid-tape=tape  (trip txid)
              =/  outpoint=tape  "{txid-tape}:{(scow %ud vout)}"
              =/  confirmed=?
                ?-  -.tx-status
                  %confirmed    %.y
                  %unconfirmed  %.n
                ==
              =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
              =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
              ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                ;span.mono.f3.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2);"): {outpoint}
                ;span.mono.f2.bold(style "flex-shrink: 0; color: var(--f1);"): {(scow %ud value)} sats
                ;button.p1.b0.br1.hover.pointer
                  =onclick  "copyToClipboard('{outpoint}')"
                  =title  "Copy outpoint"
                  =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                  ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'copy')
                  ==
                ==
              ==
        ==
    ==
  =/  transactions-section=manx
    ;div(id "address-transactions-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
      ;+  ?~  relevant-transactions
          ;div.p4.b2.br2
            ;h2.s1.bold.mb2: Transaction History
            ;div.f2.s-1(style "opacity: 0.6;"): No transactions yet
          ==
        ;div.p4.b2.br2
          ;h2.s1.bold.mb2: Transaction History
          ;div.fc.g2(style "max-height: 400px; overflow-y: auto;")
            ;*  %+  turn  relevant-transactions
                |=  [txid=@t tx=transaction]
                ^-  manx
                =/  txid-tape=tape  (trip txid)
                =/  confirmed=?
                  ?-  -.tx-status.tx
                    %confirmed    %.y
                    %unconfirmed  %.n
                  ==
                =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
                =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
                ::  Look up verification status
                =/  verification-lookup=(unit (unit (unit tang)))  (~(get by tx-verification) txid)
                =/  verification-status=(unit (unit tang))
                  ?~  verification-lookup  ~
                  u.verification-lookup
                ::  Check if address is in inputs (spent from this address)
                =/  in-vin=?
                  %+  lien  inputs.tx
                  |=  input=tx-input
                  ?~  prevout.input  %.n
                  =(address.u.prevout.input address.addr-details)
                ::  Check if address is in outputs (received at this address)
                =/  in-vout=?
                  %+  lien  outputs.tx
                  |=  output=tx-output
                  =(address.output address.addr-details)
                ::  Check if this tx has a UTXO (from cached data)
                =/  has-utxo=?
                  ?:  |(!in-vout in-vin)  %.n
                  %+  lien  utxos.addr-details
                  |=  [utxo-txid=@t vout=@ud value=@ud =tx-status]
                  =(utxo-txid txid)
                =/  direction-color=tape
                  ?:  &(in-vin in-vout)  "rgba(150, 150, 200, 0.3)"
                  ?:  in-vin  "rgba(255, 100, 100, 0.3)"
                  ?:  has-utxo  "rgba(255, 200, 50, 0.3)"
                  "rgba(100, 200, 100, 0.3)"
                =/  tx-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/tx/{txid-tape}"
                ;div.p3.b1.br2(style "background: var(--b1); display: flex; align-items: center; gap: 8px;")
                  ;span.f3.s-2.p1.br1(style "background: {status-color}; font-size: 11px; flex-shrink: 0;"): {status-text}
                  ::  Verification badge with button
                  ;+  =/  tx-verify-proc=(unit [pid=@ta act=?])  (~(get by tx-verify-proc-map) txid)
                      (tx-verify-badge txid account-pubkey tx-verify-proc verification-status)
                  ::  Direction/UTXO indicator
                  ;+  ?:  &(in-vin in-vout)
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; gap: 2px; width: 36px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent to self")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-down')
                          ==
                        ==
                      ?:  in-vin
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Sent")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'arrow-up')
                          ==
                        ==
                      ?:  has-utxo
                        ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "UTXO")
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'star')
                          ==
                        ==
                      ;span.p1.br1(style "background: {direction-color}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; flex-shrink: 0;", title "Received")
                        ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                          ;+  (make:fi 'arrow-down')
                        ==
                      ==
                  ;a.mono.f3.s-1.hover.pointer(href tx-url, style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; color: var(--f2); text-decoration: none;"): {txid-tape}
                  ;button.p1.b0.br1.hover.pointer
                    =onclick  "copyToClipboard('{txid-tape}')"
                    =title  "Copy transaction ID"
                    =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'copy')
                    ==
                  ==
                ==
          ==
        ==
    ==
  =/  refresh-status-section=manx
    ::  Wrap button in div with id and hx-swap-oob, matching initial page structure
    ;div(id "address-refresh-status-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
      ;+  (address-refresh-button account-pubkey chain index refresh-state)
    ==
  ::  Combine all sections into a single wrapper
  ;div
    ;+  balance-section
    ;+  utxo-section
    ;+  transactions-section
    ;+  refresh-status-section
  ==
::
++  handle-address-detail-sse
  |=  $:  =bowl:gall
          state=vase
          account-pubkey=@ux
          chain=@t
          index=@ud
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0:s state)
  ::  Look up account directly from flat accounts map
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    %-  manx-to-wain:sailbox
    ;div: Account not found
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ::  Determine which address map to use based on chain
  =/  leaf-map=((mop @ud hd-leaf) gth)
    ?:  =('receiving' chain)
      receiving:ac
    change:ac
  ::  Get the refresh state for this specific address
  =/  refresh-state=(unit [@ta ?])
    ?:  =('receiving' chain)
      (~(get by receiving.proc.u.details) index)
    (~(get by change.proc.u.details) index)
  ::  Find the specific address
  =/  maybe-address=(unit address-details)
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-map index)
    ?~(leaf ~ `main.u.leaf)
  ?~  maybe-address
    %-  manx-to-wain:sailbox
    ;div: Address not found
  =/  addr-details=address-details  u.maybe-address
  ::  Handle the row-update event
  =/  row-event=@t  (crip "{(trip chain)}-row-update")
  ::  Check if the event matches our row-event
  ?.  =(`row-event event)
    ::  Not the event we're listening for
    %-  manx-to-wain:sailbox
    ;div;
  ::  Check if the event ID matches our address index
  ?.  =((rash (need id) dem) index)
    ::  Not for us, return empty
    %-  manx-to-wain:sailbox
    ;div;
  ::  Extract balance stats from canonical info
  =/  [chain-funded=@ud chain-spent=@ud mempool-funded=@ud mempool-spent=@ud]
    ?~  info.addr-details
      [0 0 0 0]
    :^    chain-funded.u.info.addr-details
        chain-spent.u.info.addr-details
      mempool-funded.u.info.addr-details
    mempool-spent.u.info.addr-details
  ::  Generate OOB updates for balance section only
  ::  (transactions are handled by address-detail-sections)
  =/  balance-section=manx
      ;div(id "address-balance-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
        ;div.p4.b2.br2
          ;div.s1.f2.mb2(style "opacity: 0.8;"): Balance
          ;div(style "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; margin-bottom: 16px;")
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Chain Funded
              ;div.s0.bold.mono: {(scow %ud chain-funded)} sats
            ==
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Chain Spent
              ;div.s0.bold.mono: {(scow %ud chain-spent)} sats
            ==
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Chain Balance
              ;div.s0.bold.mono: {(scow %ud (sub chain-funded chain-spent))} sats
            ==
          ==
          ;div(style "display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px;")
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Mempool Funded
              ;div.s0.bold.mono: {(scow %ud mempool-funded)} sats
            ==
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Mempool Spent
              ;div.s0.bold.mono: {(scow %ud mempool-spent)} sats
            ==
            ;div
              ;div.f2.s-1(style "opacity: 0.6;"): Mempool Balance
              ;div.s0.bold.mono: {(scow %ud (sub mempool-funded mempool-spent))} sats
            ==
          ==
        ==
      ==
    =/  refresh-status-section=manx
      ;div(id "address-refresh-status-{(trip chain)}-{(scow %ud index)}", hx-swap-oob "true")
        ;+  ?~  refresh-state
              =/  onclick-value=tape
                "refreshAddress('{(hexn:sailbox account-pubkey)}', '{(trip chain)}', {(scow %ud index)})"
              ;div.p3.b2.br2.hover.pointer(onclick onclick-value)
                ;div(style "width: 20px; height: 20px;")
                  ;+  (make:fi 'refresh-cw')
                ==
                ;span.f2.bold.f-3: Refresh Address
              ==
            =/  [refresh-pid=@ta refresh-act=?]  u.refresh-state
            ;div.p3.b2.br2(style "display: flex; flex-direction: column; gap: 12px; background: var(--b2);")
              ;div(style "display: flex; align-items: center; gap: 12px;")
                ;div(style "width: 20px; height: 20px;")
                  ;+  ?:(refresh-act (make:fi 'loader') (make:fi 'pause'))
                ==
                ;span.f2.s-1(style "opacity: 0.8;"): {?:(refresh-act "Refreshing address..." "Refresh paused")}
              ==
              ;div.fr.g2(style "justify-content: flex-end;")
                ;form(hx-post "/spv-wallet/account/{(hexn:sailbox account-pubkey)}", style "display: inline;")
                  ;input(type "hidden", name "action", value "refresh-address");
                  ;input(type "hidden", name "chain", value (trip chain));
                  ;input(type "hidden", name "index", value (scow %ud index));
                  ;input(type "hidden", name "pause-refresh", value (scow %f refresh-act));
                  ;button.p2.b2.br2.hover.pointer.f3.s-2(type "submit", style "outline: none;")
                    ; {?:(refresh-act "Pause" "Resume")}
                  ==
                ==
                ;form(hx-post "/spv-wallet/account/{(hexn:sailbox account-pubkey)}", style "display: inline;")
                  ;input(type "hidden", name "action", value "cancel-refresh");
                  ;input(type "hidden", name "chain", value (trip chain));
                  ;button.p2.b2.br2.hover.pointer.f3.s-2(type "submit", style "outline: none;")
                    ; Cancel
                  ==
                ==
              ==
            ==
      ==
  ::  Combine OOB updates into a single response
  =/  combined=manx
    ;div
      ;+  balance-section
      ;+  refresh-status-section
    ==
  %-  manx-to-wain:sailbox
  combined
--
