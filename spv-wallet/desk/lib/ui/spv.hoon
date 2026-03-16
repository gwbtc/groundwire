/-  *spv-wallet
/+  *ui-layout, *ui-utils, *ui-wallets, sailbox, fi=feather-icons, *bitcoin-spv, wallet-account
|%
::  SPV Block Headers - Power user interface for manual chain inspection
::
++  spv-stats-section-content
  |=  $:  checkpoint-height=@ud
          checkpoint-hash=@uvI
          headers=(map @uvI block-header:bitcoin-spv)
          headers-by-height=((mop @ud (set @uvI)) lth)
      ==
  ^-  manx
  =/  header-count=@ud  (lent ~(tap by headers))
  =/  tip=(unit [key=@ud val=(set @uvI)])
    (ram:((on @ud (set @uvI)) lth) headers-by-height)
  =/  tip-height=@ud  ?~(tip 0 key.u.tip)
  ;div#spv-stats.p4.b1.br2
    ;h2.s1.bold.mb3: Chain Overview
    ;div(style "display: grid; grid-template-columns: 180px 1fr; gap: 12px;")
      ;div.f3.s-1: Checkpoint Block:
      ;div.mono.s-1: {<checkpoint-height>}
      ;div.f3.s-1: Total Headers Stored:
      ;div.mono.s-1: {<header-count>}
      ;div.f3.s-1: Current Tip Height:
      ;div.mono.s-1
        ;+  ?~  tip
              ;span.f2: (no headers yet)
            ;span: {<tip-height>}
      ==
    ==
  ==
::
++  spv-stats-section
  |=  $:  checkpoint-height=@ud
          checkpoint-hash=@uvI
          headers=(map @uvI block-header:bitcoin-spv)
          headers-by-height=((mop @ud (set @uvI)) lth)
      ==
  ^-  manx
  (hx-swap-oob (spv-stats-section-content checkpoint-height checkpoint-hash headers headers-by-height))
::
++  spv-recent-headers-content
  |=  [headers-by-height=((mop @ud (set @uvI)) lth) page=@ud net=network]
  ^-  manx
  =/  net-str=tape  (scow %tas net)
  ::  Get tip height
  =/  tip=(unit [key=@ud val=(set @uvI)])
    (ram:((on @ud (set @uvI)) lth) headers-by-height)
  ::  Calculate range for this page (25 headers per page, relative to tip)
  =/  [start-height=@ud end-height=@ud has-data=?]
    ?~  tip
      [0 0 %.n]
    =/  tip-height=@ud  key.u.tip
    =/  page-offset=@ud  (mul page 25)
    ::  Check if page offset is beyond available data
    ?:  (gth page-offset tip-height)
      [0 0 %.n]
    =/  start=@ud  (sub tip-height page-offset)
    =/  end=@ud  ?:((gte start 24) (sub start 24) 0)
    [start end %.y]
  ::  Get headers in range (descending order)
  =/  headers-list=(list [@ud (set @uvI)])
    ?:  has-data
      %+  scag  25
      %+  skim
        (bap:((on @ud (set @uvI)) lth) headers-by-height)
      |=  [height=@ud hashes=(set @uvI)]
      &((lte height start-height) (gte height end-height))
    ~
  ;div#spv-recent.p4.b1.br2
    ;h2.s1.bold.mb3: Block Headers
    ::  Navigation and range display
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;")
      ;div.f3.s-1.mono
        ;+  ?:  has-data
              ;span: Page {(scow %ud page)}: Showing blocks {(scow %ud end-height)} to {(scow %ud start-height)}
            ;span: No headers yet
      ==
      ;div(style "display: flex; gap: 4px;")
        ;+  =/  newer-btn=manx
              ;button.p2.b1.br1.hover.pointer
                =hx-get  ?:(=(page 0) "/spv-wallet/spv/{net-str}" "/spv-wallet/spv/{net-str}?page={(scow %ud (dec page))}")
                =hx-target  "#spv-recent"
                =hx-select  "#spv-recent"
                =hx-swap  "outerHTML"
                =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none; {?:(=(page 0) "opacity: 0.4; pointer-events: none;" "")}"
                =title  "Newer blocks"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'chevron-left')
                ==
              ==
            ?:(=(page 0) newer-btn(a.g [[%disabled "disabled"] a.g.newer-btn]) newer-btn)
        ;button.p2.b1.br1.hover.pointer
          =hx-get  "/spv-wallet/spv/{net-str}?page={(scow %ud +(page))}"
          =hx-target  "#spv-recent"
          =hx-select  "#spv-recent"
          =hx-swap  "outerHTML"
          =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
          =title  "Older blocks"
          ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'chevron-right')
          ==
        ==
      ==
    ==
    ::  Headers table
    ;+  ?~  headers-list
          ;div.f2.s-1.tc.p4.b2.br2: No headers in this range
        ;div.p0.b0(style "max-height: 300px; overflow-y: auto;")
          ;table(style "width: 100%; border-collapse: collapse;")
            ;thead
              ;tr.b2
                ;th.p2.tl.f3.s-1: Height
                ;th.p2.tl.f3.s-1: Hash
              ==
            ==
            ;tbody
              ;*  %+  turn  headers-list
                  |=  [height=@ud hashes=(set @uvI)]
                  =/  hash-list=(list @uvI)  ~(tap in hashes)
                  =/  first-hash=@uvI  (snag 0 hash-list)
                  =/  hash-display=tape  (scow %ux first-hash)
                  =/  is-fork=?  (gth ~(wyt in hashes) 1)
                  =/  row-style=tape
                    ?:  is-fork
                      "border-bottom: 1px solid var(--b2); background: color-mix(in srgb, var(--yellow) 10%, transparent); cursor: pointer;"
                    "border-bottom: 1px solid var(--b2); cursor: pointer;"
                  =/  hash-url=tape  (trip (crip ((x-co:co 64) first-hash)))
                  ;tr.b1.hover(style row-style, onclick "window.location.href='/spv-wallet/spv/header/{hash-url}'")
                    ;td.p2.mono.s-1
                      ;+  ?:  is-fork
                            ;span(style "display: flex; align-items: center; gap: 4px;")
                              ;+  (make:fi 'git-branch')
                              ;span: {(scow %ud height)}
                            ==
                          ;span: {(scow %ud height)}
                    ==
                    ;td.p2.mono.s-2(style "max-width: 400px; overflow: hidden; text-overflow: ellipsis;"): {hash-display}
                  ==
            ==
          ==
        ==
  ==
::
++  spv-recent-headers
  |=  [headers-by-height=((mop @ud (set @uvI)) lth) net=network]
  ^-  manx
  ::  SSE updates always show page 0 (most recent)
  (hx-swap-oob (spv-recent-headers-content headers-by-height 0 net))
::
++  spv-sync-status-content
  |=  [header-sync=(unit [pid=@ta act=? height=@ud]) sync-error=(unit tang) net=network]
  ^-  manx
  =/  net-str=tape  (scow %tas net)
  ;div#spv-sync-status.p4.b1.br2
    ::  Hidden network input - buttons will include this via hx-include
    ;input#sync-network(type "hidden", name "network", value net-str);
    ;h2.s1.bold.mb3: Auto Sync
    ::  Display error if present
    ;+  ?~  sync-error
          ;span;
        ;div.p3.b1.br2.mb3(style "background: rgba(255, 80, 80, 0.15); border: 1px solid rgba(255, 80, 80, 0.4);")
          ;div(style "display: flex; align-items: start; gap: 8px;")
            ;div(style "color: #ff5050; margin-top: 2px;")
              ;+  (make:fi 'alert-circle')
            ==
            ;div.fc.g1(style "flex: 1;")
              ;div.s-1.bold(style "color: #ff5050;"): Sync Error
              ;div.p2.b2.br1.mono.s-2(style "max-height: 200px; overflow-y: auto; word-break: break-all; white-space: pre-wrap; background: rgba(0,0,0,0.2);")
                ;*  %+  turn  u.sync-error
                    |=  t=tank
                    ;div: {~(ram re t)}
              ==
            ==
          ==
        ==
    ;+  ?~  header-sync
          ::  Stopped state
          ;div(style "display: flex; justify-content: space-between; align-items: center;")
            ;div.fc.g1
              ;div.s0: Sync stopped
              ;div.f3.s-1(style "opacity: 0.7;"): Click start to sync headers from tip
            ==
            ;button.p2.b1.br1.hover.pointer
              =title  "Start syncing headers"
              =hx-post  "/spv-wallet/spv"
              =hx-vals  "\{\"action\": \"start-header-sync\"}"
              =hx-include  "#sync-network"
              =style  "background: rgba(150, 150, 150, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'play')
              ==
            ==
          ==
        =/  [sync-pid=@ta sync-act=? sync-height=@ud]  u.header-sync
        ?:  sync-act
          ::  Syncing state
          ;div(style "display: flex; justify-content: space-between; align-items: center;")
            ;div(style "display: flex; align-items: center; gap: 12px;")
              ;div.p2.b1.br1(style "background: rgba(100, 150, 255, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center;")
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                  ;+  (make:fi 'loader')
                ==
              ==
              ;div.fc.g1
                ;div.s0: Syncing...
                ;div.f3.s-1.mono(style "opacity: 0.7;"): Height: {(scow %ud sync-height)}
              ==
            ==
            ;div(style "display: flex; gap: 4px;")
              ;button.p2.b1.br1.hover.pointer
                =title  "Pause sync"
                =hx-post  "/spv-wallet/spv"
                =hx-vals  "\{\"action\": \"pause-header-sync\"}"
                =hx-include  "#sync-network"
                =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'pause')
                ==
              ==
              ;button.p2.b1.br1.hover.pointer
                =title  "Cancel sync"
                =hx-post  "/spv-wallet/spv"
                =hx-vals  "\{\"action\": \"cancel-header-sync\"}"
                =hx-include  "#sync-network"
                =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'x-circle')
                ==
              ==
            ==
          ==
        ::  Paused state
        ;div(style "display: flex; justify-content: space-between; align-items: center;")
          ;div(style "display: flex; align-items: center; gap: 12px;")
            ;div.p2.b1.br1(style "background: rgba(150, 150, 150, 0.2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center;")
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'pause-circle')
              ==
            ==
            ;div.fc.g1
              ;div.s0: Paused
              ;div.f3.s-1.mono(style "opacity: 0.7;"): Height: {(scow %ud sync-height)}
            ==
          ==
          ;div(style "display: flex; gap: 4px;")
            ;button.p2.b1.br1.hover.pointer
              =title  "Resume sync"
              =hx-post  "/spv-wallet/spv"
              =hx-vals  "\{\"action\": \"resume-header-sync\"}"
              =hx-include  "#sync-network"
              =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'play')
              ==
            ==
            ;button.p2.b1.br1.hover.pointer
              =title  "Cancel sync"
              =hx-post  "/spv-wallet/spv"
              =hx-vals  "\{\"action\": \"cancel-header-sync\"}"
              =hx-include  "#sync-network"
              =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'x-circle')
              ==
            ==
          ==
        ==
  ==
::
++  spv-sync-status
  |=  [header-sync=(unit [pid=@ta act=? height=@ud]) sync-error=(unit tang) net=network]
  ^-  manx
  (hx-swap-oob (spv-sync-status-content header-sync sync-error net))
::
++  block-header-detail-page
  |=  [hash=@uvI spv=(map network spv-chain)]
  ^-  manx
  ::  Search across all networks for the header
  =/  found=(unit [=network header=block-header:bitcoin-spv])
    %-  ~(rep by spv)
    |=  [[net=network chain=spv-chain] result=(unit [network block-header:bitcoin-spv])]
    ?^  result  result
    =/  header=(unit block-header:bitcoin-spv)  (~(get by headers.chain) hash)
    ?~  header  ~
    `[net u.header]
  ?~  found
    %-  htmx-page
    :^  "Block Header Not Found"  &  ~
    ;div.fc.g3.p5.ma.mw-page
      ;h1.s2.bold: Block Header Not Found
      ;p: No block header found with hash {(scow %ux hash)}
      ;a.p2.b-3.f-3.br1.hover.pointer(href "/spv-wallet/spv"): Back to SPV
    ==
  =/  header-data=block-header:bitcoin-spv  header.u.found
  =/  timestamp-display=tape
    (scow %da (add ~1970.1.1 (mul ~s1 timestamp.header-data)))
  %-  htmx-page
  :^  "Block Header #{(scow %ud height.header-data)}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div.fr.g2(style "align-items: center;")
      ;a.p2.b1.br1.hover.pointer(href "/spv-wallet/spv", style "text-decoration: none;"): ← Back
      ;h1.s2.bold: Block Header #{(scow %ud height.header-data)}
    ==
    ::  Block Info Card
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Block Information
      ;div(style "display: grid; grid-template-columns: 180px 1fr; gap: 12px;")
        ;div.f3.s-1: Height:
        ;div.mono.s-1: {(scow %ud height.header-data)}
        ;div.f3.s-1: Version:
        ;div.mono.s-1: {(scow %ud version.header-data)}
        ;div.f3.s-1: Timestamp:
        ;div.s-1: {timestamp-display}
        ;div.f3.s-1: Nonce:
        ;div.mono.s-1: {(scow %ud nonce.header-data)}
        ;div.f3.s-1: Difficulty (bits):
        ;div.mono.s-1: {(scow %ux bits.header-data)}
      ==
    ==
    ::  Hashes Card
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Hashes
      ;div.fc.g3
        ;div
          ;div.f3.s-1.mb1: Block Hash:
          ;div.mono.s-2.p2.b2.br1(style "word-break: break-all;"): {(scow %ux computed-hash.header-data)}
        ==
        ;div
          ;div.f3.s-1.mb1: Previous Block Hash:
          ;div.mono.s-2.p2.b2.br1(style "word-break: break-all;"): {(scow %ux prev-hash.header-data)}
        ==
        ;div
          ;div.f3.s-1.mb1: Merkle Root:
          ;div.mono.s-2.p2.b2.br1(style "word-break: break-all;"): {(scow %ux merkle-root.header-data)}
        ==
      ==
    ==
    ::  Chain Info Card
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Chain Information
      ;div(style "display: grid; grid-template-columns: 180px 1fr; gap: 12px;")
        ;div.f3.s-1: Proof-of-Work Verified:
        ;div.s-1: {?:(verified-pow.header-data "Yes" "No")}
        ;div.f3.s-1: Cumulative Work:
        ;div.mono.s-1: {(scow %ud cumulative-work.header-data)}
        ;div.f3.s-1: Child Blocks:
        ;div.mono.s-1: {(scow %ud ~(wyt in children.header-data))}
      ==
    ==
    ::  Raw Data Card
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Raw Header Data
      ;div.mono.s-2.p3.b2.br1(style "word-break: break-all; white-space: pre-wrap;"): {(trip raw.header-data)}
    ==
  ==
::
++  spv-list-page
  |=  state=state-0
  ^-  manx
  %-  htmx-page
  :^  "SPV Block Headers"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href "/spv-wallet", style "color: var(--f3); text-decoration: none;"): ← Back to Wallets
    ==
    ;h1.s2.bold: SPV Block Headers
    ;p.f3.mb3: Select a network to manage block headers for SPV verification.
    ;div.fc.g3
      ::  Mainnet card
      ;a.p4.b1.br2.hover.pointer(href "/spv-wallet/spv/main", style "text-decoration: none; display: block;")
        ;div.fr.g3(style "align-items: center;")
          ;div.p2.br1(style "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'shield')
          ==
          ;div.fc.g1(style "flex: 1;")
            ;div.s1.bold(style "color: #ffb432;"): Mainnet
            ;div.f3.s-1: Bitcoin main network
          ==
          ;div(style "color: var(--f3);")
            ;+  (make:fi 'chevron-right')
          ==
        ==
      ==
      ::  Testnet3 card
      ;a.p4.b1.br2.hover.pointer(href "/spv-wallet/spv/testnet3", style "text-decoration: none; display: block;")
        ;div.fr.g3(style "align-items: center;")
          ;div.p2.br1(style "background: rgba(100, 150, 255, 0.2); border: 1px solid rgba(100, 150, 255, 0.4); color: #6496ff; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'code')
          ==
          ;div.fc.g1(style "flex: 1;")
            ;div.s1.bold(style "color: #6496ff;"): Testnet3
            ;div.f3.s-1: Bitcoin test network (legacy)
          ==
          ;div(style "color: var(--f3);")
            ;+  (make:fi 'chevron-right')
          ==
        ==
      ==
      ::  Testnet4 card
      ;a.p4.b1.br2.hover.pointer(href "/spv-wallet/spv/testnet4", style "text-decoration: none; display: block;")
        ;div.fr.g3(style "align-items: center;")
          ;div.p2.br1(style "background: rgba(150, 100, 255, 0.2); border: 1px solid rgba(150, 100, 255, 0.4); color: #9664ff; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'code')
          ==
          ;div.fc.g1(style "flex: 1;")
            ;div.s1.bold(style "color: #9664ff;"): Testnet4
            ;div.f3.s-1: Bitcoin test network (new)
          ==
          ;div(style "color: var(--f3);")
            ;+  (make:fi 'chevron-right')
          ==
        ==
      ==
    ==
  ==
::
++  spv-page
  |=  [state=state-0 page=@ud net=network]
  ^-  manx
  =/  chain=spv-chain  (get-spv-chain net spv.state)
  =/  net-str=tape  (scow %tas net)
  =/  net-label=tape
    ?+  net  !!
      %main      "Mainnet"
      %testnet3  "Testnet3"
      %testnet4  "Testnet4"
    ==
  %-  htmx-page
  :^  "SPV Block Headers - {net-label}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href "/spv-wallet/spv", style "color: var(--f3); text-decoration: none;"): ← Back to Networks
    ==
    ;h1.s2.bold: {net-label} Block Headers
    ::  SSE listeners for updates - include network in path for proper routing
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/spv/{net-str}", sse-swap "spv-update,sync-status-update", style "display: none;");
    ;script: document.body.addEventListener('htmx:sseMessage', function(evt) \{ if (evt.detail.type === 'spv-update') \{ const urlParams = new URLSearchParams(window.location.search); const page = urlParams.get('page') || '0'; const pathParts = window.location.pathname.split('/'); const network = pathParts[3] || 'testnet3'; htmx.ajax('GET', '/spv-wallet/spv/' + network + '?page=' + page, \{ target: '#spv-recent', select: '#spv-recent', swap: 'outerHTML' }); } });
    ::  Chain Stats
    ;+  %:  spv-stats-section-content
            checkpoint-height.chain
            checkpoint-hash.chain
            headers.chain
            headers-by-height.chain
        ==
    ::  Auto Sync Status
    ;+  (spv-sync-status-content header-sync.chain sync-error.chain net)
    ::  Recent Headers
    ;+  (spv-recent-headers-content headers-by-height.chain page net)
    ::  Checkpoint Setting
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Set Checkpoint
      ;form#set-checkpoint-form.fc.g1(method "post", action "/spv-wallet/spv", hx-boost "false")
        ;input(type "hidden", name "action", value "set-checkpoint");
        ;input(type "hidden", name "network", value net-str);
        ;label.s-1.bold: Checkpoint Block Height
        ;div.fr.g2(style "align-items: center;")
          ;input#checkpoint-height.p2.b1.br1(type "number", name "height", placeholder "2500000", required "true", style "flex: 1; background: var(--b2);");
          ;button.p2.br1.hover.pointer(type "button", onclick "confirmSetCheckpoint()", style "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; outline: none;"): Set Checkpoint
        ==
      ==
    ==
    ::  Clear Headers
    ;div.p4.b1.br2
      ;h2.s1.bold.mb3: Clear Headers
      ;div.fc.g3
        ::  Clear all
        ;form#clear-all-form.fc.g1(method "post", action "/spv-wallet/spv", hx-boost "false")
          ;input(type "hidden", name "action", value "clear-all-headers");
          ;input(type "hidden", name "network", value net-str);
          ;label.s-1.bold: Clear All Headers
          ;button.p2.br1.hover.pointer(type "button", onclick "confirmClearAll()", style "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;"): Clear All
        ==
        ::  Clear after height
        ;form#clear-after-form.fc.g1(method "post", action "/spv-wallet/spv", hx-boost "false")
          ;input(type "hidden", name "action", value "clear-headers-after");
          ;input(type "hidden", name "network", value net-str);
          ;label.s-1.bold: Clear Headers After Height
          ;div.fr.g2(style "align-items: center;")
            ;input#clear-after-height.p2.b1.br1(type "number", name "height", placeholder "2500100", required "true", style "flex: 1; background: var(--b2);");
            ;button.p2.br1.hover.pointer(type "button", onclick "confirmClearAfter()", style "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;"): Clear After
          ==
        ==
        ::  Clear before height
        ;form#clear-before-form.fc.g1(method "post", action "/spv-wallet/spv", hx-boost "false")
          ;input(type "hidden", name "action", value "clear-headers-before");
          ;input(type "hidden", name "network", value net-str);
          ;label.s-1.bold: Clear Headers Before Height
          ;div.fr.g2(style "align-items: center;")
            ;input#clear-before-height.p2.b1.br1(type "number", name "height", placeholder "2500000", required "true", style "flex: 1; background: var(--b2);");
            ;button.p2.br1.hover.pointer(type "button", onclick "confirmClearBefore()", style "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;"): Clear Before
          ==
        ==
      ==
    ==
    ::  Confirmation modal and scripts
    ;div#clear-modal(style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div.p4.b2.br2.fc.g3(style "background: var(--b1); max-width: 400px; margin: 20px;")
        ;h3.s1.bold: Confirm Clear Operation
        ;div#clear-modal-message.f3;
        ;div.fr.g2(style "justify-content: flex-end;")
          ;button.p2.br1.hover.pointer(onclick "closeClearModal()", style "background: rgba(150, 150, 150, 0.2); border: 1px solid rgba(150, 150, 150, 0.4); color: #999; outline: none;"): Cancel
          ;button#clear-modal-confirm.p2.br1.hover.pointer(style "outline: none;"): Confirm
        ==
      ==
    ==
    ;script
      ;+  ;/
          """
          function confirmSetCheckpoint() \{
            const height = document.getElementById('checkpoint-height').value;
            if (!height) return alert('Please enter a height');
            document.getElementById('clear-modal-message').textContent = 'Set checkpoint to height ' + height + '? This will be your new trusted starting point.';
            document.getElementById('clear-modal-confirm').onclick = function() \{
              document.getElementById('set-checkpoint-form').submit();
              closeClearModal();
            };
            // Green for positive action
            document.getElementById('clear-modal-confirm').style.cssText = 'background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; outline: none;';
            document.getElementById('clear-modal').style.display = 'flex';
          }
          function confirmClearAll() \{
            document.getElementById('clear-modal-message').textContent = 'Clear ALL headers? This cannot be undone.';
            document.getElementById('clear-modal-confirm').onclick = function() \{
              document.getElementById('clear-all-form').submit();
              closeClearModal();
            };
            // Red for destructive action
            document.getElementById('clear-modal-confirm').style.cssText = 'background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;';
            document.getElementById('clear-modal').style.display = 'flex';
          }
          function confirmClearAfter() \{
            const height = document.getElementById('clear-after-height').value;
            if (!height) return alert('Please enter a height');
            document.getElementById('clear-modal-message').textContent = 'Clear all headers AFTER height ' + height + '? This cannot be undone.';
            document.getElementById('clear-modal-confirm').onclick = function() \{
              document.getElementById('clear-after-form').submit();
              closeClearModal();
            };
            // Red for destructive action
            document.getElementById('clear-modal-confirm').style.cssText = 'background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;';
            document.getElementById('clear-modal').style.display = 'flex';
          }
          function confirmClearBefore() \{
            const height = document.getElementById('clear-before-height').value;
            if (!height) return alert('Please enter a height');
            document.getElementById('clear-modal-message').textContent = 'Clear all headers BEFORE height ' + height + '? This cannot be undone.';
            document.getElementById('clear-modal-confirm').onclick = function() \{
              document.getElementById('clear-before-form').submit();
              closeClearModal();
            };
            // Red for destructive action
            document.getElementById('clear-modal-confirm').style.cssText = 'background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; outline: none;';
            document.getElementById('clear-modal').style.display = 'flex';
          }
          function closeClearModal() \{
            document.getElementById('clear-modal').style.display = 'none';
          }
          """
    ==
  ==
::
++  handle-spv-sse
  |=  $:  =bowl:gall
          state=vase
          net=network
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =/  st=state-0  !<(state-0 state)
  =/  chain=spv-chain  (get-spv-chain net spv.st)
  ?~  event
    ~
  ?+  u.event  ~
      %sync-status-update
    ::  Only update sync status, not headers (preserve current page)
    (manx-to-wain:sailbox (spv-sync-status header-sync.chain sync-error.chain net))
      %spv-update
    ::  Update stats and sync status only - headers refetched by JavaScript
    ::  Wrap both OOB updates in a container
    %:  manx-to-wain:sailbox
        ;div
          ;+  %:  spv-stats-section
                  checkpoint-height.chain
                  checkpoint-hash.chain
                  headers.chain
                  headers-by-height.chain
              ==
          ;+  (spv-sync-status header-sync.chain sync-error.chain net)
        ==
    ==
      %wallet-list-update
    ::  Update all wallet lists after add/remove operations
    %:  manx-to-wain:sailbox
        ;div
          ;+  (full-wallet-list-oob wallets.st)
          ;+  (watch-only-list-oob watch-only.st accounts.st)
          ;+  (signing-list-oob signing.st accounts.st)
        ==
    ==
      %generate-clear
    ::  Clear the generate form after wallet generation
    %:  manx-to-wain:sailbox
        ;div#generate-clear-listener(hx-swap-oob "true", style "display: none;")
          ;script
            ; document.getElementById('generate-form').reset();
          ==
        ==
    ==
  ==
--
