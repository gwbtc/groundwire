/-  *spv-wallet
/+  seed-phrases, wallet-account
|%
:: Compute balance for a mop of hd-leaves using explicit fields
::
++  compute-mop-balance
  |=  leaf-mop=((mop @ud hd-leaf) gth)
  ^-  @ud
  =/  leaves=(list [@ud hd-leaf])
    (tap:((on @ud hd-leaf) gth) leaf-mop)
  |-
  ?~  leaves  0
  =/  [index=@ud =hd-leaf]  i.leaves
  =/  details=address-details  main.hd-leaf
  ?~  info.details
    $(leaves t.leaves)
  =/  balance=@ud
    (sub chain-funded.u.info.details chain-spent.u.info.details)
  (add balance $(leaves t.leaves))
:: Compute total account balance from both receiving and change addresses
::
++  compute-account-balance
  |=  account=account-details
  ^-  @ud
  =/  ac  ~(. ac:wallet-account [account active-network.account])
  =/  receiving-balance=@ud  (compute-mop-balance receiving:ac)
  =/  change-balance=@ud  (compute-mop-balance change:ac)
  (add receiving-balance change-balance)
::
++  join
  |=  [sep=tape strings=(list tape)]
  ^-  tape
  ?~  strings  ~
  ?~  t.strings  i.strings
  (welp i.strings (welp sep $(strings t.strings)))
::
++  seed-to-cord
  |=  =seed
  ^-  @t
  ?-  -.seed
    %t  t.seed
    %q  (scot %q q.seed)
  ==
::
++  mask-seed-phrase
  |=  =seed
  ^-  @t
  ?-  -.seed
      %t
    =/  words=(list tape)  (split-words:seed-phrases (trip t.seed))
    =/  first-three=(list tape)  (scag 3 words)
    =/  remaining-count=@ud  (sub (lent words) 3)
    =/  stars=(list tape)  (reap remaining-count "****")
    =/  masked-words=(list tape)  (welp first-three stars)
    (crip (join " " masked-words))
      %q
    =/  text=tape  (scow %q q.seed)
    =/  show-len=@ud  (min 12 (lent text))
    (crip (weld (scag show-len text) "..."))
  ==
::
++  bitcoin-mainnet-svg
  ^-  manx
  %-  need  %-  de-xml:html
  '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 64 64" version="1.1"><g transform="translate(0.00630876,-0.00301984)"><path fill="#f7931a" d="m63.033,39.744c-4.274,17.143-21.637,27.576-38.782,23.301-17.138-4.274-27.571-21.638-23.295-38.78,4.272-17.145,21.635-27.579,38.775-23.305,17.144,4.274,27.576,21.64,23.302,38.784z"/><path fill="#FFF" d="m46.103,27.444c0.637-4.258-2.605-6.547-7.038-8.074l1.438-5.768-3.511-0.875-1.4,5.616c-0.923-0.23-1.871-0.447-2.813-0.662l1.41-5.653-3.509-0.875-1.439,5.766c-0.764-0.174-1.514-0.346-2.242-0.527l0.004-0.018-4.842-1.209-0.934,3.75s2.605,0.597,2.55,0.634c1.422,0.355,1.679,1.296,1.636,2.042l-1.638,6.571c0.098,0.025,0.225,0.061,0.365,0.117-0.117-0.029-0.242-0.061-0.371-0.092l-2.296,9.205c-0.174,0.432-0.615,1.08-1.609,0.834,0.035,0.051-2.552-0.637-2.552-0.637l-1.743,4.019,4.569,1.139c0.85,0.213,1.683,0.436,2.503,0.646l-1.453,5.834,3.507,0.875,1.439-5.772c0.958,0.26,1.888,0.5,2.798,0.726l-1.434,5.745,3.511,0.875,1.453-5.823c5.987,1.133,10.489,0.676,12.384-4.739,1.527-4.36-0.076-6.875-3.226-8.515,2.294-0.529,4.022-2.038,4.483-5.155zm-8.022,11.249c-1.085,4.36-8.426,2.003-10.806,1.412l1.928-7.729c2.38,0.594,10.012,1.77,8.878,6.317zm1.086-11.312c-0.99,3.966-7.1,1.951-9.082,1.457l1.748-7.01c1.982,0.494,8.365,1.416,7.334,5.553z"/></g></svg>'
::
++  bitcoin-testnet-svg
  ^-  manx
  %-  need  %-  de-xml:html
  '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 64 64" version="1.1"><g transform="translate(0.00630876,-0.00301984)"><path fill="#6b8fd8" d="m63.033,39.744c-4.274,17.143-21.637,27.576-38.782,23.301-17.138-4.274-27.571-21.638-23.295-38.78,4.272-17.145,21.635-27.579,38.775-23.305,17.144,4.274,27.576,21.64,23.302,38.784z"/><path fill="#FFF" d="m46.103,27.444c0.637-4.258-2.605-6.547-7.038-8.074l1.438-5.768-3.511-0.875-1.4,5.616c-0.923-0.23-1.871-0.447-2.813-0.662l1.41-5.653-3.509-0.875-1.439,5.766c-0.764-0.174-1.514-0.346-2.242-0.527l0.004-0.018-4.842-1.209-0.934,3.75s2.605,0.597,2.55,0.634c1.422,0.355,1.679,1.296,1.636,2.042l-1.638,6.571c0.098,0.025,0.225,0.061,0.365,0.117-0.117-0.029-0.242-0.061-0.371-0.092l-2.296,9.205c-0.174,0.432-0.615,1.08-1.609,0.834,0.035,0.051-2.552-0.637-2.552-0.637l-1.743,4.019,4.569,1.139c0.85,0.213,1.683,0.436,2.503,0.646l-1.453,5.834,3.507,0.875,1.439-5.772c0.958,0.26,1.888,0.5,2.798,0.726l-1.434,5.745,3.511,0.875,1.453-5.823c5.987,1.133,10.489,0.676,12.384-4.739,1.527-4.36-0.076-6.875-3.226-8.515,2.294-0.529,4.022-2.038,4.483-5.155zm-8.022,11.249c-1.085,4.36-8.426,2.003-10.806,1.412l1.928-7.729c2.38,0.594,10.012,1.77,8.878,6.317zm1.086-11.312c-0.99,3.966-7.1,1.951-9.082,1.457l1.748-7.01c1.982,0.494,8.365,1.416,7.334,5.553z"/></g></svg>'
::
++  default-coin-svg
  ^-  manx
  %-  need  %-  de-xml:html
  '<svg xmlns="http://www.w3.org/2000/svg" height="16" width="16" viewBox="0 0 64 64" version="1.1"><g transform="translate(0.00630876,-0.00301984)"><circle cx="32" cy="32" r="30" fill="#9ca3af"/></g></svg>'
::
++  purpose-name
  |=  purpose=seg:hd-path
  ^-  tape
  =/  [hardened=? index=@ud]  purpose
  ?+  index  (scow %ud index)
      %86  "Taproot (BIP86) - 86"
      %84  "Native SegWit (BIP84) - 84"
      %49  "Wrapped SegWit (BIP49) - 49"
      %44  "Legacy (BIP44) - 44"
  ==
::
++  purpose-badge
  |=  purpose=seg:hd-path
  ^-  manx
  =/  [hardened=? index=@ud]  purpose
  =/  tooltip=tape  (purpose-name purpose)
  =/  [color=tape label=tape]
    ?+  index  ["#888" (scow %ud index)]
        %86  ["#9333ea" "86"]  ::  Taproot - modern purple
        %84  ["#10b981" "84"]  ::  Native SegWit - green
        %49  ["#f59e0b" "49"]  ::  Wrapped SegWit - amber
        %44  ["#6b7280" "44"]  ::  Legacy - gray
    ==
  ;div(title "{tooltip}", style "display: inline-flex; align-items: center; justify-content: center; width: 18px; height: 18px; border-radius: 50%; background: {color}; color: white; font-size: 10px; font-weight: bold; font-family: monospace; cursor: default;"): {label}
::
++  coin-type-name
  |=  coin-type=seg:hd-path
  ^-  tape
  =/  [hardened=? index=@ud]  coin-type
  ?+  index  (scow %ud index)
      %0  "Bitcoin Mainnet - 0"
      %1  "Bitcoin Testnet - 1"
      %2  "Litecoin - 2"
  ==
::
++  coin-type-badge
  |=  coin-type=seg:hd-path
  ^-  manx
  =/  [hardened=? index=@ud]  coin-type
  =/  tooltip=tape  (coin-type-name coin-type)
  =/  badge=manx
    ?+  index  default-coin-svg
        %0  bitcoin-mainnet-svg
        %1  bitcoin-testnet-svg
    ==
  ::  Wrap badge with title attribute
  ;span(title "{tooltip}", style "cursor: default;")
    ;+  badge
  ==
::
::  Badge for script-type (used by watch-only/signing accounts)
++  script-type-badge
  |=  =script-type
  ^-  manx
  =/  [color=tape label=tape tooltip=tape]
    ?-  script-type
        %p2pkh         ["#6b7280" "44" "Legacy (BIP44) - P2PKH"]
        %p2sh-p2wpkh   ["#f59e0b" "49" "Wrapped SegWit (BIP49) - P2SH-P2WPKH"]
        %p2wpkh        ["#10b981" "84" "Native SegWit (BIP84) - P2WPKH"]
        %p2tr          ["#9333ea" "86" "Taproot (BIP86) - P2TR"]
    ==
  ;div(title "{tooltip}", style "display: inline-flex; align-items: center; justify-content: center; width: 18px; height: 18px; border-radius: 50%; background: {color}; color: white; font-size: 10px; font-weight: bold; font-family: monospace; cursor: default;"): {label}
::
::  Badge for network (used by watch-only/signing accounts)
++  network-badge
  |=  =network
  ^-  manx
  =/  [badge=manx tooltip=tape]
    ?-  network
        %main      [bitcoin-mainnet-svg "Bitcoin Mainnet"]
        %testnet3  [bitcoin-testnet-svg "Bitcoin Testnet3"]
        %testnet4  [bitcoin-testnet-svg "Bitcoin Testnet4"]
        %regtest   [bitcoin-testnet-svg "Bitcoin Regtest"]
        %signet    [bitcoin-testnet-svg "Bitcoin Signet"]
    ==
  ;span(title "{tooltip}", style "cursor: default;")
    ;+  badge
  ==
::
++  network-to-cord
  |=  =network
  ^-  @t
  ?-  network
    %main      'Mainnet'
    %testnet3  'Testnet3'
    %testnet4  'Testnet4'
    %signet    'Signet'
    %regtest   'Regtest'
  ==
::
++  todo
  |=  [priority=?(%low %medium %high %critical) message=manx]
  ^-  manx
  =/  [emoji=tape border-color=tape bg-color=tape]
    ?-  priority
      %low       ["📝" "var(--f-2)" "rgba(200, 200, 200, 0.1)"]
      %medium    ["📝" "var(--f-2)" "rgba(255, 187, 0, 0.1)"]
      %high      ["⚠️" "#ff9500" "rgba(255, 149, 0, 0.15)"]
      %critical  ["🚨" "#ff3b30" "rgba(255, 59, 48, 0.2)"]
    ==
  ;div.p3.br2(style "border: 2px dashed {border-color}; background: {bg-color};")
    ;div(style "display: flex; align-items: flex-start; gap: 12px;")
      ;div(style "font-size: 24px; line-height: 1;"): {emoji}
      ;div.fc.g1(style "flex: 1;")
        ;div.bold.s-1: TODO
        ;+  message
      ==
    ==
  ==
--
