/-  *spv-wallet
/+  *ui-layout, *ui-utils, sailbox, fi=feather-icons
|%
::  Groundwire Management - Ordinal Comet Identity Management
::
::  This page manages satoshis with ordinal inscriptions that act as
::  proof of ownership of Urbit identities (comets). Features include:
::  - Viewing inscribed satoshis
::  - Spawning comets from inscriptions
::  - Rekeying comet identities
::  - Various identity management functions
::
++  groundwire-page
  |=  state=state-0
  ^-  manx
  %-  htmx-page
  :^  "Groundwire Management"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href "/spv-wallet", style "color: var(--f3); text-decoration: none;"): ← Back to Wallets
    ==
    ;h1.s2.bold: Groundwire Management
    ;p.f3.mb3: Manage ordinal inscriptions that prove ownership of Urbit comet identities.
    ::  Placeholder content
    ;div.fc.g3
      ::  Overview card
      ;div.p4.b1.br2
        ;div.fr.g3(style "align-items: center;")
          ;div.p2.br1(style "background: rgba(100, 200, 150, 0.2); border: 1px solid rgba(100, 200, 150, 0.4); color: #64c896; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'link')
          ==
          ;div.fc.g1(style "flex: 1;")
            ;div.s1.bold: Comet Identities
            ;div.f3.s-1: No inscribed identities found
          ==
        ==
      ==
      ::  Coming soon features
      ;div.p4.b1.br2
        ;h2.s1.bold.mb3: Coming Soon
        ;div.fc.g2
          ;div.fr.g2(style "align-items: center; opacity: 0.6;")
            ;div(style "width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
              ;+  (make:fi 'search')
            ==
            ;div.f2.s-1: Scan for inscribed satoshis in wallet
          ==
          ;div.fr.g2(style "align-items: center; opacity: 0.6;")
            ;div(style "width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
              ;+  (make:fi 'plus-circle')
            ==
            ;div.f2.s-1: Create new comet inscription
          ==
          ;div.fr.g2(style "align-items: center; opacity: 0.6;")
            ;div(style "width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
              ;+  (make:fi 'refresh-cw')
            ==
            ;div.f2.s-1: Rekey existing comet identity
          ==
          ;div.fr.g2(style "align-items: center; opacity: 0.6;")
            ;div(style "width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
              ;+  (make:fi 'zap')
            ==
            ;div.f2.s-1: Spawn comet from inscription
          ==
          ;div.fr.g2(style "align-items: center; opacity: 0.6;")
            ;div(style "width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; color: var(--f3);")
              ;+  (make:fi 'send')
            ==
            ;div.f2.s-1: Transfer inscribed satoshi
          ==
        ==
      ==
    ==
  ==
--
