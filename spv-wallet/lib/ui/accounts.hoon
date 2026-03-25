/-  *spv-wallet
/+  sailbox, fi=feather-icons, *ui-utils, *ui-layout, *wallet-address
|%
++  account-row
  |=  [pubkey=@ux acct=account:hd-path details=account-details]
  ^-  manx
  =/  [purpose=seg:hd-path coin-type=seg:hd-path account=seg:hd-path]  acct
  =/  account-path-str=tape  (format-account-path acct)
  =/  encoded-pubkey=tape  (en-urlt:html (hexn:sailbox pubkey))
  =/  encoded-path=tape  (en-urlt:html (trip (crip account-path-str)))
  =/  account-url=tape
    (welp "/spv-wallet/account/" encoded-pubkey)
  ::  Check if this account has an active scan
  =/  scan-state=(unit [pid=@ta act=? scn=account-scan])  scan.proc.details
  =/  is-scanning=?  ?=(^ scan-state)
  =/  is-active=?
    ?~  scan-state  %.n
    act.u.scan-state
  ::  Create CSS-safe ID using numeric components
  =/  row-id=tape
    %+  welp  "account-row-"
    %+  welp  (scow %ud q.purpose)
    %+  welp  "-"
    %+  welp  (scow %ud q.coin-type)
    %+  welp  "-"
    (scow %ud q.account)
  ::  Extract scan state variables if scanning
  =/  [pid=@ta act=? scn=account-scan]
    ?~  scan-state  ['' %.n *account-scan]
    u.scan-state
  ::  Compute scan progress text
  =/  phase-text=tape  ?:(=(%1 -.scn) "Receiving" "Change")
  =/  index-text=tape  (scow %ud idx.scn)
  ;div.p3.b1.br2.hover(id row-id, style "display: flex; justify-content: space-between; align-items: center; gap: 12px;")
    ;a(href account-url, style "flex: 1; min-width: 0; text-decoration: none; color: inherit; outline: none !important;")
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;+  (purpose-badge purpose)
        ;span.s0.bold: {(trip name.details)}
      ==
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;+  (coin-type-badge coin-type)
        ;div.f3.s-2.mono: {account-path-str}
      ==
    ==
    ;+  ?:  is-scanning
          ::  Scan in progress - show status indicator + control buttons
          ;div(style "display: flex; gap: 8px; align-items: center;")
            ::  Show scan progress text
            ;span.f3.s-1(style "opacity: 0.8;"): {phase-text}: {index-text}
            ;div(style "display: flex; gap: 4px;")
              ;+  ?:  act
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
            ;+  ?:  act
                  ::  Active - show pause button
                  ;button.p2.b1.br1.hover.pointer
                    =title  "Pause this scan"
                    =hx-post  "/spv-wallet/account/{(hexn:sailbox pubkey)}"
                    =hx-vals  "\{\"action\": \"pause-scan\"}"
                    =onclick  "event.preventDefault(); event.stopPropagation();"
                    =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                    ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'pause')
                    ==
                  ==
                ::  Paused - show play button
                ;button.p2.b1.br1.hover.pointer
                  =title  "Resume this scan"
                  =hx-post  "/spv-wallet/account/{(hexn:sailbox pubkey)}"
                  =hx-vals  "\{\"action\": \"resume-scan\"}"
                  =onclick  "event.preventDefault(); event.stopPropagation();"
                  =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                  ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'play')
                  ==
                ==
            ;button.p2.b1.br1.hover.pointer
              =title  "Cancel this scan"
              =hx-post  "/spv-wallet/wallet/{(hexn:sailbox pubkey)}/account/{encoded-path}"
              =hx-vals  "\{\"action\": \"cancel-scan\"}"
              =onclick  "event.preventDefault(); event.stopPropagation();"
              =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'x-circle')
              ==
            ==
          ==
        ==
        ::  Normal state - show scan and delete buttons
        ;div(style "display: flex; gap: 4px;")
          ;button.p2.b1.br1.hover.pointer
            =title  "Full scan this account"
            =hx-post  "/spv-wallet/wallet/{(hexn:sailbox pubkey)}/account/{encoded-path}"
            =hx-vals  "\{\"action\": \"full-scan\", \"chain\": \"receiving\"}"
            =onclick  "event.preventDefault(); event.stopPropagation();"
            =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'refresh-cw')
            ==
          ==
          ;button.delete-account-btn.p2.b1.br1.hover.pointer
            =data-account-name  (trip name.details)
            =data-account-path  account-path-str
            =onclick  "event.preventDefault(); event.stopPropagation(); showAccountModal(this.dataset.accountName, this.dataset.accountPath)"
            =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'trash-2')
            ==
          ==
        ==
  ==
::
++  account-row-oob
  |=  [pubkey=@ux acct=account:hd-path details=account-details]
  ^-  manx
  (hx-swap-oob (account-row pubkey acct details))
::
++  accounts-list
  |=  [pubkey=@ux waccts=(map account:hd-path @ux) accounts=(map @ux account-details)]
  ^-  manx
  =/  account-list=(list [account:hd-path @ux])  ~(tap by waccts)
  ?~  account-list
    ;div.p4.b1.br2.tc(style "background: var(--b1);")
      ;div.s0.f2.mb2: No accounts yet
      ;div.f3.s-1: Use the forms below to scan for existing accounts or add a new one
    ==
  ;div.fc.g2
    ;*  %+  turn  account-list
        |=  [acct=account:hd-path key=@ux]
        =/  details=account-details  (~(got by accounts) key)
        (account-row key acct details)
  ==
::
++  accounts-list-oob
  |=  [pubkey=@ux waccts=(map account:hd-path @ux) accounts=(map @ux account-details)]
  ^-  manx
  =/  base=manx  (accounts-list pubkey waccts accounts)
  base(a.g [[%id "account-list"] [%hx-swap-oob "true"] [%style "flex: 1; min-height: 0; overflow-y: auto;"] a.g.base])
::
++  add-unlisted-account-ui
  ^-  manx
  ;div.p4.b2.br2.add-account-form
    ;div.s0.bold.tc.hover.pointer(onclick "toggleAddPanel(this)", style "display: flex; align-items: center; justify-content: center; gap: 8px;")
      ; Add Unlisted Account
      ;div.add-chevron(style "width: 16px; height: 16px; display: flex; align-items: center; transition: transform 0.2s;")
        ;+  (make:fi 'chevron-down')
      ==
    ==
    ;div.add-panel(style "display: none;")
    ;p.f3.s-2.mb2: Manually add an account at a specific derivation path
    ;form(method "post")
      ;div.fc.g2
        ;div
          ;label.s-1.bold.f3: Account Name
          ;input.p2.b1.br1.wf(type "text", name "account-name", placeholder "My Account", required "true");
        ==
        ;div
          ;label.s-1.bold.f3: Purpose
          ;select.purpose-select.p2.b1.br1.wf.hover.pointer(name "purpose-select", required "true", style "outline: none;")
            ;option(value "84", selected "selected"): Native SegWit (BIP84) - 84
            ;option(value "49"): Wrapped SegWit (BIP49) - 49
            ;option(value "44"): Legacy (BIP44) - 44
            ;option(value "86"): Taproot (BIP86) - 86
            ;option(value "custom"): Custom...
          ==
          ;div.custom-purpose-container.fc.g1(style "display: none; margin-top: 8px;")
            ;input.custom-purpose-input.p2.b1.br1.wf(type "number", name "purpose-custom", placeholder "Enter purpose number", min "0", max "2147483647");
            ;div.f3.s-2(style "color: var(--f-2);"): ⚠️ Non-standard purposes may not work with other wallets
          ==
        ==
        ;div
          ;label.s-1.bold.f3: Coin Type
          ;select.coin-type-select.p2.b1.br1.wf.hover.pointer(name "coin-type-select", required "true", style "outline: none;")
            ;option(value "0", selected "selected"): Bitcoin Mainnet - 0
            ;option(value "1"): Bitcoin Testnet - 1
            ;option(value "2"): Litecoin - 2
            ;option(value "custom"): Custom...
          ==
          ;div.custom-coin-type-container.fc.g1(style "display: none; margin-top: 8px;")
            ;input.custom-coin-type-input.p2.b1.br1.wf(type "number", name "coin-type-custom", placeholder "Enter coin type (SLIP-44)", min "0", max "2147483647");
            ;div.f3.s-2(style "color: var(--f-2);"): ⚠️ See SLIP-44 registry for valid coin types
          ==
        ==
        ;div
          ;label.s-1.bold.f3: Account Number
          ;input.p2.b1.br1.wf(type "number", name "account-number", placeholder "0", min "0", max "2147483647", required "true");
        ==
        ;input(type "hidden", name "action", value "add-unlisted-account");
        ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
          ; Add Account
        ==
      ==
    ==
    ;script
      ; (function() {
      ;   var scripts = document.getElementsByTagName('script');
      ;   var thisScript = scripts[scripts.length - 1];
      ;   var container = thisScript.closest('.add-account-form');
      ;   if (!container) return;
      ;
      ;   var purposeSelect = container.querySelector('.purpose-select');
      ;   purposeSelect.onchange = function() {
      ;     var customContainer = this.parentElement.querySelector('.custom-purpose-container');
      ;     var customInput = customContainer.querySelector('.custom-purpose-input');
      ;     if (this.value === 'custom') {
      ;       customContainer.style.display = 'flex';
      ;       customInput.required = true;
      ;     } else {
      ;       customContainer.style.display = 'none';
      ;       customInput.required = false;
      ;     }
      ;   };
      ;
      ;   var coinTypeSelect = container.querySelector('.coin-type-select');
      ;   coinTypeSelect.onchange = function() {
      ;     var customContainer = this.parentElement.querySelector('.custom-coin-type-container');
      ;     var customInput = customContainer.querySelector('.custom-coin-type-input');
      ;     if (this.value === 'custom') {
      ;       customContainer.style.display = 'flex';
      ;       customInput.required = true;
      ;     } else {
      ;       customContainer.style.display = 'none';
      ;       customInput.required = false;
      ;     }
      ;   };
      ; })();
      ;
      ; if (!window.toggleAddPanel) {
      ;   window.toggleAddPanel = function(el) {
      ;     var panel = $(el).siblings('.add-panel');
      ;     var chevron = $(el).find('.add-chevron');
      ;     if (panel.is(':visible')) {
      ;       panel.slideUp(150);
      ;       chevron.css('transform', '');
      ;     } else {
      ;       $('.add-panel:visible').slideUp(150);
      ;       $('.add-chevron').css('transform', '');
      ;       panel.slideDown(150);
      ;       chevron.css('transform', 'rotate(180deg)');
      ;     }
      ;   };
      ; }
    ==
    ==
  ==
--
