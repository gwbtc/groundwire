/-  *spv-wallet
/+  sailbox, fi=feather-icons
|%
++  account-scanner-ui
  |=  pubkey=@ux
  ^-  manx
  ;div.p4.b2.br2.account-scanner-form
    ;div.s0.bold.tc.hover.pointer(onclick "toggleAddPanel(this)", style "display: flex; align-items: center; justify-content: center; gap: 8px;")
      ; Discover Accounts
      ;div.add-chevron(style "width: 16px; height: 16px; display: flex; align-items: center; transition: transform 0.2s;")
        ;+  (make:fi 'chevron-down')
      ==
    ==
    ;div.add-panel(style "display: none;")
    ;form(method "post", action "/spv-wallet/wallet/{(hexn:sailbox pubkey)}")
      ;div.fc.g3
        ;div.fc.g2
          ;input(type "hidden", name "action", value "discover-accounts");
          ;div
            ;label.s-1.bold.f3: Purpose
            ;select.scanner-purpose-select.p2.b1.br1.wf.hover.pointer(name "purpose", required "true", style "outline: none;")
              ;option(value "84", selected "selected"): Native SegWit (BIP84) - 84
              ;option(value "49"): Wrapped SegWit (BIP49) - 49
              ;option(value "44"): Legacy (BIP44) - 44
              ;option(value "86"): Taproot (BIP86) - 86
              ;option(value "custom"): Custom...
            ==
            ;div.scanner-custom-purpose-container.fc.g1(style "display: none; margin-top: 8px;")
              ;input.scanner-custom-purpose-input.p2.b1.br1.wf(type "number", name "purpose-custom", placeholder "Enter purpose number", min "0", max "2147483647");
              ;div.f3.s-2(style "color: var(--f-2);"): ⚠️ Non-standard purposes may not work with other wallets
            ==
          ==
          ;div
            ;label.s-1.bold.f3: Coin Type
            ;select.scanner-coin-type-select.p2.b1.br1.wf.hover.pointer(name "coin-type", required "true", style "outline: none;")
              ;option(value "0", selected "selected"): Bitcoin Mainnet - 0
              ;option(value "1"): Bitcoin Testnet - 1
              ;option(value "2"): Litecoin - 2
              ;option(value "custom"): Custom...
            ==
            ;div.scanner-custom-coin-type-container.fc.g1(style "display: none; margin-top: 8px;")
              ;input.scanner-custom-coin-type-input.p2.b1.br1.wf(type "number", name "coin-type-custom", placeholder "Enter coin type (SLIP-44)", min "0", max "2147483647");
              ;div.f3.s-2(style "color: var(--f-2);"): ⚠️ See SLIP-44 registry for valid coin types
            ==
          ==
          ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
            ; Scan for Accounts
          ==
        ==
        ::  Placeholder for discovered accounts (hidden until scan)
        ;div.fc.g2(id "discovered-accounts", style "display: none; margin-top: 16px; padding: 16px; background: var(--b1); border-radius: 6px;")
          ;div.s-1.bold.mb2: Discovered Accounts
          ;div.fc.g2
            ;div.p3.b2.br2
              ;div.fc.g2
                ;div.f2: ✓ Account 0 • m/84'/0'/0'
                ;div.f3.s-2: 5 addresses with transactions • 1.234 BTC balance
                ;div.fr.g2(style "align-items: center;")
                  ;input.p2.b1.br1(type "text", placeholder "Account name", style "flex: 1;");
                  ;button.p2.b-3.f-3.br1.hover.pointer(style "outline: none;"): Add
                ==
              ==
            ==
            ;div.p3.b2.br2
              ;div.fc.g2
                ;div.f2: ✓ Account 1 • m/84'/0'/1'
                ;div.f3.s-2: 2 addresses with transactions • 0.5 BTC balance
                ;div.fr.g2(style "align-items: center;")
                  ;input.p2.b1.br1(type "text", placeholder "Account name", style "flex: 1;");
                  ;button.p2.b-3.f-3.br1.hover.pointer(style "outline: none;"): Add
                ==
              ==
            ==
          ==
        ==
      ==
    ==
    ;script
      ; (function() {
      ;   var scripts = document.getElementsByTagName('script');
      ;   var thisScript = scripts[scripts.length - 1];
      ;   var container = thisScript.closest('.account-scanner-form');
      ;   if (!container) return;
      ;
      ;   var purposeSelect = container.querySelector('.scanner-purpose-select');
      ;   purposeSelect.onchange = function() {
      ;     var customContainer = this.parentElement.querySelector('.scanner-custom-purpose-container');
      ;     var customInput = customContainer.querySelector('.scanner-custom-purpose-input');
      ;     if (this.value === 'custom') {
      ;       customContainer.style.display = 'flex';
      ;       customInput.required = true;
      ;     } else {
      ;       customContainer.style.display = 'none';
      ;       customInput.required = false;
      ;     }
      ;   };
      ;
      ;   var coinTypeSelect = container.querySelector('.scanner-coin-type-select');
      ;   coinTypeSelect.onchange = function() {
      ;     var customContainer = this.parentElement.querySelector('.scanner-custom-coin-type-container');
      ;     var customInput = customContainer.querySelector('.scanner-custom-coin-type-input');
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
::
++  discovery-list-content
  |=  [pubkey=@ux scan-map=(map coin-type:hd-path [pid=@ta act=? idx=@ud scn=account-scan])]
  ^-  manx
  =/  scan-list=(list [coin-type:hd-path [pid=@ta act=? idx=@ud scn=account-scan]])
    ~(tap by scan-map)
  ?:  =(~ scan-list)
    ;div.p4.b1.br2.tc(style "background: var(--b1);")
      ;div.s0.f2: No running discoveries
    ==
  ;div.fc.g2
    ;*  %+  turn  scan-list
        |=  [scan-key=coin-type:hd-path pid=@ta act=? account-idx=@ud scn=account-scan]
        ^-  manx
        =/  phase-text=tape
          ?-  -.scn
            %1  "Receiving ({(scow %ud idx.scn)}, gap {(scow %ud gap.scn)}/20)"
            %2  "Change ({(scow %ud idx.scn)}, gap {(scow %ud gap.scn)}/20)"
          ==
        =/  [purpose=seg:hd-path coin=seg:hd-path]  scan-key
        =/  display-path=tape
          %+  weld  "m/"
          %+  weld  (scow %ud q.purpose)
          %+  weld  "'/"
          %+  weld  (scow %ud q.coin)
          "'"
        =/  scan-key-str=tape
          %+  weld  (scow %ud q.purpose)
          %+  weld  "-"
          (scow %ud q.coin)
        =/  border-color=tape  ?:(act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
        =/  bg-color=tape  ?:(act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
        =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
        ;div.p3.b2.br2(style container-style)
          ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
            ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
              ;+  ?:  act
                    ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                      ;+  (make:fi 'loader')
                    ==
                  ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'pause-circle')
                  ==
            ==
            ;div(style "display: flex; flex-direction: column; gap: 4px;")
              ;div.f2.bold: {display-path} • Account {(scow %ud account-idx)}
              ;div.f3.s-1: {phase-text}
            ==
          ==
          ;div(style "display: flex; gap: 4px;")
            ;+  ?:  act
                  ::  Active - show pause button
                  ;button.p2.b1.br1.hover.pointer
                    =title  "Pause discovery"
                    =onclick  "pauseDiscovery('{(hexn:sailbox pubkey)}', \"{scan-key-str}\")"
                    =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                    ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'pause')
                    ==
                  ==
                ::  Paused - show play button
                ;button.p2.b1.br1.hover.pointer
                  =title  "Resume discovery"
                  =onclick  "resumeDiscovery('{(hexn:sailbox pubkey)}', \"{scan-key-str}\")"
                  =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                  ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'play')
                  ==
                ==
            ;button.p2.b1.br1.hover.pointer
              =title  "Cancel discovery"
              =onclick  "cancelDiscovery('{(hexn:sailbox pubkey)}', \"{scan-key-str}\")"
              =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'x-circle')
              ==
            ==
          ==
        ==
  ==
::
++  discovery-status-oob
  |=  [pubkey=@ux scan-map=(map coin-type:hd-path [pid=@ta act=? idx=@ud scn=account-scan])]
  ^-  manx
  =/  base=manx  (discovery-list-content pubkey scan-map)
  base(a.g [[%id "discovery-list"] [%hx-swap-oob "true"] a.g.base])
--
