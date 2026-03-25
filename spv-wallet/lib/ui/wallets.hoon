/-  *spv-wallet
/+  sailbox, fi=feather-icons, *ui-utils, *ui-layout
|%
++  full-wallet-list
  |=  wallets=(map @ux wallet)
  ^-  manx
  =/  wallet-list=(list [@ux wallet])
    %+  sort  ~(tap by wallets)
    |=  [a=[pubkey=@ux wallet] b=[pubkey=@ux wallet]]
    (aor name.a name.b)
  ?~  wallet-list
    ;div.p4.b1.br2.tc(style "background: var(--b1);")
      ;div.s0.f2.mb2: No wallets yet
      ;div.f3.s-1: Generate a new wallet or restore from a seed phrase below
    ==
  ;div.fc.g2
    ;*  %+  turn  wallet-list
        |=  [pubkey=@ux =wallet]
        ^-  manx
        ;div.p3.b1.br2.hover(style "display: flex; justify-content: space-between; align-items: center; gap: 12px; outline: none;")
          ;a(href "/spv-wallet/wallet/{(hexn:sailbox pubkey)}", style "flex: 1; min-width: 0; text-decoration: none; color: inherit; outline: none !important;")
            ;div.s0.bold.mb-1: {(trip name.wallet)}
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;button.copy-seed-btn.p1.b0.br1.hover.pointer
                =data-seed  (trip (seed-to-cord seed.wallet))
                =onclick  "event.preventDefault(); event.stopPropagation(); copyToClipboard(this.dataset.seed);"
                =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                =title  "Copy seed phrase"
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'copy')
                ==
              ==
              ;div.f3.s-2.mono.f2(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1;"): {(trip (mask-seed-phrase seed.wallet))}
            ==
          ==
          ;button.delete-wallet-btn.p2.b1.br1.hover.pointer
            =data-wallet-name  (trip name.wallet)
            =data-pubkey  (hexn:sailbox pubkey)
            =onclick  "showModal(this.dataset.walletName, this.dataset.pubkey)"
            =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'trash-2')
            ==
          ==
        ==
  ==
::
++  generate-wallet-content
  ^-  manx
  ;div
    ;div(id "generate-clear-listener", sse-swap "generate-clear", style "display: none;");
    ;form(id "generate-form", method "post")
      ;div.fc.g1
        ;input(type "hidden", name "action", value "add-wallet-from-entropy");
        ;div
          ;label.s-1.bold: Wallet Name
          ;input.p2.b1.br1.wf(type "text", name "wallet-name", placeholder "My Bitcoin Wallet", required "true");
        ==
        ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
          ; Generate Wallet
        ==
      ==
    ==
  ==
::
++  restore-wallet-content
  ^-  manx
  ;div
    ;form(id "restore-form", method "post")
      ;div.fc.g1
        ;input(type "hidden", name "action", value "add-wallet");
        ;div
          ;label.s-1.bold: Wallet Name
          ;input.p2.b1.br1.wf(type "text", name "wallet-name", placeholder "My Restored Wallet", required "true");
        ==
        ;div
          ;label.s-1.bold: Seed Format
          ;div(style "display: flex; gap: 16px; margin-top: 4px;")
            ;label(style "display: flex; align-items: center; gap: 4px; cursor: pointer;")
              ;input(type "radio", name "seed-format", value "bip39", checked "true", onchange "updateSeedInput(this.value)");
              ; BIP39 Mnemonic
            ==
            ;label(style "display: flex; align-items: center; gap: 4px; cursor: pointer;")
              ;input(type "radio", name "seed-format", value "q", onchange "updateSeedInput(this.value)");
              ; Urbit @q
            ==
          ==
        ==
        ;div
          ;label.s-1.bold(id "seed-label"): Seed Phrase
          ;textarea.p2.b1.br1.wf(id "seed-input", name "seed-phrase", placeholder "abandon abandon abandon...", rows "3", required "true", oninput "this.value = this.value.replace(/[^a-z ]/g, '')");
        ==
        ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
          ; Restore Wallet
        ==
      ==
    ==
    ;script
      ; function updateSeedInput(format) {
      ;   var input = document.getElementById('seed-input');
      ;   var label = document.getElementById('seed-label');
      ;   if (format === 'q') {
      ;     label.textContent = 'Urbit @q';
      ;     input.placeholder = '~sampel-palnet or ~sampel-palnet-sampel-palnet...';
      ;     input.oninput = function() { this.value = this.value.replace(/[^a-z~.-]/g, ''); };
      ;   } else {
      ;     label.textContent = 'Seed Phrase';
      ;     input.placeholder = 'abandon abandon abandon...';
      ;     input.oninput = function() { this.value = this.value.replace(/[^a-z ]/g, ''); };
      ;   }
      ;   input.value = '';
      ; }
    ==
  ==
::
++  full-wallet-list-container
  |=  wallets=(map @ux wallet)
  ^-  manx
  ;div#wallet-list-container.p4.b0.br2(style "flex: 1; min-height: 0; overflow-y: auto;")
    ;+  (full-wallet-list wallets)
  ==
::
++  full-wallet-list-oob
  |=  wallets=(map @ux wallet)
  ^-  manx
  =/  base=manx  (full-wallet-list-container wallets)
  base(a.g [[%hx-swap-oob "true"] a.g.base])
::
++  wallets-content
  |=  wallets=(map @ux wallet)
  ^-  manx
  ;div.fc.g2(style "flex: 1; min-height: 0;")
    ;+  (full-wallet-list-container wallets)
    ;div.p4.b2.br2(style "flex-shrink: 0;")
      ;div.s0.bold.tc.hover.pointer(onclick "toggleAddPanel(this)", style "display: flex; align-items: center; justify-content: center; gap: 8px; padding-bottom: 4px;")
        ; Add New Wallet
        ;div.add-chevron(style "width: 16px; height: 16px; display: flex; align-items: center; transition: transform 0.2s;")
          ;+  (make:fi 'chevron-down')
        ==
      ==
      ;div.add-panel(style "display: none;")
        ;+  %+  tabs  "generate"
            :~  ["generate" "Generate" generate-wallet-content]
                ["restore" "Restore" restore-wallet-content]
            ==
      ==
    ==
    ;script
      ; function toggleAddPanel(el) {
      ;   var panel = $(el).siblings('.add-panel');
      ;   var chevron = $(el).find('.add-chevron');
      ;   if (panel.is(':visible')) {
      ;     panel.slideUp(150);
      ;     chevron.css('transform', '');
      ;   } else {
      ;     $('.add-panel:visible').slideUp(150);
      ;     $('.add-chevron').css('transform', '');
      ;     panel.slideDown(150);
      ;     chevron.css('transform', 'rotate(180deg)');
      ;   }
      ; }
    ==
  ==
::
++  watch-only-list
  |=  $:  watch-only-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  =/  accounts=(list [@ux account-details])
    %+  sort
      %+  murn  ~(tap in watch-only-set)
      |=  pubkey=@ux
      =/  details=(unit account-details)  (~(get by global-accounts) pubkey)
      ?~  details  ~
      `[pubkey u.details]
    |=  [a=[pubkey=@ux details=account-details] b=[pubkey=@ux details=account-details]]
    (aor name.details.a name.details.b)
  ?~  accounts
    ;div.p4.b1.br2.tc(style "background: var(--b1);")
      ;div.s0.f2.mb2: No watch-only accounts yet
      ;div.f3.s-1: Import xpubs or addresses to track balances
    ==
  ;div.fc.g2
    ;*  %+  turn  accounts
        |=  [pubkey=@ux details=account-details]
        ^-  manx
        =/  xpub=@t  k.extended-key.details
        ;div.p3.b1.br2.hover(style "display: flex; justify-content: space-between; align-items: center; gap: 12px; outline: none;")
          ;a(href "/spv-wallet/account/{(hexn:sailbox pubkey)}", style "flex: 1; min-width: 0; text-decoration: none; color: inherit; outline: none !important;")
            ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;")
              ;+  (script-type-badge script-type.details)
              ;span.s0.bold: {(trip name.details)}
            ==
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;+  (network-badge active-network.details)
              ;button.copy-xpub-btn.p1.b0.br1.hover.pointer
                =data-xpub  (trip xpub)
                =onclick  "event.preventDefault(); event.stopPropagation(); copyToClipboard(this.dataset.xpub);"
                =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                =title  "Copy xpub"
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'copy')
                ==
              ==
              ;div.f3.s-2.mono.f2(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1;"): {(trip xpub)}
            ==
          ==
          ;button.delete-watch-only-btn.p2.b1.br1.hover.pointer
            =onclick  "event.preventDefault(); event.stopPropagation(); showWatchOnlyModal('{(trip name.details)}', '{(hexn:sailbox pubkey)}');"
            =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'trash-2')
            ==
          ==
        ==
  ==
::
++  watch-only-list-container
  |=  $:  watch-only-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  ;div#watch-only-list-container.p4.b0.br2(style "flex: 1; min-height: 0; overflow-y: auto;")
    ;+  (watch-only-list watch-only-set global-accounts)
  ==
::
++  watch-only-list-oob
  |=  $:  watch-only-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  =/  base=manx  (watch-only-list-container watch-only-set global-accounts)
  base(a.g [[%hx-swap-oob "true"] a.g.base])
::
++  watch-only-content
  |=  $:  watch-only-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  ;div.fc.g2(style "flex: 1; min-height: 0;")
    ;+  (watch-only-list-container watch-only-set global-accounts)
    ;div.p4.b2.br2(style "flex-shrink: 0;")
      ;div.s0.bold.tc.hover.pointer(onclick "toggleAddPanel(this)", style "display: flex; align-items: center; justify-content: center; gap: 8px;")
        ; Add Watch-Only Account
        ;div.add-chevron(style "width: 16px; height: 16px; display: flex; align-items: center; transition: transform 0.2s;")
          ;+  (make:fi 'chevron-down')
        ==
      ==
      ;div.add-panel(style "display: none;")
        ;form(method "post", action "/spv-wallet")
          ;div.fc.g1
            ;input(type "hidden", name "action", value "add-watch-only");
            ;div
              ;label.s-1.bold: Account Name
              ;input.p2.b1.br1.wf(type "text", name "account-name", placeholder "Hardware Wallet", required "true");
            ==
            ;div
              ;label.s-1.bold: Extended Public Key (xpub/tpub)
              ;textarea.p2.b1.br1.wf(name "xpub", placeholder "xpub...", rows "1", required "true", style "font-family: monospace;");
            ==
            ;div
              ;label.s-1.bold: Script Type
              ;select.p2.b1.br1.wf.hover.pointer(name "script-type", required "true", style "outline: none;")
                ;option(value "p2wpkh", selected "selected"): Native SegWit (P2WPKH)
                ;option(value "p2sh-p2wpkh"): Wrapped SegWit (P2SH-P2WPKH)
                ;option(value "p2pkh"): Legacy (P2PKH)
                ;option(value "p2tr"): Taproot (P2TR)
              ==
            ==
            ;div
              ;label.s-1.bold: Network
              ;select.p2.b1.br1.wf.hover.pointer(name "network", required "true", style "outline: none;")
                ;option(value "main", selected "selected"): Bitcoin Mainnet
                ;option(value "testnet"): Bitcoin Testnet
              ==
            ==
            ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;"): Add Account
          ==
        ==
      ==
    ==
  ==
::
++  signing-list
  |=  $:  signing-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  =/  accounts=(list [@ux account-details])
    %+  sort
      %+  murn  ~(tap in signing-set)
      |=  pubkey=@ux
      =/  details=(unit account-details)  (~(get by global-accounts) pubkey)
      ?~  details  ~
      `[pubkey u.details]
    |=  [a=[pubkey=@ux details=account-details] b=[pubkey=@ux details=account-details]]
    (aor name.details.a name.details.b)
  ?~  accounts
    ;div.p4.b1.br2.tc(style "background: var(--b1);")
      ;div.s0.f2.mb2: No signing accounts yet
      ;div.f3.s-1: Import private keys or connect hardware wallets
    ==
  ;div.fc.g2
    ;*  %+  turn  accounts
        |=  [pubkey=@ux details=account-details]
        ^-  manx
        =/  xprv=@t  k.extended-key.details
        ;div.p3.b1.br2.hover(style "display: flex; justify-content: space-between; align-items: center; gap: 12px; outline: none;")
          ;a(href "/spv-wallet/account/{(hexn:sailbox pubkey)}", style "flex: 1; min-width: 0; text-decoration: none; color: inherit; outline: none !important;")
            ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;")
              ;+  (script-type-badge script-type.details)
              ;span.s0.bold: {(trip name.details)}
            ==
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;+  (network-badge active-network.details)
              ;button.copy-xprv-btn.p1.b0.br1.hover.pointer
                =data-xprv  (trip xprv)
                =onclick  "event.preventDefault(); event.stopPropagation(); copyToClipboard(this.dataset.xprv);"
                =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
                =title  "Copy xprv"
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'copy')
                ==
              ==
              ;div.f3.s-2.mono.f2(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex: 1;"): {(trip xprv)}
            ==
          ==
          ;button.delete-signing-btn.p2.b1.br1.hover.pointer
            =onclick  "event.preventDefault(); event.stopPropagation(); showSigningModal('{(trip name.details)}', '{(hexn:sailbox pubkey)}');"
            =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'trash-2')
            ==
          ==
        ==
  ==
::
++  signing-list-container
  |=  $:  signing-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  ;div#signing-list-container.p4.b0.br2(style "flex: 1; min-height: 0; overflow-y: auto;")
    ;+  (signing-list signing-set global-accounts)
  ==
::
++  signing-list-oob
  |=  $:  signing-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  =/  base=manx  (signing-list-container signing-set global-accounts)
  base(a.g [[%hx-swap-oob "true"] a.g.base])
::
++  signing-accounts-content
  |=  $:  signing-set=(set @ux)
          global-accounts=(map @ux account-details)
      ==
  ^-  manx
  ;div.fc.g2(style "flex: 1; min-height: 0;")
    ;+  (signing-list-container signing-set global-accounts)
    ;div.p4.b2.br2(style "flex-shrink: 0;")
      ;div.s0.bold.tc.hover.pointer(onclick "toggleAddPanel(this)", style "display: flex; align-items: center; justify-content: center; gap: 8px;")
        ; Add Signing Account
        ;div.add-chevron(style "width: 16px; height: 16px; display: flex; align-items: center; transition: transform 0.2s;")
          ;+  (make:fi 'chevron-down')
        ==
      ==
      ;div.add-panel(style "display: none;")
        ;form(method "post", action "/spv-wallet")
          ;div.fc.g1
            ;input(type "hidden", name "action", value "add-signing");
            ;div
              ;label.s-1.bold: Account Name
              ;input.p2.b1.br1.wf(type "text", name "account-name", placeholder "Hot Wallet", required "true");
            ==
            ;div
              ;label.s-1.bold: Extended Private Key (xprv/tprv)
              ;textarea.p2.b1.br1.wf(name "xprv", placeholder "xprv...", rows "1", required "true", style "font-family: monospace;");
            ==
            ;div
              ;label.s-1.bold: Script Type
              ;select.p2.b1.br1.wf.hover.pointer(name "script-type", required "true", style "outline: none;")
                ;option(value "p2wpkh", selected "selected"): Native SegWit (P2WPKH)
                ;option(value "p2sh-p2wpkh"): Wrapped SegWit (P2SH-P2WPKH)
                ;option(value "p2pkh"): Legacy (P2PKH)
                ;option(value "p2tr"): Taproot (P2TR)
              ==
            ==
            ;div
              ;label.s-1.bold: Network
              ;select.p2.b1.br1.wf.hover.pointer(name "network", required "true", style "outline: none;")
                ;option(value "main", selected "selected"): Bitcoin Mainnet
                ;option(value "testnet"): Bitcoin Testnet
              ==
            ==
            ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;"): Add Account
          ==
        ==
      ==
    ==
  ==
--
