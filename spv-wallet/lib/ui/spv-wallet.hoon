/-  *spv-wallet, indexer
/+  fi=feather-icons,
    sailbox, txns=tx-build,
    *ui-utils, *ui-primitives, *ui-layout, *ui-accounts,
    *ui-addresses, *ui-accounts, *ui-discovery, *ui-experiments, *ui-spv, *ui-progress,
    *ui-send, *wallet-address, *wallet-utxo, wallet-account
|%
++  wallet-detail-page
  |=  [pubkey=@ux wallets=(map @ux wallet) accounts=(map @ux account-details)]
  ^-  manx
  =/  wallet-data=(unit wallet)  (~(get by wallets) pubkey)
  ?~  wallet-data
    %-  htmx-page
    :^  "Wallet Not Found"  &  ~
    ;div.fc.g3.p5.ma.mw-page
      ;h1: Wallet Not Found
      ;a(href "/spv-wallet"): ← Back to Wallets
    ==
  %-  htmx-page
  :^  "{(trip name.u.wallet-data)}"  |  ~
  ;div.fc.g3.p5.ma.mw-page(style "height: 100%;")
    ;div(style "flex-shrink: 0; display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href "/spv-wallet", style "color: var(--f3); text-decoration: none;"): ← Back to Wallets
    ==
    ;div.p4.b1.br2(style "flex-shrink: 0;")
      ;h1.s2.bold.mb2: {(trip name.u.wallet-data)}
      ;div.mb2(style "display: flex; gap: 8px; align-items: center;")
        ;span.f3.s-1: Seed:
        ;code.mono.s-2.p2.b2.br1: {(trip (mask-seed-phrase seed.u.wallet-data))}
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{(trip (seed-to-cord seed.u.wallet-data))}')"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; justify-content: center; outline: none;"
          ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
      ==
    ==
    ;div.fc.g3(style "flex: 1; min-height: 0;")
      ::  Discover Accounts section
      ;+  (account-scanner-ui pubkey)
      ::  Single SSE connection for wallet updates
      ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/wallet/{(hexn:sailbox pubkey)}", sse-swap "account-list-update,discovery-status-update,account-row-update", style "display:none;");
      ::  Tabs for Accounts and Running Discoveries
      ;+  %+  tabs  "accounts"
          :~  :+  "accounts"  "Accounts"
              ;div#account-list(style "flex: 1; min-height: 0; overflow-y: auto;")
                ;+  (accounts-list pubkey accounts.u.wallet-data accounts)
              ==
              :+  "discoveries"  "Running Discoveries"
              ;div#discovery-list.fc.g2
                ;+  (discovery-list-content pubkey scan.u.wallet-data)
              ==
          ==
      ::
      ;+  add-unlisted-account-ui
    ==
    ::
    ;div(id "delete-account-modal", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div(style "background: var(--b0); padding: 24px; border-radius: 12px; max-width: 400px; color: var(--f0);")
        ;h3(style "margin-bottom: 16px;"): Delete Account
        ;p(id "delete-account-text", style "margin-bottom: 16px;"): Delete this account?
        ;div(style "margin-bottom: 16px;")
          ;label(style "display: block; margin-bottom: 8px; font-weight: bold;"): Type account name to confirm:
          ;input(id "confirm-account-name", type "text", placeholder "Account name", style "width: 100%; padding: 8px; border: 1px solid var(--b3); border-radius: 4px; background: var(--b1); color: var(--f0);", oninput "validateAccountName()");
          ;div(id "account-name-error", style "color: var(--f-1); font-size: 14px; margin-top: 4px; display: none;"): Account name does not match
        ==
        ;div(style "display: flex; gap: 12px; justify-content: flex-end;")
          ;button(onclick "hideAccountModal()", style "padding: 8px 16px; background: var(--b2); color: var(--f2); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Cancel
          ;button(id "confirm-account-delete-btn", onclick "confirmAccountDelete()", style "padding: 8px 16px; background: var(--f-1); color: var(--b0); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Delete
        ==
      ==
    ==
    ;script
      ; var currentAccount = {name: '', path: ''};
      ; function showAccountModal(name, path) {
      ;   currentAccount = {name: name, path: path};
      ;   document.getElementById('delete-account-text').innerHTML = 'Delete <span style="font-weight: bold; color: var(--f-1);">' + name + '</span>?';
      ;   document.getElementById('confirm-account-name').value = '';
      ;   document.getElementById('account-name-error').style.display = 'none';
      ;   document.getElementById('confirm-account-delete-btn').disabled = true;
      ;   document.getElementById('confirm-account-delete-btn').style.opacity = '0.5';
      ;   document.getElementById('delete-account-modal').style.display = 'flex';
      ; }
      ; function hideAccountModal() {
      ;   document.getElementById('delete-account-modal').style.display = 'none';
      ; }
      ; function validateAccountName() {
      ;   var input = document.getElementById('confirm-account-name').value;
      ;   var deleteBtn = document.getElementById('confirm-account-delete-btn');
      ;   var errorDiv = document.getElementById('account-name-error');
      ;   if (input === currentAccount.name) {
      ;     deleteBtn.disabled = false;
      ;     deleteBtn.style.opacity = '1';
      ;     errorDiv.style.display = 'none';
      ;   } else {
      ;     deleteBtn.disabled = true;
      ;     deleteBtn.style.opacity = '0.5';
      ;     if (input.length > 0) {
      ;       errorDiv.style.display = 'block';
      ;     } else {
      ;       errorDiv.style.display = 'none';
      ;     }
      ;   }
      ; }
      ; function confirmAccountDelete() {
      ;   if (document.getElementById('confirm-account-name').value !== currentAccount.name) {
      ;     return;
      ;   }
      ;   fetch(window.location.pathname, {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=delete-account&account-path=' + encodeURIComponent(currentAccount.path)
      ;   });
      ;   hideAccountModal();
      ; }
      ;
      ; function pauseDiscovery(pubkey, scanKey) {
      ;   fetch('/spv-wallet/wallet/' + pubkey, {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=pause-discovery&scan-key=' + encodeURIComponent(scanKey)
      ;   });
      ; }
      ;
      ; function resumeDiscovery(pubkey, scanKey) {
      ;   fetch('/spv-wallet/wallet/' + pubkey, {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=resume-discovery&scan-key=' + encodeURIComponent(scanKey)
      ;   });
      ; }
      ;
      ; function cancelDiscovery(pubkey, scanKey) {
      ;   fetch('/spv-wallet/wallet/' + pubkey, {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-discovery&scan-key=' + encodeURIComponent(scanKey)
      ;   });
      ; }
    ==
  ==
::
++  wallet-page
  |=  $:  wallets=(map @ux wallet)
          watch-only-set=(set @ux)
          signing-set=(set @ux)
          global-accounts=(map @ux account-details)
          spawn-status=?(%spawned %pending %unspawned)
          boot=(unit boot-state)
          args=(list [key=@t value=@t])
      ==
  ^-  manx
  ::  Helper to get query param
  =/  get-key
    |=  [key=@t args=(list [key=@t value=@t])]
    ^-  (unit @t)
    =/  result=(list [key=@t value=@t])  (skim args |=([k=@t v=@t] =(k key)))
    ?~  result  ~
    `value.i.result
  =/  default-tab=@t
    =/  tab-arg=(unit @t)  (get-key 'tab' args)
    ?~  tab-arg  'wallets'
    ?:  =('wallets' u.tab-arg)  'wallets'
    ?:  =('watch' u.tab-arg)  'watch'
    ?:  =('signing' u.tab-arg)  'signing'
    'wallets'
  %-  htmx-page
  :^  "Bitcoin Wallet"  |  ~
  ;div.fc(style "height: 100%; min-width: 480px;")
    ::  Fixed header
    ;div.p5.ma.mw-page(style "flex-shrink: 0; padding-bottom: 0;")
      ;div.mb2(style "position: relative; padding: 0 52px;")
        ;div.tc
          ;h1.s3.bold: ₿ Bitcoin Wallet
          ;p.f2.s-1: Manage your Bitcoin wallets and accounts
        ==
        ;div(style "position: absolute; top: 0; right: 0;")
          ;button#settings-btn.hover.pointer
            =onclick  "toggleSettingsDropdown(event)"
            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; justify-content: center; width: 40px; height: 40px; border-radius: 6px; cursor: pointer; outline: none;"
            ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'settings')
            ==
          ==
          ;div#settings-dropdown
            =style  "display: none; position: absolute; top: 45px; right: 0; background: var(--b0); border: 1px solid var(--b2); border-radius: 6px; min-width: 180px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 100; overflow: hidden;"
            ;div.p3.hover
              =onclick  "window.location.href = '/spv-wallet/spv'"
              =style  "display: block; color: var(--f0); padding: 12px 16px; border-bottom: 1px solid var(--b1); cursor: pointer;"
              Block Headers
            ==
            ;div.p3(style "display: block; color: var(--f2); padding: 12px 16px; border-bottom: 1px solid var(--b1); cursor: not-allowed; opacity: 0.5;"): Settings
            ;div.p3(style "display: block; color: var(--f2); padding: 12px 16px; cursor: not-allowed; opacity: 0.5;"): Export
          ==
        ==
      ==
    ==
    ::  Scrollable content area
    ;div.fc.g3.p5.ma.mw-page(style "flex: 1; min-height: 0; overflow-y: auto; padding-top: 0;")
      ::  Spawn banner - shows based on spawn status
      ;+  ?-  spawn-status
          ::  Confirmed on chain: hide banner
            %spawned
          ;span;
          ::  In mempool, waiting for block confirmation
            %pending
          ;div.p4.b1.br2(style "flex-shrink: 0; border: 1px solid rgba(100, 200, 100, 0.4); background: rgba(100, 200, 100, 0.08); position: relative;")
            ;div(style "display: flex; align-items: center; gap: 12px;")
              ;div(style "flex-shrink: 0;")
                ;+  (make:fi 'loader')
              ==
              ;div
                ;h2.s1.bold(style "margin-bottom: 4px;"): Spawn transaction broadcast
                ;p.f2.s-1(style "opacity: 0.7;"): Your attestation is in the mempool. Waiting for block confirmation.
              ==
            ==
          ==
          ::  Not spawned: show full boot form
            %unspawned
          ;div.p4.b1.br2(style "flex-shrink: 0; border: 1px solid rgba(255, 180, 50, 0.4); background: rgba(255, 180, 50, 0.08); position: relative;")
            ;button.hover.pointer(onclick "this.closest('.p4').style.display='none'", style "position: absolute; top: 8px; right: 8px; background: transparent; border: none; color: var(--f3); cursor: pointer; font-size: 18px; line-height: 1; padding: 4px; outline: none;"): ✕
            ;h2.s1.bold(style "margin-bottom: 8px;"): Your comet has not been spawned
            ;p.f2.s-1(style "margin-bottom: 12px; opacity: 0.7;"): Enter your boot secret from your comet's boot sequence.
            ;form(hx-post "/spv-wallet/progress", hx-swap "none")
              ;input(type "hidden", name "action", value "start");
              ;div(style "display: flex; gap: 8px;")
                ;input.p3.br2(type "text", name "seed-phrase", placeholder "~sampel-palnet-sampel-palnet", style "flex: 1; background: var(--b0); border: 1px solid var(--b3); color: var(--f0); outline: none; font-family: monospace;");
                ;+  =/  booting=?  ?&(?=(^ boot) !?=(%done step.u.boot) ?=(~ error.u.boot))
                    ?:  booting
                      ;button.p3.br2.bold(id "spawn-btn", type "submit", disabled "disabled", style "background: rgba(255, 180, 50, 0.4); color: white; border: none; white-space: nowrap; outline: none; cursor: not-allowed;"): Spawning...
                    ;button.p3.br2.bold.hover.pointer(id "spawn-btn", type "submit", style "background: rgba(255, 180, 50, 0.8); color: white; border: none; white-space: nowrap; outline: none;"): Spawn Comet
              ==
              ;div(style "margin-top: 8px; display: flex; align-items: center; gap: 12px;")
                ;label.s-1.f2(style "display: flex; align-items: center; gap: 6px; cursor: pointer;")
                  ;input(type "radio", name "boot-mode", value "fief", onchange "document.getElementById('fief-fields').style.display='flex'; document.getElementById('sponsor-fields').style.display='none';");
                  ;span: Direct (Fief)
                ==
                ;label.s-1.f2(style "display: flex; align-items: center; gap: 6px; cursor: pointer;")
                  ;input(type "radio", name "boot-mode", value "normal", checked "", onchange "document.getElementById('sponsor-fields').style.display='flex'; document.getElementById('fief-fields').style.display='none';");
                  ;span: Sponsor (Relay)
                ==
              ==
              ;div(id "fief-fields", style "display: none; margin-top: 8px; gap: 8px;")
                ;div.s-2.f2(style "margin-bottom: 4px;"): Your public IP and port — use this if your machine is directly reachable from the internet
                ;div(style "display: flex; gap: 8px;")
                  ;input.p3.br2(type "text", name "fief-ip", placeholder "1.2.3.4", style "flex: 2; background: var(--b0); border: 1px solid var(--b3); color: var(--f0); outline: none; font-family: monospace;");
                  ;input.p3.br2(type "text", name "fief-port", placeholder "34343", style "flex: 1; background: var(--b0); border: 1px solid var(--b3); color: var(--f0); outline: none; font-family: monospace;");
                ==
              ==
              ;div(id "sponsor-fields", style "margin-top: 8px;")
                ;div.s-2.f2(style "margin-bottom: 4px;"): Relay traffic through a sponsor — recommended for home networks without port forwarding
                ;+  =/  ds=tape  (scow %p default-sponsor)
                    ;input.p3.br2(type "text", name "sponsor", value ds, style "width: 100%; background: var(--b0); border: 1px solid var(--b3); color: var(--f0); outline: none; font-family: monospace;");
              ==
            ==
            ;div(hx-ext "sse", sse-connect "/spv-wallet/progress", sse-swap "progress-update", style "margin-top: 12px;")
              ;+  =/  prog=(unit progress-info)
                    ?~(boot ~ `(boot-progress u.boot))
                  =/  err=(unit [term tang])
                    ?~(boot ~ error.u.boot)
                  (progress-bar prog err)
            ==
          ==
          ==
      ::  SSE connection for wallet list updates
      ;div(hx-ext "sse", sse-connect "/spv-wallet/stream", sse-swap "wallet-list-update,generate-clear", style "display:none;");
      ;+  %+  tabs  (trip default-tab)
          :~  ["wallets" "Full Wallets" (wallets-content wallets)]
              ["watch" "Watch-Only" (watch-only-content watch-only-set global-accounts)]
              ["signing" "Signing" (signing-accounts-content signing-set global-accounts)]
          ==
    ==
    :: 🧪 HYPOTHESIS: Simplest possible modal works without syntax errors
    ;div(id "delete-wallet-modal", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div(style "background: var(--b0); padding: 24px; border-radius: 12px; max-width: 400px; color: var(--f0);")
        ;h3(style "margin-bottom: 16px;"): Delete Wallet
        ;p(id "delete-confirm-text", style "margin-bottom: 16px;"): Are you sure you want to delete this wallet?
        ;div(style "margin-bottom: 16px;")
          ;label(style "display: block; margin-bottom: 8px; font-weight: bold;"): Type wallet name to confirm:
          ;input(id "confirm-wallet-name", type "text", placeholder "Wallet name", style "width: 100%; padding: 8px; border: 1px solid var(--b3); border-radius: 4px; background: var(--b1); color: var(--f0);", oninput "validateWalletName()");
          ;div(id "name-error", style "color: var(--f-1); font-size: 14px; margin-top: 4px; display: none;"): Wallet name does not match
        ==
        ;div(id "delete-buttons", style "display: flex; gap: 12px; justify-content: flex-end;")
          ;button(onclick "hideModal()", style "padding: 8px 16px; background: var(--b2); color: var(--f2); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Cancel
          ;button(id "confirm-delete-btn", onclick "confirmDelete()", style "padding: 8px 16px; background: var(--f-1); color: var(--b0); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Delete
        ==
      ==
    ==
    :: Watch-only delete modal
    ;div(id "delete-watch-only-modal", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div(style "background: var(--b0); padding: 24px; border-radius: 12px; max-width: 400px; color: var(--f0);")
        ;h3(style "margin-bottom: 16px;"): Delete Watch-Only Account
        ;p(id "delete-watch-only-text", style "margin-bottom: 16px;"): Are you sure?
        ;div(style "margin-bottom: 16px;")
          ;label(style "display: block; margin-bottom: 8px; font-weight: bold;"): Type account name to confirm:
          ;input(id "confirm-watch-only-name", type "text", placeholder "Account name", style "width: 100%; padding: 8px; border: 1px solid var(--b3); border-radius: 4px; background: var(--b1); color: var(--f0);", oninput "validateWatchOnlyName()");
          ;div(id "watch-only-name-error", style "color: var(--f-1); font-size: 14px; margin-top: 4px; display: none;"): Account name does not match
        ==
        ;div(style "display: flex; gap: 12px; justify-content: flex-end;")
          ;button(onclick "hideWatchOnlyModal()", style "padding: 8px 16px; background: var(--b2); color: var(--f2); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Cancel
          ;button(id "confirm-delete-watch-only-btn", onclick "confirmDeleteWatchOnly()", style "padding: 8px 16px; background: var(--f-1); color: var(--b0); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Delete
        ==
      ==
    ==
    :: Signing account delete modal
    ;div(id "delete-signing-modal", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div(style "background: var(--b0); padding: 24px; border-radius: 12px; max-width: 400px; color: var(--f0);")
        ;h3(style "margin-bottom: 16px;"): Delete Signing Account
        ;p(id "delete-signing-text", style "margin-bottom: 16px;"): Are you sure?
        ;div(style "margin-bottom: 16px;")
          ;label(style "display: block; margin-bottom: 8px; font-weight: bold;"): Type account name to confirm:
          ;input(id "confirm-signing-name", type "text", placeholder "Account name", style "width: 100%; padding: 8px; border: 1px solid var(--b3); border-radius: 4px; background: var(--b1); color: var(--f0);", oninput "validateSigningName()");
          ;div(id "signing-name-error", style "color: var(--f-1); font-size: 14px; margin-top: 4px; display: none;"): Account name does not match
        ==
        ;div(style "display: flex; gap: 12px; justify-content: flex-end;")
          ;button(onclick "hideSigningModal()", style "padding: 8px 16px; background: var(--b2); color: var(--f2); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Cancel
          ;button(id "confirm-delete-signing-btn", onclick "confirmDeleteSigning()", style "padding: 8px 16px; background: var(--f-1); color: var(--b0); border: none; border-radius: 6px; cursor: pointer; outline: none;"): Delete
        ==
      ==
    ==
    ;script
      ; var currentWallet = {};
      ; var currentWatchOnly = {};
      ; var currentSigning = {};
      ;
      ; function showModal(name, pubkey) {
      ;   currentWallet = {name: name, pubkey: pubkey};
      ;   document.getElementById('delete-confirm-text').innerHTML = 'Delete <span style="font-weight: bold; color: var(--f-1);">' + name + '</span>?';
      ;   document.getElementById('confirm-wallet-name').value = '';
      ;   document.getElementById('name-error').style.display = 'none';
      ;   document.getElementById('confirm-delete-btn').disabled = true;
      ;   document.getElementById('confirm-delete-btn').style.opacity = '0.5';
      ;   document.getElementById('delete-wallet-modal').style.display = 'flex';
      ; }
      ;
      ; function hideModal() {
      ;   document.getElementById('delete-wallet-modal').style.display = 'none';
      ; }
      ;
      ; function validateWalletName() {
      ;   var input = document.getElementById('confirm-wallet-name').value;
      ;   var deleteBtn = document.getElementById('confirm-delete-btn');
      ;   var errorDiv = document.getElementById('name-error');
      ;
      ;   if (input === currentWallet.name) {
      ;     deleteBtn.disabled = false;
      ;     deleteBtn.style.opacity = '1';
      ;     errorDiv.style.display = 'none';
      ;   } else {
      ;     deleteBtn.disabled = true;
      ;     deleteBtn.style.opacity = '0.5';
      ;     if (input.length > 0) {
      ;       errorDiv.style.display = 'block';
      ;     } else {
      ;       errorDiv.style.display = 'none';
      ;     }
      ;   }
      ; }
      ;
      ; function confirmDelete() {
      ;   if (document.getElementById('confirm-wallet-name').value !== currentWallet.name) {
      ;     return;
      ;   }
      ;   fetch('/spv-wallet', {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=remove-wallet&pubkey=' + encodeURIComponent(currentWallet.pubkey) + '&wallet-name=' + encodeURIComponent(currentWallet.name)
      ;   });
      ;   hideModal();
      ; }
      ;
      ; function showWatchOnlyModal(name, id) {
      ;   currentWatchOnly = {name: name, id: id};
      ;   document.getElementById('delete-watch-only-text').innerHTML = 'Delete <span style="font-weight: bold; color: var(--f-1);">' + name + '</span>?';
      ;   document.getElementById('confirm-watch-only-name').value = '';
      ;   document.getElementById('watch-only-name-error').style.display = 'none';
      ;   document.getElementById('confirm-delete-watch-only-btn').disabled = true;
      ;   document.getElementById('confirm-delete-watch-only-btn').style.opacity = '0.5';
      ;   document.getElementById('delete-watch-only-modal').style.display = 'flex';
      ; }
      ;
      ; function hideWatchOnlyModal() {
      ;   document.getElementById('delete-watch-only-modal').style.display = 'none';
      ; }
      ;
      ; function validateWatchOnlyName() {
      ;   var input = document.getElementById('confirm-watch-only-name').value;
      ;   var deleteBtn = document.getElementById('confirm-delete-watch-only-btn');
      ;   var errorDiv = document.getElementById('watch-only-name-error');
      ;   if (input === currentWatchOnly.name) {
      ;     deleteBtn.disabled = false;
      ;     deleteBtn.style.opacity = '1';
      ;     errorDiv.style.display = 'none';
      ;   } else {
      ;     deleteBtn.disabled = true;
      ;     deleteBtn.style.opacity = '0.5';
      ;     if (input.length > 0) {
      ;       errorDiv.style.display = 'block';
      ;     } else {
      ;       errorDiv.style.display = 'none';
      ;     }
      ;   }
      ; }
      ;
      ; function confirmDeleteWatchOnly() {
      ;   if (document.getElementById('confirm-watch-only-name').value !== currentWatchOnly.name) {
      ;     return;
      ;   }
      ;   fetch('/spv-wallet', {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=delete-watch-only&pubkey=' + encodeURIComponent(currentWatchOnly.id)
      ;   });
      ;   hideWatchOnlyModal();
      ; }
      ;
      ; function showSigningModal(name, id) {
      ;   currentSigning = {name: name, id: id};
      ;   document.getElementById('delete-signing-text').innerHTML = 'Delete <span style="font-weight: bold; color: var(--f-1);">' + name + '</span>?';
      ;   document.getElementById('confirm-signing-name').value = '';
      ;   document.getElementById('signing-name-error').style.display = 'none';
      ;   document.getElementById('confirm-delete-signing-btn').disabled = true;
      ;   document.getElementById('confirm-delete-signing-btn').style.opacity = '0.5';
      ;   document.getElementById('delete-signing-modal').style.display = 'flex';
      ; }
      ;
      ; function hideSigningModal() {
      ;   document.getElementById('delete-signing-modal').style.display = 'none';
      ; }
      ;
      ; function validateSigningName() {
      ;   var input = document.getElementById('confirm-signing-name').value;
      ;   var deleteBtn = document.getElementById('confirm-delete-signing-btn');
      ;   var errorDiv = document.getElementById('signing-name-error');
      ;   if (input === currentSigning.name) {
      ;     deleteBtn.disabled = false;
      ;     deleteBtn.style.opacity = '1';
      ;     errorDiv.style.display = 'none';
      ;   } else {
      ;     deleteBtn.disabled = true;
      ;     deleteBtn.style.opacity = '0.5';
      ;     if (input.length > 0) {
      ;       errorDiv.style.display = 'block';
      ;     } else {
      ;       errorDiv.style.display = 'none';
      ;     }
      ;   }
      ; }
      ;
      ; function confirmDeleteSigning() {
      ;   if (document.getElementById('confirm-signing-name').value !== currentSigning.name) {
      ;     return;
      ;   }
      ;   fetch('/spv-wallet', {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=delete-signing&pubkey=' + encodeURIComponent(currentSigning.id)
      ;   });
      ;   hideSigningModal();
      ; }
      ;
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Seed phrase copied to clipboard');
      ;   }).catch(function(err) {
      ;     console.error('Failed to copy: ', err);
      ;   });
      ; }
      ;
      ; function toggleSettingsDropdown(event) {
      ;   event.stopPropagation();
      ;   var dropdown = document.getElementById('settings-dropdown');
      ;   if (dropdown.style.display === 'none' || dropdown.style.display === '') {
      ;     dropdown.style.display = 'block';
      ;   } else {
      ;     dropdown.style.display = 'none';
      ;   }
      ; }
      ;
      ; document.addEventListener('click', function(event) {
      ;   var dropdown = document.getElementById('settings-dropdown');
      ;   var btn = document.getElementById('settings-btn');
      ;   if (dropdown && btn && !btn.contains(event.target) && !dropdown.contains(event.target)) {
      ;     dropdown.style.display = 'none';
      ;   }
      ; });
      ;
      ; function toggleCustomPurpose() {
      ;   var select = document.getElementById('purpose-select');
      ;   var container = document.getElementById('custom-purpose-container');
      ;   var customInput = document.getElementById('custom-purpose');
      ;   if (select.value === 'custom') {
      ;     container.style.display = 'flex';
      ;     customInput.required = true;
      ;   } else {
      ;     container.style.display = 'none';
      ;     customInput.required = false;
      ;   }
      ; }
      ;
      ; function toggleCustomCoinType() {
      ;   var select = document.getElementById('coin-type-select');
      ;   var container = document.getElementById('custom-coin-type-container');
      ;   var customInput = document.getElementById('custom-coin-type');
      ;   if (select.value === 'custom') {
      ;     container.style.display = 'flex';
      ;     customInput.required = true;
      ;   } else {
      ;     container.style.display = 'none';
      ;     customInput.required = false;
      ;   }
      ; }
      ;
      ; document.addEventListener('DOMContentLoaded', function() {
      ;   document.querySelectorAll('.delete-wallet-btn').forEach(function(btn) {
      ;     btn.onclick = function() {
      ;       showModal(btn.dataset.walletName, btn.dataset.pubkey);
      ;     };
      ;   });
      ;
      ;   var seedTextarea = document.querySelector('textarea[name="seed-phrase"]');
      ;   if (seedTextarea) {
      ;     seedTextarea.addEventListener('keydown', function(e) {
      ;       if (e.key === 'Enter' && !e.shiftKey) {
      ;         e.preventDefault();
      ;         this.form.submit();
      ;       }
      ;     });
      ;   }
      ;
      ;   var confirmInput = document.getElementById('confirm-wallet-name');
      ;   if (confirmInput) {
      ;     confirmInput.addEventListener('keydown', function(e) {
      ;       if (e.key === 'Enter') {
      ;         confirmDelete();
      ;       }
      ;     });
      ;   }
      ; });
    ==
  ==
::
++  receive-modal-content
  |=  next-address=(unit @t)
  ^-  manx
  ;div
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;h2.s1.bold: Receive Bitcoin
      ;button.p2.b1.br1.hover.pointer(onclick "hideModal('receive-modal')", style "background: transparent; border: 1px solid var(--b3); outline: none;")
        ;+  (make:fi 'x')
      ==
    ==
    ;+  ?~  next-address
          ;div.tc.p4
            ;p.f2: No address available. Please scan your account first.
          ==
        ;div
          ;div.tc(style "margin-bottom: 16px;")
            ;div#receive-qr(data-address "{(trip u.next-address)}", style "display: inline-block;");
          ==
          ;div.p3.b2.br2(style "display: flex; align-items: center; gap: 8px;")
            ;button.p1.b0.br1.hover.pointer
              =onclick  "copyToClipboard('{(trip u.next-address)}')"
              =title  "Copy address"
              =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'copy')
              ==
            ==
            ;div.mono.f3(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; min-width: 0;"): {(trip u.next-address)}
          ==
        ==
  ==
::
++  account-detail-page
  |=  [pubkey=@ux identifier=@t wallet-name=(unit @t) acct=(unit account:hd-path) details=account-details now=@da hide-empty=?]
  ^-  manx
  =/  ac  ~(. ac:wallet-account [details active-network.details])
  =/  total-balance=@ud  (compute-account-balance details)
  =/  next-unused=(unit @t)  (get-next-unused-address receiving:ac)
  ::  Determine account type
  =/  is-full=?  ?=(^ wallet-name)
  =/  is-signing=?
    ?&  !is-full
        ?=([%xprv *] extended-key.details)
    ==
  ::  Compute coin-type constraints for full wallets
  =/  is-mainnet-cointype=?
    ?.  is-full  %.n
    =/  [purpose=seg:hd-path ct=seg:hd-path account=seg:hd-path]  (need acct)
    =(0 q.ct)
  =/  is-testnet-cointype=?
    ?.  is-full  %.n
    !is-mainnet-cointype
  ::  Should we show the network edit button?
  =/  show-network-edit=?
    ?:  !is-full  %.y                          ::  Always show for watch-only/signing
    ?:  is-mainnet-cointype
      !=(active-network.details %main)         ::  Only show if NOT already on mainnet
    %.y                                        ::  Always show for testnet cointype
  ::  Does current network conflict with coin-type?
  =/  has-network-conflict=?
    ?:  !is-full  %.n
    ?:  is-mainnet-cointype
      !=(active-network.details %main)
    =(active-network.details %main)
  ::  Network name for display
  =/  network-name=tape
    ?-  active-network.details
      %main      "Mainnet"
      %testnet3  "Testnet3"
      %testnet4  "Testnet4"
      %signet    "Signet"
      %regtest   "Regtest"
    ==
  ::  Pre-encode identifier for use in URLs
  =/  encoded-identifier=tape  (en-urlt:html (trip identifier))
  ::  Pre-compute SSE URL based on account type - all use account pubkey now
  =/  pubkey-hex=tape  (hexn:sailbox pubkey)
  =/  sse-url=tape
    ::  Universal account stream path - works for all account types
    "/spv-wallet/stream/account/{pubkey-hex}"
  ::  All accounts support full scan, so include scan-status-update for all
  =/  sse-swap=tape
    ?:  is-full  "receiving-row-update,change-row-update,receiving-list-update,change-list-update,account-summary-update,scan-status-update,tx-verify-update,indexer-status-update"
    "receiving-row-update,change-row-update,receiving-list-update,change-list-update,account-summary-update,scan-status-update,indexer-status-update"
  =/  back-href=tape
    ?:  is-full  "/spv-wallet/wallet/{(hexn:sailbox (need wallet.details))}"
    ?:  is-signing  "/spv-wallet?tab=signing"
    "/spv-wallet?tab=watch"
  %-  htmx-page
  :^  "{(trip name.details)}"  |  ~
  ;div
    =id  "account-page"
    =class  "fc g3 p5 ma mw-page{?:(hide-empty " hide-empty" "")}"
    =style  "height: 100%;"
    ;div(style "flex-shrink: 0; display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer
        =href  back-href
        =style  "color: var(--f3); text-decoration: none;"
        ; ← Back to {?:(is-full "Wallet" "Wallets")}
      ==
    ==
    ;div.p4.b1.br2(style "flex-shrink: 0; position: relative;")
      ;h1.s2.bold.mb2
        ; {(trip name.details)}
        ;+  ?:  is-full
              ;span
                ;span(style "opacity: 0.4; margin: 0 8px;"): |
                ;span.f3(style "opacity: 0.5; font-weight: normal;"): {(trip (need wallet-name))}
              ==
            ;span;
      ==
      ;+  ?:  is-full
            ;div.f2: Path: {(format-account-path (need acct))}
          ;div(style "display: flex; align-items: center; gap: 8px;")
            ;+  (script-type-badge script-type.details)
            ;div.f2.s-1: {?:(is-signing "Signing Account" "Watch-Only Account")}
          ==
      ;div(style "display: flex; align-items: center; gap: 8px; margin-top: 12px;")
        ::  Network container
        ;div.p2.br1(style "display: flex; align-items: center; gap: 8px; background: var(--b2);")
          ;+  ?.  has-network-conflict
                ;span;
              ;div
                =title  "Network mismatch: coin-type doesn't match selected network"
                =style  "background: rgba(245, 158, 11, 0.15); border: 1px solid rgba(245, 158, 11, 0.4); color: #f59e0b; display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'alert-triangle')
                ==
              ==
          ;div.p2.b1.br2(style "display: flex; align-items: center; gap: 6px;")
            ;+  (network-badge active-network.details)
            ;span.f2.s-1: {network-name}
          ==
          ;+  ?.  show-network-edit
                ;span;
              ;button.hover.pointer
                =onclick  "showNetworkModal()"
                =title  "Change network"
                =style  "background: var(--b1); border: none; color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'edit-2')
                ==
              ==
        ==
        ::  Indexer container
        ;div#indexer-status.p2.br1(style "display: flex; align-items: center; gap: 8px; background: var(--b2);")
          ;div.p2.b1.br2(style "display: flex; align-items: center; gap: 6px;")
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'database')
            ==
            ;+  ?:  indexer-registered.details
                  ;span.f2.s-1: Indexed
                ;span.f2.s-1.f3: Not Indexed
          ==
          ;+  ?:  indexer-registered.details
                ;button.hover.pointer
                  =hx-post  "/spv-wallet/account/{pubkey-hex}"
                  =hx-vals  "\{\"action\": \"deregister-indexer\"}"
                  =hx-swap  "none"
                  =title  "Remove from indexer"
                  =style  "background: var(--b1); border: none; color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
                  ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'x')
                  ==
                ==
              ;button.hover.pointer
                =hx-post  "/spv-wallet/account/{pubkey-hex}"
                =hx-vals  "\{\"action\": \"register-indexer\"}"
                =hx-swap  "none"
                =title  "Add to indexer"
                =style  "background: var(--b1); border: none; color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'plus')
                ==
              ==
        ==
      ==
      ::  Eye toggle button
      ;button#empty-toggle.hover.pointer
        =onclick  "toggleEmptyAddresses()"
        =hx-post  "/spv-wallet"
        =hx-vals  "\{\"action\": \"toggle-empty-addresses\"}"
        =hx-trigger  "click"
        =hx-swap  "none"
        =title  ?:(hide-empty "Show addresses without transactions" "Hide addresses without transactions")
        =style  "position: absolute; top: 16px; right: 16px; background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
        ;div#eye-icon(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi ?:(hide-empty 'eye-off' 'eye'))
        ==
      ==
    ==
    ;div.p4.b2.br2(style "flex-shrink: 0;")
      ;h2.s1.bold.mb2: Account Summary
      ;div#account-summary(style "display: flex; justify-content: space-between; align-items: baseline;")
        ;span.f2(style "opacity: 0.8;"): Total Balance
        ;span.s0.bold.mono: {(scow %ud total-balance)} sats
      ==
      ;div(style "display: flex; gap: 8px; margin-top: 12px; justify-content: center;")
        ;+  ?:  ?|(is-full is-signing)
              =/  send-url=tape
                "/spv-wallet/account/{(hexn:sailbox pubkey)}/send"
              ;a.p2.b1.br2.hover.pointer
                =href  send-url
                =style  "display: flex; align-items: center; justify-content: center; gap: 6px; background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); color: var(--f3); text-decoration: none; outline: none; white-space: nowrap;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'arrow-up')
                ==
                ;span.f2.bold: Send
              ==
            ;span;
        ;button.p2.b1.br2.hover.pointer
          =onclick  "showReceiveModal(); return false;"
          =style  "display: flex; align-items: center; justify-content: center; gap: 6px; background: rgba(50, 200, 100, 0.15); border: 1px solid rgba(50, 200, 100, 0.4); color: var(--f3); outline: none; white-space: nowrap;"
          ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'arrow-down')
          ==
          ;span.f2.bold: Receive
        ==
      ==
    ==
    ;div.fc.g3(style "flex: 1; min-height: 0; overflow-y: auto;")
      ::  Full Scan button or progress indicator (all accounts)
      ;+  ?~  scan.proc.details
          ::  No scan running - show Full Scan button
          ;div.p3.b2.br2.hover.pointer(id "scan-status", onclick "fullScan('{(hexn:sailbox pubkey)}', 'receiving')", style "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2);")
            ;div(style "font-size: 24px; color: var(--f-3);")
              ; ↻
            ==
            ;span.f2.bold.f-3: Full Scan
          ==
        ::  Scan running - show progress and cancel button
        =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan.proc.details
        =/  scan-idx=@ud  idx.scn
        =/  scan-gap=@ud  gap.scn
        =/  border-color=tape  ?:(scan-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
        =/  bg-color=tape  ?:(scan-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
        =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
        ;div#scan-status.p3.b2.br2(style container-style)
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
      ::  Single SSE connection for all address updates
      ;div(hx-ext "sse", sse-connect sse-url, sse-swap sse-swap, style "display:none;");
      ::  Address tabs
      ;+  %+  tabs  "receiving"
          :~  :+  "receiving"
                "Receiving Addresses"
              ;div#receiving-list(style "display: flex; flex-direction: column; flex: 1; min-height: 0;")
                ;+  (address-list receiving:ac now pubkey 'receiving' receiving.proc.details tapscript.proc.details)
              ==
              :+  "change"
                "Change Addresses"
              ;div#change-list(style "display: flex; flex-direction: column; flex: 1; min-height: 0;")
                ;+  (address-list change:ac now pubkey 'change' change.proc.details tapscript.proc.details)
              ==
          ==
    ==
    ;script
      ; var accountType = '{?:(is-full "full" ?:(is-signing "signing" "watch-only"))}';
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Address copied to clipboard');
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
      ; function fullScan(pubkey, chain) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=full-scan&chain=' + encodeURIComponent(chain)
      ;   });
      ; }
      ;
      ; function deleteAddress(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=delete-address&chain=' + encodeURIComponent(chain) + '&index=' + index
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
      ; function deleteTapscript(pubkey, chain, index, tapscriptAddr) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=delete-tapscript&chain=' + encodeURIComponent(chain) + '&index=' + index + '&tapscript-addr=' + encodeURIComponent(tapscriptAddr)
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
      ; function cancelRefresh(pubkey, chain, index) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-refresh&chain=' + encodeURIComponent(chain) + '&index=' + index
      ;   });
      ; }
      ;
      ; function cancelScan(pubkey) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-scan'
      ;   });
      ; }
      ;
      ; function pauseScan(pubkey) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=pause-scan'
      ;   });
      ; }
      ;
      ; function resumeScan(pubkey) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=resume-scan'
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
      ; function showSendModal() {
      ;   document.getElementById('send-modal').style.display = 'flex';
      ; }
      ;
      ; function showReceiveModal() {
      ;   document.getElementById('receive-modal').style.display = 'flex';
      ;   var qrContainer = document.getElementById('receive-qr');
      ;   var address = qrContainer.getAttribute('data-address');
      ;   qrContainer.innerHTML = '';
      ;   new QRCode(qrContainer, {text: address, width: 200, height: 200});
      ; }
      ;
      ; function hideModal(modalId) {
      ;   document.getElementById(modalId).style.display = 'none';
      ; }
      ;
      ; function showNetworkModal() {
      ;   document.getElementById('network-modal').style.display = 'flex';
      ; }
      ;
      ; function setNetwork(network) {
      ;   fetch(window.location.pathname, {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=set-network&network=' + network
      ;   }).then(function() {
      ;     window.location.reload();
      ;   });
      ; }
      ;
      ; function buildTransaction() {
      ;   var toAddress = document.getElementById('send-to-address').value;
      ;   var amount = document.getElementById('send-amount').value;
      ;   alert('Building transaction to send ' + amount + ' sats to ' + toAddress + '\n\n(Transaction building coming soon!)');
      ; }
      ;
      ; function toggleEmptyAddresses() {
      ;   var page = document.getElementById('account-page');
      ;   var button = document.getElementById('empty-toggle');
      ;   var iconContainer = document.getElementById('eye-icon');
      ;   var isHiding = page.classList.contains('hide-empty');
      ;
      ;   if (isHiding) {
      ;     page.classList.remove('hide-empty');
      ;     button.setAttribute('title', 'Hide addresses without transactions');
      ;     iconContainer.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z"></path><circle cx="12" cy="12" r="3"></circle></svg>';
      ;   } else {
      ;     page.classList.add('hide-empty');
      ;     button.setAttribute('title', 'Show addresses without transactions');
      ;     iconContainer.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24"></path><line x1="1" y1="1" x2="23" y2="23"></line></svg>';
      ;   }
      ; }
    ==
    ::  Modals
    ;div#receive-modal.modal-overlay(style "display: none;")
      ;div.modal-content.p4.b1.br2(style "max-width: 400px; background: var(--b1);")
        ;+  (receive-modal-content next-unused)
      ==
    ==
    ;div#network-modal.modal-overlay(style "display: none;", onclick "if(event.target === this) hideModal('network-modal')")
      ;div.modal-content.p4.b1.br2(style "max-width: 320px; background: var(--b1);")
        ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
          ;h3.s1.bold: Select Network
          ;button.hover.pointer(onclick "hideModal('network-modal')", style "background: transparent; border: none; color: var(--f3); cursor: pointer; padding: 4px;")
            ;div(style "width: 20px; height: 20px;")
              ;+  (make:fi 'x')
            ==
          ==
        ==
        ;div.fc.g2
          ::  Show Mainnet only if NOT testnet cointype
          ;+  ?:  is-testnet-cointype
                ;span;
              ;button.p3.b1.br2.hover.pointer.wf(onclick "setNetwork('main')", style "text-align: left; display: flex; align-items: center; gap: 8px;{?:(=(%main active-network.details) " background: rgba(100, 150, 255, 0.15); border-color: rgba(100, 150, 255, 0.4);" "")}")
                ;+  bitcoin-mainnet-svg
                ;span: Mainnet
              ==
          ::  Show testnets only if NOT mainnet cointype
          ;+  ?:  is-mainnet-cointype
                ;span;
              ;button.p3.b1.br2.hover.pointer.wf(onclick "setNetwork('testnet4')", style "text-align: left; display: flex; align-items: center; gap: 8px;{?:(=(%testnet4 active-network.details) " background: rgba(100, 150, 255, 0.15); border-color: rgba(100, 150, 255, 0.4);" "")}")
                ;+  bitcoin-testnet-svg
                ;span: Testnet4
              ==
          ;+  ?:  is-mainnet-cointype
                ;span;
              ;button.p3.b1.br2.hover.pointer.wf(onclick "setNetwork('testnet3')", style "text-align: left; display: flex; align-items: center; gap: 8px;{?:(=(%testnet3 active-network.details) " background: rgba(100, 150, 255, 0.15); border-color: rgba(100, 150, 255, 0.4);" "")}")
                ;+  bitcoin-testnet-svg
                ;span: Testnet3
              ==
          ;+  ?:  is-mainnet-cointype
                ;span;
              ;button.p3.b1.br2.hover.pointer.wf(onclick "setNetwork('signet')", style "text-align: left; display: flex; align-items: center; gap: 8px;{?:(=(%signet active-network.details) " background: rgba(100, 150, 255, 0.15); border-color: rgba(100, 150, 255, 0.4);" "")}")
                ;+  bitcoin-testnet-svg
                ;span: Signet
              ==
          ;+  ?:  is-mainnet-cointype
                ;span;
              ;button.p3.b1.br2.hover.pointer.wf(onclick "setNetwork('regtest')", style "text-align: left; display: flex; align-items: center; gap: 8px;{?:(=(%regtest active-network.details) " background: rgba(100, 150, 255, 0.15); border-color: rgba(100, 150, 255, 0.4);" "")}")
                ;+  bitcoin-testnet-svg
                ;span: Regtest
              ==
        ==
      ==
    ==
    ;style
      ; @keyframes spin {
      ;   from { transform: rotate(0deg); }
      ;   to { transform: rotate(360deg); }
      ; }
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
      ; #account-page.hide-empty .empty-address {
      ;   display: none !important;
      ; }
      ; .modal-overlay {
      ;   position: fixed;
      ;   top: 0;
      ;   left: 0;
      ;   width: 100%;
      ;   height: 100%;
      ;   background: rgba(0, 0, 0, 0.7);
      ;   display: flex;
      ;   align-items: center;
      ;   justify-content: center;
      ;   z-index: 1000;
      ; }
      ; .modal-content {
      ;   max-height: 90vh;
      ;   overflow-y: auto;
      ; }
    ==
  ==
::
::  SSE Handlers
::
++  handle-test-pages-sse
  |=  $:  =bowl:gall
          state=vase
          site=(list @t)
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0 state)
  ?+    site  !!
      [%spv-wallet %timer ~]
    ?+    event  !!
        [~ %'/timer/counter-update']
      %-  manx-to-wain:sailbox
      ;p: {(scow %ud counter)}
    ==
  ==
::
++  transaction-detail-page
  |=  $:  account-pubkey=@ux
          account-pubkey-hex=@t
          txid=@t
          wallets=(map @ux wallet)
          accounts=(map @ux account-details)
      ==
  ^-  manx
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    ;div.p5: Account not found
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ::  Look up transaction in account's transaction map
  =/  tx-data=(unit transaction)  (~(get by transactions:ac) txid)
  ::  Build back link
  =/  account-url=tape
    "/spv-wallet/account/{(hexn:sailbox account-pubkey)}"
  ::  Check verification status
  =/  tx-verify-map=(map @t [pid=@ta act=?])  tx-verify.proc.u.details
  =/  verify-proc-entry=(unit [pid=@ta act=?])
    (~(get by tx-verify-map) txid)
  ::  Look up verification result
  =/  verification-lookup=(unit (unit (unit tang)))  (~(get by tx-verification:ac) txid)
  =/  verification-status=(unit (unit tang))
    ?~  verification-lookup  ~
    u.verification-lookup
  ::  Build verification button manx
  =/  verify-button=manx
    ?~  verify-proc-entry
      ::  Not verifying - show verify button
      ;div.p3.b2.br2.hover.pointer(id "tx-verify-button")
        =onclick  "verifyTransaction('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
        =style  "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2); cursor: pointer;"
        ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'check-circle')
        ==
        ;span.f2.bold.f-3: Verify Transaction
      ==
    =/  [proc-pid=@ta proc-act=?]  u.verify-proc-entry
    =/  border-color=tape  ?:(proc-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
    =/  bg-color=tape  ?:(proc-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
    =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
    ;div.p3.b2.br2(id "tx-verify-button", style container-style)
      ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
        ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
          ;+  ?:  proc-act
                ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                  ;+  (make:fi 'loader')
                ==
              ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'pause-circle')
              ==
        ==
        ;div(style "display: flex; flex-direction: column; gap: 4px;")
          ;div.f2.bold: {?:(proc-act "Verifying Transaction..." "Verification Paused")}
          ;div.f3.s-1: Checking against SPV headers
        ==
      ==
      ;div(style "display: flex; gap: 4px;")
        ;+  ?:  proc-act
              ::  Active - show pause button
              ;button.p2.b1.br1.hover.pointer
                =title  "Pause verification"
                =onclick  "pauseVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
                =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'pause')
                ==
              ==
            ::  Paused - show play button
            ;button.p2.b1.br1.hover.pointer
              =title  "Resume verification"
              =onclick  "resumeVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
              =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'play')
              ==
            ==
        ;button.p2.b1.br1.hover.pointer
          =title  "Cancel verification"
          =onclick  "cancelVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
          =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
          ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'x-circle')
          ==
        ==
      ==
    ==
  %-  htmx-page
  :^  "Transaction: {(trip txid)}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;script
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Copied to clipboard');
      ;   }).catch(function(err) {
      ;     console.error('Failed to copy: ', err);
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
      ; function pauseVerify(pubkey, txid) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=pause-verify&txid=' + encodeURIComponent(txid)
      ;   });
      ; }
      ;
      ; function resumeVerify(pubkey, txid) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=resume-verify&txid=' + encodeURIComponent(txid)
      ;   });
      ; }
      ;
      ; function cancelVerify(pubkey, txid) {
      ;   fetch('/spv-wallet/account/' + encodeURIComponent(pubkey), {
      ;     method: 'POST',
      ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
      ;     body: 'action=cancel-verify&txid=' + encodeURIComponent(txid)
      ;   });
      ; }
    ==
    ;style
      ; @keyframes pulseButton {
      ;   0% {
      ;     filter: brightness(1.8) saturate(1.5);
      ;     transform: scale(1.02);
      ;   }
      ;   100% {
      ;     filter: brightness(1) saturate(1);
      ;     transform: scale(1);
      ;   }
      ; }
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
      ; @keyframes pulseError {
      ;   0% {
      ;     filter: brightness(1.15);
      ;   }
      ;   100% {
      ;     filter: brightness(1);
      ;   }
      ; }
      ; .pulse-button {
      ;   animation: pulseButton 0.8s cubic-bezier(0.4, 0, 0.2, 1) forwards;
      ; }
      ; .pulse-badge {
      ;   animation: pulseBadge 0.8s cubic-bezier(0.4, 0, 0.2, 1) forwards;
      ; }
      ; .pulse-error {
      ;   animation: pulseError 0.6s ease-out forwards;
      ; }
    ==
    ::  SSE connection for transaction updates
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/account/{(hexn:sailbox account-pubkey)}", sse-swap "tx-verify-update", style "display:none;");
    ;a.hover.pointer(href account-url, style "color: var(--f3); text-decoration: none; margin-bottom: 16px; display: inline-block;"): ← Back to Account
    ;h1: Transaction Details
    ;div.p3.b2.br2
      ;div.f3.s-2.pb2: Transaction ID
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;div.mono.f2(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1;"): {(trip txid)}
        ;button.p1.b0.br1.hover.pointer
          =onclick  "copyToClipboard('{(trip txid)}')"
          =title  "Copy transaction ID"
          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none;"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'copy')
          ==
        ==
      ==
    ==
    ;+  ?~  tx-data
          ;div.p3.b2.br2
            ;div.f3.s-2: No transaction data found
          ==
        =/  confirmed=?
          ?=(%confirmed -.tx-status.u.tx-data)
        =/  block-height=(unit @ud)
          ?:  ?=(%unconfirmed -.tx-status.u.tx-data)  ~
          `block-height.tx-status.u.tx-data
        =/  fee=@ud
          (fall fee.u.tx-data 0)
        =/  size=@ud
          (fall size.u.tx-data 0)
        =/  status-color=tape  ?:(confirmed "rgba(50, 200, 100, 0.3)" "rgba(255, 180, 50, 0.3)")
        =/  status-text=tape  ?:(confirmed "Confirmed" "Unconfirmed")
        ::  Get inputs and outputs from canonical transaction
        =/  inputs=(list tx-input)  inputs.u.tx-data
        =/  outputs=(list tx-output)  outputs.u.tx-data
        ::  Enumerate outputs with indices
        =/  indexed-outputs=(list [vout-index=@ud output=tx-output])
          =/  index-loop
            |=  [outs=(list tx-output) idx=@ud]
            ^-  (list [vout-index=@ud output=tx-output])
            ?~  outs  ~
            [[idx i.outs] $(outs t.outs, idx +(idx))]
          (index-loop outputs 0)
        ::  Enumerate inputs with indices (for witness modal IDs)
        =/  indexed-inputs=(list [vin-index=@ud input=tx-input])
          =/  index-loop
            |=  [ins=(list tx-input) idx=@ud]
            ^-  (list [vin-index=@ud input=tx-input])
            ?~  ins  ~
            [[idx i.ins] $(ins t.ins, idx +(idx))]
          (index-loop inputs 0)
        ::  Build UTXO set from cached per-address UTXO data
        =/  utxo-set=(set [@t @ud])
          =/  recv-list=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) receiving:ac)
          =/  change-list=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) change:ac)
          =/  all-leaves=(list hd-leaf)
            %+  weld
              (turn recv-list |=([@ l=hd-leaf] l))
            (turn change-list |=([@ l=hd-leaf] l))
          =|  result=(set [@t @ud])
          |-
          ?~  all-leaves  result
          =/  leaf-utxos=(list [txid=@t vout=@ud value=@ud =tx-status])
            utxos.main.i.all-leaves
          =/  new-pairs=(set [@t @ud])
            %-  ~(gas in *(set [@t @ud]))
            (turn leaf-utxos |=([t=@t v=@ud *] [t v]))
          $(all-leaves t.all-leaves, result (~(uni in result) new-pairs))
        ;div.fc.g3
          ::  Transaction Info section
          ;div.p3.b2.br2
            ;div.f3.s-2.pb2: Transaction Info
            ;div(style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 16px;")
              ;div
                ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Status
                ;span.f3.s-2.p2.br1(style "background: {status-color}; display: inline-block;"): {status-text}
              ==
              ;div
                ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Block
                ;+  ?~  block-height
                      ;div.f3: Pending
                    ;div.f3: {(scow %ud u.block-height)}
              ==
              ;div
                ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Fee
                ;div.f3: {(scow %ud fee)} sats
              ==
              ;div
                ;div.f3.s-1(style "opacity: 0.8; margin-bottom: 4px;"): Size
                ;div.f3: {(scow %ud size)} bytes
              ==
            ==
          ==
          ::  Verification button
          ;+  verify-button
          ::  Verification Status Display
          ;div(id "tx-verify-status-icon")
            ;+  ?~  verification-status
                  ::  Unverified
                  ;div.p4.b1.br2(style "background: rgba(150, 150, 150, 0.15); border: 1px solid rgba(150, 150, 150, 0.4);")
                    ;div(style "display: flex; align-items: center; gap: 8px;")
                      ;div(style "color: #999; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                        ;+  (make:fi 'help-circle')
                      ==
                      ;div.s-1.bold(style "color: #999;"): Not SPV Verified
                    ==
                  ==
                ?~  u.verification-status
                  ::  Verified successfully
                  ;div.p4.b1.br2(style "background: rgba(50, 200, 100, 0.15); border: 1px solid rgba(50, 200, 100, 0.4);")
                    ;div(style "display: flex; align-items: center; gap: 8px;")
                      ;div(style "color: #32c864; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                        ;+  (make:fi 'check-circle')
                      ==
                      ;div.s-1.bold(style "color: #32c864;"): SPV Verified
                    ==
                  ==
                ::  Verification failed with errors
                ;div.p4.b1.br2.pulse-error(style "background: rgba(255, 80, 80, 0.15); border: 1px solid rgba(255, 80, 80, 0.4);")
                  ;div(style "display: flex; align-items: start; gap: 8px;")
                    ;div(style "color: #ff5050; margin-top: 2px; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                      ;+  (make:fi 'alert-circle')
                    ==
                    ;div.fc.g1(style "flex: 1;")
                      ;div.s-1.bold(style "color: #ff5050;"): Verification Failed
                      ;div.p2.b2.br1.mono.s-2(style "max-height: 200px; overflow-y: auto; word-break: break-all; white-space: pre-wrap; background: rgba(0,0,0,0.2);")
                        ;*  %+  turn  u.u.verification-status
                            |=  t=tank
                            ;div: {~(ram re t)}
                      ==
                    ==
                  ==
                ==
          ==
          ::  Verification error container for SSE updates
          ;div(id "tx-verify-error");
          ::  Inputs section
          ;div.p3.b2.br2
            ;div.f3.s-2.pb2: Inputs
            ;div(style "max-height: 400px; overflow-y: auto;")
              ;div.fc.g2
                ;*  %+  turn  indexed-inputs
                  |=  [vin-index=@ud input=tx-input]
                  ^-  manx
                  =/  txid=tape  (trip spent-txid.input)
                  =/  vout=@ud  spent-vout.input
                  =/  has-witness=?  !=(~ witness.input)
                  =/  modal-id=tape  "witness-modal-{(scow %ud vin-index)}"
                  ::  Get prevout details
                  ?~  prevout.input
                    ;div
                      ;div.p3.b1.br2(style "display: flex; justify-content: space-between; align-items: center;")
                        ;span.f3(style "opacity: 0.5;"): [Prevout data not available]
                        ;+  (witness-eye-btn modal-id has-witness)
                      ==
                      ;+  (witness-modal modal-id vin-index witness.input)
                    ==
                  =/  value=@ud  value.u.prevout.input
                  =/  address=tape  (trip address.u.prevout.input)
                  ::  Check if this address belongs to our account
                  =/  addr-suffix=(unit address-suffix:hd-path)
                    (~(get by address-cache:ac) (crip address))
                  ::  Check if this transaction exists in our account
                  =/  tx-exists=?  (~(has by transactions:ac) (crip txid))
                  =/  tx-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/tx/{txid}"
                  ;div
                    ;div.p3.b1.br2(style "display: flex; justify-content: space-between; align-items: center; gap: 12px;")
                      ;div(style "flex: 1; min-width: 0;")
                        ::  Transaction ID row with copy button on left
                        ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;")
                          ;button.p1.b0.br1.hover.pointer
                            =onclick  "copyToClipboard('{txid}')"
                            =title  "Copy transaction ID"
                            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                              ;+  (make:fi 'copy')
                            ==
                          ==
                          ;+  ?:  tx-exists
                                ;a.mono.f2.s-1(href tx-url, style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                                  ;span(style "overflow: hidden; text-overflow: ellipsis;"): {txid}
                                  ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                                    ;+  (make:fi 'external-link')
                                  ==
                                ==
                              ;div.mono.f2.s-1(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); flex: 1; min-width: 0;"): {txid}
                        ==
                        ::  Address row with copy button on far left
                        ;div(style "display: flex; align-items: center; gap: 8px;")
                          ;button.p1.b0.br1.hover.pointer
                            =onclick  "copyToClipboard('{address}')"
                            =title  "Copy address"
                            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                              ;+  (make:fi 'copy')
                            ==
                          ==
                          ;span.f3.s-2.mono(style "opacity: 0.8; white-space: nowrap; flex-shrink: 0;"): Output #{(scow %ud vout)}
                          ;+  ?~  addr-suffix
                                ;div.mono.f2.s-1(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); flex: 1; min-width: 0;"): {address}
                              =/  [change=seg:hd-path index=seg:hd-path]  u.addr-suffix
                              =/  chain=tape  ?:  =([%.y 1] change)  "change"  "receiving"
                              =/  idx=@ud  +.index
                              =/  addr-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/address/{chain}/{(scow %ud idx)}"
                              ;a.mono.f2.s-1(href addr-url, style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                                ;span(style "overflow: hidden; text-overflow: ellipsis;"): {address}
                                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                                  ;+  (make:fi 'external-link')
                                ==
                              ==
                        ==
                      ==
                      ::  Amount and witness on the right
                      ;div(style "display: flex; align-items: center; gap: 8px; flex-shrink: 0;")
                        ;div.f3.s-2(style "white-space: nowrap;"): {(scow %ud value)} sats
                        ;+  (witness-eye-btn modal-id has-witness)
                      ==
                    ==
                    ;+  (witness-modal modal-id vin-index witness.input)
                  ==
              ==
            ==
          ==
          ::  Outputs section
          ;div.p3.b2.br2
            ;div.f3.s-2.pb2: Outputs
            ;div(style "max-height: 400px; overflow-y: auto;")
              ;div.fc.g2
                ;*  %+  turn  indexed-outputs
                  |=  [vout-index=@ud output=tx-output]
                  ^-  manx
                  =/  value=@ud  value.output
                  =/  address=tape  (trip address.output)
                  ::  Check if this address belongs to our account
                  =/  addr-suffix=(unit address-suffix:hd-path)
                    (~(get by address-cache:ac) (crip address))
                  ::  Check if this output is a UTXO (from cached data)
                  =/  is-utxo=?
                    (~(has in utxo-set) [txid vout-index])
                  =/  row-background=tape
                    ?:(is-utxo "background: rgba(255, 200, 50, 0.15);" "background: var(--b1);")
                  ;div.p3.b1.br2(style "display: flex; justify-content: space-between; align-items: center; gap: 12px; {row-background}")
                    ;div(style "flex: 1; min-width: 0;")
                      ::  Address row with copy button on far left
                      ;div(style "display: flex; align-items: center; gap: 8px;")
                        ;button.p1.b0.br1.hover.pointer
                          =onclick  "copyToClipboard('{address}')"
                          =title  "Copy address"
                          =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                            ;+  (make:fi 'copy')
                          ==
                        ==
                        ;span.f3.s-2.mono(style "opacity: 0.8; white-space: nowrap; flex-shrink: 0;"): Output #{(scow %ud vout-index)}
                        ;+  ?~  addr-suffix
                              ;div.mono.f2.s-1(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); flex: 1; min-width: 0;"): {address}
                            =/  [change=seg:hd-path index=seg:hd-path]  u.addr-suffix
                            =/  chain=tape  ?:  =([%.y 1] change)  "change"  "receiving"
                            =/  idx=@ud  +.index
                            =/  addr-url=tape  "/spv-wallet/account/{(hexn:sailbox account-pubkey)}/address/{chain}/{(scow %ud idx)}"
                            ;a.mono.f2.s-1(href addr-url, style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                              ;span(style "overflow: hidden; text-overflow: ellipsis;"): {address}
                              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                                ;+  (make:fi 'external-link')
                              ==
                            ==
                      ==
                    ==
                    ::  Amount on the right with optional UTXO star
                    ;div(style "display: flex; align-items: center; gap: 8px;")
                      ;+  ?:  is-utxo
                            ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;", title "UTXO")
                              ;+  (make:fi 'star')
                            ==
                          ;div;
                      ;div.f3.s-2(style "white-space: nowrap;"): {(scow %ud value)} sats
                    ==
                  ==
              ==
            ==
          ==
        ==
  ==
::
++  handle-account-sse
  |=  $:  =bowl:gall
          state=vase
          account-pubkey=@ux
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0 state)
  ::  Look up account directly from flat accounts map
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    %-  manx-to-wain:sailbox
    ;div: Account not found
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ?+    event  !!
      [~ %receiving-list-update]
    =/  list-manx=manx  (address-list receiving:ac now.bowl account-pubkey 'receiving' receiving.proc.u.details tapscript.proc.u.details)
    =/  wrapped=manx
      :_  [list-manx ~]
      :-  %div
      :~  [%id "receiving-list"]
          [%hx-swap-oob "true"]
          [%style "display: flex; flex-direction: column; flex: 1; min-height: 0;"]
      ==
    %-  manx-to-wain:sailbox
    wrapped
      [~ %change-list-update]
    =/  list-manx=manx  (address-list change:ac now.bowl account-pubkey 'change' change.proc.u.details tapscript.proc.u.details)
    =/  wrapped=manx
      :_  [list-manx ~]
      :-  %div
      :~  [%id "change-list"]
          [%hx-swap-oob "true"]
          [%style "display: flex; flex-direction: column; flex: 1; min-height: 0;"]
      ==
    %-  manx-to-wain:sailbox
    wrapped
      [~ %receiving-row-update]
    =/  index=@ud  (rash (need id) dem)
    ::  Send both row update (for account page) and detail sections (for address detail page)
    =/  row-manx=manx
      (address-row-oob index receiving:ac now.bowl account-pubkey 'receiving' receiving.proc.u.details tapscript.proc.u.details)
    =/  detail-sections=manx
      (address-detail-sections index receiving:ac account-pubkey 'receiving' receiving.proc.u.details tx-verification:ac tx-verify.proc.u.details transactions:ac)
    =/  combined=manx
      ;div
        ;+  row-manx
        ;+  detail-sections
      ==
    %-  manx-to-wain:sailbox
    combined
      [~ %change-row-update]
    =/  index=@ud  (rash (need id) dem)
    ::  Send both row update (for account page) and detail sections (for address detail page)
    =/  row-manx=manx
      (address-row-oob index change:ac now.bowl account-pubkey 'change' change.proc.u.details tapscript.proc.u.details)
    =/  detail-sections=manx
      (address-detail-sections index change:ac account-pubkey 'change' change.proc.u.details tx-verification:ac tx-verify.proc.u.details transactions:ac)
    =/  combined=manx
      ;div
        ;+  row-manx
        ;+  detail-sections
      ==
    %-  manx-to-wain:sailbox
    combined
    ::
      [~ %account-summary-update]
    =/  total-balance=@ud  (compute-account-balance u.details)
    =/  summary-manx=manx
      ;div#account-summary(hx-swap-oob "true", style "display: flex; justify-content: space-between; align-items: baseline;")
        ;span.f2(style "opacity: 0.8;"): Total Balance
        ;span.s0.bold.mono: {(scow %ud total-balance)} sats
      ==
    %-  manx-to-wain:sailbox
    summary-manx
    ::
      [~ %scan-status-update]
    %-  manx-to-wain:sailbox
    (scan-status-oob account-pubkey scan.proc.u.details)
    ::
      [~ %indexer-status-update]
    =/  pubkey-hex=tape  (hexn:sailbox account-pubkey)
    =/  indexer-manx=manx
      ;div#indexer-status.p2.br1(hx-swap-oob "true", style "display: flex; align-items: center; gap: 8px; background: var(--b2);")
        ;div.p2.b1.br2(style "display: flex; align-items: center; gap: 6px;")
          ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'database')
          ==
          ;+  ?:  indexer-registered.u.details
                ;span.f2.s-1: Indexed
              ;span.f2.s-1.f3: Not Indexed
        ==
        ;+  ?:  indexer-registered.u.details
              ;button.hover.pointer
                =hx-post  "/spv-wallet/account/{pubkey-hex}"
                =hx-vals  "\{\"action\": \"deregister-indexer\"}"
                =hx-swap  "none"
                =title  "Remove from indexer"
                =style  "background: var(--b1); border: none; color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'x')
                ==
              ==
            ;button.hover.pointer
              =hx-post  "/spv-wallet/account/{pubkey-hex}"
              =hx-vals  "\{\"action\": \"register-indexer\"}"
              =hx-swap  "none"
              =title  "Add to indexer"
              =style  "background: var(--b1); border: none; color: var(--f3); display: flex; align-items: center; justify-content: center; width: 32px; height: 32px; border-radius: 4px; cursor: pointer; outline: none;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'plus')
              ==
            ==
      ==
    %-  manx-to-wain:sailbox
    indexer-manx
    ::
      [~ %tx-verify-update]
    ::  Build verification button and status icon updates
    =/  tx-verify-map=(map @t [pid=@ta act=?])  tx-verify.proc.u.details
    =/  txid=@t  (need id)
    =/  verify-proc-entry=(unit [pid=@ta act=?])
      (~(get by tx-verify-map) txid)
    =/  verification-lookup=(unit (unit (unit tang)))  (~(get by tx-verification:ac) txid)
    =/  verification-status=(unit (unit tang))
      ?~  verification-lookup  ~
      u.verification-lookup
    ::  Build verification button
    =/  verify-button=manx
      ?~  verify-proc-entry
        ;div.p3.b2.br2.hover.pointer(id "tx-verify-button", hx-swap-oob "true")
          =onclick  "verifyTransaction('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
          =style  "display: flex; align-items: center; justify-content: center; gap: 8px; border: 2px solid var(--b3); background: var(--b2); cursor: pointer;"
          ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'check-circle')
          ==
          ;span.f2.bold.f-3: Verify Transaction
        ==
      =/  [proc-pid=@ta proc-act=?]  u.verify-proc-entry
      =/  border-color=tape  ?:(proc-act "rgba(100, 150, 255, 0.4)" "rgba(150, 150, 150, 0.4)")
      =/  bg-color=tape  ?:(proc-act "rgba(100, 150, 255, 0.1)" "rgba(150, 150, 150, 0.1)")
      =/  container-style=tape  "display: flex; align-items: center; justify-content: space-between; gap: 12px; border: 2px solid {border-color}; background: {bg-color};"
      ;div.p3.b2.br2(id "tx-verify-button", hx-swap-oob "true", style container-style)
        ;div(style "display: flex; align-items: center; gap: 12px; flex: 1;")
          ;div(style "display: flex; align-items: center; justify-content: center; width: 32px; height: 32px;")
            ;+  ?:  proc-act
                  ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center; animation: spin 1s linear infinite;")
                    ;+  (make:fi 'loader')
                  ==
                ;div(style "width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'pause-circle')
                ==
          ==
          ;div(style "display: flex; flex-direction: column; gap: 4px;")
            ;div.f2.bold: {?:(proc-act "Verifying Transaction..." "Verification Paused")}
            ;div.f3.s-1: Checking against SPV headers
          ==
        ==
        ;div(style "display: flex; gap: 4px;")
          ;+  ?:  proc-act
                ;button.p2.b1.br1.hover.pointer
                  =title  "Pause verification"
                  =onclick  "pauseVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
                  =style  "background: rgba(255, 180, 50, 0.2); border: 1px solid rgba(255, 180, 50, 0.4); color: #ffb432; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                  ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'pause')
                  ==
                ==
              ;button.p2.b1.br1.hover.pointer
                =title  "Resume verification"
                =onclick  "resumeVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
                =style  "background: rgba(50, 200, 100, 0.2); border: 1px solid rgba(50, 200, 100, 0.4); color: #32c864; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
                ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'play')
                ==
              ==
          ;button.p2.b1.br1.hover.pointer
            =title  "Cancel verification"
            =onclick  "cancelVerify('{(hexn:sailbox account-pubkey)}', '{(trip txid)}')"
            =style  "background: rgba(255, 80, 80, 0.2); border: 1px solid rgba(255, 80, 80, 0.4); color: #ff5050; display: flex; align-items: center; width: 32px; height: 32px; justify-content: center; outline: none;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'x-circle')
            ==
          ==
        ==
      ==
    ::  Build verification status display
    =/  verify-status-icon=manx
      ;div(id "tx-verify-status-icon", hx-swap-oob "true")
        ;+  ?~  verification-status
              ::  Unverified
              ;div.p4.b1.br2(style "background: rgba(150, 150, 150, 0.15); border: 1px solid rgba(150, 150, 150, 0.4);")
                ;div(style "display: flex; align-items: center; gap: 8px;")
                  ;div(style "color: #999; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'help-circle')
                  ==
                  ;div.s-1.bold(style "color: #999;"): Not SPV Verified
                ==
              ==
            ?~  u.verification-status
              ::  Verified successfully
              ;div.p4.b1.br2(style "background: rgba(50, 200, 100, 0.15); border: 1px solid rgba(50, 200, 100, 0.4);")
                ;div(style "display: flex; align-items: center; gap: 8px;")
                  ;div(style "color: #32c864; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                    ;+  (make:fi 'check-circle')
                  ==
                  ;div.s-1.bold(style "color: #32c864;"): SPV Verified
                ==
              ==
            ::  Verification failed with errors
            ;div.p4.b1.br2.pulse-error(style "background: rgba(255, 80, 80, 0.15); border: 1px solid rgba(255, 80, 80, 0.4);")
              ;div(style "display: flex; align-items: start; gap: 8px;")
                ;div(style "color: #ff5050; margin-top: 2px; width: 20px; height: 20px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'alert-circle')
                ==
                ;div.fc.g1(style "flex: 1;")
                  ;div.s-1.bold(style "color: #ff5050;"): Verification Failed
                  ;div.p2.b2.br1.mono.s-2(style "max-height: 200px; overflow-y: auto; word-break: break-all; white-space: pre-wrap; background: rgba(0,0,0,0.2);")
                    ;*  %+  turn  u.u.verification-status
                        |=  t=tank
                        ;div: {~(ram re t)}
                  ==
                ==
              ==
            ==
      ==
    ::  Build verification error display (no longer used - kept for backward compatibility)
    =/  verify-error=manx
      ;div(id "tx-verify-error", hx-swap-oob "true");
    ::  Build badge update for transaction rows (refresh button state)
    =/  verify-badge-update=manx
      (hx-swap-oob (tx-verify-badge txid account-pubkey verify-proc-entry verification-status))
    ::  Build status badge update with animation
    =/  verify-status-badge-update=manx
      (tx-verify-status-badge-oob txid verification-status)
    ::  Return all five updates
    %-  manx-to-wain:sailbox
    ;div
      ;+  verify-button
      ;+  verify-status-icon
      ;+  verify-error
      ;+  verify-badge-update
      ;+  verify-status-badge-update
    ==
    ::
      [~ %indexer-subscribed]
    ::  Indexer subscription confirmed - update status UI
    =/  data=@t  (fall id '')
    =/  parsed=(unit [chain=@t idx=@ud])
      %+  rush  data
      ;~  plug
        (cook crip (star ;~(less fas prn)))
        ;~(pfix fas dem)
      ==
    ?~  parsed
      %-  manx-to-wain:sailbox
      ;div;
    =/  chn=@t  chain.u.parsed
    =/  idx=@ud  idx.u.parsed
    =/  addr-unit=(unit address-details)
      (~(get-addr ac:wallet-account [u.details active-network.u.details]) chn idx)
    ?~  addr-unit
      %-  manx-to-wain:sailbox
      ;div;
    %-  manx-to-wain:sailbox
    ;div#indexer-sub-status(hx-swap-oob "true")
      ;+  (indexer-status-ui account-pubkey address.u.addr-unit chn idx %.y)
    ==
    ::
      [~ %indexer-unsubscribed]
    ::  Indexer unsubscription confirmed - update status UI
    =/  data=@t  (fall id '')
    =/  parsed=(unit [chain=@t idx=@ud])
      %+  rush  data
      ;~  plug
        (cook crip (star ;~(less fas prn)))
        ;~(pfix fas dem)
      ==
    ?~  parsed
      %-  manx-to-wain:sailbox
      ;div;
    =/  chn=@t  chain.u.parsed
    =/  idx=@ud  idx.u.parsed
    =/  addr-unit=(unit address-details)
      (~(get-addr ac:wallet-account [u.details active-network.u.details]) chn idx)
    ?~  addr-unit
      %-  manx-to-wain:sailbox
      ;div;
    %-  manx-to-wain:sailbox
    ;div#indexer-sub-status(hx-swap-oob "true")
      ;+  (indexer-status-ui account-pubkey address.u.addr-unit chn idx %.n)
    ==
    ::
      [~ %indexer-update]
    ::  Indexer update received - data is stored, page refresh will show it
    %-  manx-to-wain:sailbox
    ;div;
  ==
::
++  handle-discovery-sse
  |=  $:  =bowl:gall
          state=vase
          pubkey=@ux
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0 state)
  ?+    event  !!
      [~ %account-list-update]
    =/  wallet=(unit wallet)  (~(get by wallets) pubkey)
    ?~  wallet
      %-  manx-to-wain:sailbox
      ;div: Wallet not found
    %-  manx-to-wain:sailbox
    (accounts-list-oob pubkey accounts.u.wallet accounts)
    ::
      [~ %discovery-status-update]
    =/  wallet=(unit wallet)  (~(get by wallets) pubkey)
    ?~  wallet
      %-  manx-to-wain:sailbox
      ;div: Wallet not found
    %-  manx-to-wain:sailbox
    (discovery-status-oob pubkey scan.u.wallet)
    ::
      [~ %account-row-update]
    =/  account-path=@t  (need id)
    =/  wallet=(unit wallet)  (~(get by wallets) pubkey)
    ?~  wallet
      %-  manx-to-wain:sailbox
      ;div: Wallet not found
    ::  Convert to old format for iteration
    =/  wallet-accts-with-details=(map account:hd-path account-details)
      (wallet-accounts-with-details:wallet-account accounts.u.wallet accounts)
    ::  Find the account
    =/  matching-account=(unit [account:hd-path account-details])
      %-  ~(rep by wallet-accts-with-details)
      |=  [[acct=account:hd-path details=account-details] result=(unit [account:hd-path account-details])]
      ?^  result  result
      =/  path-str=tape  (format-account-path acct)
      ?.  =((crip path-str) account-path)  ~
      `[acct details]
    ?~  matching-account
      %-  manx-to-wain:sailbox
      ;div: Account not found
    %-  manx-to-wain:sailbox
    (account-row-oob pubkey -.u.matching-account +.u.matching-account)
  ==
--
