/-  *spv-wallet
/+  fi=feather-icons,
    sailbox, txns=tx-build, fees=tx-fees, drft=tx-draft,
    *ui-utils, *ui-primitives, *ui-layout, *ui-accounts,
    *ui-addresses, *ui-accounts, *ui-discovery, *ui-experiments, *ui-spv,
    *wallet-address, *wallet-utxo, wallet-account
|%
::  Fee calculation result structure
::
+$  fee-calc
  $:  total-inputs=@ud
      total-outputs=@ud
      total-outputs-with-change=@ud
      target=@ud                      ::  outputs + est-fee (what we need to select)
      has-change-config=?
      fee-rate=@ud
      est-vbytes=@ud
      est-fee=@ud
      change-result=change-result:fees
      actual-fee=@sd
      fee-color=tape
  ==
::  Calculate all fee-related values from a draft
::
++  compute-fee-info
  |=  draft=(unit transaction:drft)
  ^-  fee-calc
  =/  total-inputs=@ud
    ?~  draft  0
    %+  roll  inputs.u.draft
    |=([input=utxo-input:drft sum=@ud] (add sum amount.input))
  =/  total-outputs=@ud
    ?~  draft  0
    %+  roll  outputs.u.draft
    |=([output=output:drft sum=@ud] (add sum amount.output))
  =/  has-change-config=?
    ?~  draft  %.n
    ?=(^ change.u.draft)
  =/  fee-rate=@ud
    ?~  draft  0
    ?~  change.u.draft  0
    fee-rate.u.change.u.draft
  =/  est-vbytes=@ud
    ?~  draft  0
    (calculate-vbytes:drft u.draft)
  =/  est-fee=@ud  (calculate-fee:fees est-vbytes fee-rate)
  =/  change-result=change-result:fees
    (calculate-change-result:fees total-inputs total-outputs est-fee)
  =/  total-outputs-with-change=@ud
    %+  add  total-outputs
    ?.  has-change-config  0
    ?:(?=(%ok -.change-result) amount.change-result 0)
  =/  actual-fee=@sd
    ?:  (gte total-inputs total-outputs-with-change)
      (sun:si (sub total-inputs total-outputs-with-change))
    (new:si | (sub total-outputs-with-change total-inputs))
  =/  fee-color=tape
    ?:  (syn:si actual-fee)  "var(--f3)"
    "rgba(255, 100, 100, 0.8)"
  =/  target=@ud  (add total-outputs est-fee)
  :*  total-inputs
      total-outputs
      total-outputs-with-change
      target
      has-change-config
      fee-rate
      est-vbytes
      est-fee
      change-result
      actual-fee
      fee-color
  ==
::  Fee info display bar
::
++  fee-info-bar
  |=  =fee-calc
  ^-  manx
  ;div#fee-info.f2(style "margin-top: 4px; display: flex; gap: 16px; flex-wrap: wrap;")
    ;span: Inputs: {(scow %ud total-inputs.fee-calc)} sats
    ;span: Outputs: {(scow %ud total-outputs-with-change.fee-calc)} sats
    ;span: Size: ~{(scow %ud est-vbytes.fee-calc)} vB
    ;span(style "color: {fee-color.fee-calc};"): Fee: {?:((syn:si actual-fee.fee-calc) (scow %ud (abs:si actual-fee.fee-calc)) "-{(scow %ud (abs:si actual-fee.fee-calc))}")} sats
  ==
++  send-modal-content
  |=  total-balance=@ud
  ^-  manx
  ;div
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;h2.s1.bold: Send Bitcoin
      ;button.p2.b1.br1.hover.pointer(onclick "hideModal('send-modal')", style "background: transparent; border: 1px solid var(--b3); outline: none;")
        ;+  (make:fi 'x')
      ==
    ==
    ;form.fc.g3(onsubmit "event.preventDefault(); buildTransaction();")
      ;div.fc.g2
        ;label.f2.bold: To Address
        ;input.p2.b1.br2
          =type  "text"
          =id  "send-to-address"
          =placeholder  "bc1q..."
          =required  "true"
          =style  "width: 100%; background: var(--b2); border: 1px solid var(--b3); outline: none; color: var(--f3);";
      ==
      ;div.fc.g2
        ;label.f2.bold: Amount (sats)
        ;div(style "display: flex; gap: 8px;")
          ;input.p2.b1.br2
            =type  "number"
            =id  "send-amount"
            =placeholder  "10000"
            =required  "true"
            =style  "flex: 1; background: var(--b2); border: 1px solid var(--b3); outline: none; color: var(--f3);";
          ;button.p2.b1.br2.hover.pointer
            =type  "button"
            =onclick  "document.getElementById('send-amount').value = {(scow %ud total-balance)}"
            =style  "background: var(--b2); border: 1px solid var(--b3); outline: none;"
            : Max
          ==
        ==
      ==
      ;div(style "display: flex; gap: 8px; margin-top: 8px;")
        ;button.p3.b1.br2.hover.pointer
          =type  "button"
          =onclick  "hideModal('send-modal')"
          =style  "flex: 1; background: var(--b2); border: 1px solid var(--b3); outline: none;"
          : Cancel
        ==
        ;button.p3.b1.br2.hover.pointer
          =type  "submit"
          =style  "flex: 1; background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); outline: none;"
          : Build Transaction
        ==
      ==
    ==
  ==
::
++  utxo-checkbox-item
  |=  $:  utxo-idx=@ud
          txid=@t
          vout=@ud
          value=@ud
          address=@t
          chain=@t
          idx-addr=@ud
          confs=(unit @ud)
          account-pubkey=@ux
          draft=(unit transaction:drft)
          output-labels=(map @t (set label-entry:bip329))
          transactions=(map @t transaction)
          address-cache=(map @t address-suffix:hd-path)
      ==
  ^-  manx
  =/  pubkey-hex=tape  (hexn:sailbox account-pubkey)
  ::  Check if tx and address exist in account data
  =/  tx-exists=?  (~(has by transactions) txid)
  =/  addr-exists=?  (~(has by address-cache) address)
  ::  Only link to internal pages if they exist
  =/  tx-url=(unit tape)
    ?.(tx-exists ~ `"/spv-wallet/account/{pubkey-hex}/tx/{(trip txid)}")
  =/  addr-url=(unit tape)
    ?.(addr-exists ~ `"/spv-wallet/account/{pubkey-hex}/address/{(trip chain)}/{(scow %ud idx-addr)}")
  ::  Look up label for this UTXO (key format: "txid:vout")
  =/  label-key=@t  (crip "{(trip txid)}:{(scow %ud vout)}")
  =/  label-set=(unit (set label-entry:bip329))  (~(get by output-labels) label-key)
  ::  Check if frozen (any entry has spendable=%.n)
  =/  is-frozen=?
    ?~  label-set  %.n
    %+  lien  ~(tap in u.label-set)
    |=(e=label-entry:bip329 =([~ %.n] spendable.e))
  ::  Get all label texts sorted alphabetically
  =/  label-texts=(list @t)
    ?~  label-set  ~
    %+  sort  (turn ~(tap in u.label-set) |=(e=label-entry:bip329 label.e))
    |=([a=@t b=@t] (aor a b))
  =/  label-text=(unit @t)
    ?~  label-texts  ~
    `i.label-texts
  ::  Build JSON array of labels for modal
  =/  labels-json=tape
    ?~  label-texts  "[]"
    ::  Build ["label1","label2",...] format
    =/  quoted=(list tape)
      %+  turn  label-texts
      |=(l=@t (weld "\"" (weld (trip l) "\"")))
    =/  joined=tape
      ?~  quoted  ""
      =/  acc=tape  i.quoted
      |-  ^-  tape
      ?~  t.quoted  acc
      $(acc (weld acc (weld "," i.t.quoted)), t.quoted t.t.quoted)
    (weld "[" (weld joined "]"))
  ::  Check if this UTXO is in the draft inputs
  =/  is-selected=?
    ?~  draft  %.n
    =/  matching-input=(unit utxo-input:drft)
      |-
      ?~  inputs.u.draft  ~
      =/  input=utxo-input:drft  i.inputs.u.draft
      ?:  &(=(txid.input txid) =(vout.input vout))
        `input
      $(inputs.u.draft t.inputs.u.draft)
    ?=(^ matching-input)
  ;div.p3.b1.br2(style "display: flex; align-items: center; gap: 12px;")
    ::  Checkbox form (disabled when frozen)
    ;form(method "post", action "/spv-wallet/account/{pubkey-hex}/send", style "margin: 0;")
      ;input(type "hidden", name "action", value ?:(is-selected "remove-input" "add-input"));
      ;input(type "hidden", name "utxo-txid", value (trip txid));
      ;input(type "hidden", name "utxo-vout", value (scow %ud vout));
      ;input(type "hidden", name "utxo-value", value (numb:sailbox value));
      ;+  ?:  is-frozen
            ;button.p1.b0.br1
              =type  "submit"
              =title  "UTXO is frozen"
              =disabled  ""
              =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0; cursor: not-allowed; opacity: 0.4;"
              ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi ?:(is-selected 'check-square' 'square'))
              ==
            ==
          ;button.p1.b0.br1.hover.pointer
            =type  "submit"
            =title  ?:(is-selected "Remove from inputs" "Add to inputs")
            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0; cursor: pointer;"
            ;div(style "width: 16px; height: 16px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi ?:(is-selected 'check-square' 'square'))
            ==
          ==
    ==
      ;div(style "flex: 1; min-width: 0;{?:(is-frozen " opacity: 0.5;" "")}")
        ::  Transaction ID row with copy button
        ;div(style "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;")
          ;button.p1.b0.br1.hover.pointer
            =type  "button"
            =onclick  "copyToClipboard('{(trip txid)}'); event.stopPropagation();"
            =title  "Copy transaction ID"
            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'copy')
            ==
          ==
          ;+  ?~  tx-url
                ;span.mono.f2.s-1(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); flex: 1; min-width: 0;"): {(trip txid)}
              ;a.mono.f2.s-1(href u.tx-url, onclick "event.stopPropagation()", style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                ;span(style "overflow: hidden; text-overflow: ellipsis;"): {(trip txid)}
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                  ;+  (make:fi 'external-link')
                ==
              ==
        ==
        ::  Address row with copy button and amount
        ;div(style "display: flex; align-items: center; gap: 8px;")
          ;button.p1.b0.br1.hover.pointer
            =type  "button"
            =onclick  "copyToClipboard('{(trip address)}'); event.stopPropagation();"
            =title  "Copy address"
            =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
            ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
              ;+  (make:fi 'copy')
            ==
          ==
          ;span.f3.s-2.mono(style "opacity: 0.8; white-space: nowrap; flex-shrink: 0;"): Output #{(scow %ud vout)}
          ;+  ?~  addr-url
                ;span.mono.f2.s-1(style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); flex: 1; min-width: 0;"): {(trip address)}
              ;a.mono.f2.s-1(href u.addr-url, onclick "event.stopPropagation()", style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                ;span(style "overflow: hidden; text-overflow: ellipsis;"): {(trip address)}
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                  ;+  (make:fi 'external-link')
                ==
              ==
          ::  Amount on the right
          ;div.f3.s-2(style "white-space: nowrap; flex-shrink: 0; margin-left: auto;"): {(numb:sailbox value)} sats
        ==
      ==
      ::  Tag and lock buttons (outside faded area)
      ;div(style "display: flex; flex-direction: column; gap: 4px; flex-shrink: 0;")
        ::  Tag button for label (filled if label exists)
        ;button.p1.b0.br1.hover.pointer
          =type  "button"
          =title  ?~(label-text "No labels" (trip u.label-text))
          =onclick  "showLabelModal('{(trip txid)}', '{(scow %ud vout)}', {labels-json}); event.stopPropagation();"
          =style  "background: transparent; border: 1px solid var(--b3); color: {?~(label-text "var(--f3)" "var(--f1)")}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0; opacity: {?~(label-text "0.5" "1")};"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi 'tag')
          ==
        ==
        ::  Lock/unlock button
        ;button.p1.b0.br1.hover.pointer
          =type  "button"
          =title  ?:(is-frozen "Unlock UTXO" "Freeze UTXO")
          =onclick  "toggleFreeze('{(trip txid)}', '{(scow %ud vout)}', {?:(is-frozen "true" "false")}); event.stopPropagation();"
          =style  "background: transparent; border: 1px solid {?:(is-frozen "rgba(255,100,100,0.5)" "var(--b3)")}; color: {?:(is-frozen "rgba(255,100,100,0.8)" "var(--f3)")}; display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0; opacity: {?:(is-frozen "1" "0.5")};"
          ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
            ;+  (make:fi ?:(is-frozen 'lock' 'unlock'))
          ==
        ==
      ==
    ==
::
++  output-list
  |=  [draft=(unit transaction:drft) account-pubkey=@ux address-cache=(map @t address-suffix:hd-path)]
  ^-  manx
  =/  pubkey-hex=tape  (hexn:sailbox account-pubkey)
  ;div.fc.g3(style "max-height: 200px; overflow-y: auto;")
    ;*  ?~  draft
          :~  ;div.p4.b1.br2.tc
                ;div.s0.f2.mb2: No outputs yet
                ;div.f3.s-1: Add your first output below
              ==
          ==
        =/  outputs-list=(list output:drft)  outputs.u.draft
        =/  change-config=(unit change-config:drft)  change.u.draft
        ?:  &(?=(~ outputs-list) ?=(~ change-config))
          :~  ;div.p4.b1.br2.tc
                ;div.s0.f2.mb2: No outputs yet
                ;div.f3.s-1: Add your first output below
              ==
          ==
        =/  indexed-outputs=(list [@ud output:drft])
          =/  remaining=(list output:drft)  outputs-list
          =/  i=@ud  0
          |-
          ?~  remaining  ~
          [[i i.remaining] $(i +(i), remaining t.remaining)]
        ::  Build regular outputs list
        =/  output-rows=(list manx)
          %+  turn  indexed-outputs
          |=  [idx=@ud out=output:drft]
          ^-  manx
          =/  addr-info=(unit address-suffix:hd-path)  (~(get by address-cache) address.out)
          ;div.output-row.p3.b1.br2(id "output-row-{(scow %ud idx)}", style "display: flex; align-items: center; gap: 8px;")
            ;button.p1.b0.br1.hover.pointer
              =type  "button"
              =onclick  "copyToClipboard('{(trip address.out)}')"
              =title  "Copy address"
              =style  "background: transparent; border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                ;+  (make:fi 'copy')
              ==
            ==
            ;*  ?~  addr-info
                  :~  ;span.mono.f2.s-1(style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; flex: 1; min-width: 0;"): {(trip address.out)}
                  ==
                =/  addr-url=tape  "/spv-wallet/account/{pubkey-hex}"
                :~  ;a.mono.f2.s-1(href addr-url, style "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; color: var(--f3); text-decoration: none; display: flex; align-items: center; gap: 4px; flex: 1; min-width: 0;")
                      ;span(style "overflow: hidden; text-overflow: ellipsis;"): {(trip address.out)}
                      ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                        ;+  (make:fi 'external-link')
                      ==
                    ==
                ==
            ;span.f3.s-2(style "white-space: nowrap; flex-shrink: 0;"): {(scow %ud amount.out)} sats
            ;form(method "post", action "/spv-wallet/account/{pubkey-hex}/send", style "display: inline; margin: 0;")
              ;input(type "hidden", name "action", value "delete-output");
              ;input(type "hidden", name "output-index", value (scow %ud idx));
              ;button.p1.b0.br1.hover.pointer
                =type  "submit"
                =title  "Delete output"
                =style  "background: var(--b2); border: 1px solid var(--b3); color: var(--f3); display: flex; align-items: center; width: 24px; height: 24px; justify-content: center; outline: none; flex-shrink: 0;"
                ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center;")
                  ;+  (make:fi 'trash-2')
                ==
              ==
            ==
          ==
        output-rows
  ==
::
++  utxo-selection-list
  |=  $:  utxos=(list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
          draft=(unit transaction:drft)
          account-pubkey=@ux
          output-labels=(map @t (set label-entry:bip329))
          transactions=(map @t transaction)
          address-cache=(map @t address-suffix:hd-path)
      ==
  ^-  manx
  ;div.fc.g2
    ;*  =/  indexed-utxos=(list [@ud @t @ud @ud @t @t @ud (unit @ud)])
          =/  remaining=(list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])  utxos
          =/  i=@ud  0
          |-
          ?~  remaining  ~
          :-  [i i.remaining]
          $(i +(i), remaining t.remaining)
        %+  turn  indexed-utxos
        |=  [idx=@ud txid=@t vout=@ud value=@ud address=@t chain=@t idx-addr=@ud confs=(unit @ud)]
        (utxo-checkbox-item idx txid vout value address chain idx-addr confs account-pubkey draft output-labels transactions address-cache)
  ==
::  JavaScript for send page - clipboard, change config, auto-select, labels, freeze
::
++  send-page-scripts
  |=  [next-change-addr-tape=tape]
  ^-  marl
  :~
    ;script
      ; function copyToClipboard(text) {
      ;   navigator.clipboard.writeText(text).then(function() {
      ;     console.log('Copied to clipboard');
      ;   }).catch(function(err) {
      ;     console.error('Failed to copy: ', err);
      ;   });
      ; }
    ==
    ;script: var changeAddress = '{next-change-addr-tape}';
    ;script
      ; function toggleChangeAddress(enabled) {
      ;   document.getElementById('change-details').style.display = enabled ? 'block' : 'none';
      ;   if (enabled) {
      ;     updateChangeConfig();
      ;   } else {
      ;     clearChangeConfig();
      ;   }
      ; }
      ; function updateChangeConfig() {
      ;   if (!changeAddress) return;
      ;   var feeRate = parseInt(document.getElementById('fee-rate').value) || 10;
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', 'set-change-config');
      ;   formData.append('fee-rate', feeRate.toString());
      ;   formData.append('change-address', changeAddress);
      ;   fetch(window.location.pathname, {method: 'POST', body: formData});
      ; }
      ; function clearChangeConfig() {
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', 'clear-change-config');
      ;   fetch(window.location.pathname, {method: 'POST', body: formData});
      ; }
      ; function toggleAutoSelect(enabled) {
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', enabled ? 'enable-auto-select' : 'disable-auto-select');
      ;   fetch(window.location.pathname, {method: 'POST', body: formData});
      ; }
      ; function setAutoSelectMode(mode) {
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', 'enable-auto-select');
      ;   formData.append('mode', mode);
      ;   fetch(window.location.pathname, {method: 'POST', body: formData});
      ; }
      ; function refreshAutoSelect() {
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', 'refresh-auto-select');
      ;   fetch(window.location.pathname, {method: 'POST', body: formData});
      ; }
      ; var labelState = {txid: '', vout: '', original: [], current: [], deleted: []};
      ; function showLabelModal(txid, vout, labels) {
      ;   var sorted = labels.slice().sort();
      ;   labelState = {txid: txid, vout: vout, original: sorted.slice(), current: sorted.slice(), deleted: []};
      ;   document.getElementById('label-txid').value = txid;
      ;   document.getElementById('label-vout').value = vout;
      ;   document.getElementById('new-label-input').value = '';
      ;   renderLabels();
      ;   document.getElementById('label-modal').style.display = 'flex';
      ;   document.getElementById('new-label-input').focus();
      ; }
      ; function hideLabelModal() {
      ;   document.getElementById('label-modal').style.display = 'none';
      ; }
      ; function resetLabels() {
      ;   labelState.current = labelState.original.slice();
      ;   labelState.deleted = [];
      ;   document.getElementById('new-label-input').value = '';
      ;   renderLabels();
      ; }
      ; function hasChanges() {
      ;   if (labelState.deleted.length > 0) return true;
      ;   if (labelState.current.length !== labelState.original.length) return true;
      ;   for (var i = 0; i < labelState.current.length; i++) {
      ;     if (labelState.current[i] !== labelState.original[i]) return true;
      ;   }
      ;   return false;
      ; }
      ; function renderLabels() {
      ;   var container = document.getElementById('labels-list');
      ;   var hidden = document.getElementById('labels-hidden');
      ;   var saveBtn = document.getElementById('label-save-btn');
      ;   var html = '';
      ;   labelState.deleted.forEach(function(label) {
      ;     html += '<div style="display: flex; align-items: center; gap: 8px; padding: 4px 8px; background: rgba(255,100,100,0.1); border-radius: 4px; margin-bottom: 4px; opacity: 0.6;">' +
      ;       '<span style="flex: 1; text-decoration: line-through; color: var(--f3);">' + label + '</span>' +
      ;       '<button type="button" onclick="restoreLabel(\'' + label + '\')" style="background: none; border: none; color: var(--f3); cursor: pointer; padding: 2px 6px; font-size: 12px;">restore</button>' +
      ;     '</div>';
      ;   });
      ;   labelState.current.forEach(function(label, idx) {
      ;     var isNew = !labelState.original.includes(label);
      ;     var bg = isNew ? 'rgba(100,200,100,0.15)' : 'var(--b2)';
      ;     html += '<div style="display: flex; align-items: center; gap: 8px; padding: 4px 8px; background: ' + bg + '; border-radius: 4px; margin-bottom: 4px;">' +
      ;       '<span style="flex: 1;">' + label + '</span>' +
      ;       '<button type="button" onclick="removeLabelAt(' + idx + ')" style="background: none; border: none; color: var(--f3); cursor: pointer; padding: 2px 6px; opacity: 0.6;">&times;</button>' +
      ;     '</div>';
      ;   });
      ;   if (html === '') {
      ;     html = '<div style="color: var(--f3); opacity: 0.6; padding: 8px 0;">No labels</div>';
      ;   }
      ;   container.innerHTML = html;
      ;   hidden.value = labelState.current.join(',');
      ;   var changed = hasChanges();
      ;   saveBtn.disabled = !changed;
      ;   saveBtn.style.opacity = changed ? '1' : '0.4';
      ;   saveBtn.style.cursor = changed ? 'pointer' : 'not-allowed';
      ;   var resetBtn = document.getElementById('label-reset-btn');
      ;   resetBtn.disabled = !changed;
      ;   resetBtn.style.opacity = changed ? '0.7' : '0.3';
      ;   resetBtn.style.cursor = changed ? 'pointer' : 'not-allowed';
      ; }
      ; function addLabel() {
      ;   var input = document.getElementById('new-label-input');
      ;   var label = input.value.trim();
      ;   if (label && !labelState.current.includes(label)) {
      ;     var delIdx = labelState.deleted.indexOf(label);
      ;     if (delIdx >= 0) labelState.deleted.splice(delIdx, 1);
      ;     labelState.current.push(label);
      ;     labelState.current.sort();
      ;     renderLabels();
      ;   }
      ;   input.value = '';
      ;   input.focus();
      ; }
      ; function removeLabelAt(idx) {
      ;   var label = labelState.current[idx];
      ;   labelState.current.splice(idx, 1);
      ;   if (labelState.original.includes(label)) {
      ;     labelState.deleted.push(label);
      ;     labelState.deleted.sort();
      ;   }
      ;   renderLabels();
      ; }
      ; function restoreLabel(label) {
      ;   var idx = labelState.deleted.indexOf(label);
      ;   if (idx >= 0) {
      ;     labelState.deleted.splice(idx, 1);
      ;     labelState.current.push(label);
      ;     labelState.current.sort();
      ;     renderLabels();
      ;   }
      ; }
      ; document.addEventListener('keydown', function(e) {
      ;   if (e.key === 'Escape') hideLabelModal();
      ; });
      ; function toggleFreeze(txid, vout, isFrozen) {
      ;   var action = isFrozen ? 'unfreeze' : 'freeze';
      ;   var msg = isFrozen ? 'Unlock this UTXO?\n\nIt will become spendable again.' : 'Freeze this UTXO?\n\nIt will be excluded from spending until unlocked.';
      ;   if (!confirm(msg)) return;
      ;   var formData = new URLSearchParams();
      ;   formData.append('action', 'set-utxo-frozen');
      ;   formData.append('utxo-txid', txid);
      ;   formData.append('utxo-vout', vout);
      ;   formData.append('frozen', isFrozen ? 'false' : 'true');
      ;   fetch(window.location.pathname.replace('/send', ''), {
      ;     method: 'POST',
      ;     body: formData
      ;   }).then(function(response) {
      ;     if (response.ok) {
      ;       window.location.reload();
      ;     } else {
      ;       alert('Failed to ' + action + ' UTXO');
      ;     }
      ;   }).catch(function(err) {
      ;     console.error('Failed to ' + action + ':', err);
      ;     alert('Failed to ' + action + ' UTXO');
      ;   });
      ; }
      ; function saveLabels(event) {
      ;   event.preventDefault();
      ;   var added = labelState.current.filter(function(l) { return !labelState.original.includes(l); });
      ;   var removed = labelState.deleted;
      ;   var msg = 'Save label changes?\n';
      ;   if (added.length > 0) msg += '\nAdding: ' + added.join(', ');
      ;   if (removed.length > 0) msg += '\nRemoving: ' + removed.join(', ');
      ;   if (!confirm(msg)) return;
      ;   var form = event.target;
      ;   var formData = new FormData(form);
      ;   var saveBtn = document.getElementById('label-save-btn');
      ;   var url = form.getAttribute('action');
      ;   saveBtn.disabled = true;
      ;   saveBtn.querySelector('span').textContent = 'Saving...';
      ;   fetch(url, {
      ;     method: 'POST',
      ;     body: new URLSearchParams(formData)
      ;   }).then(function(response) {
      ;     if (response.ok) {
      ;       labelState.original = labelState.current.slice();
      ;       labelState.deleted = [];
      ;       renderLabels();
      ;       saveBtn.querySelector('span').textContent = 'Saved!';
      ;       setTimeout(function() {
      ;         saveBtn.querySelector('span').textContent = 'Save';
      ;       }, 1500);
      ;     } else {
      ;       saveBtn.querySelector('span').textContent = 'Error';
      ;       setTimeout(function() {
      ;         saveBtn.querySelector('span').textContent = 'Save';
      ;         renderLabels();
      ;       }, 1500);
      ;     }
      ;   }).catch(function(err) {
      ;     console.error('Save failed:', err);
      ;     saveBtn.querySelector('span').textContent = 'Error';
      ;     setTimeout(function() {
      ;       saveBtn.querySelector('span').textContent = 'Save';
      ;       renderLabels();
      ;     }, 1500);
      ;   });
      ; }
    ==
  ==
::  Label editing modal component
::
++  label-edit-modal
  |=  account-pubkey-hex=tape
  ^-  manx
  ;div#label-modal(style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
    ;form.p4.b1.br2
      =method  "post"
      =action  "/spv-wallet/account/{account-pubkey-hex}"
      =onsubmit  "saveLabels(event)"
      =style  "min-width: 320px; max-width: 400px; position: relative;"
      ;input(type "hidden", name "action", value "set-output-labels");
      ;input(type "hidden", name "label-type", value "output");
      ;input(type "hidden", name "utxo-txid", id "label-txid", value "");
      ;input(type "hidden", name "utxo-vout", id "label-vout", value "");
      ;input(type "hidden", name "labels", id "labels-hidden", value "");
      ::  Header with title and X button
      ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;")
        ;h3.s1.bold: Edit Labels
        ;button.hover.pointer(type "button", onclick "hideLabelModal()", style "background: none; border: none; color: var(--f3); cursor: pointer; font-size: 20px; padding: 0 4px; opacity: 0.6;")
          ;span: ×
        ==
      ==
      ::  Labels list container
      ;div#labels-list(style "margin-bottom: 12px; max-height: 150px; overflow-y: auto;")
        ;div(style "color: var(--f3); opacity: 0.6; padding: 8px 0;"): No labels
      ==
      ::  Add new label input
      ;div(style "display: flex; gap: 8px; margin-bottom: 16px;")
        ;input#new-label-input.p2.b2.br1(type "text", placeholder "Add label...", style "outline: none; flex: 1;", onkeydown "if(event.key === 'Enter') \{ event.preventDefault(); addLabel(); }");
        ;button.p2.b1.br1.hover.pointer(type "button", onclick "addLabel()", style "background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); outline: none; color: var(--f3);")
          ;span: Add
        ==
      ==
      ;div(style "display: flex; gap: 12px; justify-content: space-between;")
        ;button#label-reset-btn.p2.b1.br1(type "button", onclick "resetLabels()", style "background: var(--b2); color: var(--f3); border: none; outline: none; opacity: 0.3; cursor: not-allowed;", disabled "true")
          ;span: Reset
        ==
        ;button#label-save-btn.p2.b1.br1(type "submit", style "background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); outline: none; color: var(--f3); opacity: 0.4; cursor: not-allowed;", disabled "true")
          ;span: Save
        ==
      ==
    ==
  ==
::  Change address/fee configuration section
::
++  change-address-section
  |=  $:  has-change-config=?
          fee-rate=@ud
          est-fee=@ud
          est-vbytes=@ud
          change-result=change-result:fees
          next-change-addr=(unit @t)
          next-change-addr-url=(unit tape)
      ==
  ^-  manx
  ;div.p3.b1.br2.mb2(style "border: 1px dashed var(--b3);")
    ;div(style "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;")
      ;label.pointer(style "display: flex; align-items: center; gap: 8px;")
        ;+  ?:  has-change-config
              ;input#use-change(type "checkbox", checked "", onchange "toggleChangeAddress(this.checked)", style "width: 16px; height: 16px; cursor: pointer;");
            ;input#use-change(type "checkbox", onchange "toggleChangeAddress(this.checked)", style "width: 16px; height: 16px; cursor: pointer;");
        ;span.f2.bold: Send change to self
      ==
    ==
    ;div#change-details(style "display: {?:(has-change-config "block" "none")};")
      ;div(style "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;")
        ;label.f3(style "white-space: nowrap;"): Fee rate:
        ;input#fee-rate.p2.b1.br1.mono(type "number", min "1", value "{(scow %ud fee-rate)}", oninput "updateChangeConfig()", style "width: 80px; background: var(--b1); border: 1px solid var(--b3); color: var(--f2); outline: none;");
        ;span.f3: sat/vB
      ==
      ;+  ?~  next-change-addr
            ;div(style "display: flex; align-items: center; gap: 8px; padding: 8px; background: rgba(255,100,100,0.1); border: 1px solid rgba(255,100,100,0.3); border-radius: 4px;")
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; color: rgba(255,100,100,0.8);")
                ;+  (make:fi 'alert-triangle')
              ==
              ;span.f3(style "color: rgba(255,100,100,0.9);"): No unused change address available - derive more addresses first
            ==
          ;div(style "display: flex; flex-direction: column; gap: 4px; padding: 8px; background: var(--b2); border-radius: 4px;")
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;span.f3(style "opacity: 0.6;"): Est. fee:
              ;span#est-fee.mono.f2: {(scow %ud est-fee)} sats ({(scow %ud est-vbytes)} vB × {(scow %ud fee-rate)} sat/vB)
            ==
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; opacity: 0.6;")
                ;+  (make:fi 'corner-down-left')
              ==
              ;span.f3(style "opacity: 0.8;"): Change:
              ;+  ?-  -.change-result
                    %ok  ;span#change-amount.mono.f2: {(scow %ud amount.change-result)} sats
                    %insufficient  ;span#change-amount.mono.f2(style "background: rgba(220,80,80,0.9); color: white; padding: 2px 6px; border-radius: 3px;"): need {(scow %ud shortfall.change-result)} more sats
                    %dust  ;span#change-amount.mono.f2(style "background: rgba(200,150,50,0.9); color: white; padding: 2px 6px; border-radius: 3px;"): {(scow %ud amount.change-result)} sats → fee (dust)
                  ==
            ==
            ;div(style "display: flex; align-items: center; gap: 8px;")
              ;span.f3(style "opacity: 0.6;"): To:
              ;+  ?~  next-change-addr-url
                    ;span.mono.f3.s-1(style "opacity: 0.8; overflow: hidden; text-overflow: ellipsis;"): {(trip u.next-change-addr)}
                  ;a.mono.f3.s-1.hover(href u.next-change-addr-url, style "color: var(--f2); text-decoration: none; display: flex; align-items: center; gap: 4px;")
                    ;span(style "overflow: hidden; text-overflow: ellipsis;"): {(trip u.next-change-addr)}
                    ;div(style "width: 12px; height: 12px; display: flex; align-items: center; justify-content: center; flex-shrink: 0;")
                      ;+  (make:fi 'external-link')
                    ==
                  ==
            ==
          ==
    ==
  ==
::  Auto-select controls (checkbox + mode radios + target)
::
++  auto-select-controls
  |=  [has-auto-select=? is-random=? is-largest=? target=@ud]
  ^-  manx
  ;div#auto-select-controls(style "margin-bottom: 8px;")
    ;div(style "display: flex; align-items: center; gap: 8px;")
      ;label.pointer(style "display: flex; align-items: center; gap: 8px;")
        ;+  ?:  has-auto-select
              ;input#auto-select-toggle(type "checkbox", checked "", onchange "toggleAutoSelect(this.checked)", style "width: 16px; height: 16px; cursor: pointer;");
            ;input#auto-select-toggle(type "checkbox", onchange "toggleAutoSelect(this.checked)", style "width: 16px; height: 16px; cursor: pointer;");
        ;span.f3: Auto-select UTXOs
      ==
      ;+  ?:  has-auto-select
            ;span.f3(style "opacity: 0.6;"): (target: {(scow %ud target)} sats)
          ;span;
    ==
    ;div#auto-select-mode-row(style "margin-left: 24px; margin-top: 8px; display: {?:(has-auto-select "flex" "none")}; align-items: center; gap: 16px;")
      ;button.p1.b1.br1.hover.pointer
        =type  "button"
        =onclick  "refreshAutoSelect()"
        =title  "Re-select UTXOs"
        =style  "background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); color: var(--f3); display: flex; align-items: center; justify-content: center; width: 28px; height: 28px; outline: none;"
        ;div(style "width: 14px; height: 14px; display: flex; align-items: center; justify-content: center;")
          ;+  (make:fi 'refresh-cw')
        ==
      ==
      ;label.pointer(style "display: flex; align-items: center; gap: 4px;")
        ;+  ?:  is-random
              ;input(type "radio", name "auto-select-mode", value "random", checked "", onchange "setAutoSelectMode('random')", style "cursor: pointer;");
            ;input(type "radio", name "auto-select-mode", value "random", onchange "setAutoSelectMode('random')", style "cursor: pointer;");
        ;span.f3: Random
      ==
      ;label.pointer(style "display: flex; align-items: center; gap: 4px;")
        ;+  ?:  is-largest
              ;input(type "radio", name "auto-select-mode", value "largest-first", checked "", onchange "setAutoSelectMode('largest-first')", style "cursor: pointer;");
            ;input(type "radio", name "auto-select-mode", value "largest-first", onchange "setAutoSelectMode('largest-first')", style "cursor: pointer;");
        ;span.f3: Largest first
      ==
    ==
  ==
::  Add output form
::
++  add-output-form
  |=  account-pubkey-hex=tape
  ^-  manx
  ;form.p3.b2.br2.fc
    =method  "post"
    =action  "/spv-wallet/account/{account-pubkey-hex}/send"
    =style  "gap: 8px;"
    ;input(type "hidden", name "action", value "add-output");
    ;h3.s0.bold: Add Output
    ;div(style "display: flex; gap: 8px; width: 100%;")
      ;div(style "flex: 2;")
        ;label.f3.bold(style "display: block; margin-bottom: 4px;"): Address
        ;input.p2.b1.br1
          =type  "text"
          =name  "output-address"
          =placeholder  "bc1q... or tb1q..."
          =required  "true"
          =style  "outline: none; width: 100%;";
      ==
      ;div(style "flex: 1;")
        ;label.f3.bold(style "display: block; margin-bottom: 4px;"): Amount (sats)
        ;input.p2.b1.br1
          =type  "number"
          =name  "output-amount"
          =placeholder  "10000"
          =required  "true"
          =min  "1"
          =style  "outline: none; width: 100%;";
      ==
    ==
    ;button.p2.b1.br2.hover.pointer
      =type  "submit"
      =style  "background: rgba(100, 150, 255, 0.15); border: 1px solid rgba(100, 150, 255, 0.4); outline: none; color: var(--f3); width: 100%;"
      ;span.f3: + Add Output
    ==
  ==
::  Action buttons (clear/send) for bottom of send page
::
++  send-action-buttons
  |=  [account-pubkey-hex=tape has-tapscript-inputs=?]
  ^-  manx
  =/  confirm-msg=tape
    ?.  has-tapscript-inputs
      "Broadcast this transaction to the Bitcoin testnet?"
    "Some selected inputs are tapscript UTXOs. They will be spent via key-path (the tapscript will NOT be revealed on-chain). Continue with broadcast?"
  ;div.p4.b2.br2(style "display: flex; gap: 12px; justify-content: center;")
    ;form(method "post", action "/spv-wallet/account/{account-pubkey-hex}/send", style "display: inline;", onsubmit "return confirm('Clear all transaction outputs? This cannot be undone.');")
      ;input(type "hidden", name "action", value "clear-draft");
      ;button.p3.b1.br2.hover.pointer
        =type  "submit"
        =style  "background: rgba(255, 100, 100, 0.15); border: 1px solid rgba(255, 100, 100, 0.4); outline: none; color: var(--f3); min-width: 120px;"
        ;span.f2: Clear
      ==
    ==
    ;form(method "post", action "/spv-wallet/account/{account-pubkey-hex}/send", style "display: inline;", onsubmit "return confirm('{confirm-msg}');")
      ;input(type "hidden", name "action", value "build-transaction");
      ;button.p3.b1.br2.hover.pointer
        =type  "submit"
        =style  "background: rgba(100, 200, 100, 0.2); border: 1px solid rgba(100, 200, 100, 0.5); outline: none; color: var(--f3); min-width: 120px;"
        ;span.f2: Send
      ==
    ==
  ==
::
++  send-page
  |=  [account-pubkey=@ux wallets=(map @ux wallet) accounts=(map @ux account-details) =labels:bip329]
  ^-  manx
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    %-  htmx-page
    :^  "Account Not Found"  &  ~
    ;div.fc.g3.p5.ma.mw-page
      ;h1: Account Not Found
      ;a(href "/spv-wallet"): ← Back to Accounts
    ==
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ::  Get output labels from global labels
  =/  output-labels=(map @t (set label-entry:bip329))
    output.labels
  ::  Get next unused change address and its index for linking
  =/  next-change-addr=(unit @t)  (get-next-unused-address change:ac)
  =/  next-change-addr-tape=tape  ?~(next-change-addr "" (trip u.next-change-addr))
  =/  account-pubkey-hex=tape  (hexn:sailbox account-pubkey)
  ::  Look up address index from cache for URL
  =/  next-change-addr-url=(unit tape)
    ?~  next-change-addr  ~
    =/  suffix=(unit address-suffix:hd-path)  (~(get by address-cache:ac) u.next-change-addr)
    ?~  suffix  ~
    =/  idx=@ud  q.index.u.suffix
    `"/spv-wallet/account/{account-pubkey-hex}/address/change/{(scow %ud idx)}"
  =/  account-url=tape  "/spv-wallet/account/{account-pubkey-hex}"
  =/  total-balance=@ud  (compute-account-balance u.details)
  =/  utxos=(list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
    (collect-cached-utxos u.details)
  ::  Calculate fee info
  =/  fi=fee-calc  (compute-fee-info draft:ac)
  =/  dr  draft:ac
  =/  auto-select-mode=(unit ?(%random %largest-first))
    ?~  dr  ~
    auto-select.u.dr
  =/  has-auto-select=?  ?=(^ auto-select-mode)
  =/  is-random=?  =(auto-select-mode `%random)
  =/  is-largest=?  =(auto-select-mode `%largest-first)
  ::  Check if any draft inputs are tapscript UTXOs (key-path warning)
  =/  tapscript-addrs=(set @t)
    =/  recv-list=(list [@ud hd-leaf])
      (tap:((on @ud hd-leaf) gth) receiving:ac)
    =/  change-list=(list [@ud hd-leaf])
      (tap:((on @ud hd-leaf) gth) change:ac)
    =/  addrs=(set @t)  ~
    =.  addrs
      |-
      ?~  recv-list  addrs
      =/  leaf=hd-leaf  +.i.recv-list
      $(recv-list t.recv-list, addrs (~(uni in addrs) ~(key by script-trees.leaf)))
    |-
    ?~  change-list  addrs
    =/  leaf=hd-leaf  +.i.change-list
    $(change-list t.change-list, addrs (~(uni in addrs) ~(key by script-trees.leaf)))
  =/  has-tapscript-inputs=?
    ?~  dr  %.n
    %+  lien  inputs.u.dr
    |=  din=utxo-input:drft
    %+  lien  utxos
    |=  [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)]
    ?&  =(txid.din txid)
        =(vout.din vout)
        (~(has in tapscript-addrs) address)
    ==
  %-  htmx-page
  :^  "Send - {(trip name.u.details)}"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;*  (send-page-scripts next-change-addr-tape)
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;")
      ;a.hover.pointer(href account-url, style "color: var(--f3); text-decoration: none;"): ← Back to Account
    ==
    ::  Header with account context
    ;div.p4.b1.br2
      ;h1.s2.bold(style "margin-bottom: 8px;")
        ; Send Bitcoin
        ;span(style "opacity: 0.4; margin: 0 8px;"): |
        ;span.f2(style "opacity: 0.5; font-weight: normal;"): {(trip name.u.details)}
      ==
      ;div.f2(style "margin-top: 4px;"): Available: {(scow %ud total-balance)} sats
      ;+  (fee-info-bar fi)
    ==
    ::  Single SSE connection for draft updates (outputs, UTXOs, and fee are all updated via OOB swaps)
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/account/{(hexn:sailbox account-pubkey)}/send", sse-swap "draft-outputs-update", style "display:none;");
    ::  UTXO Selection section (no form wrapper - each checkbox has its own form)
    ;div.p4.b2.br2
      ;h2.s1.bold.mb2: Select UTXOs (Inputs)
      ;+  (auto-select-controls has-auto-select is-random is-largest target.fi)
      ;div.f3(style "opacity: 0.8; margin-bottom: 8px;"): Select which coins to spend
      ;+  ?~  utxos
            ;div.p3.b1.br2.f3: No UTXOs available
          ;div(id "utxo-selection-list", style "max-height: 150px; overflow-y: auto;")
            ;+  (utxo-selection-list utxos draft:ac account-pubkey output-labels transactions:ac address-cache:ac)
          ==
    ==
    ;+  (add-output-form account-pubkey-hex)
    ::  Draft outputs section
    ;div.p4.b2.br2
      ;h2.s1.bold.mb2: Transaction Outputs
      ;div.f3(style "opacity: 0.8; margin-bottom: 12px;"): Draft outputs for this transaction
      ;+  (change-address-section has-change-config.fi fee-rate.fi est-fee.fi est-vbytes.fi change-result.fi next-change-addr next-change-addr-url)
      ;div#output-list
        ;+  (output-list draft:ac account-pubkey address-cache:ac)
      ==
    ==
    ;+  (send-action-buttons account-pubkey-hex has-tapscript-inputs)
    ;+  (label-edit-modal account-pubkey-hex)
  ==
::
++  handle-send-sse
  |=  $:  =bowl:gall
          state=vase
          account-pubkey=@ux
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0 state)
  =/  details=(unit account-details)  (~(get by accounts) account-pubkey)
  ?~  details
    %-  manx-to-wain:sailbox
    ;div: Account not found
  ::  Get output labels from global labels
  =/  output-labels=(map @t (set label-entry:bip329))
    output.labels
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ?+    event  !!
      [~ %draft-outputs-update]
    ~&  >>  "=== SSE HANDLER: draft-outputs-update EVENT ==="
    ::  Build output list wrapper
    =/  output-list-manx=manx  (output-list draft:ac account-pubkey address-cache:ac)
    =/  output-wrapped=manx
      :_  [output-list-manx ~]
      :-  %div
      :~  [%id "output-list"]
          [%hx-swap-oob "true"]
      ==
    ::  Build UTXO selection list wrapper
    =/  utxos=(list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
      (collect-cached-utxos u.details)
    =/  utxo-list-manx=manx  (utxo-selection-list utxos draft:ac account-pubkey output-labels transactions:ac address-cache:ac)
    =/  utxo-wrapped=manx
      :_  [utxo-list-manx ~]
      :-  %div
      :~  [%id "utxo-selection-list"]
          [%hx-swap-oob "true"]
      ==
    ::  Calculate fee info and auto-select state
    =/  fi=fee-calc  (compute-fee-info draft:ac)
    =/  dr  draft:ac
    =/  auto-select-mode=(unit ?(%random %largest-first))
      ?~  dr  ~
      auto-select.u.dr
    =/  has-auto-select=?  ?=(^ auto-select-mode)
    =/  is-random=?  =(auto-select-mode `%random)
    =/  is-largest=?  =(auto-select-mode `%largest-first)
    ::  Build auto-select controls with OOB swap
    =/  auto-select-manx=manx  (auto-select-controls has-auto-select is-random is-largest target.fi)
    =/  auto-select-wrapped=manx
      auto-select-manx(a.g [[%hx-swap-oob "true"] a.g.auto-select-manx])
    ::  Build fee info wrapper with OOB swap
    =/  fee-wrapped=manx
      :_  [(fee-info-bar fi) ~]
      :-  %div
      :~  [%id "fee-info"]
          [%hx-swap-oob "true"]
      ==
    ::  Build change details elements for OOB swap
    =/  est-fee-manx=manx
      ;span#est-fee.mono.f2(hx-swap-oob "true"): {(scow %ud est-fee.fi)} sats ({(scow %ud est-vbytes.fi)} vB × {(scow %ud fee-rate.fi)} sat/vB)
    =/  change-amount-manx=manx
      ?-  -.change-result.fi
        %ok  ;span#change-amount.mono.f2(hx-swap-oob "true"): {(scow %ud amount.change-result.fi)} sats
        %insufficient  ;span#change-amount.mono.f2(hx-swap-oob "true", style "background: rgba(220,80,80,0.9); color: white; padding: 2px 6px; border-radius: 3px;"): need {(scow %ud shortfall.change-result.fi)} more sats
        %dust  ;span#change-amount.mono.f2(hx-swap-oob "true", style "background: rgba(200,150,50,0.9); color: white; padding: 2px 6px; border-radius: 3px;"): {(scow %ud amount.change-result.fi)} sats → fee (dust)
      ==
    ::  Return all elements
    %-  manx-to-wain:sailbox
    ;div
      ;+  output-wrapped
      ;+  utxo-wrapped
      ;+  auto-select-wrapped
      ;+  fee-wrapped
      ;+  est-fee-manx
      ;+  change-amount-manx
    ==
  ==
--
