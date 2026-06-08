::  Frozen types for state-0
::
::  These are snapshots of types as they existed at state-0.
::  They must never be modified — only the migration in
::  app/spv-wallet.hoon should reference them.
::
/-  bitcoin-spv, hd-path, indexer, tt=transactions, urb
/+  tarball, txns=tx-build, drft=tx-draft, bip329, taproot
|%
+$  spv-chain  spv-chain:bitcoin-spv
+$  network  network:tt
::
+$  tx-status
  $%  [%unconfirmed ~]
      [%confirmed block-hash=@t block-height=@ud]
  ==
::
+$  address-info
  $:  address=@t
      tx-count=@ud
      chain-funded=@ud
      chain-spent=@ud
      mempool-funded=@ud
      mempool-spent=@ud
  ==
::
+$  tx-input
  $:  spent-txid=@t
      spent-vout=@ud
      prevout=(unit tx-output)
      witness=(list @t)
  ==
::
+$  tx-output
  $:  value=@ud
      address=@t
  ==
::
+$  transaction
  $:  txid=@t
      inputs=(list tx-input)
      outputs=(list tx-output)
      =tx-status
      fee=(unit @ud)
      size=(unit @ud)
  ==
::
+$  address-details
  $:  address=@t
      last-check=(unit @da)
      info=(unit address-info)
      indexer-history=sh-tx-history:indexer
      utxos=(list [txid=@t vout=@ud value=@ud =tx-status])
  ==
::
+$  tapscript-details  [name=@t =ptst:taproot =address-details]
::
+$  hd-leaf
  $:  main=address-details
      script-trees=(map @t tapscript-details)
  ==
::
+$  network-details
  $:  change=((mop @ud hd-leaf) gth)
      receiving=((mop @ud hd-leaf) gth)
      address-cache=(map @t address-suffix:hd-path)
      transactions=(map @t transaction)
      tx-addresses=(set @t)
      tx-verification=(map @t (unit (unit tang)))
      draft=(unit transaction:drft)
  ==
::
+$  script-type
  $?  %p2pkh
      %p2sh-p2wpkh
      %p2wpkh
      %p2tr
  ==
::
++  account-scan
  $%  [%1 idx=@ud gap=@ud]
      [%2 idx=@ud gap=@ud]
  ==
::
+$  account-details
  $:  name=@t
      wallet=(unit @ux)
      extended-key=$%([%xpub k=@t] [%xprv k=@t])
      =script-type
      active-network=network
      networks=(map network network-details)
      $=  proc
      $:  scan=(unit [pid=@ta act=? scn=account-scan])
          change=(map @ud [pid=@ta act=?])
          receiving=(map @ud [pid=@ta act=?])
          tx-verify=(map @t [pid=@ta act=?])
          tapscript=(map @t [pid=@ta act=?])
      ==
      indexer-registered=?
  ==
::
+$  seed
  $%  [%q =@q]
      [%t =@t]
  ==
::
+$  wallet
  $:  name=@t
      =seed
      fingerprint=@ux
      accounts=(map account:hd-path @ux)
      scan=(map coin-type:hd-path [pid=@ta act=? idx=@ud scn=account-scan])
  ==
::
+$  indexer-sub
  $:  account-pubkey=@ux
      chain=@t
      index=@ud
      address=@t
      pid=@ta
      act=?
      last-update=(unit @da)
  ==
::
+$  boot-mode  ?(%sponsor %normal)
::
+$  boot-data
  $:  =boot-mode
      boot-secret=@q
      fief=(unit fief:urb)
      sponsor=(unit @p)
      wallet-pubkey=(unit @ux)
      account-pubkey=(unit @ux)
      address=(unit @t)
      selected-utxo=(unit [txid=@t vout=@ud value=@ud])
      sponsor-sig=(unit @)
      spawn-script=(unit @)
      spawn-script-wid=(unit @ud)
      commit-address=(unit @t)
      commit-txid=(unit @t)
      reveal-address=(unit @t)
      reveal-txid=(unit @t)
  ==
::
+$  boot-state
  $:  step=@tas
      data=boot-data
      error=(unit [term tang])
  ==
::
+$  state-0
  $:  %0
      auto-sponsor=_&
      boot=(unit boot-state)
      sponsor-response=(unit [sig=@ height=@ud])
      accounts=(map @ux account-details)
      watch-only=(set @ux)
      signing=(set @ux)
      wallets=(map @ux wallet)
      =labels:bip329
      spv=(map network spv-chain)
      indexer-subs=(map @ux indexer-sub)
      hide-empty-addresses=?
      binding=binding:eyre
      counter=@ud
  ==
--
