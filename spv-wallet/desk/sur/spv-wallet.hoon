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
+$  address-details
  $:  address=@t
      last-check=(unit @da)
      info=(unit address-info)  :: canonical form
      indexer-history=sh-tx-history:indexer
      utxos=(list [txid=@t vout=@ud value=@ud =tx-status])
  ==
::
+$  tapscript-details  [name=@t =ptst:taproot =address-details]
::
::  HD wallet leaf - one derivation path endpoint (one pubkey)
::  Can have a main address plus multiple tapscript addresses
::
+$  hd-leaf
  $:  main=address-details
      script-trees=(map @t tapscript-details)
  ==
::  Canonical address information (API-independent)
::
+$  address-info
  $:  address=@t
      tx-count=@ud
      chain-funded=@ud        :: total satoshis received (confirmed)
      chain-spent=@ud         :: total satoshis spent (confirmed)
      mempool-funded=@ud      :: total satoshis received (unconfirmed)
      mempool-spent=@ud       :: total satoshis spent (unconfirmed)
  ==
::  Transaction components (API-independent)
::
+$  tx-input
  $:  spent-txid=@t
      spent-vout=@ud
      prevout=(unit tx-output)  :: details of the output being spent (if available)
      witness=(list @t)         :: witness stack items (hex strings)
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
++  account-scan
  $%  [%1 idx=@ud gap=@ud] :: Phase 1 - receiving address scan
      [%2 idx=@ud gap=@ud] :: Phase 2 - change address *derivation*
  ==
::
+$  script-type
  $?  %p2pkh         :: BIP44, purpose 44, legacy
      %p2sh-p2wpkh   :: BIP49, purpose 49, wrapped segwit
      %p2wpkh        :: BIP84, purpose 84, native segwit
      %p2tr          :: BIP86, purpose 86, taproot
  ==
::
+$  network-details
  $:  change=((mop @ud hd-leaf) gth)
      receiving=((mop @ud hd-leaf) gth)
      address-cache=(map @t address-suffix:hd-path)
      transactions=(map @t transaction)
      tx-addresses=(set @t) :: check change addresses against these
      tx-verification=(map @t (unit (unit tang)))  :: verification status: ~ unverified, [~ ~] verified, [~ ~ tang] failed with error
      draft=(unit transaction:drft)
  ==
::
+$  account-details
  $:  name=@t
      wallet=(unit @ux)
      extended-key=$%([%xpub k=@t] [%xprv k=@t])
      =script-type            :: Address derivation script type
      active-network=network  :: Bitcoin network
      networks=(map network network-details)
      $=  proc
      $:  scan=(unit [pid=@ta act=? scn=account-scan])
          change=(map @ud [pid=@ta act=?])    :: map from address index to process id
          receiving=(map @ud [pid=@ta act=?]) :: map from address index to process id
          tx-verify=(map @t [pid=@ta act=?])  :: map from txid to process id
          tapscript=(map @t [pid=@ta act=?])  :: map from tapscript address to process id
      ==
      indexer-registered=?    :: registered with indexer agent
  ==
::
+$  seed
  $%  [%q =@q]
      [%t =@t]
      :: ...
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
::  Indexer subscription tracking
+$  indexer-sub
  $:  account-pubkey=@ux         :: which account owns this address
      chain=@t                   :: 'receiving' or 'change'
      index=@ud                  :: address index
      address=@t                 :: the address string
      pid=@ta                    :: fiber process id
      act=?                      :: is the fiber active
      last-update=(unit @da)     :: last time we got an update
  ==
::  Boot state machine
::
+$  progress-info  [step=@ud total=@ud label=@t]
+$  boot-mode  ?(%sponsor %normal)
++  default-sponsor
  ~hinnyx-naplut-dozhut-lapsyx--wordur-sopheb-nidlut-daplyd
::
+$  boot-data
  $:  =boot-mode
      boot-secret=@q
      fief=(unit fief:urb)
      sponsor=(unit @p)
      ::  Accumulated during boot
      wallet-pubkey=(unit @ux)
      account-pubkey=(unit @ux)
      address=(unit @t)
      selected-utxo=(unit [txid=@t vout=@ud value=@ud])
      sponsor-sig=(unit @)
      spawn-script=(unit @)    :: hexb dat stored as @
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
++  boot-progress
  |=  bs=boot-state
  ^-  progress-info
  =/  total=@ud  ?:(?=(%sponsor boot-mode.data.bs) 10 12)
  =/  os=@ud  ?:(?=(%sponsor boot-mode.data.bs) 0 2)  :: offset for discover + sponsor steps
  ?+  step.bs  [0 total 'Booting...']
    %start              [0 total 'Generating wallet...']
    %derive             [1 total 'Creating account...']
    %address            [2 total 'Deriving address...']
    %fetch-utxos        [3 total 'Fetching UTXOs...']
    %discover-sponsor   [4 total 'Discovering sponsor on chain (this can take some time if urb-watcher is still processing blocks)...']
    %sponsor            [5 total 'Requesting sponsorship...']
    %poll-sponsor       [5 total 'Waiting for sponsor...']
    %attest             [(add 4 os) total 'Building attestation...']
    %commit             [(add 5 os) total 'Broadcasting commit tx...']
    %confirm-commit     [(add 6 os) total 'Confirming commit tx...']
    %reveal             [(add 7 os) total 'Building reveal tx...']
    %confirm-reveal     [(add 8 os) total 'Confirming reveal tx...']
    %refresh            [(add 9 os) total 'Refreshing addresses...']
    %done               [total total 'Boot complete']
  ==
::
+$  state-0
  $:  %0
      auto-sponsor=_&
      boot=(unit boot-state)
      sponsor-response=(unit [sig=@ height=@ud])
      ::  Core wallet state
      accounts=(map @ux account-details)
      watch-only=(set @ux)
      signing=(set @ux)
      wallets=(map @ux wallet)
      ::  Global labels (BIP-329)
      =labels:bip329
      ::  SPV block verification (per network)
      spv=(map network spv-chain)
      ::  Indexer subscriptions (script-hash -> subscription info)
      indexer-subs=(map @ux indexer-sub)
      ::  User settings
      hide-empty-addresses=?
      ::  System
      binding=binding:eyre
      ::  Experimental/testing features
      counter=@ud
  ==
--
