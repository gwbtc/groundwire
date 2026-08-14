|%
+$  revision
  $+  keygen-revision
  @ud
::
+$  nodetype
  $+  keygen-nodetype
  tape
::
+$  mnemonic
  $+  keygen-mnemonic
  tape
::
+$  vault
  $+  keygen-vault
  $:  ownership=node
      voting=node
      management=node
      transfer=node
      spawn=node
      network=uode
  ==
::
+$  node
  $+  keygen-node
  [type=nodetype seed=mnemonic keys=wallet]
::
+$  uode
  $+  keygen-uode
  [revi=revision seed=@ux keys=edkeys]
::
+$  wallet
  $+  keygen-wallet
  [keys=[public=@ux private=@ux] addr=@ux chain=@ux]
::
+$  edkeys
  $+  keygen-edkeys
  [auth=keypair crypt=keypair]
::
+$  keypair
  $+  keygen-keypair
  [public=@ux secret=@ux]
--
