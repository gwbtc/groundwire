::  Tests for Bitcoin merkle proof verification
::  Using real transaction data from Bitcoin testnet
::
::  Data source: mempool.space API
::  All hashes verified against blockchain.info and mempool.space
::
/+  *test, bitcoin-spv
=,  bitcoin-spv
|%
::  Real transaction from block 4,000,000
::  TXID: 6b423c8e551a0922db07e6a9d345996ed5a5dfb96cfae0959b82e4dbcd826b80
::  Block merkle root: d477dbc72b1a38e15f85e282f4a2d68ce62dad8a1ecc3bb34f36919a3e8da77e
::  Position: 0 (leftmost transaction in merkle tree)
::
::  Merkle tree structure (512 transactions, 9 levels):
::  Level 0: txid (position 0)
::  Level 1: hash(txid, sibling[0]) -> 66222c54b132de288b00fe0c98a43dfbce18960091f638dfdd1a9b34f2882e55
::  Level 2: hash(level1, sibling[1]) -> position 0, so we're on left again
::  ... continues for 9 levels total
::
++  test-merkle-proof-block-4000000
  %-  expect
  !>
  =/  txid=@t  '6b423c8e551a0922db07e6a9d345996ed5a5dfb96cfae0959b82e4dbcd826b80'
  =/  merkle=(list @t)
    :~  '26d971fca7b727cb326dc6dd5b36f2bc4aa83b6f865055b313e0df0a994a947f'
        '49fd0efba77872797ca2097d39d76ba7f559e316a09da2bc2438f728dcd172e9'
        'be086253b45f06bfa3e259bda1020fc96f9a45874cf80891738680f5c67ffe38'
        'f89b1255ad428455e02993eb5431ef8ccf5e6542e7e2da3e9b02a9b8a228f0cb'
        '5f026da03c1e9706711037e6e422ee534339c0422fe94c788ec9f154c91e0fab'
        'fac7aa0ac5e8f4754afe25e39cd4de2bc68284b26a86604636cee855d56550d6'
        '44943245a7f31135302499311b1da7c7873a9ccc1292b60b80cfe8826ca11e64'
        'cec81bae46fadfd96e6f4092ae87f1061301341fc8999e7aa3e7dd1adf25c991'
        'ecd5967be16ac4bb842e901692b40309ae3d347ce934e1a81e4725012162c359'
    ==
  =/  pos=@  0
  =/  expected-root=@t  'd477dbc72b1a38e15f85e282f4a2d68ce62dad8a1ecc3bb34f36919a3e8da77e'
  ::
  (verify-merkle-proof txid merkle pos expected-root)
::
::  Test individual step: first hash in merkle tree
::  TXID is at position 0 (even), so it's on the LEFT
::  Sibling is on the RIGHT
::  Expected result (verified with Python):
::    66222c54b132de288b00fe0c98a43dfbce18960091f638dfdd1a9b34f2882e55
::
++  test-merkle-step-0
  %-  expect-eq
  !>  [expected=0x6622.2c54.b132.de28.8b00.fe0c.98a4.3dfb.ce18.9600.91f6.38df.dd1a.9b34.f288.2e55 actual=(bitcoin-hash (rash '6b423c8e551a0922db07e6a9d345996ed5a5dfb96cfae0959b82e4dbcd826b80' hex) (rash '26d971fca7b727cb326dc6dd5b36f2bc4aa83b6f865055b313e0df0a994a947f' hex))]
::
::  Test step 1: hash level1 result with second sibling
::  Position after step 0: 0 / 2 = 0 (even), so we're LEFT again
::  Python verification:
::    hashlib.sha256(hashlib.sha256(
::      bytes.fromhex('66222c54b132de288b00fe0c98a43dfbce18960091f638dfdd1a9b34f2882e55') +
::      bytes.fromhex('49fd0efba77872797ca2097d39d76ba7f559e316a09da2bc2438f728dcd172e9')
::    ).digest()).hexdigest()
::
++  test-merkle-step-1
  %-  expect-eq
  !>
  =/  level1=@ux  0x6622.2c54.b132.de28.8b00.fe0c.98a4.3dfb.ce18.9600.91f6.38df.dd1a.9b34.f288.2e55
  =/  sibling1=@ux  (rash '49fd0efba77872797ca2097d39d76ba7f559e316a09da2bc2438f728dcd172e9' hex)
  =/  actual=@ux  (bitcoin-hash level1 sibling1)
  ::  Expected from Python calculation
  =/  expected=@ux  0x2d8c.7c20.e97e.3352.417b.1212.406d.00b0.4288.599b.2ae4.ef78.2193.8483.478b.7fe8
  [expected expected actual actual]
--
