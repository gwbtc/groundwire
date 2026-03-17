::  tests for lib/taproot.hoon - Taproot script tree utilities
::
/+  *test, taproot, btc=bitcoin
|%
::  ============================================================================
::  Test Vectors
::  ============================================================================
::
::  Simple test script: OP_TRUE (0x51)
::
++  script-1
  ^-  hexb:btc
  [1 0x51]
::
::  Another test script: OP_1 OP_1 OP_EQUAL (0x51 0x51 0x87)
::
++  script-2
  ^-  hexb:btc
  [3 0x87.5151]
::
::  Default tapscript version (0xc0)
::
++  tapscript-version  0xc0
::
::  ============================================================================
::  Type Construction Tests
::  ============================================================================
::
::  Test: make-leaf creates a leaf node
::
++  test-make-leaf-structure
  =/  tl=tapleaf:taproot  [tapscript-version script-1]
  =/  result=ptst:taproot  (make-leaf:taproot tl)
  ::  Should be a leaf
  %-  expect
    !>  ?=(%leaf -.result)
::
::  Test: make-branch creates a branch node
::
++  test-make-branch-structure
  =/  leaf1=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  leaf2=ptst:taproot  (make-leaf:taproot [tapscript-version script-2])
  =/  result=ptst:taproot  (make-branch:taproot leaf1 leaf2)
  ::  Should be a branch
  %-  expect
    !>  ?=(%branch -.result)
::
::  Test: make-opaque creates an opaque node
::
++  test-make-opaque-structure
  =/  test-hash=@ux  0xdead.beef.cafe.babe
  =/  result=ptst:taproot  (make-opaque:taproot test-hash)
  ;:  weld
    ::  Should be opaque
    %-  expect
      !>  ?=(%opaque -.result)
    ::  Should contain our hash (use hash function)
    %+  expect-eq
      !>  test-hash
      !>  (hash:taproot result)
  ==
::
::  ============================================================================
::  Hash Computation Tests
::  ============================================================================
::
::  Test: hash of empty tree is 0
::
++  test-hash-empty-tree
  %+  expect-eq
    !>  0x0
    !>  (hash:taproot ~)
::
::  Test: hash of opaque node returns stored hash
::
++  test-hash-opaque-returns-stored
  =/  test-hash=@ux  0x1234.5678.9abc.def0
  =/  opaque=ptst:taproot  (make-opaque:taproot test-hash)
  %+  expect-eq
    !>  test-hash
    !>  (hash:taproot opaque)
::
::  Test: hash of leaf is computed (non-zero)
::
++  test-hash-leaf-computed
  =/  leaf=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  result=@ux  (hash:taproot leaf)
  ::  Should produce a non-zero 256-bit hash
  %-  expect
    !>  (gth result 0)
::
::  Test: same leaf produces same hash (deterministic)
::
++  test-hash-leaf-deterministic
  =/  leaf1=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  leaf2=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  %+  expect-eq
    !>  (hash:taproot leaf1)
    !>  (hash:taproot leaf2)
::
::  Test: different leaves produce different hashes
::
++  test-hash-different-leaves
  =/  leaf1=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  leaf2=ptst:taproot  (make-leaf:taproot [tapscript-version script-2])
  %-  expect
    !>  !=((hash:taproot leaf1) (hash:taproot leaf2))
::
::  Test: branch hash is computed from children
::
++  test-hash-branch-computed
  =/  leaf1=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  leaf2=ptst:taproot  (make-leaf:taproot [tapscript-version script-2])
  =/  branch=ptst:taproot  (make-branch:taproot leaf1 leaf2)
  =/  result=@ux  (hash:taproot branch)
  ;:  weld
    ::  Should produce a non-zero hash
    %-  expect
      !>  (gth result 0)
    ::  Should be different from either child hash
    %-  expect
      !>  !=(result (hash:taproot leaf1))
    %-  expect
      !>  !=(result (hash:taproot leaf2))
  ==
::
::  Test: TapBranch hash is order-independent (sorted)
::
++  test-hash-branch-order-independent
  =/  leaf1=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  leaf2=ptst:taproot  (make-leaf:taproot [tapscript-version script-2])
  =/  branch-lr=ptst:taproot  (make-branch:taproot leaf1 leaf2)
  =/  branch-rl=ptst:taproot  (make-branch:taproot leaf2 leaf1)
  ::  Both orderings should produce the same root hash
  %+  expect-eq
    !>  (hash:taproot branch-lr)
    !>  (hash:taproot branch-rl)
::
::  ============================================================================
::  Leaf Enumeration Tests
::  ============================================================================
::
::  Test: leaves of empty tree is empty
::
++  test-leaves-empty-tree
  %+  expect-eq
    !>  ~
    !>  (leaves:taproot ~)
::
::  Test: leaves of single leaf returns that leaf at axis 1
::
++  test-leaves-single-leaf
  =/  tl=tapleaf:taproot  [tapscript-version script-1]
  =/  leaf=ptst:taproot  (make-leaf:taproot tl)
  =/  result=(list [axis=@ =tapleaf:taproot])  (leaves:taproot leaf)
  ;:  weld
    ::  Should have exactly one leaf
    %+  expect-eq
      !>  1
      !>  (lent result)
    ::  First leaf should be at axis 1
    %+  expect-eq
      !>  1
      !>  axis:(snag 0 result)
    ::  Should be our tapleaf
    %+  expect-eq
      !>  tl
      !>  tapleaf:(snag 0 result)
  ==
::
::  Test: leaves of opaque tree is empty
::
++  test-leaves-opaque-empty
  =/  opaque=ptst:taproot  (make-opaque:taproot 0x1234)
  %+  expect-eq
    !>  ~
    !>  (leaves:taproot opaque)
::
::  Test: leaves of branch with two leaves
::
++  test-leaves-two-leaf-branch
  =/  tl1=tapleaf:taproot  [tapscript-version script-1]
  =/  tl2=tapleaf:taproot  [tapscript-version script-2]
  =/  leaf1=ptst:taproot  (make-leaf:taproot tl1)
  =/  leaf2=ptst:taproot  (make-leaf:taproot tl2)
  =/  branch=ptst:taproot  (make-branch:taproot leaf1 leaf2)
  =/  result=(list [axis=@ =tapleaf:taproot])  (leaves:taproot branch)
  ;:  weld
    ::  Should have two leaves
    %+  expect-eq
      !>  2
      !>  (lent result)
    ::  Left leaf at axis 2
    %+  expect-eq
      !>  2
      !>  axis:(snag 0 result)
    ::  Right leaf at axis 3
    %+  expect-eq
      !>  3
      !>  axis:(snag 1 result)
  ==
::
::  Test: leaves with mixed opaque and leaf
::
++  test-leaves-mixed-branch
  =/  tl=tapleaf:taproot  [tapscript-version script-1]
  =/  leaf=ptst:taproot  (make-leaf:taproot tl)
  =/  opaque=ptst:taproot  (make-opaque:taproot 0xabcd)
  =/  branch=ptst:taproot  (make-branch:taproot leaf opaque)
  =/  result=(list [axis=@ =tapleaf:taproot])  (leaves:taproot branch)
  ;:  weld
    ::  Should have one leaf (opaque contributes none)
    %+  expect-eq
      !>  1
      !>  (lent result)
    ::  Leaf should be at axis 2 (left child)
    %+  expect-eq
      !>  2
      !>  axis:(snag 0 result)
  ==
::
::  ============================================================================
::  Axis-to-Path Tests
::  ============================================================================
::
::  Test: axis 1 (root) has empty path
::
++  test-axis-to-path-root
  %+  expect-eq
    !>  ~
    !>  (axis-to-path:taproot 1)
::
::  Test: axis 2 (left child) is [%.y]
::
++  test-axis-to-path-left
  %+  expect-eq
    !>  ~[%.y]
    !>  (axis-to-path:taproot 2)
::
::  Test: axis 3 (right child) is [%.n]
::
++  test-axis-to-path-right
  %+  expect-eq
    !>  ~[%.n]
    !>  (axis-to-path:taproot 3)
::
::  Test: axis 4 (left-left) is [%.y %.y]
::
++  test-axis-to-path-left-left
  %+  expect-eq
    !>  ~[%.y %.y]
    !>  (axis-to-path:taproot 4)
::
::  Test: axis 5 (left-right) is [%.y %.n]
::
++  test-axis-to-path-left-right
  %+  expect-eq
    !>  ~[%.y %.n]
    !>  (axis-to-path:taproot 5)
::
::  ============================================================================
::  Proof Extraction Tests
::  ============================================================================
::
::  Test: proof of empty tree is ~
::
++  test-proof-empty-tree
  %+  expect-eq
    !>  ~
    !>  (proof:taproot ~ 1)
::
::  Test: proof of single leaf at axis 1 is empty list
::
++  test-proof-single-leaf
  =/  leaf=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  result=(unit (list @ux))  (proof:taproot leaf 1)
  ;:  weld
    ::  Should succeed
    %-  expect
      !>  ?=(^ result)
    ::  Proof should be empty (no siblings)
    %+  expect-eq
      !>  ~
      !>  (need result)
  ==
::
::  Test: proof of leaf in branch includes sibling hash
::
++  test-proof-branch-leaf
  =/  tl1=tapleaf:taproot  [tapscript-version script-1]
  =/  tl2=tapleaf:taproot  [tapscript-version script-2]
  =/  leaf1=ptst:taproot  (make-leaf:taproot tl1)
  =/  leaf2=ptst:taproot  (make-leaf:taproot tl2)
  =/  branch=ptst:taproot  (make-branch:taproot leaf1 leaf2)
  ::  Get proof for left leaf (axis 2)
  =/  result=(unit (list @ux))  (proof:taproot branch 2)
  ;:  weld
    ::  Should succeed
    %-  expect
      !>  ?=(^ result)
    ::  Proof should have one element (sibling hash)
    %+  expect-eq
      !>  1
      !>  (lent (need result))
    ::  Sibling hash should be hash of leaf2
    %+  expect-eq
      !>  (hash:taproot leaf2)
      !>  (snag 0 (need result))
  ==
::
::  Test: proof of opaque node fails
::
++  test-proof-opaque-fails
  =/  opaque=ptst:taproot  (make-opaque:taproot 0x1234)
  %+  expect-eq
    !>  ~
    !>  (proof:taproot opaque 1)
::
::  Test: proof at invalid axis fails
::
++  test-proof-invalid-axis
  =/  leaf=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  ::  Axis 2 doesn't exist for a single leaf
  %+  expect-eq
    !>  ~
    !>  (proof:taproot leaf 2)
::
::  ============================================================================
::  has-leaf Tests
::  ============================================================================
::
::  Test: empty tree has no leaves
::
++  test-has-leaf-empty
  %+  expect-eq
    !>  %.n
    !>  (has-leaf:taproot ~)
::
::  Test: leaf node has leaves
::
++  test-has-leaf-leaf
  =/  leaf=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  %+  expect-eq
    !>  %.y
    !>  (has-leaf:taproot leaf)
::
::  Test: opaque node has no leaves
::
++  test-has-leaf-opaque
  =/  opaque=ptst:taproot  (make-opaque:taproot 0x1234)
  %+  expect-eq
    !>  %.n
    !>  (has-leaf:taproot opaque)
::
::  Test: branch with leaf has leaves
::
++  test-has-leaf-branch-with-leaf
  =/  leaf=ptst:taproot  (make-leaf:taproot [tapscript-version script-1])
  =/  opaque=ptst:taproot  (make-opaque:taproot 0x1234)
  =/  branch=ptst:taproot  (make-branch:taproot leaf opaque)
  %+  expect-eq
    !>  %.y
    !>  (has-leaf:taproot branch)
::
::  Test: branch with only opaque children has no leaves
::
++  test-has-leaf-branch-all-opaque
  =/  op1=ptst:taproot  (make-opaque:taproot 0x1234)
  =/  op2=ptst:taproot  (make-opaque:taproot 0x5678)
  =/  branch=ptst:taproot  (make-branch:taproot op1 op2)
  %+  expect-eq
    !>  %.n
    !>  (has-leaf:taproot branch)
::
::  ============================================================================
::  Tagged Hash Tests
::  ============================================================================
::
::  Test: tagged-hash produces 32-byte output
::
++  test-tagged-hash-output-size
  =/  result=@ux  (tagged-hash:taproot 'TapLeaf' [4 0xdead.beef])
  ::  Result should be a 256-bit hash (32 bytes)
  ::  Check it's non-zero and fits in 32 bytes
  ;:  weld
    %-  expect
      !>  (gth result 0)
    %-  expect
      !>  (lte (met 3 result) 32)
  ==
::
::  Test: tagged-hash is deterministic
::
++  test-tagged-hash-deterministic
  =/  result1=@ux  (tagged-hash:taproot 'TapBranch' [8 0x1234.5678.9abc.def0])
  =/  result2=@ux  (tagged-hash:taproot 'TapBranch' [8 0x1234.5678.9abc.def0])
  %+  expect-eq
    !>  result1
    !>  result2
::
::  Test: different tags produce different hashes
::
++  test-tagged-hash-different-tags
  =/  data=hexb:btc  [4 0xdead.beef]
  =/  result1=@ux  (tagged-hash:taproot 'TapLeaf' data)
  =/  result2=@ux  (tagged-hash:taproot 'TapBranch' data)
  %-  expect
    !>  !=(result1 result2)
--
