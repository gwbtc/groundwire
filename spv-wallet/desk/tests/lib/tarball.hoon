/+  *test, tarball
|%
++  test-empty-ball
  %+  expect-eq
    !>  [fil=~ dir=~]
  !>  *ball:tarball
::
++  test-put-and-get
  =/  my-ball  *ball:tarball
  =/  test-content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  updated  (~(put ba:tarball my-ball) /foo %test test-content)
  =/  result  (~(get ba:tarball updated) /foo %test)
  %+  expect-eq
    !>  `test-content
  !>  result
::
++  test-get-nonexistent
  =/  my-ball  *ball:tarball
  =/  result  (~(get ba:tarball my-ball) /foo %nonexistent)
  %+  expect-eq
    !>  ~
  !>  result
::
++  test-has-exists
  =/  my-ball  *ball:tarball
  =/  test-content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  updated  (~(put ba:tarball my-ball) /foo %test test-content)
  %-  expect
  !>  (~(has ba:tarball updated) /foo %test)
::
++  test-has-not-exists
  =/  my-ball  *ball:tarball
  =/  result  (~(has ba:tarball my-ball) /foo %test)
  %+  expect-eq
    !>  %.n
  !>  result
::
++  test-del
  =/  my-ball  *ball:tarball
  =/  test-content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test test-content)
  =/  g2  (~(del ba:tarball g1) /foo %test)
  =/  result  (~(get ba:tarball g2) /foo %test)
  %+  expect-eq
    !>  ~
  !>  result
::
++  test-lis
  =/  my-ball  *ball:tarball
  =/  test1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  test2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test test1)
  =/  g2  (~(put ba:tarball g1) /foo %other test2)
  =/  files  (~(lis ba:tarball g2) /foo)
  ::  Check that both files are in the list
  ;:  weld
    %-  expect
    !>  (~(has in (~(gas in *(set @ta)) files)) %test)
    %-  expect
    !>  (~(has in (~(gas in *(set @ta)) files)) %other)
  ==
::
++  test-multiple-paths
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ;:  weld
    %+  expect-eq
      !>  `content1
    !>  (~(get ba:tarball g2) /foo %test)
    %+  expect-eq
      !>  `content2
    !>  (~(get ba:tarball g2) /bar %other)
  ==
::
++  test-got
  =/  my-ball  *ball:tarball
  =/  test-content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  updated  (~(put ba:tarball my-ball) /foo %test test-content)
  %+  expect-eq
    !>  test-content
  !>  (~(got ba:tarball updated) /foo %test)
::
++  test-got-crash
  =/  my-ball  *ball:tarball
  %-  expect-fail
  |.((~(got ba:tarball my-ball) /foo %nonexistent))
::
++  test-gut
  =/  my-ball  *ball:tarball
  =/  default=content:tarball  [%file ~ [%text %plain ~] [7 'default']]
  =/  result  (~(gut ba:tarball my-ball) /foo %test default)
  %+  expect-eq
    !>  default
  !>  result
::
++  test-wyt
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  %+  expect-eq
    !>  2
  !>  ~(wyt ba:tarball g2)
::
++  test-gas
  =/  my-ball  *ball:tarball
  =/  files=(list [path @ta content:tarball])
    :~  [/foo %test [%file ~ [%text %plain ~] [5 'hello']]]
        [/foo %other [%file ~ [%text %html ~] [3 'bye']]]
        [/bar %thing [%file ~ [%text %css ~] [4 'hmm']]]
    ==
  =/  updated  (~(gas ba:tarball my-ball) files)
  ;:  weld
    %+  expect-eq
      !>  `[%file ~ [%text %plain ~] [5 'hello']]
    !>  (~(get ba:tarball updated) /foo %test)
    %+  expect-eq
      !>  `[%file ~ [%text %css ~] [4 'hmm']]
    !>  (~(get ba:tarball updated) /bar %thing)
  ==
::
++  test-tap
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /foo %other content2)
  =/  result  ~(tap ba:tarball g2)
  %-  expect
  !>  =((lent result) 2)
::
++  test-run
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  ::  Identity transform - run should preserve content
  =/  updated  (~(run ba:tarball g1) |=(c=content:tarball c))
  =/  result  (~(got ba:tarball updated) /foo %test)
  %+  expect-eq
    !>  content1
  !>  result
::
++  test-rep
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ::  Count all entries
  =/  total  (~(rep ba:tarball g2) |=([[* * c=content:tarball] acc=@ud] (add acc 1)))
  %+  expect-eq
    !>  2
  !>  total
::
++  test-all
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %plain ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ::  Check all are %file
  %-  expect
  !>  (~(all ba:tarball g2) |=(c=content:tarball ?=([%file *] c)))
::
++  test-all-false
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ::  Check if all match false predicate (should be false)
  %+  expect-eq
    !>  %.n
  !>  (~(all ba:tarball g2) |=(c=content:tarball %.n))
::
++  test-put-overwrite
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /foo %test content2)
  =/  result  (~(get ba:tarball g2) /foo %test)
  %+  expect-eq
    !>  `content2
  !>  result
::
++  test-del-nonexistent
  =/  my-ball  *ball:tarball
  =/  result  (~(del ba:tarball my-ball) /foo %nonexistent)
  %+  expect-eq
    !>  my-ball
  !>  result
::
++  test-lis-empty
  =/  my-ball  *ball:tarball
  =/  result  (~(lis ba:tarball my-ball) /foo)
  %+  expect-eq
    !>  ~
  !>  result
::
++  test-gut-exists
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  default=content:tarball  [%file ~ [%text %html ~] [7 'default']]
  =/  updated  (~(put ba:tarball my-ball) /foo %test content)
  =/  result  (~(gut ba:tarball updated) /foo %test default)
  %+  expect-eq
    !>  content
  !>  result
::
++  test-wyt-empty
  =/  my-ball  *ball:tarball
  %+  expect-eq
    !>  0
  !>  ~(wyt ba:tarball my-ball)
::
++  test-tap-empty
  =/  my-ball  *ball:tarball
  %+  expect-eq
    !>  ~
  !>  ~(tap ba:tarball my-ball)
::
++  test-run-empty
  =/  my-ball  *ball:tarball
  =/  result  (~(run ba:tarball my-ball) |=(c=content:tarball c))
  %+  expect-eq
    !>  my-ball
  !>  result
::
++  test-gas-empty
  =/  my-ball  *ball:tarball
  =/  result  (~(gas ba:tarball my-ball) ~)
  %+  expect-eq
    !>  my-ball
  !>  result
::
++  test-rep-empty
  =/  my-ball  *ball:tarball
  =/  result  (~(rep ba:tarball my-ball) |=([[* * c=content:tarball] acc=@ud] acc))
  %+  expect-eq
    !>  0
  !>  result
::
++  test-all-empty
  =/  my-ball  *ball:tarball
  ::  Vacuous truth - all files match predicate when there are no files
  %+  expect-eq
    !>  %.y
  !>  (~(all ba:tarball my-ball) |=(c=content:tarball %.n))
::
++  test-any-empty
  =/  my-ball  *ball:tarball
  %+  expect-eq
    !>  %.n
  !>  (~(any ba:tarball my-ball) |=(c=content:tarball %.y))
::
++  test-any-none-match
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %plain ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ::  Check if any match false predicate (should be false)
  %+  expect-eq
    !>  %.n
  !>  (~(any ba:tarball g2) |=(c=content:tarball %.n))
::
++  test-lop-nonexistent
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar %test content)
  =/  result  (~(lop ba:tarball g1) /baz)
  ::  Should be no-op, file still exists
  %+  expect-eq
    !>  `content
  !>  (~(get ba:tarball result) /foo/bar %test)
::
++  test-dip-nonexistent
  =/  my-ball  *ball:tarball
  =/  result  (~(dip ba:tarball my-ball) /nonexistent)
  %+  expect-eq
    !>  [fil=~ dir=~]
  !>  result
::
++  test-any
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content1)
  =/  g2  (~(put ba:tarball g1) /bar %other content2)
  ::  Check if any are %file
  %-  expect
  !>  (~(any ba:tarball g2) |=(c=content:tarball ?=([%file *] c)))
::
++  test-lop
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar %test content1)
  =/  g2  (~(put ba:tarball g1) /foo/bar %other content2)
  ::  Delete entire /foo subtree
  =/  updated  (~(lop ba:tarball g2) /foo)
  %+  expect-eq
    !>  ~
  !>  (~(get ba:tarball updated) /foo/bar %test)
::
++  test-dip
  =/  my-ball  *ball:tarball
  =/  content1=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  content2=content:tarball  [%file ~ [%text %html ~] [3 'bye']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar %test content1)
  =/  g2  (~(put ba:tarball g1) /foo/bar %other content2)
  ::  Get directory at /foo/bar as a ball
  =/  subball  (~(dip ba:tarball g2) /foo/bar)
  =/  files  (~(get of subball) /)
  %-  expect
  !>  ?=(^ files)
::
++  test-dap-empty-root
  ::  Root path ALWAYS exists, even in empty ball
  =/  my-ball  *ball:tarball
  =/  dap-result  (~(dap ba:tarball my-ball) /)
  =/  dip-result  (~(dip ba:tarball my-ball) /)
  ;:  weld
    ::  dap should return [~ ball] - root exists
    %-  expect
    !>  ?=(^ dap-result)
    ::  dip returns the ball itself
    %+  expect-eq
      !>  my-ball
    !>  dip-result
  ==
::
++  test-dap-nonexistent-in-empty
  ::  Non-existent path in empty ball
  =/  my-ball  *ball:tarball
  =/  dap-result  (~(dap ba:tarball my-ball) /foo)
  =/  dip-result  (~(dip ba:tarball my-ball) /foo)
  ;:  weld
    ::  dap should return ~ - path doesn't exist
    %+  expect-eq
      !>  ~
    !>  dap-result
    ::  dip returns [~ ~] - went off the rails
    %+  expect-eq
      !>  [fil=~ dir=~]
    !>  dip-result
  ==
::
++  test-dap-exists-with-files
  ::  Path exists when files are present
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar %test content)
  =/  dap-result  (~(dap ba:tarball g1) /foo/bar)
  =/  dip-result  (~(dip ba:tarball g1) /foo/bar)
  ;:  weld
    ::  dap should return [~ ball] - path exists
    %-  expect
    !>  ?=(^ dap-result)
    ::  dip returns the node at /foo/bar
    %-  expect
    !>  ?=(^ fil.dip-result)
  ==
::
++  test-dap-exists-after-delete
  ::  Path still exists after deleting files (structure remains)
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar %test content)
  =/  g2  (~(del ba:tarball g1) /foo/bar %test)
  =/  dap-result  (~(dap ba:tarball g2) /foo/bar)
  =/  dip-result  (~(dip ba:tarball g2) /foo/bar)
  ;:  weld
    ::  dap should return [~ ball] - path exists (but empty)
    %-  expect
    !>  ?=(^ dap-result)
    ::  dip returns node with empty lump (empty metadata, empty contents)
    %+  expect-eq
      !>  [fil=[~ [metadata=~ contents=~]] dir=~]
    !>  dip-result
  ==
::
++  test-dap-nonexistent-sibling
  ::  Path doesn't exist if we never created it
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content)
  =/  dap-result  (~(dap ba:tarball g1) /bar)
  =/  dip-result  (~(dip ba:tarball g1) /bar)
  ;:  weld
    ::  dap should return ~ - path doesn't exist
    %+  expect-eq
      !>  ~
    !>  dap-result
    ::  dip returns [~ ~] - went off the rails
    %+  expect-eq
      !>  [fil=~ dir=~]
    !>  dip-result
  ==
::
++  test-dap-nonexistent-child
  ::  Path doesn't exist if we go deeper than structure
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo %test content)
  =/  dap-result  (~(dap ba:tarball g1) /foo/bar)
  =/  dip-result  (~(dip ba:tarball g1) /foo/bar)
  ;:  weld
    ::  dap should return ~ - path doesn't exist
    %+  expect-eq
      !>  ~
    !>  dap-result
    ::  dip returns [~ ~] - went off the rails
    %+  expect-eq
      !>  [fil=~ dir=~]
    !>  dip-result
  ==
::
++  test-dap-parent-exists
  ::  Parent path exists when child has files
  =/  my-ball  *ball:tarball
  =/  content=content:tarball  [%file ~ [%text %plain ~] [5 'hello']]
  =/  g1  (~(put ba:tarball my-ball) /foo/bar/baz %test content)
  =/  dap-result  (~(dap ba:tarball g1) /foo)
  =/  dip-result  (~(dip ba:tarball g1) /foo)
  ;:  weld
    ::  dap should return [~ ball] - path exists
    %-  expect
    !>  ?=(^ dap-result)
    ::  dip returns node with subdirectories
    %-  expect
    !>  !=(~ dir.dip-result)
  ==
::
::  parse-road tests
::
++  test-parse-road-absolute-simple
  =/  result  (parse-road:tarball '/foo')
  %+  expect-eq
    !>  `[%& /foo]
  !>  result
::
++  test-parse-road-absolute-multi
  =/  result  (parse-road:tarball '/foo/bar/baz')
  %+  expect-eq
    !>  `[%& /foo/bar/baz]
  !>  result
::
++  test-parse-road-absolute-root
  =/  result  (parse-road:tarball '/')
  %+  expect-eq
    !>  `[%& ~]
  !>  result
::
++  test-parse-road-absolute-two-level
  =/  result  (parse-road:tarball '/a/b')
  %+  expect-eq
    !>  `[%& /a/b]
  !>  result
::
++  test-parse-road-relative-simple
  =/  result  (parse-road:tarball 'foo')
  %+  expect-eq
    !>  `[%| [0 /foo]]
  !>  result
::
++  test-parse-road-relative-multi
  =/  result  (parse-road:tarball 'foo/bar')
  %+  expect-eq
    !>  `[%| [0 /foo/bar]]
  !>  result
::
++  test-parse-road-relative-three
  =/  result  (parse-road:tarball 'foo/bar/baz')
  %+  expect-eq
    !>  `[%| [0 /foo/bar/baz]]
  !>  result
::
++  test-parse-road-relative-with-dots
  =/  result  (parse-road:tarball 'foo.txt')
  ::  Just check it parses successfully
  %+  expect-eq
    !>  `[%| [0 /'foo.txt']]
  !>  result
::
++  test-parse-road-up-one
  =/  result  (parse-road:tarball '../foo')
  %+  expect-eq
    !>  `[%| [1 /foo]]
  !>  result
::
++  test-parse-road-up-two
  =/  result  (parse-road:tarball '../../foo')
  %+  expect-eq
    !>  `[%| [2 /foo]]
  !>  result
::
++  test-parse-road-up-three
  =/  result  (parse-road:tarball '../../../foo')
  %+  expect-eq
    !>  `[%| [3 /foo]]
  !>  result
::
++  test-parse-road-up-with-multi-path
  =/  result  (parse-road:tarball '../foo/bar')
  %+  expect-eq
    !>  `[%| [1 /foo/bar]]
  !>  result
::
++  test-parse-road-up-two-with-path
  =/  result  (parse-road:tarball '../../foo/bar/baz')
  %+  expect-eq
    !>  `[%| [2 /foo/bar/baz]]
  !>  result
::
++  test-parse-road-just-one-up
  =/  result  (parse-road:tarball '..')
  %+  expect-eq
    !>  `[%| [1 ~]]
  !>  result
::
++  test-parse-road-just-two-up
  =/  result  (parse-road:tarball '../..')
  %+  expect-eq
    !>  `[%| [2 ~]]
  !>  result
::
++  test-parse-road-just-three-up
  =/  result  (parse-road:tarball '../../..')
  %+  expect-eq
    !>  `[%| [3 ~]]
  !>  result
::
++  test-parse-road-empty
  =/  result  (parse-road:tarball '')
  %+  expect-eq
    !>  `[%| [0 ~]]
  !>  result
::
++  test-parse-road-absolute-trailing-slash
  ::  stap parser handles trailing slashes
  =/  result  (parse-road:tarball '/foo/')
  %-  expect
  !>  ?=(^ result)
::
++  test-parse-road-relative-complex
  =/  result  (parse-road:tarball 'a/b/c/d')
  %+  expect-eq
    !>  `[%| [0 /a/b/c/d]]
  !>  result
::
++  test-parse-road-up-four
  =/  result  (parse-road:tarball '../../../../foo')
  %+  expect-eq
    !>  `[%| [4 /foo]]
  !>  result
::
++  test-parse-road-up-many-no-path
  =/  result  (parse-road:tarball '../../../..')
  %+  expect-eq
    !>  `[%| [4 ~]]
  !>  result
::
++  test-parse-road-single-char
  =/  result  (parse-road:tarball 'a')
  %+  expect-eq
    !>  `[%| [0 /a]]
  !>  result
::
++  test-parse-road-absolute-single-char
  =/  result  (parse-road:tarball '/x')
  %+  expect-eq
    !>  `[%& /x]
  !>  result
::
++  test-parse-road-up-then-simple
  =/  result  (parse-road:tarball '../x')
  %+  expect-eq
    !>  `[%| [1 /x]]
  !>  result
::
++  test-parse-road-numbers-in-path
  =/  result  (parse-road:tarball 'foo123/bar456')
  %+  expect-eq
    !>  `[%| [0 /foo123/bar456]]
  !>  result
::
++  test-parse-road-hyphens-in-path
  =/  result  (parse-road:tarball 'foo-bar/baz-qux')
  %+  expect-eq
    !>  `[%| [0 /foo-bar/baz-qux]]
  !>  result
::
++  test-parse-road-absolute-deep
  =/  result  (parse-road:tarball '/a/b/c/d/e/f')
  %+  expect-eq
    !>  `[%& /a/b/c/d/e/f]
  !>  result
::
++  test-parse-road-up-mixed
  =/  result  (parse-road:tarball '../foo/../bar')
  ::  Should parse but normalize differently - just check it parses
  %-  expect
  !>  ?=(^ result)
::
::  encode-road tests
::
++  test-encode-road-absolute-simple
  =/  result  (encode-road:tarball [%& /foo])
  %+  expect-eq
    !>  '/foo'
  !>  result
::
++  test-encode-road-absolute-multi
  =/  result  (encode-road:tarball [%& /foo/bar/baz])
  %+  expect-eq
    !>  '/foo/bar/baz'
  !>  result
::
++  test-encode-road-absolute-root
  =/  result  (encode-road:tarball [%& ~])
  %+  expect-eq
    !>  '/'
  !>  result
::
++  test-encode-road-relative-simple
  =/  result  (encode-road:tarball [%| [0 /foo]])
  %+  expect-eq
    !>  'foo'
  !>  result
::
++  test-encode-road-relative-multi
  =/  result  (encode-road:tarball [%| [0 /foo/bar]])
  %+  expect-eq
    !>  'foo/bar'
  !>  result
::
++  test-encode-road-relative-empty
  =/  result  (encode-road:tarball [%| [0 ~]])
  %+  expect-eq
    !>  ''
  !>  result
::
++  test-encode-road-up-one
  =/  result  (encode-road:tarball [%| [1 /foo]])
  %+  expect-eq
    !>  '../foo'
  !>  result
::
++  test-encode-road-up-two
  =/  result  (encode-road:tarball [%| [2 /foo]])
  %+  expect-eq
    !>  '../../foo'
  !>  result
::
++  test-encode-road-up-three
  =/  result  (encode-road:tarball [%| [3 /foo]])
  %+  expect-eq
    !>  '../../../foo'
  !>  result
::
++  test-encode-road-just-one-up
  =/  result  (encode-road:tarball [%| [1 ~]])
  %+  expect-eq
    !>  '..'
  !>  result
::
++  test-encode-road-just-two-up
  =/  result  (encode-road:tarball [%| [2 ~]])
  %+  expect-eq
    !>  '../..'
  !>  result
::
++  test-encode-road-just-three-up
  =/  result  (encode-road:tarball [%| [3 ~]])
  %+  expect-eq
    !>  '../../..'
  !>  result
::
++  test-encode-road-up-with-multi-path
  =/  result  (encode-road:tarball [%| [1 /foo/bar]])
  %+  expect-eq
    !>  '../foo/bar'
  !>  result
::
++  test-encode-road-up-four-with-path
  =/  result  (encode-road:tarball [%| [4 /foo]])
  %+  expect-eq
    !>  '../../../../foo'
  !>  result
::
::  Round-trip tests: parse -> encode should give back original
::
++  test-roundtrip-absolute-simple
  =/  original  '/foo'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-absolute-multi
  =/  original  '/foo/bar/baz'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-absolute-root
  =/  original  '/'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-relative-simple
  =/  original  'foo'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-relative-multi
  =/  original  'foo/bar'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-empty
  =/  original  ''
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-up-one
  =/  original  '../foo'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-up-two
  =/  original  '../../foo'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-just-one-up
  =/  original  '..'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-just-two-up
  =/  original  '../..'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
++  test-roundtrip-complex
  =/  original  '../../foo/bar/baz'
  =/  parsed  (parse-road:tarball original)
  ?~  parsed  !!
  =/  encoded  (encode-road:tarball u.parsed)
  %+  expect-eq
    !>  original
  !>  encoded
::
::  resolve-road tests
::
++  test-resolve-road-absolute
  =/  result  (resolve-road:tarball [%& /absolute/path] /foo/bar)
  %+  expect-eq
    !>  /absolute/path
  !>  result
::
++  test-resolve-road-absolute-from-root
  =/  result  (resolve-road:tarball [%& /foo] ~)
  %+  expect-eq
    !>  /foo
  !>  result
::
++  test-resolve-road-relative-simple
  =/  result  (resolve-road:tarball [%| [0 /baz]] /foo/bar)
  %+  expect-eq
    !>  /foo/bar/baz
  !>  result
::
++  test-resolve-road-relative-multi
  =/  result  (resolve-road:tarball [%| [0 /baz/qux]] /foo/bar)
  %+  expect-eq
    !>  /foo/bar/baz/qux
  !>  result
::
++  test-resolve-road-up-one
  =/  result  (resolve-road:tarball [%| [1 /baz]] /foo/bar)
  %+  expect-eq
    !>  /foo/baz
  !>  result
::
++  test-resolve-road-up-two
  =/  result  (resolve-road:tarball [%| [2 /baz]] /foo/bar/qux)
  %+  expect-eq
    !>  /foo/baz
  !>  result
::
++  test-resolve-road-up-to-root
  =/  result  (resolve-road:tarball [%| [2 /baz]] /foo/bar)
  %+  expect-eq
    !>  /baz
  !>  result
::
++  test-resolve-road-just-up-one
  =/  result  (resolve-road:tarball [%| [1 ~]] /foo/bar)
  %+  expect-eq
    !>  /foo
  !>  result
::
++  test-resolve-road-just-up-two
  =/  result  (resolve-road:tarball [%| [2 ~]] /foo/bar/baz)
  %+  expect-eq
    !>  /foo
  !>  result
::
++  test-resolve-road-current-dir
  =/  result  (resolve-road:tarball [%| [0 ~]] /foo/bar)
  %+  expect-eq
    !>  /foo/bar
  !>  result
::
++  test-resolve-road-relative-from-root
  =/  result  (resolve-road:tarball [%| [0 /foo]] ~)
  %+  expect-eq
    !>  /foo
  !>  result
::
++  test-resolve-road-up-past-root
  ::  Going up past root should just give root
  =/  result  (resolve-road:tarball [%| [5 /foo]] /bar)
  %+  expect-eq
    !>  /foo
  !>  result
::
++  test-resolve-road-complex
  =/  result  (resolve-road:tarball [%| [1 /sibling/child]] /foo/bar/baz)
  %+  expect-eq
    !>  /foo/bar/sibling/child
  !>  result
::
::  da-oct round-trip tests
::
++  test-da-oct-epoch
  ::  Unix epoch ~1970.1.1 should convert to '0'
  =/  epoch  ~1970.1.1
  =/  octal-result  (da-oct:tarball epoch)
  %+  expect-eq
    !>  '0'
  !>  octal-result
::
++  test-da-oct-roundtrip-epoch
  ::  Round-trip: @da -> octal -> @da
  =/  original  ~1970.1.1
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-roundtrip-2024
  ::  Test with a realistic date
  =/  original  ~2024.1.1
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-roundtrip-2025
  =/  original  ~2025.10.14
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-roundtrip-future
  ::  Test with a future date
  =/  original  ~2030.12.31
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-year-2000
  =/  original  ~2000.1.1
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-year-1990
  =/  original  ~1990.6.15
  =/  octal-text  (da-oct:tarball original)
  =/  unix-secs  (rash octal-text oct:tarball)
  =/  restored  (add ~1970.1.1 (mul unix-secs ~s1))
  %+  expect-eq
    !>  original
  !>  restored
::
++  test-da-oct-is-octal-format
  ::  Verify the output is actually octal (only digits 0-7)
  =/  original  ~2024.1.1
  =/  octal-text  (da-oct:tarball original)
  =/  text-tape  (trip octal-text)
  ::  All characters should be octal digits (0-7)
  %-  expect
  !>  %+  levy  text-tape
      |=  c=@t
      ?&  (gte c '0')
          (lte c '7')
      ==
::
++  test-da-oct-monotonic
  ::  Later dates should have larger octal values
  =/  earlier  ~2020.1.1
  =/  later    ~2025.1.1
  =/  earlier-oct  (rash (da-oct:tarball earlier) oct:tarball)
  =/  later-oct    (rash (da-oct:tarball later) oct:tarball)
  %-  expect
  !>  (gth later-oct earlier-oct)
::
--
