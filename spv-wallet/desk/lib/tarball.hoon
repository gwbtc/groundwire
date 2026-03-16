::  tarball: hierarchical file storage using axal with tar archive support
::
/+  multipart
|%
+$  metadata  (map @t @t)
+$  bend  (pair @ud path)      :: relative path
+$  road  (each path bend)     :: absolute or relative path
+$  content
  $%  [%file =metadata =mime]
      [%symlink =metadata =road]
  ==
+$  lump  [=metadata contents=(map @ta content)]
+$  ball  (axal lump)
::  Tarball archive types
::
+$  calp   ?(%'A' %'B' %'C' %'D' %'E' %'F' %'G' %'H' %'I' %'J' %'K' %'L' %'M' %'N' %'O' %'P' %'Q' %'R' %'S' %'T' %'U' %'V' %'W' %'X' %'Y' %'Z')
+$  octal  (list ?(%'0' %'1' %'2' %'3' %'4' %'5' %'6' %'7'))
+$  typeflag
  $?  %'0'  %'' :: Regular file
      %'1'      :: Hard link
      %'2'      :: Symbolic link
      %'3'      :: Character special
      %'4'      :: Block special
      %'5'      :: Directory
      %'6'      :: FIFO
      %'7'      :: Contiguous file
      %'g'      :: Global extended header
      %'x'      :: Extended header
      calp      :: Vendor-specific extensions
  ==
+$  tarball-header
  $:  name=@t     :: file or directory name
      mode=@t     :: octal - permissions
      uid=@t      :: octal - user id
      gid=@t      :: octal - group id
      size=@t     :: octal - size
      mtime=@t    :: octal - modification time
      typeflag=@t :: type of file, directory, etc.
      linkname=@t :: linkname for symlink and hardlink
      uname=@t    :: user name
      gname=@t    :: group name
      devmajor=@t :: octal - for devices
      devminor=@t :: octal - for devices
      prefix=@t   :: name prefix
  ==
+$  tarball-entry  [header=tarball-header data=(unit octs)]
+$  tarball        (list tarball-entry)
::
++  ext-to-mime
  |=  ext=@ta
  ^-  (unit mite)
  ?+  ext  ~
    %md    `/text/plain
    %txt   `/text/plain
    %json  `/application/json
    %html  `/text/html
    %css   `/text/css
    %js    `/text/javascript
  ==
::  Parse Unix-style path string into road
::
++  parse-road
  |=  target=@t
  ^-  (unit road)
  ::  Empty path is current directory
  ?:  =(target '')
    `[%| [0 ~]]
  ::  Absolute path (starts with /)
  ?:  =('/' (snag 0 (trip target)))
    ::  Strip trailing slash if present (except for root "/")
    =/  target-clean=@t
      ?:  =(target '/')
        target
      ?:  =('/' (rear (trip target)))
        (crip (snip (trip target)))
      target
    =/  parsed=(unit path)  (rush target-clean stap)
    ?~  parsed  ~
    `[%& u.parsed]
  ::  Relative path - count ../ prefixes and final ..
  =/  target-text=tape  (trip target)
  =/  up-count=@ud  0
  |-
  ::  Check if starts with ../
  ?:  ?&  (gte (lent target-text) 3)
          =("../" (scag 3 target-text))
      ==
    $(up-count +(up-count), target-text (slag 3 target-text))
  ::  Check if exactly ".." remains (no trailing slash)
  ?:  =(".." target-text)
    `[%| [+(up-count) ~]]
  ::  Parse remaining path
  =/  remaining=@t  (crip target-text)
  ::  If empty after ../ stripping, just going up
  ?:  =(remaining '')
    `[%| [up-count ~]]
  ::  Parse as path by prepending /
  =/  path-text=@t  (crip (weld "/" target-text))
  =/  parsed=(unit path)  (rush path-text stap)
  ?~  parsed  ~
  `[%| [up-count u.parsed]]
::  Encode road back to Unix-style path string
::
++  encode-road
  |=  r=road
  ^-  @t
  ?-  -.r
    %&  (spat p.r)
    %|
  =/  [up-count=@ud pax=path]  p.r
  ::  Build up-navigation prefix (../ repeated)
  =/  prefix=tape
    =/  count=@ud  up-count
    =/  result=tape  ""
    |-
    ?:  =(count 0)
      result
    ?:  =(count 1)
      ?:  =(pax ~)
        (weld result "..")
      (weld result "../")
    $(count (dec count), result (weld result "../"))
  ::  Convert path to text without leading /
  =/  path-text=tape
    ?~  pax
      ""
    =/  parts=(list tape)
      %+  turn  pax
      |=(term=@ta (trip term))
    (roll parts |=([part=tape acc=tape] ?~(acc part (weld acc (weld "/" part)))))
  ::  Combine prefix and path
  (crip (weld prefix path-text))
  ==
::  Resolve a road relative to a base path to get absolute path
::
++  resolve-road
  |=  [r=road base=path]
  ^-  path
  ?-  -.r
      %&  p.r
      %|
    =/  [up-count=@ud pax=path]  p.r
    ::  Go up from base by up-count
    =/  resolved-base=path
      =/  count=@ud  up-count
      =/  current=path  base
      |-
      ?:  =(count 0)
        current
      ?~  current
        ~  ::  Can't go up from root
      $(count (dec count), current (snip `path`current))
    ::  Append remaining path
    (weld resolved-base pax)
  ==
::  Process multipart file uploads into ball
::
++  from-parts
  |=  [base=ball base-path=path parts=(list [@t part:multipart]) now=@da]
  ^-  ball
  ?~  parts  base
  =/  [field-name=@t file-part=part:multipart]  i.parts
  ?.  =('file' field-name)
    $(parts t.parts)
  ::  Get filename (which might include a path like "test/test.txt")
  =/  filename-raw=@t
    ?~  file.file-part
      %uploaded-file
    u.file.file-part
  ::  Parse filename as path (prepend '/' for stap)
  =/  filename-path=path
    (rash (crip (weld "/" (trip filename-raw))) stap)
  ::  Split into parent directory and filename
  =/  [file-parent=path file-name=@ta]
    ?~  filename-path
      [~ %uploaded-file]  :: empty, shouldn't happen
    ?~  t.filename-path
      [~ i.filename-path]  :: just a filename, no directory
    =/  parent=(list @ta)  (snip `(list @ta)`filename-path)
    [`(list @ta)`parent (rear filename-path)]
  ::  Combine with base path from URL
  =/  full-parent=path  (weld base-path file-parent)
  ::  Explicitly create all parent directories to avoid implicit creation
  =/  base-with-dirs=ball
    =/  current-path=path  base-path
    |-
    ?~  file-parent
      base
    =/  next-dir=@ta  i.file-parent
    =/  dir-path=path  (snoc current-path next-dir)
    ::  Only create if doesn't exist
    =/  dir-exists=(unit lump)  (~(get of base) dir-path)
    =/  updated-base=ball
      ?^  dir-exists
        base
      =/  dir-metadata=(map @t @t)
        %-  ~(gas by *(map @t @t))
        :~  ['mtime' (da-oct now)]
        ==
      (~(mkd ba base) dir-path dir-metadata)
    $(base updated-base, current-path dir-path, file-parent t.file-parent)
  ::  Parse filename to extract extension
  =/  parsed=(unit [ext=(unit @ta) pax=path])
    (rush (crip (weld "/" (trip file-name))) apat:de-purl:html)
  ::  Get mime type (check extension override first, then browser-provided type)
  =/  mime-type=mite
    =/  browser-type=mite
      ?~  type.file-part
        /application/octet-stream
      u.type.file-part
    ?~  parsed
      browser-type
    ?~  ext.u.parsed
      browser-type
    (fall (ext-to-mime u.ext.u.parsed) browser-type)
  ::  Create file content with metadata
  =/  file-size=@ud  (met 3 body.file-part)
  =/  file-metadata=(map @t @t)
    %-  ~(gas by *(map @t @t))
    :~  ['mtime' (da-oct now)]
        ['size' (scot %ud file-size)]
    ==
  =/  file-content=content
    [%file file-metadata mime-type [file-size body.file-part]]
  ::  Add file to base with explicit directories
  =/  new-base=ball
    (~(put ba base-with-dirs) full-parent file-name file-content)
  $(parts t.parts, base new-base)
::
++  ba
  |_  b=ball
  ::  Get a content item (file or symlink) by directory path and name
  ::
  ++  get
    |=  [pax=path name=@ta]
    ^-  (unit content)
    ?~  nod=(~(get of b) pax)
      ~
    (~(get by contents.u.nod) name)
  ::  Put a content item at directory path with name
  ::
  ++  put
    |=  [pax=path name=@ta c=content]
    ^-  ball
    ::  Reject empty files
    ?:  ?&  ?=([%file *] c)
            =(0 p.q.mime.c)
        ==
      ~|("empty file {(spud (weld pax /[name]))}" !!)
    =/  lmp=lump
      ?~  nod=(~(get of b) pax)
        [~ ~]
      u.nod
    (~(put of b) pax lmp(contents (~(put by contents.lmp) name c)))
  ::  Check if a content item exists
  ::
  ++  has
    |=  [pax=path name=@ta]
    ^-  ?
    !=(~ (get pax name))
  ::  Delete a content item
  ::
  ++  del
    |=  [pax=path name=@ta]
    ^-  ball
    ?~  nod=(~(get of b) pax)
      b
    (~(put of b) pax u.nod(contents (~(del by contents.u.nod) name)))
  ::  List all content items in a directory
  ::
  ++  lis
    |=  pax=path
    ^-  (list @ta)
    ?~  nod=(~(get of b) pax)
      ~
    ~(tap in ~(key by contents.u.nod))
  ::  Get or crash
  ::
  ++  got
    |=  [pax=path name=@ta]
    (need (get pax name))
  ::  Get with default
  ::
  ++  gut
    |=  [pax=path name=@ta default=content]
    (fall (get pax name) default)
  ::  Count total content items across all directories
  ::
  ++  wyt
    ^-  @ud
    %+  roll  ~(tap of b)
    |=  [[pax=path lmp=lump] acc=@ud]
    (add acc ~(wyt by contents.lmp))
  ::  Convert entire ball to flat list
  ::
  ++  tap
    ^-  (list [path @ta content])
    %-  zing
    %+  turn  ~(tap of b)
    |=  [pax=path lmp=lump]
    %+  turn  ~(tap by contents.lmp)
    |=  [name=@ta c=content]
    [pax name c]
  ::  Apply function to all content items
  ::
  ++  run
    |=  fn=$-(content content)
    ^-  ball
    %+  roll  ~(tap of b)
    |=  [[pax=path lmp=lump] acc=ball]
    (~(put of acc) pax lmp(contents (~(run by contents.lmp) fn)))
  ::  Insert list of content items
  ::
  ++  gas
    |=  items=(list [path @ta content])
    ^-  ball
    %+  roll  items
    |=  [[pax=path name=@ta c=content] acc=ball]
    (~(put ba acc) pax name c)
  ::  Reduce over all content items
  ::
  ++  rep
    |*  fn=$-([* *] *)
    =/  items  tap
    (roll items fn)
  ::  Check if all content items match predicate
  ::
  ++  all
    |=  fn=$-(content ?)
    ^-  ?
    %+  levy  tap
    |=  [pax=path name=@ta c=content]
    (fn c)
  ::  Check if any content item matches predicate
  ::
  ++  any
    |=  fn=$-(content ?)
    ^-  ?
    %+  lien  tap
    |=  [pax=path name=@ta c=content]
    (fn c)
  ::  Delete entire subtree at path
  ::
  ++  lop
    |=  pax=path
    ^-  ball
    (~(lop of b) pax)
  ::  Make directory at path
  ::
  ++  mkd
    |=  [pax=path met=metadata]
    ^-  ball
    (~(put of b) pax [met ~])
  ::  Descend to subdirectory as new ball
  ::
  ++  dip
    |=  pax=path
    ^-  ball
    (~(dip of b) pax)
  ::  Descend to subdirectory, return ~ if path doesn't exist
  ::
  ++  dap
    |=  pax=path
    ^-  (unit ball)
    |-
    ?~  pax
      [~ b]
    ?~  kid=(~(get by dir.b) i.pax)
      ~
    $(b u.kid, pax t.pax)
  --
::  Tarball encoding utilities
::
++  sud-base
  |=  [a=@u b=@u]
  ^-  @t
  ?>  &((gth b 0) (lte b 10))
  ?:  =(0 a)  '0'
  %-  crip
  %-  flop
  |-  ^-  tape
  ?:(=(0 a) ~ [(add '0' (mod a b)) $(a (div a b))])
::
++  numb      (curr sud-base 10)
++  ud-oct    (curr sud-base 8)
++  da-oct    |=(=@da (ud-oct (unt:chrono:userlib da)))
++  oct       (bass 8 (most gon cit))
::
++  validate-header
  |=  tarball-header
  ^-  tarball-header
  =*  header  +<
  ?>  ?&  (lte (met 3 name) 100)
          (lte (met 3 mode) 8)
          (lte (met 3 uid) 8)
          (lte (met 3 gid) 8)
          (lte (met 3 size) 8)
          (lte (met 3 mtime) 12)
          (lte (met 3 typeflag) 1)
          (lte (met 3 linkname) 100)
          (lte (met 3 uname) 32)
          (lte (met 3 gname) 32)
          (lte (met 3 devmajor) 8)
          (lte (met 3 devminor) 8)
          (lte (met 3 prefix) 155)
      ==
  =:  mode      (crip ;;(octal (trip mode)))
      uid       (crip ;;(octal (trip uid)))
      gid       (crip ;;(octal (trip gid)))
      size      (crip ;;(octal (trip size)))
      mtime     (crip ;;(octal (trip mtime)))
      devmajor  (crip ;;(octal (trip devmajor)))
      devminor  (crip ;;(octal (trip devminor)))
    ==
  =.  typeflag  ;;(^typeflag typeflag)
  header
::
++  validate-entry
  |=  entry=tarball-entry
  ^-  tarball-entry
  =/  header  (validate-header header.entry)
  ?~  data.entry
    entry
  ?>  =(0 (mod p.u.data.entry 512))
  entry
::
++  common-mode
  |=  typeflag=@t
  ^-  @t
  ?+  typeflag   '0000'
    ?(%'0' %'')  '0644'
    %'2'         '0777'
    %'5'         '0755'
    %'6'         '0644'
  ==
::
++  octs-cat
  |=  [a=octs b=octs]
  ^-  octs
  =/  z=@  (sub p.a (met 3 q.a))
  :-  (add p.a p.b)
  (cat 3 q.a (lsh [3 z] q.b))
::
++  octs-rap
  |=  =(list octs)
  ^-  octs
  ?<  ?=(?(~ [octs ~]) list)
  ?:  ?=([octs octs ~] list)
    (octs-cat i.list i.t.list)
  %+  octs-cat  i.list
  $(list t.list)
::
++  pack  |=([f=@t l=@] `octs`?>((lte (met 3 f) l) l^f))
++  sum   |=(@ (roll (rip 3 +<) add))
::
++  encode-header
  =|  checksum=(unit @t)
  |=  header=tarball-header
  ^-  octs
  =.  header  (validate-header header)
  =/  fields
    :~  [name.header 100]
        [mode.header 8]
        [uid.header 8]
        [gid.header 8]
        [size.header 12]
        [mtime.header 12]
        [?^(checksum u.checksum '        ') 8]
        [typeflag.header 1]
        [linkname.header 100]
        ['ustar' 6]
        ['00' 2]
        [uname.header 32]
        [gname.header 32]
        [devmajor.header 8]
        [devminor.header 8]
        [prefix.header 155]
        ['' 12]
    ==
  =/  data=octs  (octs-rap (turn fields pack))
  ?>  =(512 p.data)
  ?^  checksum
    data
  $(checksum `(ud-oct (sum q.data)))
::
++  encode-tarball
  =|  =octs
  |=  tar=tarball
  ?~  tar
    octs(p (add 1.024 p.octs))
  =/  head  (encode-header header.i.tar)
  =/  data  ?~(data.i.tar 0^0 u.data.i.tar)
  $(tar t.tar, octs (octs-rap octs head data ~))
::
++  split-path
  |=  =path
  ^-  [prefix=^path name=^path]
  =/  p=^path  (flop path)
  =|  n=^path
  |-
  ?>  ?=(^ p)
  ?:  (lte (sub (lent (spud p)) 1) 155)
    ?~  n
      [(flop t.p) [i.p ~]]
    [(flop p) n]
  $(p t.p, n [i.p n])
::
++  gen
  |_  =bowl:gall
  ::  TODO: implement PAX extended headers (typeflag 'x' and 'g')
  ::  to preserve arbitrary metadata fields like date-created
  ::  Format: <length> <key>=<value>\n
  ::
  ++  generate-header
    |=  fields=(map @t @t)
    ^-  tarball-header
    =|  header=tarball-header
    =.  name.header       (~(got by fields) 'name')
    =.  typeflag.header   (~(got by fields) 'typeflag')
    =.  mode.header       (~(gut by fields) 'mode' (common-mode typeflag.header))
    =.  uid.header        (~(gut by fields) 'uid' '0000000')
    =.  gid.header        (~(gut by fields) 'gid' '0000000')
    =.  size.header       (~(gut by fields) 'size' '0')
    =.  mtime.header      (~(gut by fields) 'mtime' (da-oct now.bowl))
    =.  linkname.header   (~(gut by fields) 'linkname' '')
    =.  uname.header      (~(gut by fields) 'uname' 'root')
    =.  gname.header      (~(gut by fields) 'gname' 'root')
    =.  devmajor.header   (~(gut by fields) 'devmajor' '')
    =.  devminor.header   (~(gut by fields) 'devminor' '')
    =.  prefix.header     (~(gut by fields) 'prefix' '')
    header
  ::
  ++  generate-entry
    |=  [fields=(map @t @t) data=(unit octs)]
    ^-  tarball-entry
    =/  tf=@t  (~(got by fields) 'typeflag')
    ~?  >>>  &(?=(^ data) ?=(?(%'1' %'2' %'3' %'4' %'5' %'6') tf))
      `@t`(cat 3 'tarball: unexpected data for header with typeflag ' tf)
    ~?  >>  (~(has by fields) 'size')  'tarball: ignoring size field'
    =.  fields
      %+  ~(put by fields)
        'size'
      (ud-oct ?~(data 0 p.u.data))
    %-  validate-entry
    :-  (generate-header fields)
    ?~  data
      ~
    `u.data(p (add p.u.data (sub 512 (mod p.u.data 512))))
  ::
  ++  make-directory-entry
    |=  [=path =metadata]
    ^-  tarball-entry
    =/  [prefix=^path name=^path]  (split-path path)
    =.  metadata
      %-  ~(gas by metadata)
      :~  ['typeflag' '5']
          ['prefix' (rsh [3 1] (spat prefix))]
          ['name' (cat 3 (rsh [3 1] (spat name)) '/')]
      ==
    (generate-entry metadata ~)
  ::
  ++  make-content-entry
    |=  [=path =content]
    ^-  tarball-entry
    =/  [prefix=^path name=^path]  (split-path path)
    ?-    -.content
        %symlink
      =/  sym-metadata=metadata
        %-  ~(gas by metadata.content)
        :~  ['typeflag' '2']
            ['prefix' (rsh [3 1] (spat prefix))]
            ['name' (rsh [3 1] (spat name))]
            ['linkname' (encode-road road.content)]
        ==
      (generate-entry sym-metadata ~)
      ::
        %file
      =/  file-metadata=metadata
        %-  ~(gas by metadata.content)
        :~  ['typeflag' '0']
            ['prefix' (rsh [3 1] (spat prefix))]
            ['name' (rsh [3 1] (spat name))]
        ==
      (generate-entry file-metadata `q.mime.content)
    ==
  ::
  ++  make-tarball
    |=  [=path =ball]
    ^-  tarball
    =/  tar-entries=tarball
      ?~  fil.ball
        ~
      =/  contents-list=(list [@ta content])  ~(tap by contents.u.fil.ball)
      %+  weld
        ?~  path
          ~
        [(make-directory-entry path metadata.u.fil.ball) ~]
      %+  turn  contents-list
      |=  [name=@ta =content]
      (make-content-entry (snoc path name) content)
    =/  directories  ~(tap by dir.ball)
    |-
    ?~  directories
      tar-entries
    =/  [name=@ta sub-ball=^ball]  i.directories
    =/  sub-tar=tarball
      (make-tarball (snoc path name) sub-ball)
    %=  $
      directories  t.directories
      tar-entries  (weld tar-entries sub-tar)
    ==
  --
--
