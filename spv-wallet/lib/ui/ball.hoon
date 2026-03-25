/-  *spv-wallet
/+  server, tarball
|%
::  Handle GET requests for ball (tarball file browser)
::  Returns mime response based on path and parameters
::
++  handle-ball-get
  |=  $:  =ball:tarball
          =bowl:gall
          ball-path=(list @t)
          ext=(unit @ta)
          args=(list [key=@t value=@t])
      ==
  ^-  mime
  ::  Helper to get query param
  =/  get-key
    |=  [key=@t args=(list [key=@t value=@t])]
    ^-  (unit @t)
    ?~  args  ~
    ?:  =(key key.i.args)
      `value.i.args
    $(args t.args)
  ::  Check for download=tar parameter
  =/  download-param=(unit @t)  (get-key 'download' args)
  ?:  ?&(?=(^ download-param) =(u.download-param 'tar'))
    ::  Generate tarball for current directory
    =/  subball=ball:tarball  (~(dip ba:tarball ball) ball-path)
    =/  tar=tarball:tarball  (~(make-tarball gen:tarball bowl) ball-path subball)
    =/  tar-data=octs  (encode-tarball:tarball tar)
    ::  Generate filename based on directory path
    =/  dir-name=@t
      ?~  ball-path
        'root'
      (rear ball-path)
    [/application/x-tar tar-data]
  ::  If ext exists, always treat as file request
  ?^  ext
    ::  Split path into parent directory and filename
    ?~  ball-path
      [/text/plain (as-octs:mimes:html 'file not found')]
    =/  parent=path  (snip `path`ball-path)
    =/  base-name=@ta  (rear ball-path)
    ::  Append extension to filename
    =/  filename=@ta  (crip "{(trip base-name)}.{(trip u.ext)}")
    =/  content-data=(unit content:tarball)  (~(get ba:tarball ball) parent filename)
    ?^  content-data
      ?>  ?=([%file *] u.content-data)
      mime.u.content-data
    [/text/plain (as-octs:mimes:html 'file not found')]
  ::  No ext - try as directory first, fallback to file
  =/  dir-exists=(unit ball:tarball)  (~(dap ba:tarball ball) ball-path)
  ?^  dir-exists
    ::  Directory exists, show browser
    [/text/html (manx-to-octs:server (ball-browser ball ball-path %.y))]
  ::  Directory doesn't exist, try as file
  ?~  ball-path
    ::  Root always shows browser even if empty
    [/text/html (manx-to-octs:server (ball-browser ball ball-path %.y))]
  =/  parent=path  (snip `path`ball-path)
  =/  filename=@ta  (rear ball-path)
  =/  content-data=(unit content:tarball)  (~(get ba:tarball ball) parent filename)
  ?~  content-data
    [/text/plain (as-octs:mimes:html 'file not found')]
  ?>  ?=([%file *] u.content-data)
  mime.u.content-data
::  Render ball file browser UI
::
++  ball-browser
  |=  [g=ball:tarball pax=path write=?]
  ^-  manx
  =/  path-display=tape
    ?~  pax  "/"
    (trip (spat pax))
  =/  upload-path=tape
    ?~  pax
      "/spv-wallet/ball"
    (weld "/spv-wallet/ball" (trip (spat pax)))
  =/  upload-section=manx
    ?:  =(write %.n)
      ;div;
    ;div(class "upload-section")
      ;h3: Upload Files
      ;div(class "upload-form")
        ;form(method "POST", action upload-path, enctype "multipart/form-data", class "upload-file-form")
          ;label: Single File:
          ;input#file-single(type "file", name "file", onchange "document.getElementById('btn-single').disabled = this.files.length === 0");
          ;button#btn-single(type "submit", disabled ""): Upload
        ==
      ==
      ;div(class "upload-form")
        ;form(method "POST", action upload-path, enctype "multipart/form-data", class "upload-file-form")
          ;label: Multiple Files:
          ;input#file-multiple(type "file", name "file", multiple "", onchange "document.getElementById('btn-multiple').disabled = this.files.length === 0");
          ;button#btn-multiple(type "submit", disabled ""): Upload All
        ==
      ==
      ;div(class "upload-form")
        ;form(method "POST", action upload-path, enctype "multipart/form-data", class "upload-file-form")
          ;label: Directory:
          ;input#file-directory(type "file", name "file", webkitdirectory "", directory "", onchange "document.getElementById('btn-directory').disabled = this.files.length === 0");
          ;button#btn-directory(type "submit", disabled ""): Upload Directory
        ==
      ==
      ;div(class "upload-form")
        ;form(method "POST", action upload-path)
          ;label: Create Folder:
          ;input(type "text", name "foldername", placeholder "folder-name", required "");
          ;input(type "hidden", name "action", value "create-folder");
          ;button(type "submit"): Create
        ==
      ==
      ;div(class "upload-form")
        ;form(method "POST", action upload-path)
          ;label: Create Symlink:
          ;input(type "text", name "linkname", placeholder "link-name", required "");
          ;input(type "text", name "target", placeholder "target-path", required "");
          ;input(type "hidden", name "action", value "create-symlink");
          ;button(type "submit"): Create
        ==
      ==
      ;div(class "upload-form")
        ;label: Download Current Directory:
        ;a/"{upload-path}?download=tar"
          ;button(type "button"): Download as Tarball
        ==
      ==
    ==
  ;html
    ;head
      ;title: Index of {path-display}
      ;style
        ; body { font-family: monospace; margin: 20px; }
        ; h1 { font-size: 18px; }
        ; table { border-collapse: collapse; width: 100%; }
        ; th, td { text-align: left; padding: 8px; }
        ; th { border-bottom: 1px solid #ccc; }
        ; a { color: #0366d6; text-decoration: none; }
        ; a:hover { text-decoration: underline; }
        ; .upload-section { margin: 20px 0; padding: 15px; background: #f6f8fa; border-radius: 6px; }
        ; .upload-form { margin: 10px 0; }
        ; input[type="file"] { margin: 5px 0; }
        ; button { padding: 5px 10px; margin-left: 10px; cursor: pointer; }
      ==
      ;script
        ; document.addEventListener('DOMContentLoaded', function() {
        ;   var forms = document.querySelectorAll('.upload-file-form');
        ;   forms.forEach(function(form) {
        ;     form.addEventListener('submit', function() {
        ;       setTimeout(function() { location.reload(); }, 100);
        ;     });
        ;   });
        ; });
      ==
    ==
    ;body
      ;h1: Index of {path-display}
      ;+  upload-section
      ;table
        ;tr
          ;th: Name
          ;th: Mime Type
          ;th: Size
          ;th: Date Modified
          ;th: Actions
        ==
        ;*
        ::  Get subdirectories at current path
        =/  current-ball=ball:tarball  (~(dip ba:tarball g) pax)
        =/  subdirs=(list @ta)  ~(tap in ~(key by dir.current-ball))
        ::  Add parent directory link if not at root
        =/  parent-row=(list manx)
          ?~  pax  ~
          =/  parent-path=path  `path`(snip `(list @ta)`pax)
          =/  parent-prefix=tape
            ?~  parent-path  ""
            (trip (spat parent-path))
          =/  parent-url=tape  "/spv-wallet/ball{parent-prefix}"
          :~  ;tr
                ;td
                  ;a/"{parent-url}"
                    ; ../
                  ==
                ==
                ;td: -
                ;td: -
                ;td: -
                ;td: -
              ==
          ==
        =/  subdir-rows=(list manx)
          %+  turn  subdirs
          |=  dirname=@ta
          ^-  manx
          =/  path-prefix=tape
            ?~  pax  ""
            (trip (spat pax))
          =/  dir-url=tape  "/spv-wallet/ball{path-prefix}/{(trip dirname)}"
          =/  download-url=tape  "{dir-url}?download=tar"
          ;tr
            ;td
              ;a/"{dir-url}"
                ; {(trip dirname)}/
              ==
            ==
            ;td: -
            ;td: -
            ;td: -
            ;td
              ;a/"{download-url}"
                ;button(type "button", style "margin-right: 5px;"): Download
              ==
              ;form(method "POST", action upload-path, style "display: inline;")
                ;input(type "hidden", name "action", value "delete-folder");
                ;input(type "hidden", name "foldername", value (trip dirname));
                ;button(type "submit", onclick "return confirm('Delete folder {(trip dirname)} and all its contents?')"): Delete
              ==
            ==
          ==
        ::  Get files at current path
        =/  files=(list @ta)  (~(lis ba:tarball g) pax)
        =/  file-rows=(list manx)
          %+  turn  files
          |=  filename=@ta
          ^-  manx
          =/  content-data=content:tarball  (~(got ba:tarball g) pax filename)
          ::  Extract modified date from metadata
          =/  mtime=(unit @t)  (~(get by metadata.content-data) 'mtime')
          =/  modified-display=tape
            ?~  mtime
              "Unknown"
            =/  da-time=@da
              %-  from-unix:chrono:userlib
              (rash u.mtime oct:tarball)
            (scow %da da-time)
          =/  path-prefix=tape
            ?~  pax  ""
            (trip (spat pax))
          ?-  -.content-data
              %file
            =/  size=@ud  p.q.mime.content-data
            =/  mime-raw=tape  (trip (spat p.mime.content-data))
            =/  mime-display=tape  ?~(mime-raw "" (tail mime-raw))
            =/  file-url=tape  "/spv-wallet/ball{path-prefix}/{(trip filename)}"
            ;tr
              ;td
                ;a/"{file-url}"
                  ; {(trip filename)}
                ==
              ==
              ;td: {mime-display}
              ;td: {(scow %ud size)} bytes
              ;td: {modified-display}
              ;td
                ;a/"{file-url}"(download "")
                  ;button(type "button"): Download
                ==
                ;form(method "POST", action upload-path, style "display: inline; margin-left: 5px;")
                  ;input(type "hidden", name "action", value "delete-file");
                  ;input(type "hidden", name "filename", value (trip filename));
                  ;button(type "submit", onclick "return confirm('Delete {(trip filename)}?')"): Delete
                ==
              ==
            ==
              %symlink
            =/  target-display=tape  (trip (encode-road:tarball road.content-data))
            =/  resolved-path=path  (resolve-road:tarball road.content-data pax)
            =/  target-url=tape  "/spv-wallet/ball{(trip (spat resolved-path))}"
            ;tr
              ;td
                ;a/"{target-url}"
                  ; {(trip filename)}
                ==
                ;  -> {target-display}
              ==
              ;td: symlink
              ;td: -
              ;td: {modified-display}
              ;td
                ;form(method "POST", action upload-path, style "display: inline;")
                  ;input(type "hidden", name "action", value "delete-file");
                  ;input(type "hidden", name "filename", value (trip filename));
                  ;button(type "submit", onclick "return confirm('Delete {(trip filename)}?')"): Delete
                ==
              ==
            ==
          ==
        (weld parent-row (weld subdir-rows file-rows))
      ==
    ==
  ==
--
