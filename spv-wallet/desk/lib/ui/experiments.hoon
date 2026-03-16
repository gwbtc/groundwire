/-  *spv-wallet
/+  *ui-layout
|%
++  timer-page
  ^-  manx
  %-  htmx-page
  :^  "SSE Timer Demo"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div.tc.mb2
      ;h1.s3.bold: SSE Timer Demo
      ;p.f2.s-1: Counter increments every second for 5 seconds
    ==
    ;div.tc.p4.b1.br2
      ;div.s2.bold(hx-ext "sse", sse-connect "/spv-wallet/timer", sse-swap "/timer/counter-update")
        ; Ready to count...
      ==
      ;form(hx-post "/spv-wallet/timer")
        ;input(type "hidden", name "action", value "start");
        ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
          ; Start Counter
        ==
      ==
    ==
  ==
::
++  mempool-test-page
  ^-  manx
  %-  htmx-page
  :^  "Mempool.space API Test"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div.tc.mb2
      ;h1.s3.bold: Mempool.space API Test
      ;p.f2.s-1: Fetch recommended fee rates from mempool.space
    ==
    ;div.p4.b1.br2
      ;div#result.p4.b0.br2.mono.f2(hx-ext "sse", sse-connect "/spv-wallet/mempool-test", sse-swap "fetch-result", style "margin-top: 16px; white-space: pre-wrap; word-break: break-all;")
        ; Result will appear here...
      ==
      ;form(hx-post "/spv-wallet/mempool-test")
        ;input(type "hidden", name "action", value "fetch");
        ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
          ; Fetch Fee Rates
        ==
      ==
    ==
  ==
::
++  pdf-test-page
  ^-  manx
  %-  htmx-page
  :^  "PDF Upload & Display"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/pdf-test", sse-swap "pdf-update", style "display:none;");
    ;div.tc.mb2
      ;h1.s3.bold: PDF Upload & Display Test
      ;p.f2.s-1: Upload a PDF and view it in an iframe
    ==
    ;div.p4.b1.br2
      ;div(style "display: flex; gap: 16px; margin-bottom: 16px;")
        ;form(enctype "multipart/form-data", action "/spv-wallet/pdf-test", method "post", style "flex: 1;")
          ;div(style "margin-bottom: 16px;")
            ;label.f2.s-1.bold(for "pdf-upload", style "display: block; margin-bottom: 8px;"): Upload PDF
            ;input#pdf-upload(type "file", name "pdf", accept "application/pdf", required "");
          ==
          ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
            ; Upload PDF
          ==
        ==
        ;form(hx-post "/spv-wallet/pdf-test")
          ;input(type "hidden", name "action", value "clear");
          ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
            ; Clear PDF
          ==
        ==
      ==
      ;div(style "margin-top: 32px;")
        ;h2.s2.bold.mb2: Uploaded PDF
        ;iframe#pdf-iframe(src "/spv-wallet/pdf", style "width: 100%; height: 600px; border: 1px solid var(--b3); border-radius: 8px;");
        ;script
          ; // Force iframe reload on page load to show uploaded PDF
          ; window.addEventListener('load', function() \{
          ;   var iframe = document.getElementById('pdf-iframe');
          ;   iframe.src = iframe.src;
          ; });
        ==
      ==
    ==
  ==
::
++  mp4-test-page
  ^-  manx
  %-  htmx-page
  :^  "MP4 Upload & Display"  &  ~
  ;div.fc.g3.p5.ma.mw-page
    ;div(hx-ext "sse", sse-connect "/spv-wallet/stream/mp4-test", sse-swap "mp4-update", style "display:none;");
    ;div.tc.mb2
      ;h1.s3.bold: MP4 Upload & Display Test
      ;p.f2.s-1: Upload an MP4 video file
    ==
    ;div.p4.b1.br2
      ;div(style "display: flex; gap: 16px; margin-bottom: 16px;")
        ;form(enctype "multipart/form-data", action "/spv-wallet/mp4-test", method "post", style "flex: 1;")
          ;div(style "margin-bottom: 16px;")
            ;label.f2.s-1.bold(for "mp4-upload", style "display: block; margin-bottom: 8px;"): Upload MP4
            ;input#mp4-upload(type "file", name "mp4", accept "video/mp4", required "");
          ==
          ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
            ; Upload MP4
          ==
        ==
        ;form(hx-post "/spv-wallet/mp4-test")
          ;input(type "hidden", name "action", value "clear");
          ;button.p3.b-3.f-3.br2.hover.pointer(type "submit", style "outline: none;")
            ; Clear MP4
          ==
        ==
      ==
      ;div(style "margin-top: 32px;")
        ;h2.s2.bold.mb2: Uploaded MP4
        ;video#mp4-player(src "/spv-wallet/mp4", controls "", style "width: 100%; max-height: 600px; border: 1px solid var(--b3); border-radius: 8px;");
      ==
    ==
  ==
--
