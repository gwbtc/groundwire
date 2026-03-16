/-  *spv-wallet
/+  *ui-layout, sailbox, fi=feather-icons
|%
::  Render the progress bar partial (swapped via SSE)
::
++  progress-bar
  |=  [prog=(unit progress-info) err=(unit [label=term trace=tang])]
  ^-  manx
  ::  Error state: show red bar frozen at last progress, with crash report
  ?^  err
    =/  pct-str=tape
      ?~  prog  "0"
      ?:  =(0 total.u.prog)  "0"
      (scow %ud (div (mul 100 step.u.prog) total.u.prog))
    =/  step-str=tape  ?~(prog "0" (scow %ud step.u.prog))
    =/  total-str=tape  ?~(prog "0" (scow %ud total.u.prog))
    ;div
      ;div(style "height: 24px; background: var(--b2); border-radius: 6px; overflow: hidden;")
        ;div(style "height: 100%; width: {pct-str}%; background: rgba(255, 80, 80, 0.8); transition: width 0.3s ease;");
      ==
      ;div(style "margin-top: 8px;")
        ;span.f2.s-1(style "color: #ff5050;"): Failed at step {step-str}/{total-str}
      ==
      ;div.p3.b1.br2(style "margin-top: 8px; background: rgba(255, 80, 80, 0.1); border: 1px solid rgba(255, 80, 80, 0.3);")
        ;div(style "display: flex; align-items: start; gap: 8px;")
          ;div(style "color: #ff5050; margin-top: 2px; flex-shrink: 0;")
            ;+  (make:fi 'alert-circle')
          ==
          ;div.fc.g1(style "flex: 1; min-width: 0;")
            ;div.s-1.bold(style "color: #ff5050;"): {(trip label.u.err)}
            ;div.p2.b2.br1.mono.s-2(style "max-height: 150px; overflow-y: auto; word-break: break-all; white-space: pre-wrap; background: rgba(0,0,0,0.2); margin-top: 4px;")
              ;*  %+  turn  trace.u.err
                  |=  t=tank
                  ;div: {~(ram re t)}
            ==
          ==
        ==
      ==
      ;div(style "margin-top: 8px; display: flex; gap: 8px;")
        ;form(hx-post "/spv-wallet/progress", hx-swap "none", style "display: inline;")
          ;input(type "hidden", name "action", value "retry");
          ;button.s-1.hover.pointer(type "submit", style "background: transparent; border: 1px solid var(--b3); color: var(--f2); border-radius: 4px; padding: 4px 12px; cursor: pointer; outline: none; font-family: monospace;"): Retry
        ==
        ;form(hx-post "/spv-wallet/progress", hx-swap "none", style "display: inline;")
          ;input(type "hidden", name "action", value "cancel");
          ;button.s-1.hover.pointer(type "submit", style "background: transparent; border: 1px solid rgba(255, 80, 80, 0.5); color: rgba(255, 80, 80, 0.8); border-radius: 4px; padding: 4px 12px; cursor: pointer; outline: none; font-family: monospace;"): Cancel
        ==
      ==
    ==
  ::  No progress and no error: idle
  ?~  prog
    ;div
      ;div(style "height: 24px; background: var(--b2); border-radius: 6px; overflow: hidden;")
        ;div(style "height: 100%; width: 0%; background: var(--f3); transition: width 0.3s ease;");
      ==
      ;div.f2.s-1(style "margin-top: 8px; opacity: 0.5;"): Idle
    ==
  ::  Normal progress display (step = number of steps completed)
  =/  pct=@ud
    ?:  =(0 total.u.prog)  0
    (div (mul 100 step.u.prog) total.u.prog)
  =/  pct-str=tape  (scow %ud pct)
  =/  step-str=tape  (scow %ud step.u.prog)
  =/  total-str=tape  (scow %ud total.u.prog)
  =/  done=?  (gte step.u.prog total.u.prog)
  ;div
    ;div(style "height: 24px; background: var(--b2); border-radius: 6px; overflow: hidden;")
      ;div(style "height: 100%; width: {pct-str}%; background: {?:(done "rgba(100, 200, 100, 0.8)" "rgba(100, 150, 255, 0.8)")}; transition: width 0.3s ease;");
    ==
    ;div(style "display: flex; justify-content: space-between; align-items: center; margin-top: 8px;")
      ;span.f2.s-1: {(trip label.u.prog)}
      ;div(style "display: flex; align-items: center; gap: 8px;")
        ;span.f2.s-1(style "opacity: 0.5;"): {step-str}/{total-str}
        ;+  ?.  done
              ;form(hx-post "/spv-wallet/progress", hx-swap "none", style "display: inline;")
                ;input(type "hidden", name "action", value "cancel");
                ;button.s-1.hover.pointer(type "submit", style "background: transparent; border: 1px solid rgba(255, 80, 80, 0.5); color: rgba(255, 80, 80, 0.8); border-radius: 4px; padding: 2px 8px; cursor: pointer; outline: none; font-family: monospace;"): Cancel
              ==
            ;span;
      ==
    ==
  ==
::  SSE event handler for progress updates
::
++  spawn-button
  |=  booting=?
  ^-  manx
  ?:  booting
    ;button.p3.br2.bold(id "spawn-btn", type "submit", disabled "disabled", hx-swap-oob "true", style "background: rgba(255, 180, 50, 0.4); color: white; border: none; white-space: nowrap; outline: none; cursor: not-allowed;"): Spawning...
  ;button.p3.br2.bold.hover.pointer(id "spawn-btn", type "submit", hx-swap-oob "true", style "background: rgba(255, 180, 50, 0.8); color: white; border: none; white-space: nowrap; outline: none;"): Spawn Comet
::
++  handle-progress-sse
  |=  [prog=(unit progress-info) err=(unit [label=term trace=tang]) event=(unit @t)]
  ^-  wain
  ?+    event  ~
      [~ %'progress-update']
    =/  booting=?  &(?=(^ prog) !=(step.u.prog total.u.prog) ?=(~ err))
    =/  bar=wain  (manx-to-wain:sailbox (progress-bar prog err))
    =/  btn=wain  (manx-to-wain:sailbox (spawn-button booting))
    (weld bar btn)
  ==
--
