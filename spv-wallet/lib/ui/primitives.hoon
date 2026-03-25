/-  *spv-wallet
/+  sigil
|%
:: Basic card component for content containers
::
++  card
  |=  [title=tape body=tape]
  ^-  manx
  ;div(style "border: 1px solid #ddd; padding: 10px; margin: 10px 0; border-radius: 4px;")
    ;h3(style "margin: 0 0 10px 0;"): {title}
    ;p: {body}
  ==
:: Ship badge with status indicator
::
++  ship-badge
  |=  [=ship status=?(%online %offline %unknown)]
  ^-  manx
  =/  color=tape
    ?-  status
      %online   "background: #22c55e; color: white;"
      %offline  "background: #ef4444; color: white;"
      %unknown  "background: #6b7280; color: white;"
    ==
  ;span(style "padding: 4px 8px; border-radius: 12px; font-size: 12px; {color}")
    ; {(scow %p ship)}
  ==
:: Ship badge with optional sigil display
::
++  ship-badge-with-sigil
  |=  [=ship status=?(%online %offline %unknown) show-sigil=?]
  ^-  manx
  =/  color=tape
    ?-  status
      %online   "background: #22c55e; color: white;"
      %offline  "background: #ef4444; color: white;"
      %unknown  "background: #6b7280; color: white;"
    ==
  =/  badge-style=tape
    "display: inline-flex; align-items: center; gap: 8px; padding: 8px 12px; border-radius: 16px; font-size: 14px; {color}"
  ;span(style badge-style)
    ;+  ?.  show-sigil  ;span;
        :: Black container with padding around sigil
        =/  container-style=tape  "background: black; padding: 2px; border-radius: 3px; flex-shrink: 0;"
        ;div(style container-style)
          ;+  %.  ship
              %_  sigil
                size  20
                margin  |
              ==
        ==
    ; {(scow %p ship)}
  ==
:: Full-width ship badge for container layouts
::
++  ship-badge-full-width
  |=  [=ship status=?(%online %offline %unknown) show-sigil=?]
  ^-  manx
  =/  color=tape
    ?-  status
      %online   "background: #22c55e; color: white;"
      %offline  "background: #ef4444; color: white;"
      %unknown  "background: #6b7280; color: white;"
    ==
  =/  badge-style=tape
    "display: flex; align-items: center; gap: 8px; padding: 8px 12px; border-radius: 16px; font-size: 14px; width: 100%; {color}"
  ;div(style badge-style)
    ;+  ?.  show-sigil  ;span;
        :: Black container with padding around sigil
        =/  container-style=tape  "background: black; padding: 2px; border-radius: 3px; flex-shrink: 0;"
        ;div(style container-style)
          ;+  %.  ship
              %_  sigil
                size  20
                margin  |
              ==
        ==
    ; {(scow %p ship)}
  ==
:: Vertical layout component with consistent spacing
::
++  stack
  |=  [gap=@ud items=(list manx)]
  ^-  manx
  =/  container-style=tape
    "display: flex; flex-direction: column; gap: {<gap>}px;"
  ;div(style container-style)
    ;*  items
  ==
:: Alert component for status messages
::
++  alert
  |=  [type=?(%success %warning %error %info) message=tape dismiss=(unit tape)]
  ^-  manx
  =/  [bg=tape fg=tape icon=tape]
    ?-  type
      %success  ["#dcfce7" "#166534" "✅"]
      %warning  ["#fef3c7" "#d97706" "⚠️"]
      %error    ["#fecaca" "#dc2626" "❌"]
      %info     ["#dbeafe" "#2563eb" "ℹ️"]
    ==
  =/  alert-style=tape
    "background: {bg}; color: {fg}; padding: 12px 16px; border-radius: 8px; border: 1px solid currentColor; display: flex; align-items: center; gap: 8px; justify-content: space-between;"
  ;div(style alert-style)
    ;div(style "display: flex; align-items: center; gap: 8px;")
      ;span: {icon}
      ;span: {message}
    ==
    ;*  ?~  dismiss  ~
        :~  ;button
              =style  "background: none; border: none; color: currentColor; cursor: pointer; padding: 4px; font-size: 16px; line-height: 1;"
              =onclick  u.dismiss
              ×
            ==
        ==
  ==
:: Form input with integrated submit button
::
++  input-with-button
  |=  [input-name=tape placeholder=tape button-text=tape action-name=tape]
  ^-  manx
  =/  form-style=tape
    "display: flex; gap: 8px; align-items: stretch;"
  =/  input-style=tape
    "flex: 1; padding: 8px 12px; border: 1px solid #d1d5db; border-radius: 6px; font-family: inherit;"
  =/  button-style=tape
    "padding: 8px 16px; background: #2563eb; color: white; border: none; border-radius: 6px; cursor: pointer; font-family: inherit;"
  ;form(method "post", style form-style)
    ;input(type "text", name input-name, placeholder placeholder, style input-style);
    ;button(type "submit", name "action", value action-name, style button-style): {button-text}
  ==
:: Generic vertical list component for any manx elements
::
++  vlist
  |=  [items=(list manx) gap=@ud]
  ^-  manx
  ?.  ?=(^ items)
    ;div(style "color: #6b7280; font-style: italic; padding: 16px;"): No items to display
  =/  list-style=tape
    "display: flex; flex-direction: column; gap: {<gap>}px;"
  ;div(style list-style)
    ;*  items
  ==
:: Ship list component using generic vertical list
::
++  ship-list
  |=  [ships=(list ship) gap=@ud]
  ^-  manx
  =/  ship-items=(list manx)
    %+  turn  ships
    |=  =ship
    (ship-badge-with-sigil ship %unknown %.y)
  (vlist ship-items gap)
:: Generic drag-and-drop list with JavaScript integration
::
++  vlist-sortable
  |=  [items=(list [id=tape content=manx]) gap=@ud]
  ^-  manx
  ?.  ?=(^ items)
    ;div(style "color: #6b7280; font-style: italic; padding: 16px;"): No items to display
  =/  container-style=tape
    "display: flex; flex-direction: column; gap: {<gap>}px;"
  =/  item-style=tape
    "cursor: grab; transition: transform 0.2s ease, box-shadow 0.2s ease;"
  ;div
    ;div(id "sortable-list", style container-style)
      ;*  %+  turn  items
          |=  [id=tape content=manx]
          ;div(class "sortable-item", style item-style, data-id id)
            ;+  content
          ==
    ==
    ;script(src "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js");
    ;script
      ; var sortable = Sortable.create(document.getElementById('sortable-list'), {
      ;   animation: 150,
      ;   ghostClass: 'sortable-ghost',
      ;   chosenClass: 'sortable-chosen',
      ;   dragClass: 'sortable-drag',
      ;   onUpdate: function(evt) {
      ;     console.log('Item order changed:',
      ;       Array.from(evt.to.children).map(el => el.dataset.id));
      ;   }
      ; });
    ==
    ;style
      ; .sortable-ghost {
      ;   opacity: 0.4;
      ; }
      ; .sortable-chosen {
      ;   transform: rotate(5deg);
      ; }
      ; .sortable-drag {
      ;   transform: rotate(5deg);
      ;   box-shadow: 0 5px 15px rgba(0,0,0,0.3);
      ; }
      ; .sortable-item:hover {
      ;   transform: scale(1.02);
      ; }
    ==
  ==
:: Ship sortable list using generic drag-and-drop component
::
++  ship-list-sortable
  |=  [ships=(list ship) gap=@ud]
  ^-  manx
  =/  ship-items=(list [id=tape content=manx])
    %+  turn  ships
    |=  =ship
    [(scow %p ship) (ship-badge-full-width ship %unknown %.y)]
  (vlist-sortable ship-items gap)
:: Scrollable container component
:: Provides full-screen scrollable area that works within feather's overflow constraints
::
++  scrollable-container
  |=  content=manx
  ^-  manx
  ;div(style "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; overflow-y: auto; padding: 20px; box-sizing: border-box;")
    ;+  content
  ==
:: Scrollable container with custom padding
::
++  scrollable-container-padded
  |=  [content=manx padding=tape]
  ^-  manx
  =/  container-style=tape
    "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; overflow-y: auto; padding: {padding}; box-sizing: border-box;"
  ;div(style container-style)
    ;+  content
  ==
:: Scrollable container with feather classes
::
++  scrollable-container-feather
  |=  content=manx
  ^-  manx
  ;div.fixed.scroll-y.wf.hf.p5
    =style  "top: 0; left: 0;"
    ;+  content
  ==
--
