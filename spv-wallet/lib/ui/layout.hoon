/-  *spv-wallet
/+  feather
|%
::  Fixed viewport page frame: fixed header + scrollable body
::
++  page-frame
  |=  [header=manx body=manx]
  ^-  manx
  ;div.fc(style "height: 100%;")
    ;div.p5.ma.mw-page(style "flex-shrink: 0; padding-bottom: 0;")
      ;+  header
    ==
    ;div.fc.g3.p5.ma.mw-page(style "flex: 1; min-height: 0; overflow-y: auto; padding-top: 0;")
      ;+  body
    ==
  ==
::  Add hx-swap-oob="true" attribute to any manx element
::  Used for SSE out-of-band swaps
::
++  hx-swap-oob
  |=  =manx
  ^+  manx
  manx(a.g [[%hx-swap-oob "true"] a.g.manx])
::  Generic tabs component for clean tabbed interfaces
::
++  tabs
  |=  [selected=tape items=(list [id=tape label=tape content=manx])]
  ^-  manx
  =/  tab-script=tape
    """
    $(function() \{
      function activateTab(container, tabName) \{
        container.find('.tab-content').hide().css('display', 'none');
        container.find('#content-' + tabName).css(\{'display': 'flex', 'flex-direction': 'column', 'flex': '1', 'min-height': '0'});
        container.find('.tab-button').removeClass('active').css(\{
          'background': 'var(--b1)',
          'color': 'var(--f2)',
          'border-bottom': '3px solid transparent'
        });
        container.find('.tab-button[data-tab="' + tabName + '"]').addClass('active').css(\{
          'background': 'var(--b0)',
          'color': 'var(--f0)',
          'border-bottom': '3px solid var(--f-3)'
        });
        container.attr('data-active-tab', tabName);
      }

      $('.tab-button').click(function() \{
        var tabName = $(this).data('tab');
        var container = $(this).closest('.tab-container');
        activateTab(container, tabName);
        // Update URL with tab parameter
        var url = new URL(window.location);
        url.searchParams.set('tab', tabName);
        history.replaceState(null, '', url);

        // Re-initialize any nested tab containers that become visible
        setTimeout(function() \{
          $('#content-' + tabName + ' .tab-container').each(function() \{
            var nestedContainer = $(this);
            var activeTab = nestedContainer.attr('data-active-tab');
            if (!activeTab) \{
              // No active tab set, activate the first one
              var firstTab = nestedContainer.find('.tab-button').first().data('tab');
              if (firstTab) \{
                activateTab(nestedContainer, firstTab);
              }
            } else \{
              // Re-activate the previously active tab
              activateTab(nestedContainer, activeTab);
            }
          });
        }, 10);
      });

      // Initialize all tab containers on load
      $('.tab-container').each(function() \{
        var container = $(this);
        var activeTab = container.attr('data-active-tab');
        if (!activeTab) \{
          var firstTab = container.find('.tab-button').first().data('tab');
          if (firstTab) \{
            activateTab(container, firstTab);
          }
        } else \{
          activateTab(container, activeTab);
        }
      });
    });
    """
  ;div.tab-container.b0.br2(data-active-tab "{selected}", style "box-shadow: 0 4px 12px rgba(0,0,0,0.15); overflow: hidden; display: flex; flex-direction: column; min-height: 0; flex: 1;")
    :: Tab buttons
    ;div.fr.b1(style "flex-shrink: 0;")
      ;*  %+  turn  items
          |=  [id=tape label=tape content=manx]
          =/  is-selected=?  =(id selected)
          =/  button-style=tape
            ?:  is-selected
              "border: none; background: var(--b0); color: var(--f0); border-bottom: 3px solid var(--f-3); outline: none; flex: 1;"
            "border: none; background: var(--b1); color: var(--f2); border-bottom: 3px solid transparent; outline: none; flex: 1;"
          ;button.tab-button.p4.grow.hover.pointer(data-tab id, style button-style)
            ; {label}
          ==
    ==
    :: Tab content area (flex container, children handle own scrolling)
    ;div.p3.b0(style "flex: 1; min-height: 0; display: flex; flex-direction: column;")
      ;*  %+  turn  items
          |=  [id=tape label=tape content=manx]
          =/  is-selected=?  =(id selected)
          =/  display-style=tape
            ?:  is-selected
              "display: flex; flex-direction: column; flex: 1; min-height: 0;"
            "display: none;"
          ;div(id "content-{id}", class "tab-content", style display-style)
            ;+  content
          ==
    ==
    ;script: {tab-script}
  ==
:: Generic modal container component
:: Provides overlay and centering, accepts any content
:: INCLUDES basic show/hide JavaScript behavior
::
++  modal
  |=  [modal-id=tape content=manx]
  ^-  manx
  ;div
    ;div(id modal-id, class "modal-backdrop", style "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); z-index: 1000; align-items: center; justify-content: center;")
      ;div(class "modal-content", style "background: var(--b0); padding: 24px; border-radius: 12px; box-shadow: 0 8px 24px rgba(0,0,0,0.3); max-width: 400px; width: 90%;")
        ;+  content
      ==
    ==
    ;script
      ; $(function() \{
      ;   window.showModal = window.showModal || function(modalId) \{
      ;     $('#' + modalId).css('display', 'flex');
      ;   };
      ;   window.hideModal = window.hideModal || function(modalId) \{
      ;     $('#' + modalId).hide();
      ;   };
      ;   $('#{modal-id}').click(function(e) \{
      ;     if (e.target === this) \{
      ;       $(this).hide();
      ;     }
      ;   });
      ; });
    ==
  ==
:: Confirmation form content (to be used inside a modal)
::
++  confirmation-form
  |=  $:  modal-id=tape
          title=tape
          prompt=tape
          placeholder=tape
          action-url=tape
          action-name=tape
          data-field=tape
      ==
  ^-  manx
  ;div
    ;div(style "margin-bottom: 16px;")
      ;h3(style "margin: 0 0 8px 0; color: var(--f0);"): {title}
      ;p(id "{modal-id}-message", style "margin: 0; color: var(--f2); font-size: 14px;");
    ==
    ;div(style "margin-bottom: 16px;")
      ;p(style "margin: 0 0 8px 0; color: var(--f1); font-size: 14px;"): {prompt}
      ;input(id "{modal-id}-input", class "p2 b1 br1 wf", type "text", placeholder placeholder, style "font-family: inherit;");
    ==
    ;div(style "display: flex; gap: 8px; justify-content: flex-end;")
      ;button(id "{modal-id}-cancel", class "p2 b1 br1 hover pointer", style "background: var(--b1); color: var(--f2); border: 1px solid var(--b3);"): Cancel
      ;button(id "{modal-id}-confirm", class "p2 b1 br1 hover pointer", style "background: #dc2626; color: white; border: 1px solid #b91c1c;"): Confirm
    ==
    :: Hidden form for action
    ;form(id "{modal-id}-form", method "post", action action-url, style "display: none;")
      ;input(type "hidden", name "action", value action-name);
      ;input(id "{modal-id}-data", type "hidden", name data-field, value "");
    ==
  ==
:: Convenience function: confirmation modal (generic modal + confirmation form)
:: FULLY SELF-CONTAINED - includes all needed JavaScript
::
++  confirmation-modal
  |=  $:  modal-id=tape
          title=tape
          prompt=tape
          placeholder=tape
          button-class=tape
          action-url=tape
          action-name=tape
          data-field=tape
          validation-field=tape
      ==
  ^-  manx
  ;div
    ;+  %+  modal  modal-id
        (confirmation-form modal-id title prompt placeholder action-url action-name data-field)
    ;script
      ; $(function() \{
      ;   var currentValidation = '';
      ;   var currentData = '';
      ;   $(document).on('click', '.{button-class}', function(e) \{
      ;     e.preventDefault();
      ;     e.stopPropagation();
      ;     currentValidation = $(this).data('{validation-field}');
      ;     currentData = $(this).data('{data-field}');
      ;     $('#{modal-id}-message').text('Are you sure you want to delete "' + currentValidation + '"?');
      ;     $('#{modal-id}-input').val('');
      ;     showModal('{modal-id}');
      ;     setTimeout(function() \{ $('#{modal-id}-input').focus(); }, 100);
      ;   });
      ;   $('#{modal-id}-cancel').click(function() \{
      ;     hideModal('{modal-id}');
      ;   });
      ;   $('#{modal-id}-confirm').click(function() \{
      ;     var enteredValue = $('#{modal-id}-input').val().trim();
      ;     if (enteredValue === currentValidation) \{
      ;       $('#{modal-id}-data').val(currentData);
      ;       $('#{modal-id}-form').submit();
      ;     } else \{
      ;       alert('Value does not match. Please try again.');
      ;       $('#{modal-id}-input').focus();
      ;     }
      ;   });
      ;   $('#{modal-id}-input').keypress(function(e) \{
      ;     if (e.which === 13) \{
      ;       $('#{modal-id}-confirm').click();
      ;     }
      ;   });
      ; });
    ==
  ==
:: Standard HTMX page wrapper with common head elements
::
++  htmx-page
  |=  [title=tape scrollable=? styles=(unit @t) body=manx]
  ^-  manx
  ;html
    ;head
      ;title: {title}
      ;meta(charset "utf-8");
      ;meta(name "viewport", content "width=device-width, initial-scale=1");
      ;script(src "https://unpkg.com/htmx.org@2.0.3");
      ;script(src "https://unpkg.com/htmx-ext-sse@2.2.2/sse.js");
      ;script(src "https://code.jquery.com/jquery-3.7.1.min.js");
      ;script(src "https://cdnjs.cloudflare.com/ajax/libs/qrcodejs/1.0.0/qrcode.min.js");
      ;+  feather:feather
      ;*  ?~(styles ~ ~[;style:"{(trip u.styles)}"])
      ;style
        ; a:focus, button:focus { outline: none !important; }
      ==
    ==
    ;body(hx-boost "true", hx-ext "sse", sse-connect "/spv-wallet/stream")
      ;+  ?.  scrollable
            ;style
              ; html, body {
              ;   height: 100vh !important;
              ;   overflow: hidden !important;
              ;   margin: 0 !important;
              ; }
            ==
          ;style
            ; html, body {
            ;   overflow: auto !important;
            ;   height: auto !important;
            ;   min-height: 100vh !important;
            ; }
          ==
      ;div(style "min-width: 650px; height: 100%;")
        ;+  body
      ==
    ==
  ==
--
