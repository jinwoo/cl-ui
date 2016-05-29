(defpackage #:cl-ui.raw
  (:use #:cl)
  (:export #:box-append
           #:box-delete
           #:box-padded
           #:box-set-padded
           #:button-on-clicked
           #:button-set-text
           #:button-text
           #:checkbox-checked
           #:checkbox-on-toggled
           #:checkbox-set-checked
           #:checkbox-set-text
           #:checkbox-text
           #:color-button-color
           #:color-button-on-changed
           #:color-button-set-color
           #:combobox-append
           #:combobox-on-selected
           #:combobox-selected
           #:combobox-set-selected
           #:control-destroy
           #:control-disable
           #:control-enable
           #:control-enabled
           #:control-hide
           #:control-parent
           #:control-set-parent
           #:control-show
           #:control-toplevel
           #:control-visible
           #:editable-combobox-append
           #:editable-combobox-on-changed
           #:editable-combobox-set-text
           #:editable-combobox-text
           #:entry-on-changed
           #:entry-read-only
           #:entry-set-read-only
           #:entry-set-text
           #:entry-text
           #:font-button-font
           #:font-button-on-changed
           #:free-init-error
           #:free-text
           #:group-margined
           #:group-set-child
           #:group-set-margined
           #:group-set-title
           #:group-title
           #:init
           #:init-options
           #:label-set-text
           #:label-text
           #:main
           #:main-step
           #:menu-append-about-item
           #:menu-append-check-item
           #:menu-append-item
           #:menu-append-preferences-item
           #:menu-append-quit-item
           #:menu-append-separator
           #:menu-item-checked
           #:menu-item-disable
           #:menu-item-enable
           #:menu-item-on-clicked
           #:menu-item-set-checked
           #:msg-box
           #:msg-box-error
           #:multiline-entry-append
           #:multiline-entry-on-changed
           #:multiline-entry-read-only
           #:multiline-entry-set-read-only
           #:multiline-entry-set-text
           #:multiline-entry-text
           #:new-button
           #:new-checkbox
           #:new-color-button
           #:new-combobox
           #:new-date-picker
           #:new-date-time-picker
           #:new-editable-combobox
           #:new-entry
           #:new-font-button
           #:new-group
           #:new-horizontal-box
           #:new-horizontal-separator
           #:new-label
           #:new-menu
           #:new-multiline-entry
           #:new-non-wrapping-multiline-entry
           #:new-progress-bar
           #:new-radio-buttons
           #:new-slider
           #:new-spinbox
           #:new-tab
           #:new-time-picker
           #:new-vertical-box
           #:new-window
           #:on-should-quit
           #:open-file
           #:progress-bar-set-value
           #:queue-main
           #:quit
           #:radio-buttons-append
           #:save-file
           #:slider-on-changed
           #:slider-set-value
           #:slider-value
           #:spinbox-on-changed
           #:spinbox-set-value
           #:spinbox-value
           #:tab-append
           #:tab-delete
           #:tab-insert-at
           #:tab-margined
           #:tab-num-pages
           #:tab-set-margined
           #:uninit
           #:window-margined
           #:window-on-closing
           #:window-set-child
           #:window-set-margined
           #:window-set-title
           #:window-title))

(defpackage #:cl-ui
  (:use #:cl)
  (:import-from #:cl-ui.raw
                #:main
                #:quit)
  (:export #:box
           #:box-append
           #:box-delete
           #:box-padded
           #:button
           #:button-on-clicked
           #:button-text
           #:control
           #:control-destroy
           #:control-enabled-p
           #:control-parent
           #:control-toplevel-p
           #:control-visible-p
           #:entry
           #:entry-on-changed
           #:entry-read-only
           #:entry-text
           #:main
           #:main-step
           #:on-should-quit
           #:quit
           #:window
           #:window-child
           #:window-margined
           #:window-on-closing
           #:window-title
           #:with-ui))