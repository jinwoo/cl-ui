(in-package #:cl-ui.raw)

(cffi:defctype size :ulong)
(cffi:defctype intmax :int64)
(cffi:defctype uintmax :uint64)

(cffi:defcstruct init-options
  (size size))

(cffi:defcfun (init "uiInit") :pointer
  (options (:pointer (:struct init-options))))

(cffi:defcfun (uninit "uiUninit") :void)

(cffi:defcfun (free-init-error "uiFreeInitError") :void
  (err :pointer))

(cffi:defcfun (main "uiMain") :void)

(cffi:defcfun (main-step "uiMainStep") (:boolean :int)
  (wait (:boolean :int)))

(cffi:defcfun (quit "uiQuit") :void)

(cffi:defcfun (queue-main "uiQueueMain") :void
  (f :pointer)
  (data :pointer))

(cffi:defcfun (on-should-quit "uiOnShouldQuit") :void
  (f :pointer)
  (data :pointer))

(cffi:defcfun (free-text "uiFreeText") :void
  (text :pointer))

;;; Translation of libui texts

(cffi:define-foreign-type ui-string ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method ui-string ()
  (make-instance 'ui-string))

(defmethod cffi:translate-from-foreign (pointer (type ui-string))
  (prog1 (cffi:foreign-string-to-lisp pointer)
    (free-text pointer)))

;;; Control

(cffi:defcfun (control-destroy "uiControlDestroy") :void
  (c :pointer))

(cffi:defcfun (control-parent "uiControlParent") :pointer
  (c :pointer))

(cffi:defcfun (control-set-parent "uiControlSetParent") :void
  (c :pointer)
  (parent :pointer))

(cffi:defcfun (control-toplevel "uiControlToplevel") (:boolean :int)
  (c :pointer))

(cffi:defcfun (control-visible "uiControlVisible") (:boolean :int)
  (c :pointer))

(cffi:defcfun (control-show "uiControlShow") :void
  (c :pointer))

(cffi:defcfun (control-hide "uiControlHide") :void
  (c :pointer))

(cffi:defcfun (control-enabled "uiControlEnabled") (:boolean :int)
  (c :pointer))

(cffi:defcfun (control-enable "uiControlEnable") :void
  (c :pointer))

(cffi:defcfun (control-disable "uiControlDisable") :void
  (c :pointer))

;;; Window

(cffi:defcfun (window-title "uiWindowTitle") ui-string
  (w :pointer))

(cffi:defcfun (window-set-title "uiWindowSetTitle") :void
  (w :string)
  (title :string))

(cffi:defcfun (window-on-closing "uiWindowOnClosing") :void
  (w :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (window-set-child "uiWindowSetChild") :void
  (w :pointer)
  (child :pointer))

(cffi:defcfun (window-margined "uiWindowMargined") (:boolean :int)
  (w :pointer))

(cffi:defcfun (window-set-margined "uiWindowSetMargined") :void
  (w :pointer)
  (margined (:boolean :int)))

(cffi:defcfun (new-window "uiNewWindow") :pointer
  (title :string)
  (width :int)
  (height :int)
  (has-menubar (:boolean :int)))

;;; Button

(cffi:defcfun (button-text "uiButtonText") ui-string
  (b :pointer))

(cffi:defcfun (button-set-text "uiButtonSetText") :void
  (b :pointer)
  (text :string))

(cffi:defcfun (button-on-clicked "uiButtonOnClicked") :void
  (b :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-button "uiNewButton") :pointer
  (text :string))

;;; Box

(cffi:defcfun (box-append "uiBoxAppend") :void
  (b :pointer)
  (child :pointer)
  (stretchy (:boolean :int)))

(cffi:defcfun (box-delete "uiBoxDelete") :void
  (b :pointer)
  (index uintmax))

(cffi:defcfun (box-padded "uiBoxPadded") (:boolean :int)
  (b :pointer))

(cffi:defcfun (box-set-padded "uiBoxSetPadded") :void
  (b :pointer)
  (padded (:boolean :int)))

(cffi:defcfun (new-horizontal-box "uiNewHorizontalBox") :pointer)

(cffi:defcfun (new-vertical-box "uiNewVerticalBox") :pointer)

;;; Entry

(cffi:defcfun (entry-text "uiEntryText") ui-string
  (e :pointer))

(cffi:defcfun (entry-set-text "uiEntrySetText") :void
  (e :pointer)
  (text :string))

(cffi:defcfun (entry-on-changed "uiEntryOnChanged") :void
  (e :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (entry-read-only "uiEntryReadOnly") (:boolean :int)
  (e :pointer))

(cffi:defcfun (entry-set-read-only "uiEntrySetReadOnly") :void
  (e :pointer)
  (readonly (:boolean :int)))

(cffi:defcfun (new-entry "uiNewEntry") :pointer)

(cffi:defcfun (new-password-entry "uiNewPasswordEntry") :pointer)

(cffi:defcfun (new-search-entry "uiNewSearchEntry") :pointer)

;;; Checkbox

(cffi:defcfun (checkbox-text "uiCheckboxText") ui-string
  (c :pointer))

(cffi:defcfun (checkbox-set-text "uiCheckboxSetText") :void
  (c :pointer)
  (text :string))

(cffi:defcfun (checkbox-on-toggled "uiCheckboxOnToggled") :void
  (c :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (checkbox-checked "uiCheckboxChecked") (:boolean :int)
  (c :pointer))

(cffi:defcfun (checkbox-set-checked "uiCheckboxSetChecked") :void
  (c :pointer)
  (checked (:boolean :int)))

(cffi:defcfun (new-checkbox "uiNewCheckbox") :pointer
  (text :string))

;;; Label

(cffi:defcfun (label-text "uiLabelText") ui-string
  (l :pointer))

(cffi:defcfun (label-set-text "uiLabelSetText") :void
  (l :pointer)
  (text :string))

(cffi:defcfun (new-label "uiNewLabel") :pointer
  (text :string))

;;; Tab

(cffi:defcfun (tab-append "uiTabAppend") :void
  (tab :pointer)
  (name :string)
  (c :pointer))

(cffi:defcfun (tab-insert-at "uiTabInsertAt") :void
  (tab :pointer)
  (name :string)
  (before uintmax)
  (c :pointer))

(cffi:defcfun (tab-delete "uiTabDelete") :void
  (tab :pointer)
  (index uintmax))

(cffi:defcfun (tab-num-pages "uiTabNumPages") uintmax
  (tab :pointer))

(cffi:defcfun (tab-margined "uiTabMargined") (:boolean :int)
  (tab :pointer)
  (page uintmax))

(cffi:defcfun (tab-set-margined "uiTabSetMargined") :void
  (tab :pointer)
  (page uintmax)
  (margined (:boolean :int)))

(cffi:defcfun (new-tab "uiNewTab") :pointer)

;;; Group

(cffi:defcfun (group-title "uiGroupTitle") ui-string
  (g :pointer))

(cffi:defcfun (group-set-title "uiGroupSetTitle") :void
  (g :pointer)
  (title :string))

(cffi:defcfun (group-set-child "uiGroupSetChild") :void
  (g :pointer)
  (c :pointer))

(cffi:defcfun (group-margined "uiGroupMargined") (:boolean :int)
  (g :pointer))

(cffi:defcfun (group-set-margined "uiGroupSetMargined") :void
  (g :pointer)
  (margined (:boolean :int)))

(cffi:defcfun (new-group "uiNewGroup") :pointer
  (title :string))

;;; Spinbox

(cffi:defcfun (spinbox-value "uiSpinboxValue") intmax
  (s :pointer))

(cffi:defcfun (spinbox-set-value "uiSpinboxSetValue") :void
  (s :pointer)
  (value intmax))

(cffi:defcfun (spinbox-on-changed "uiSpinboxOnChanged") :void
  (s :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-spinbox "uiNewSpinbox") :pointer
  (min intmax)
  (max intmax))

;;; ProgressBar

(cffi:defcfun (progress-bar-set-value "uiProgressBarSetValue") :void
  (p :pointer)
  (n :int))

(cffi:defcfun (new-progress-bar "uiNewProgressBar") :pointer)

;;; Slider

(cffi:defcfun (slider-value "uiSliderValue") intmax
  (s :pointer))

(cffi:defcfun (slider-set-value "uiSliderSetValue") :void
  (s :pointer)
  (value intmax))

(cffi:defcfun (slider-on-changed "uiSliderOnChanged") :void
  (s :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-slider "uiNewSlider") :pointer
  (min intmax)
  (max intmax))

;;; Separator

(cffi:defcfun (new-horizontal-separator "uiNewHorizontalSeparator") :pointer)

;;; Comboxbox

(cffi:defcfun (combobox-append "uiComboboxAppend") :void
  (c :pointer)
  (text :string))

(cffi:defcfun (combobox-selected "uiComboboxSelected") intmax
  (c :pointer))

(cffi:defcfun (combobox-set-selected "uiComboboxSetSelected") :void
  (c :pointer)
  (n intmax))

(cffi:defcfun (combobox-on-selected "uiComboboxOnSelected") :void
  (c :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-combobox "uiNewCombobox") :pointer)

;;; EditableComboxbox

(cffi:defcfun (editable-combobox-append "uiEditableComboboxAppend") :void
  (c :pointer)
  (text :string))

(cffi:defcfun (editable-combobox-text "uiEditableComboboxText") ui-string
  (c :pointer))

(cffi:defcfun (editable-combobox-set-text "uiEditableComboboxSetText") :void
  (c :pointer)
  (text :string))

(cffi:defcfun (editable-combobox-on-changed "uiEditableComboboxOnChanged") :void
  (c :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-editable-combobox "uiNewEditableCombobox") :pointer)

;;; RadioButtons

(cffi:defcfun (radio-buttons-append "uiRadioButtonsAppend") :void
  (r :pointer)
  (text :string))

(cffi:defcfun (radio-buttons-selected "uiRadioButtonsSelected") intmax
  (r :pointer))

(cffi:defcfun (radio-buttons-set-selected "uiRadioButtonsSetSelected") :void
  (r :pointer)
  (n intmax))

(cffi:defcfun (radio-buttons-on-selected "uiRadioButtonsOnSelected") :void
  (r :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-radio-buttons "uiNewRadioButtons") :pointer)

;;; DateTimePicker

(cffi:defcfun (new-date-time-picker "uiNewDateTimePicker") :pointer)

(cffi:defcfun (new-date-picker "uiNewDatePicker") :pointer)

(cffi:defcfun (new-time-picker "uiNewTimePicker") :pointer)

;;; MultilineEntry

(cffi:defcfun (multiline-entry-text "uiMultilineEntryText") ui-string
  (e :pointer))

(cffi:defcfun (multiline-entry-set-text "uiMultilineEntrySetText") :void
  (e :pointer)
  (text :string))

(cffi:defcfun (multiline-entry-append "uiMultilineEntryAppend") :void
  (e :pointer)
  (text :string))

(cffi:defcfun (multiline-entry-on-changed "uiMultilineEntryOnChanged") :void
  (e :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (multiline-entry-read-only "uiMultilineEntryReadOnly") (:boolean :int)
  (e :pointer))

(cffi:defcfun (multiline-entry-set-read-only "uiMultilineEntrySetReadOnly") :void
  (e :pointer)
  (readonly (:boolean :int)))

(cffi:defcfun (new-multiline-entry "uiNewMultilineEntry") :pointer)

(cffi:defcfun (new-non-wrapping-multiline-entry "uiNewNonWrappingMultilineEntry") :pointer)

;;; MenuItem

(cffi:defcfun (menu-item-enable "uiMenuItemEnable") :void
  (m :pointer))

(cffi:defcfun (menu-item-disable "uiMenuItemDisable") :void
  (m :pointer))

(cffi:defcfun (menu-item-on-clicked "uiMenuItemOnClicked") :void
  (m :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (menu-item-checked "uiMenuItemChecked") (:boolean :int)
  (m :pointer))

(cffi:defcfun (menu-item-set-checked "uiMenuItemSetChecked") :void
  (m :pointer)
  (checked (:boolean :int)))

;;; Menu

(cffi:defcfun (menu-append-item "uiMenuAppendItem") :pointer
  (m :pointer)
  (name :string))

(cffi:defcfun (menu-append-check-item "uiMenuAppendCheckItem") :pointer
  (m :pointer)
  (name :string))

(cffi:defcfun (menu-append-quit-item "uiMenuAppendQuitItem") :pointer
  (m :pointer))

(cffi:defcfun (menu-append-preferences-item "uiMenuAppendPreferencesItem") :pointer
  (m :pointer))

(cffi:defcfun (menu-append-about-item "uiMenuAppendAboutItem") :pointer
  (m :pointer))

(cffi:defcfun (menu-append-separator "uiMenuAppendSeparator") :void
  (m :pointer))

(cffi:defcfun (new-menu "uiNewMenu") :pointer
  (name :string))

;;; Dialog boxes

(cffi:defcfun (open-file "uiOpenFile") ui-string
  (parent :pointer))

(cffi:defcfun (save-file "uiSaveFile") ui-string
  (parent :pointer))

(cffi:defcfun (msg-box "uiMsgBox") :void
  (parent :pointer)
  (title :string)
  (description :string))

(cffi:defcfun (msg-box-error "uiMsgBoxError") :void
  (parent :pointer)
  (title :string)
  (description :string))

;;; FontButton

(cffi:defcfun (font-button-font "uiFontButtonFont") :pointer
  (b :pointer))

(cffi:defcfun (font-button-on-changed "uiFontButtonOnChanged") :void
  (b :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-font-button "uiNewFontButton") :pointer)

;;; ColorButton

(cffi:defcfun (color-button-color "uiColorButtonColor") :void
  (b :pointer)
  (r :pointer)
  (g :pointer)
  (bl :pointer)
  (a :pointer))

(cffi:defcfun (color-button-set-color "uiColorButtonSetColor") :void
  (b :pointer)
  (r :double)
  (g :double)
  (bl :double)
  (a :double))

(cffi:defcfun (color-button-on-changed "uiColorButtonOnChanged") :void
  (b :pointer)
  (f :pointer)
  (data :pointer))

(cffi:defcfun (new-color-button "uiNewColorButton") :pointer)

;;; Form

(cffi:defcfun (form-append "uiFormAppend") :void
  (f :pointer)
  (label :string)
  (c :pointer)
  (stretchy (:boolean :int)))

(cffi:defcfun (form-padded "uiFormPadded") (:boolean :int)
  (f :pointer))

(cffi:defcfun (form-set-padded "uiFormSetPadded") :void
  (f :pointer)
  (padded (:boolean :int)))

(cffi:defcfun (new-form "uiNewForm") :pointer)
