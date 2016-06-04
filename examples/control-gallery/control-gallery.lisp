(in-package #:control-gallery)

(defvar *mainwin* nil)

(cffi:defcallback open-clicked :void ((item :pointer) (w :pointer) (data :pointer))
  (declare (ignore item w data))
  (let ((filename (cl-ui.raw:open-file (cl-ui::control-pointer *mainwin*))))
    (if (null filename)
        (cl-ui.raw:msg-box-error (cl-ui::control-pointer *mainwin*) "No file selected" "Don't be alarmed!")
        (cl-ui.raw:msg-box (cl-ui::control-pointer *mainwin*) "File selected" filename))))

(cffi:defcallback save-clicked :void ((item :pointer) (w :pointer) (data :pointer))
  (declare (ignore item w data))
  (let ((filename (cl-ui.raw:save-file (cl-ui::control-pointer *mainwin*))))
    (if (null filename)
        (cl-ui.raw:msg-box-error (cl-ui::control-pointer *mainwin*) "No file selected" "Don't be alarmed!")
        (cl-ui.raw:msg-box (cl-ui::control-pointer *mainwin*) "File selected (don't worry, it's still there)"
                           filename))))

(defvar *spinbox* nil)
(defvar *slider* nil)
(defvar *progressbar* nil)

(defun update (value)
  (cl-ui.raw:spinbox-set-value *spinbox* value)
  (cl-ui.raw:slider-set-value *slider* value)
  (cl-ui.raw:progress-bar-set-value *progressbar* value))

(cffi:defcallback on-spinbox-changed :void ((s :pointer) (data :pointer))
  (declare (ignore s data))
  (update (cl-ui.raw:spinbox-value *spinbox*)))

(cffi:defcallback on-slider-changed :void ((s :pointer) (data :pointer))
  (declare (ignore s data))
  (update (cl-ui.raw:slider-value *slider*)))

(defun %main ()
  (let ((menu (cl-ui.raw:new-menu "File")))
    (cl-ui.raw:menu-item-on-clicked (cl-ui.raw:menu-append-item menu "Open")
                                    (cffi:callback open-clicked) (cffi:null-pointer))
    (cl-ui.raw:menu-item-on-clicked (cl-ui.raw:menu-append-item menu "Save")
                                    (cffi:callback save-clicked) (cffi:null-pointer))
    (cl-ui.raw:menu-append-quit-item menu)
    (setf (cl-ui:on-should-quit) (lambda ()
                                   (cl-ui:control-destroy *mainwin*)
                                   (setf *mainwin* nil)
                                   t)))
  (let ((menu (cl-ui.raw:new-menu "Edit")))
    (cl-ui.raw:menu-append-check-item menu "Checkable Item")
    (cl-ui.raw:menu-append-separator menu)
    (cl-ui.raw:menu-item-disable (cl-ui.raw:menu-append-item menu "Disabled Item"))
    (cl-ui.raw:menu-append-preferences-item menu))
  (let ((menu (cl-ui.raw:new-menu "Help")))
    (cl-ui.raw:menu-append-item menu "Help")
    (cl-ui.raw:menu-append-about-item menu))

  (setf *mainwin* (make-instance 'cl-ui:window :title "libui Control Gallery"
                                               :width 640
                                               :height 480
                                               :has-menu-bar t))
  (setf (cl-ui:window-margined *mainwin*) t
        (cl-ui:window-on-closing *mainwin*) (lambda ()
                                              (cl-ui:control-destroy *mainwin*)
                                              (setf *mainwin* nil)
                                              (cl-ui:quit)
                                              nil))

  (let ((box (make-instance 'cl-ui:box :direction :vertical))
        (hbox (make-instance 'cl-ui:box :direction :horizontal))
        (group (cl-ui.raw:new-group "Basic Controls"))
        (inner (make-instance 'cl-ui:box :direction :vertical))
        (entry (make-instance 'cl-ui:entry))
        (inner2 (make-instance 'cl-ui:box :direction :vertical))
        (group2 (cl-ui.raw:new-group "Numbers"))
        (inner3 (make-instance 'cl-ui:box :direction :vertical))
        (group3 (cl-ui.raw:new-group "Lists"))
        (inner4 (make-instance 'cl-ui:box :direction :vertical))
        (cbox (cl-ui.raw:new-combobox))
        (ecbox (cl-ui.raw:new-editable-combobox))
        (rb (cl-ui.raw:new-radio-buttons))
        (tab (cl-ui.raw:new-tab)))
    (setf (cl-ui:box-padded box) t
          (cl-ui:window-child *mainwin*) box
          (cl-ui:box-padded hbox) t)
    (cl-ui:box-append box hbox :stretchy t)

    (cl-ui.raw:group-set-margined group t)
    (cl-ui.raw:box-append (cl-ui::control-pointer hbox) group nil)

    (setf (cl-ui:box-padded inner) t)
    (cl-ui.raw:group-set-child group (cl-ui::control-pointer inner))

    (cl-ui:box-append inner (make-instance 'cl-ui:button :text "Button"))
    (cl-ui:box-append inner (make-instance 'cl-ui:checkbox :text "Checkbox"))
    (setf (cl-ui:entry-text entry) "Entry")
    (cl-ui:box-append inner entry)
    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-label "Label") nil)

    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-horizontal-separator) nil)

    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-date-picker) nil)
    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-time-picker) nil)
    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-date-time-picker) nil)

    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-font-button) nil)

    (cl-ui.raw:box-append (cl-ui::control-pointer inner) (cl-ui.raw:new-color-button) nil)

    (setf (cl-ui:box-padded inner2) t)
    (cl-ui:box-append hbox inner2 :stretchy t)

    (cl-ui.raw:group-set-margined group2 t)
    (cl-ui.raw:box-append (cl-ui::control-pointer inner2) group2 nil)

    (setf (cl-ui:box-padded inner3) t)
    (cl-ui.raw:group-set-child group2 (cl-ui::control-pointer inner3))

    (setf *spinbox* (cl-ui.raw:new-spinbox 0 100))
    (cl-ui.raw:spinbox-on-changed *spinbox* (cffi:callback on-spinbox-changed)
                                  (cffi:null-pointer))
    (cl-ui.raw:box-append (cl-ui::control-pointer inner3) *spinbox* nil)

    (setf *slider* (cl-ui.raw:new-slider 0 100))
    (cl-ui.raw:slider-on-changed *slider* (cffi:callback on-slider-changed)
                                 (cffi:null-pointer))
    (cl-ui.raw:box-append (cl-ui::control-pointer inner3) *slider* nil)

    (setf *progressbar* (cl-ui.raw:new-progress-bar))
    (cl-ui.raw:box-append (cl-ui::control-pointer inner3) *progressbar* nil)

    (cl-ui.raw:group-set-margined group3 t)
    (cl-ui.raw:box-append (cl-ui::control-pointer inner2) group3 nil)

    (setf (cl-ui:box-padded inner4) t)
    (cl-ui.raw:group-set-child group3 (cl-ui::control-pointer inner4))

    (cl-ui.raw:combobox-append cbox "Combobox Item 1")
    (cl-ui.raw:combobox-append cbox "Combobox Item 2")
    (cl-ui.raw:combobox-append cbox "Combobox Item 3")
    (cl-ui.raw:box-append (cl-ui::control-pointer inner4) cbox nil)

    (cl-ui.raw:editable-combobox-append ecbox "Editable Item 1")
    (cl-ui.raw:editable-combobox-append ecbox "Editable Item 2")
    (cl-ui.raw:editable-combobox-append ecbox "Editable Item 3")
    (cl-ui.raw:box-append (cl-ui::control-pointer inner4) ecbox nil)

    (cl-ui.raw:radio-buttons-append rb "Radio Button 1")
    (cl-ui.raw:radio-buttons-append rb "Radio Button 2")
    (cl-ui.raw:radio-buttons-append rb "Radio Button 3")
    (cl-ui.raw:box-append (cl-ui::control-pointer inner4) rb t)

    (cl-ui.raw:tab-append tab "Page 1" (cl-ui.raw:new-horizontal-box))
    (cl-ui.raw:tab-append tab "Page 2" (cl-ui.raw:new-horizontal-box))
    (cl-ui.raw:tab-append tab "Page 3" (cl-ui.raw:new-horizontal-box))
    (cl-ui.raw:box-append (cl-ui::control-pointer inner2) tab t))

  (setf (cl-ui:control-visible-p *mainwin*) t)
  (cl-ui:main))

(defun main ()
  (cl-ui:with-ui ()
    (%main)))
