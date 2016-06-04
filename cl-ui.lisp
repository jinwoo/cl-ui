(in-package #:cl-ui)

(cffi:define-foreign-library libui
  (:darwin "libui/out/libui.dylib")
  (:unix "libui/out/libui.so")
  (t (:default "libui")))

(cffi:use-foreign-library libui)

(defun main-step (&key wait)
  (cl-ui.raw:main-step wait))

(defvar *queue-main-callbacks* nil)

(cffi:defcallback %queue-main-callback :void ((data :pointer))
  (declare (ignore data))
  (let ((cb (first (last *queue-main-callbacks*))))
    (when cb
      (funcall cb)
      (setf *queue-main-callbacks* (butlast *queue-main-callbacks*)))))

(defun queue-main (fun)
  (push fun *queue-main-callbacks*)
  (cl-ui.raw:queue-main (cffi:callback %queue-main-callback) (cffi:null-pointer)))

(defvar *on-should-quit* nil)

(cffi:defcallback %on-should-quit (:boolean :int) ((data :pointer))
  (declare (ignore data))
  (if *on-should-quit*
      (funcall *on-should-quit*)
      t))

(defun call-with-ui (fun)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (cffi:with-foreign-object (o '(:struct cl-ui.raw:init-options))
      (let ((err (cl-ui.raw:init o)))
        (unless (cffi:null-pointer-p err)
          (let ((err-string (cffi:foreign-string-to-lisp err)))
            (cl-ui.raw:free-init-error err)
            (error 'simple-error :format-arguments "UI init error: ~A"
                                 :format-control error-string))))
      (unwind-protect
           (progn
             (cl-ui.raw:on-should-quit (cffi:callback %on-should-quit)
                                       (cffi:null-pointer))
             (funcall fun))
        (cl-ui.raw:uninit)))))

(defmacro with-ui (() &body body)
  `(call-with-ui (lambda () ,@body)))

(defun on-should-quit () *on-should-quit*)

(defun (setf on-should-quit) (fun)
  (setf *on-should-quit* fun))

;;; object

(defvar *object-table* (make-hash-table))

(defclass object ()
  ((pointer :type cffi:foreign-pointer
            :initform (cffi:null-pointer)
            :reader object-pointer)))

(defun (setf object-pointer) (pointer object)
  (setf (slot-value object 'pointer)                            pointer
        (gethash (cffi:pointer-address pointer) *object-table*) object))

(defun pointer->object (pointer)
  (gethash (cffi:pointer-address pointer) *object-table*))

;;; control

(defclass control (object)
  ())

(defun control-destroy (control)
  (with-slots (pointer) control
    (remhash (cffi:pointer-address pointer) *object-table*)
    (cl-ui.raw:control-destroy pointer)))

(defun control-parent (control)
  (pointer->object (cl-ui.raw:control-parent (object-pointer control))))

(defun (setf control-parent) (parent control)
  (cl-ui.raw:control-set-parent (object-pointer control) (object-pointer parent)))

(defun control-toplevel-p (control)
  (cl-ui.raw:control-toplevel (object-pointer control)))

(defun control-visible-p (control)
  (cl-ui.raw:control-visible (object-pointer control)))

(defun (setf control-visible-p) (visible control)
  (if visible
      (cl-ui.raw:control-show (object-pointer control))
      (cl-ui.raw:control-hide (object-pointer control))))

(defun control-enabled-p (control)
  (cl-ui.raw:control-enabled (object-pointer control)))

(defun (setf control-enabled-p) (enabled control)
  (if enabled
      (cl-ui.raw:control-enable (object-pointer control))
      (cl-ui.raw:control-disable (object-pointer control))))

;;; window

(defclass window (control)
  ((title :type string :initarg :title :reader window-title)
   (initial-width :type integer :initarg :width)
   (initial-height :type integer :initarg :height)
   (has-menu-bar :type boolean :initarg :has-menu-bar)
   (on-closing :type (or function null) :initform nil :accessor window-on-closing)
   (child :type (or control null) :initform nil :reader window-child)))

(cffi:defcallback %window-on-closing-cb (:boolean :int) ((w :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-closing (window-on-closing (pointer->object w))))
    (when on-closing
      (funcall on-closing))))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (with-slots (title initial-width initial-height has-menu-bar) window
    (setf (object-pointer window)
          (cl-ui.raw:new-window title initial-width initial-height has-menu-bar)))
  (cl-ui.raw:window-on-closing (object-pointer window)
                               (cffi:callback %window-on-closing-cb)
                               (cffi:null-pointer)))

(defun (setf window-title) (title window)
  (setf (slot-value window 'title) title)
  (cl-ui.raw:window-set-title (object-pointer window) title))

(defun (setf window-child) (child window)
  (setf (slot-value window 'child) child)
  (cl-ui.raw:window-set-child (object-pointer window) (object-pointer child)))

(defun window-margined (window)
  (cl-ui.raw:window-margined (object-pointer window)))

(defun (setf window-margined) (margined window)
  (cl-ui.raw:window-set-margined (object-pointer window) margined))

;;; button

(defclass button (control)
  ((text :type string :initarg :text :reader button-text)
   (on-clicked :type (or function null) :initform nil :accessor button-on-clicked)))

(cffi:defcallback %button-on-clicked-cb :void ((b :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-clicked (button-on-clicked (pointer->object b))))
    (when on-clicked
      (funcall on-clicked))))

(defmethod initialize-instance :after ((button button) &key &allow-other-keys)
  (setf (object-pointer button) (cl-ui.raw:new-button (button-text button)))
  (cl-ui.raw:button-on-clicked (object-pointer button)
                               (cffi:callback %button-on-clicked-cb)
                               (cffi:null-pointer)))

(defun (setf button-text) (text button)
  (setf (slot-value button 'text) text)
  (cl-ui.raw:button-set-text (object-pointer button) text))

;;; box

(defclass box (control)
  ((direction :type (member :horizontal :vertical) :initarg :direction)))

(defmethod initialize-instance :after ((box box) &key &allow-other-keys)
  (setf (object-pointer box)
        (ecase (slot-value box 'direction)
          (:horizontal (cl-ui.raw:new-horizontal-box))
          (:vertical (cl-ui.raw:new-vertical-box)))))

(defun box-append (box child &key stretchy)
  (cl-ui.raw:box-append (object-pointer box) (object-pointer child) stretchy))

(defun box-delete (box index)
  (cl-ui.raw:box-delete (object-pointer box) index))

(defun box-padded (box)
  (cl-ui.raw:box-padded (object-pointer box)))

(defun (setf box-padded) (padded box)
  (cl-ui.raw:box-set-padded (object-pointer box) padded))

;;; entry

(defclass entry (control)
  ((on-changed :type (or function null) :initform nil :accessor entry-on-changed)))

(cffi:defcallback %entry-on-changed-cb :void ((e :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (entry-on-changed (pointer->object e))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((entry entry) &key &allow-other-keys)
  (setf (object-pointer entry) (cl-ui.raw:new-entry))
  (cl-ui.raw:entry-on-changed (object-pointer entry)
                              (cffi:callback %entry-on-changed-cb)
                              (cffi:null-pointer)))

(defun entry-text (entry)
  (cl-ui.raw:entry-text (object-pointer entry)))

(defun (setf entry-text) (text entry)
  (cl-ui.raw:entry-set-text (object-pointer entry) text))

(defun entry-read-only (entry)
  (cl-ui.raw:entry-read-only (object-pointer entry)))

(defun (setf entry-read-only) (read-only entry)
  (cl-ui.raw:entry-set-read-only (object-pointer entry) read-only))

;;; checkbox

(defclass checkbox (control)
  ((text :type string :initarg :text :reader checkbox-text)
   (on-toggled :type (or function null) :initform nil :accessor checkbox-on-toggled)))

(cffi:defcallback %checkbox-on-toggled-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-toggled (checkbox-on-toggled (pointer->object c))))
    (when on-toggled
      (funcall on-toggled))))

(defmethod initialize-instance :after ((checkbox checkbox) &key &allow-other-keys)
  (setf (object-pointer checkbox) (cl-ui.raw:new-checkbox (checkbox-text checkbox)))
  (cl-ui.raw:checkbox-on-toggled (object-pointer checkbox)
                                 (cffi:callback %checkbox-on-toggled-cb)
                                 (cffi:null-pointer)))

(defun (setf checkbox-text) (text checkbox)
  (setf (slot-value checkbox 'text) text)
  (cl-ui.raw:checkbox-set-text (object-pointer checkbox) text))

(defun checkbox-checked (checkbox)
  (cl-ui.raw:checkbox-checked (object-pointer checkbox)))

(defun (setf checkbox-checked) (checked checkbox)
  (cl-ui.raw:checkbox-set-checked (object-pointer checkbox) checked))

;;; label

(defclass label (control)
  ((text :type string :initarg :text :reader label-text)))

(defmethod initialize-instance :after ((label label) &key &allow-other-keys)
  (setf (object-pointer label) (cl-ui.raw:new-label (label-text label))))

(defun (setf label-text) (text label)
  (setf (slot-value label 'text) text)
  (cl-ui.raw:label-set-text (object-pointer label) text))

;;; tab

(defclass tab (control)
  ())

(defmethod initialize-instance :after ((tab tab) &key &allow-other-keys)
  (setf (object-pointer tab) (cl-ui.raw:new-tab)))

(defun tab-append (tab name control)
  (cl-ui.raw:tab-append (object-pointer tab) name (object-pointer control)))

(defun tab-insert (tab name control &key (at 0))
  (cl-ui.raw:tab-insert-at (object-pointer tab) name at (object-pointer control)))

(defun tab-delete (tab index)
  (cl-ui.raw:tab-delete (object-pointer tab) index))

(defun tab-num-pages (tab)
  (cl-ui.raw:tab-num-pages (object-pointer tab)))

(defun tab-margined (tab page)
  (cl-ui.raw:tab-margined (object-pointer tab) page))

(defun (setf tab-margined) (margined tab page)
  (cl-ui.raw:tab-set-margined (object-pointer tab) page margined))

;;; group

(defclass group (control)
  ((title :type string :initarg :title :reader group-title)
   (child :type (or control null) :initform nil :reader group-child)))

(defmethod initialize-instance :after ((group group) &key &allow-other-keys)
  (setf (object-pointer group) (cl-ui.raw:new-group (group-title group))))

(defun (setf group-title) (title group)
  (setf (slot-value group 'title) title)
  (cl-ui.raw:group-set-title (object-pointer group) title))

(defun (setf group-child) (child group)
  (setf (slot-value group 'child) child)
  (cl-ui.raw:group-set-child (object-pointer group) (object-pointer child)))

(defun group-margined (group)
  (cl-ui.raw:group-margined (object-pointer group)))

(defun (setf group-margined) (margined group)
  (cl-ui.raw:group-set-margined (object-pointer group) margined))

;;; spinbox

(defclass spinbox (control)
  ((min-value :type integer :initarg :min-value :reader spinbox-min-value)
   (max-value :type integer :initarg :max-value :reader spinbox-max-value)
   (on-changed :type (or function null) :initform nil :accessor spinbox-on-changed)))

(cffi:defcallback %spinbox-on-changed-cb :void ((s :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (spinbox-on-changed (pointer->object s))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((spinbox spinbox) &key &allow-other-keys)
  (setf (object-pointer spinbox) (cl-ui.raw:new-spinbox (spinbox-min-value spinbox)
                                                         (spinbox-max-value spinbox)))
  (cl-ui.raw:spinbox-on-changed (object-pointer spinbox)
                                (cffi:callback %spinbox-on-changed-cb)
                                (cffi:null-pointer)))

(defun spinbox-value (spinbox)
  (cl-ui.raw:spinbox-value (object-pointer spinbox)))

(defun (setf spinbox-value) (value spinbox)
  (cl-ui.raw:spinbox-set-value (object-pointer spinbox) value))

;;; progress-bar

(defclass progress-bar (control)
  ((value :type integer :initform 0 :reader progress-bar-value)))

(defmethod initialize-instance :after ((progress-bar progress-bar) &key &allow-other-keys)
  (setf (object-pointer progress-bar) (cl-ui.raw:new-progress-bar)))

(defun (setf progress-bar-value) (value progress-bar)
  (setf (slot-value progress-bar 'value) value)
  (cl-ui.raw:progress-bar-set-value (object-pointer progress-bar) value))

;;; slider

(defclass slider (control)
  ((min-value :type integer :initarg :min-value :reader slider-min-value)
   (max-value :type integer :initarg :max-value :reader slider-max-value)
   (on-changed :type (or function null) :initform nil :accessor slider-on-changed)))

(cffi:defcallback %slider-on-changed-cb :void ((s :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (slider-on-changed (pointer->object s))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((slider slider) &key &allow-other-keys)
  (setf (object-pointer slider) (cl-ui.raw:new-slider (slider-min-value slider)
                                                       (slider-max-value slider)))
  (cl-ui.raw:slider-on-changed (object-pointer slider)
                               (cffi:callback %slider-on-changed-cb)
                               (cffi:null-pointer)))

(defun slider-value (slider)
  (cl-ui.raw:slider-value (object-pointer slider)))

(defun (setf slider-value) (value slider)
  (cl-ui.raw:slider-set-value (object-pointer slider) value))

;;; separator

(defclass separator (control)
  ())

(defmethod initialize-instance :after ((separator separator) &key &allow-other-keys)
  (setf (object-pointer separator) (cl-ui.raw:new-horizontal-separator)))

;;; combobox

(defclass combobox (control)
  ((on-selected :type (or function null) :initform nil :accessor combobox-on-selected)))

(cffi:defcallback %combobox-on-selected-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-selected (combobox-on-selected (pointer->object c))))
    (when on-selected
      (funcall on-selected))))

(defmethod initialize-instance :after ((combobox combobox) &key &allow-other-keys)
  (setf (object-pointer combobox) (cl-ui.raw:new-combobox))
  (cl-ui.raw:combobox-on-selected (object-pointer combobox)
                                  (cffi:callback %combobox-on-selected-cb)
                                  (cffi:null-pointer)))

(defun combobox-append (combobox text)
  (cl-ui.raw:combobox-append (object-pointer combobox) text))

(defun combobox-selected (combobox)
  (cl-ui.raw:combobox-selected (object-pointer combobox)))

(defun (setf combobox-selected) (selected combobox)
  (cl-ui.raw:combobox-set-selected (object-pointer combobox) selected))

;;; editable-combobox

(defclass editable-combobox (control)
  ((on-changed :type (or function null) :initform nil :accessor editable-combobox-on-changed)))

(cffi:defcallback %editable-combobox-on-changed-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (editable-combobox-on-changed (pointer->object c))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((editable-combobox editable-combobox) &key &allow-other-keys)
  (setf (object-pointer editable-combobox) (cl-ui.raw:new-editable-combobox))
  (cl-ui.raw:editable-combobox-on-changed (object-pointer editable-combobox)
                                          (cffi:callback %editable-combobox-on-changed-cb)
                                          (cffi:null-pointer)))

(defun editable-combobox-append (editable-combobox text)
  (cl-ui.raw:editable-combobox-append (object-pointer editable-combobox) text))

(defun editable-combobox-text (editable-combobox)
  (cl-ui.raw:editable-combobox-text (object-pointer editable-combobox)))

(defun (setf editable-combobox-text) (text editable-combobox)
  (cl-ui.raw:editable-combobox-set-text (object-pointer editable-combobox) text))

;;; radio-buttons

(defclass radio-buttons (control)
  ())

(defmethod initialize-instance :after ((radio-buttons radio-buttons) &key &allow-other-keys)
  (setf (object-pointer radio-buttons) (cl-ui.raw:new-radio-buttons)))

(defun radio-buttons-append (radio-buttons text)
  (cl-ui.raw:radio-buttons-append (object-pointer radio-buttons) text))

;;; date-time-picker

(defclass date-time-picker (control)
  ((type :type (or :date :time :both) :initarg :type :initform :both)))

(defmethod initialize-instance :after ((date-time-picker date-time-picker) &key &allow-other-keys)
  (setf (object-pointer date-time-picker)
        (ecase (slot-value date-time-picker 'type)
          (:both (cl-ui.raw:new-date-time-picker))
          (:date (cl-ui.raw:new-date-picker))
          (:time (cl-ui.raw:new-time-picker)))))

;;; multiline-entry

(defclass multiline-entry (control)
  ((wrapping :type boolean :initarg :wrapping :initform t :reader multiline-entry-wrapping-p)
   (on-changed :type (or function null) :initform nil :accessor multiline-entry-on-changed)))

(cffi:defcallback %multiline-entry-on-changed-cb :void ((e :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (multiline-entry-on-changed (pointer->object e))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((multiline-entry multiline-entry) &key &allow-other-keys)
  (setf (object-pointer multiline-entry)
        (if (multiline-entry-wrapping-p multiline-entry)
            (cl-ui.raw:new-multiline-entry)
            (cl-ui.raw:new-non-wrapping-multiline-entry)))
  (cl-ui.raw:multiline-entry-on-changed (object-pointer multiline-entry)
                                        (cffi:callback %multiline-entry-on-changed-cb)
                                        (cffi:null-pointer)))

(defun multiline-entry-text (multiline-entry)
  (cl-ui.raw:multiline-entry-text (object-pointer multiline-entry)))

(defun (setf multiline-entry-text) (text multiline-entry)
  (cl-ui.raw:multiline-entry-set-text (object-pointer multiline-entry) text))

(defun multiline-entry-append (multiline-entry text)
  (cl-ui.raw:multiline-entry-append (object-pointer multiline-entry) text))

(defun multiline-entry-read-only-p (multiline-entry)
  (cl-ui.raw:multiline-entry-read-only (object-pointer multiline-entry)))

(defun (setf multiline-entry-read-only-p) (read-only multiline-entry)
  (cl-ui.raw:multiline-entry-set-read-only (object-pointer multiline-entry) read-only))

;;; menu-item

(defclass menu-item (object)
  ((enabled :type boolean :initform t :reader menu-item-enabled-p)
   (on-clicked :type (or function null) :initform nil :accessor menu-item-on-clicked)))

(cffi:defcallback %menu-item-on-clicked-cb :void ((menu-item :pointer)
                                                  (window :pointer)
                                                  (data :pointer))
  (declare (ignore data))
  (let ((on-clicked (menu-item-on-clicked (pointer->object menu-item))))
    (when on-clicked
      (funcall on-clicked (pointer->object window)))))

(defun %make-menu-item (pointer &key (install-on-clicked t))
  (let ((menu-item (make-instance 'menu-item)))
    (setf (object-pointer menu-item) pointer)
    (when install-on-clicked
      (cl-ui.raw:menu-item-on-clicked (object-pointer menu-item)
                                      (cffi:callback %menu-item-on-clicked-cb)
                                      (cffi:null-pointer)))
    menu-item))

(defun (setf menu-item-enabled-p) (enabled menu-item)
  (setf (slot-value menu-item 'enabled) enabled)
  (if enabled
      (cl-ui.raw:menu-item-enable (object-pointer menu-item))
      (cl-ui.raw:menu-item-disable (object-pointer menu-item))))

(defun menu-item-checked-p (menu-item)
  (cl-ui.raw:menu-item-checked (object-pointer menu-item)))

(defun (setf menu-item-checked-p) (checked menu-item)
  (cl-ui.raw:menu-item-set-checked (object-pointer menu-item) checked))

;;; menu

(defclass menu (object)
  ((name :type string :initarg :name :reader menu-name)))

(defmethod initialize-instance :after ((menu menu) &key &allow-other-keys)
  (setf (object-pointer menu) (cl-ui.raw:new-menu (menu-name menu))))

(defun menu-append-item (menu name)
  (%make-menu-item (cl-ui.raw:menu-append-item (object-pointer menu) name)))

(defun menu-append-check-item (menu name)
  (%make-menu-item (cl-ui.raw:menu-append-check-item (object-pointer menu) name)))

(defun menu-append-quit-item (menu)
  (%make-menu-item (cl-ui.raw:menu-append-quit-item (object-pointer menu))
                   :install-on-clicked nil))

(defun menu-append-preferences-item (menu)
  (%make-menu-item (cl-ui.raw:menu-append-preferences-item (object-pointer menu))))

(defun menu-append-about-item (menu)
  (%make-menu-item (cl-ui.raw:menu-append-about-item (object-pointer menu))))

(defun menu-append-separator (menu)
  (cl-ui.raw:menu-append-separator (object-pointer menu)))

;;; dialog boxes

(defun open-file (parent)
  (cl-ui.raw:open-file (object-pointer parent)))

(defun save-file (parent)
  (cl-ui.raw:save-file (object-pointer parent)))

(defun msg-box (parent title description)
  (cl-ui.raw:msg-box (object-pointer parent) title description))

(defun msg-box-error (parent title description)
  (cl-ui.raw:msg-box-error (object-pointer parent) title description))

;;; font-button

(defclass font-button (control)
  ((on-changed :type (or function null) :initform nil :accessor font-button-on-changed)))

(cffi:defcallback %font-button-on-changed-cb :void ((font-button :pointer)
                                                    (data :pointer))
  (declare (ignore data))
  (let ((on-changed (font-button-on-changed (pointer->object font-button))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((font-button font-button) &key &allow-other-keys)
  (setf (object-pointer font-button) (cl-ui.raw:new-font-button))
  (cl-ui.raw:font-button-on-changed (object-pointer font-button)
                                    (cffi:callback %font-button-on-changed-cb)
                                    (cffi:null-pointer)))

(defun font-button-font (font-button)
  (cl-ui.raw:font-button-font (object-pointer font-button)))

;;; color-button

(defclass color-button (control)
  ((on-changed :type (or function null) :initform nil :accessor color-button-on-changed)))

(cffi:defcallback %color-button-on-changed-cb :void ((color-button :pointer)
                                                     (data :pointer))
  (declare (ignore data))
  (let ((on-changed (color-button-on-changed (pointer->object color-button))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((color-button color-button) &key &allow-other-keys)
  (setf (object-pointer color-button) (cl-ui.raw:new-color-button))
  (cl-ui.raw:color-button-on-changed (object-pointer color-button)
                                     (cffi:callback %color-button-on-changed-cb)
                                     (cffi:null-pointer)))

(defun color-button-color (color-button)
  (cffi:with-foreign-objects ((r :double) (g :double) (b :double) (a :double))
    (cl-ui.raw:color-button-color (object-pointer color-button)
                                  r g b a)
    (list r g b a)))

(defun (setf color-button-color) (values color-button)
  (destructuring-bind (r g b a) values
    (cl-ui.raw:color-button-set-color (object-pointer color-button)
                                      r g b a)))
