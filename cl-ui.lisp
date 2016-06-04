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

;;; Control

(defvar *control-table* (make-hash-table))

(defclass control ()
  ((pointer :type cffi:foreign-pointer
            :initform (cffi:null-pointer)
            :reader control-pointer)))

(defun pointer->control (pointer)
  (gethash (cffi:pointer-address pointer) *control-table*))

(defun (setf control-pointer) (pointer control)
  (setf (slot-value control 'pointer)                            pointer
        (gethash (cffi:pointer-address pointer) *control-table*) control))

(defun control-destroy (control)
  (with-slots (pointer) control
    (remhash (cffi:pointer-address pointer) *control-table*)
    (cl-ui.raw:control-destroy pointer)))

(defun control-parent (control)
  (pointer->control (cl-ui.raw:control-parent (control-pointer control))))

(defun (setf control-parent) (parent control)
  (cl-ui.raw:control-set-parent (control-pointer control) (control-pointer parent)))

(defun control-toplevel-p (control)
  (cl-ui.raw:control-toplevel (control-pointer control)))

(defun control-visible-p (control)
  (cl-ui.raw:control-visible (control-pointer control)))

(defun (setf control-visible-p) (visible control)
  (if visible
      (cl-ui.raw:control-show (control-pointer control))
      (cl-ui.raw:control-hide (control-pointer control))))

(defun control-enabled-p (control)
  (cl-ui.raw:control-enabled (control-pointer control)))

(defun (setf control-enabled-p) (enabled control)
  (if enabled
      (cl-ui.raw:control-enable (control-pointer control))
      (cl-ui.raw:control-disable (control-pointer control))))

;;; Window

(defclass window (control)
  ((title :type string :initarg :title :reader window-title)
   (initial-width :type integer :initarg :width)
   (initial-height :type integer :initarg :height)
   (has-menu-bar :type boolean :initarg :has-menu-bar)
   (on-closing :type (or function null) :initform nil :accessor window-on-closing)
   (child :type (or control null) :initform nil :reader window-child)))

(cffi:defcallback %window-on-closing-cb (:boolean :int) ((w :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-closing (window-on-closing (pointer->control w))))
    (when on-closing
      (funcall on-closing))))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (with-slots (title initial-width initial-height has-menu-bar) window
    (setf (control-pointer window)
          (cl-ui.raw:new-window title initial-width initial-height has-menu-bar)))
  (cl-ui.raw:window-on-closing (control-pointer window)
                               (cffi:callback %window-on-closing-cb)
                               (cffi:null-pointer)))

(defun (setf window-title) (title window)
  (setf (slot-value window 'title) title)
  (cl-ui.raw:window-set-title (control-pointer window) title))

(defun (setf window-child) (child window)
  (setf (slot-value window 'child) child)
  (cl-ui.raw:window-set-child (control-pointer window) (control-pointer child)))

(defun window-margined (window)
  (cl-ui.raw:window-margined (control-pointer window)))

(defun (setf window-margined) (margined window)
  (cl-ui.raw:window-set-margined (control-pointer window) margined))

;;; Button

(defclass button (control)
  ((text :type string :initarg :text :reader button-text)
   (on-clicked :type (or function null) :initform nil :accessor button-on-clicked)))

(cffi:defcallback %button-on-clicked-cb :void ((b :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-clicked (button-on-clicked (pointer->control b))))
    (when on-clicked
      (funcall on-clicked))))

(defmethod initialize-instance :after ((button button) &key &allow-other-keys)
  (setf (control-pointer button) (cl-ui.raw:new-button (button-text button)))
  (cl-ui.raw:button-on-clicked (control-pointer button)
                               (cffi:callback %button-on-clicked-cb)
                               (cffi:null-pointer)))

(defun (setf button-text) (text button)
  (setf (slot-value button 'text) text)
  (cl-ui.raw:button-set-text (control-pointer button) text))

;;; Box

(defclass box (control)
  ((direction :type (member :horizontal :vertical) :initarg :direction)))

(defmethod initialize-instance :after ((box box) &key &allow-other-keys)
  (setf (control-pointer box)
        (ecase (slot-value box 'direction)
          (:horizontal (cl-ui.raw:new-horizontal-box))
          (:vertical (cl-ui.raw:new-vertical-box)))))

(defun box-append (box child &key stretchy)
  (cl-ui.raw:box-append (control-pointer box) (control-pointer child) stretchy))

(defun box-delete (box index)
  (cl-ui.raw:box-delete (control-pointer box) index))

(defun box-padded (box)
  (cl-ui.raw:box-padded (control-pointer box)))

(defun (setf box-padded) (padded box)
  (cl-ui.raw:box-set-padded (control-pointer box) padded))

;;; Entry

(defclass entry (control)
  ((on-changed :type (or function null) :initform nil :accessor entry-on-changed)))

(cffi:defcallback %entry-on-changed-cb :void ((e :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (entry-on-changed (pointer->control e))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((entry entry) &key &allow-other-keys)
  (setf (control-pointer entry) (cl-ui.raw:new-entry))
  (cl-ui.raw:entry-on-changed (control-pointer entry)
                              (cffi:callback %entry-on-changed-cb)
                              (cffi:null-pointer)))

(defun entry-text (entry)
  (cl-ui.raw:entry-text (control-pointer entry)))

(defun (setf entry-text) (text entry)
  (cl-ui.raw:entry-set-text (control-pointer entry) text))

(defun entry-read-only (entry)
  (cl-ui.raw:entry-read-only (control-pointer entry)))

(defun (setf entry-read-only) (read-only entry)
  (cl-ui.raw:entry-set-read-only (control-pointer entry) read-only))

;;; Checkbox

(defclass checkbox (control)
  ((text :type string :initarg :text :reader checkbox-text)
   (on-toggled :type (or function null) :initform nil :accessor checkbox-on-toggled)))

(cffi:defcallback %checkbox-on-toggled-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-toggled (checkbox-on-toggled (pointer->control c))))
    (when on-toggled
      (funcall on-toggled))))

(defmethod initialize-instance :after ((checkbox checkbox) &key &allow-other-keys)
  (setf (control-pointer checkbox) (cl-ui.raw:new-checkbox (checkbox-text checkbox)))
  (cl-ui.raw:checkbox-on-toggled (control-pointer checkbox)
                                 (cffi:callback %checkbox-on-toggled-cb)
                                 (cffi:null-pointer)))

(defun (setf checkbox-text) (text checkbox)
  (setf (slot-value checkbox 'text) text)
  (cl-ui.raw:checkbox-set-text (control-pointer checkbox) text))

(defun checkbox-checked (checkbox)
  (cl-ui.raw:checkbox-checked (control-pointer checkbox)))

(defun (setf checkbox-checked) (checked checkbox)
  (cl-ui.raw:checkbox-set-checked (control-pointer checkbox) checked))

;;; Label

(defclass label (control)
  ((text :type string :initarg :text :reader label-text)))

(defmethod initialize-instance :after ((label label) &key &allow-other-keys)
  (setf (control-pointer label) (cl-ui.raw:new-label (label-text label))))

(defun (setf label-text) (text label)
  (setf (slot-value label 'text) text)
  (cl-ui.raw:label-set-text (control-pointer label) text))

;;; Tab

(defclass tab (control)
  ())

(defmethod initialize-instance :after ((tab tab) &key &allow-other-keys)
  (setf (control-pointer tab) (cl-ui.raw:new-tab)))

(defun tab-append (tab name control)
  (cl-ui.raw:tab-append (control-pointer tab) name (control-pointer control)))

(defun tab-insert (tab name control &key (at 0))
  (cl-ui.raw:tab-insert-at (control-pointer tab) name at (control-pointer control)))

(defun tab-delete (tab index)
  (cl-ui.raw:tab-delete (control-pointer tab) index))

(defun tab-num-pages (tab)
  (cl-ui.raw:tab-num-pages (control-pointer tab)))

(defun tab-margined (tab page)
  (cl-ui.raw:tab-margined (control-pointer tab) page))

(defun (setf tab-margined) (margined tab page)
  (cl-ui.raw:tab-set-margined (control-pointer tab) page margined))

;;; Group

(defclass group (control)
  ((title :type string :initarg :title :reader group-title)
   (child :type (or control null) :initform nil :reader group-child)))

(defmethod initialize-instance :after ((group group) &key &allow-other-keys)
  (setf (control-pointer group) (cl-ui.raw:new-group (group-title group))))

(defun (setf group-title) (title group)
  (setf (slot-value group 'title) title)
  (cl-ui.raw:group-set-title (control-pointer group) title))

(defun (setf group-child) (child group)
  (setf (slot-value group 'child) child)
  (cl-ui.raw:group-set-child (control-pointer group) (control-pointer child)))

(defun group-margined (group)
  (cl-ui.raw:group-margined (control-pointer group)))

(defun (setf group-margined) (margined group)
  (cl-ui.raw:group-set-margined (control-pointer group) margined))

;;; Spinbox

(defclass spinbox (control)
  ((min-value :type integer :initarg :min-value :reader spinbox-min-value)
   (max-value :type integer :initarg :max-value :reader spinbox-max-value)
   (on-changed :type (or function null) :initform nil :accessor spinbox-on-changed)))

(cffi:defcallback %spinbox-on-changed-cb :void ((s :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (spinbox-on-changed (pointer->control s))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((spinbox spinbox) &key &allow-other-keys)
  (setf (control-pointer spinbox) (cl-ui.raw:new-spinbox (spinbox-min-value spinbox)
                                                         (spinbox-max-value spinbox)))
  (cl-ui.raw:spinbox-on-changed (control-pointer spinbox)
                                (cffi:callback %spinbox-on-changed-cb)
                                (cffi:null-pointer)))

(defun spinbox-value (spinbox)
  (cl-ui.raw:spinbox-value (control-pointer spinbox)))

(defun (setf spinbox-value) (value spinbox)
  (cl-ui.raw:spinbox-set-value (control-pointer spinbox) value))

;;; ProgressBar

(defclass progress-bar (control)
  ((value :type integer :initform 0 :reader progress-bar-value)))

(defmethod initialize-instance :after ((progress-bar progress-bar) &key &allow-other-keys)
  (setf (control-pointer progress-bar) (cl-ui.raw:new-progress-bar)))

(defun (setf progress-bar-value) (value progress-bar)
  (setf (slot-value progress-bar 'value) value)
  (cl-ui.raw:progress-bar-set-value (control-pointer progress-bar) value))

;;; Slider

(defclass slider (control)
  ((min-value :type integer :initarg :min-value :reader slider-min-value)
   (max-value :type integer :initarg :max-value :reader slider-max-value)
   (on-changed :type (or function null) :initform nil :accessor slider-on-changed)))

(cffi:defcallback %slider-on-changed-cb :void ((s :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (slider-on-changed (pointer->control s))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((slider slider) &key &allow-other-keys)
  (setf (control-pointer slider) (cl-ui.raw:new-slider (slider-min-value slider)
                                                       (slider-max-value slider)))
  (cl-ui.raw:slider-on-changed (control-pointer slider)
                               (cffi:callback %slider-on-changed-cb)
                               (cffi:null-pointer)))

(defun slider-value (slider)
  (cl-ui.raw:slider-value (control-pointer slider)))

(defun (setf slider-value) (value slider)
  (cl-ui.raw:slider-set-value (control-pointer slider) value))

;;; Separator

(defclass separator (control)
  ())

(defmethod initialize-instance :after ((separator separator) &key &allow-other-keys)
  (setf (control-pointer separator) (cl-ui.raw:new-horizontal-separator)))

;;; Combobox

(defclass combobox (control)
  ((on-selected :type (or function null) :initform nil :accessor combobox-on-selected)))

(cffi:defcallback %combobox-on-selected-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-selected (combobox-on-selected (pointer->control c))))
    (when on-selected
      (funcall on-selected))))

(defmethod initialize-instance :after ((combobox combobox) &key &allow-other-keys)
  (setf (control-pointer combobox) (cl-ui.raw:new-combobox))
  (cl-ui.raw:combobox-on-selected (control-pointer combobox)
                                  (cffi:callback %combobox-on-selected-cb)
                                  (cffi:null-pointer)))

(defun combobox-append (combobox text)
  (cl-ui.raw:combobox-append (control-pointer combobox) text))

(defun combobox-selected (combobox)
  (cl-ui.raw:combobox-selected (control-pointer combobox)))

(defun (setf combobox-selected) (selected combobox)
  (cl-ui.raw:combobox-set-selected (control-pointer combobox) selected))

;;; EditableCombobox

(defclass editable-combobox (control)
  ((on-changed :type (or function null) :initform nil :accessor editable-combobox-on-changed)))

(cffi:defcallback %editable-combobox-on-changed-cb :void ((c :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (editable-combobox-on-changed (pointer->control c))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((editable-combobox editable-combobox) &key &allow-other-keys)
  (setf (control-pointer editable-combobox) (cl-ui.raw:new-editable-combobox))
  (cl-ui.raw:editable-combobox-on-changed (control-pointer editable-combobox)
                                          (cffi:callback %editable-combobox-on-changed-cb)
                                          (cffi:null-pointer)))

(defun editable-combobox-append (editable-combobox text)
  (cl-ui.raw:editable-combobox-append (control-pointer editable-combobox) text))

(defun editable-combobox-text (editable-combobox)
  (cl-ui.raw:editable-combobox-text (control-pointer editable-combobox)))

(defun (setf editable-combobox-text) (text editable-combobox)
  (cl-ui.raw:editable-combobox-set-text (control-pointer editable-combobox) text))

;;; RadioButtons

(defclass radio-buttons (control)
  ())

(defmethod initialize-instance :after ((radio-buttons radio-buttons) &key &allow-other-keys)
  (setf (control-pointer radio-buttons) (cl-ui.raw:new-radio-buttons)))

(defun radio-buttons-append (radio-buttons text)
  (cl-ui.raw:radio-buttons-append (control-pointer radio-buttons) text))

;;; DateTimePicker

(defclass date-time-picker (control)
  ((type :type (or :date :time :both) :initarg :type :initform :both)))

(defmethod initialize-instance :after ((date-time-picker date-time-picker) &key &allow-other-keys)
  (setf (control-pointer date-time-picker)
        (ecase (slot-value date-time-picker 'type)
          (:both (cl-ui.raw:new-date-time-picker))
          (:date (cl-ui.raw:new-date-picker))
          (:time (cl-ui.raw:new-time-picker)))))

;;; MultilineEntry

(defclass multiline-entry (control)
  ((wrapping :type boolean :initarg :wrapping :initform t :reader multiline-entry-wrapping-p)
   (on-changed :type (or function null) :initform nil :accessor multiline-entry-on-changed)))

(cffi:defcallback %multiline-entry-on-changed-cb :void ((e :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (multiline-entry-on-changed (pointer->control e))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((multiline-entry multiline-entry) &key &allow-other-keys)
  (setf (control-pointer multiline-entry)
        (if (multiline-entry-wrapping-p multiline-entry)
            (cl-ui.raw:new-multiline-entry)
            (cl-ui.raw:new-non-wrapping-multiline-entry)))
  (cl-ui.raw:multiline-entry-on-changed (control-pointer multiline-entry)
                                        (cffi:callback %multiline-entry-on-changed-cb)
                                        (cffi:null-pointer)))

(defun multiline-entry-text (multiline-entry)
  (cl-ui.raw:multiline-entry-text (control-pointer multiline-entry)))

(defun (setf multiline-entry-text) (text multiline-entry)
  (cl-ui.raw:multiline-entry-set-text (control-pointer multiline-entry) text))

(defun multiline-entry-append (multiline-entry text)
  (cl-ui.raw:multiline-entry-append (control-pointer multiline-entry) text))

(defun multiline-entry-read-only-p (multiline-entry)
  (cl-ui.raw:multiline-entry-read-only (control-pointer multiline-entry)))

(defun (setf multiline-entry-read-only-p) (read-only multiline-entry)
  (cl-ui.raw:multiline-entry-set-read-only (control-pointer multiline-entry) read-only))
