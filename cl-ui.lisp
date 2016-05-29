(in-package #:cl-ui)

(defparameter *libui-path* "~/playground/libui/out/libui.dylib")

(cffi:load-foreign-library *libui-path*)

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
  (when *on-should-quit*
    (funcall *on-should-quit*)))

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

(defun set-control-pointer (control pointer)
  (setf (slot-value control 'pointer) pointer
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

(cffi:defcallback %window-on-closing-cb :int ((w :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-closing (window-on-closing (pointer->control w))))
    (when on-closing
      (funcall on-closing))))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (with-slots (title initial-width initial-height has-menu-bar) window
    (set-control-pointer window
                         (cl-ui.raw:new-window title initial-width initial-height
                                               has-menu-bar)))
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

(cffi:defcallback %button-on-clicked-cb :int ((b :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-clicked (button-on-closing (pointer->control b))))
    (when on-clicked
      (funcall on-clicked))))

(defmethod initialize-instance :after ((button button) &key &allow-other-keys)
  (set-control-pointer button (cl-ui.raw:new-button (slot-value button 'text)))
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
  (set-control-pointer box
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

(cffi:defcallback %entry-on-changed-cb :int ((e :pointer) (data :pointer))
  (declare (ignore data))
  (let ((on-changed (entry-on-changed (pointer->control e))))
    (when on-changed
      (funcall on-changed))))

(defmethod initialize-instance :after ((entry entry) &key &allow-other-keys)
  (set-control-pointer entry (cl-ui.raw:new-entry))
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
