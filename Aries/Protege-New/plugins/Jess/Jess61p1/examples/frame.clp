; -*- clips -*-

;; **********************************************************************
;;                           Frame.clp
;;
;; A nifty example of building a GUI using jess reflection.
;; Using this package, we can create java objects, call their methods,
;; access their fields, and respond to GUI events.
;; You can therefore build en entire GUI application without actually
;; writing any Java code!
;;
;; **********************************************************************

;; ******************************
;; Declarations

(import java.awt.*)
(import jess.awt.*)

;; ******************************
;; DEFGLOBALS

(defglobal ?*f* = 0)
(defglobal ?*m* = 0)

;; ******************************
;; DEFFUNCTIONS

(deffunction create-frame ()
  (bind ?*f* (new Frame "Jess Reflection Demo"))
  (set ?*f* background (new Color 255 0 255))
  (set ?*f* layout (new GridLayout 1 2)))


(deffunction add-widgets ()
  (?*f* add (new Label "This is: "))
  (bind ?*m* (new Choice))
  (?*m* addItem "Cool")
  (?*m* addItem "Really Cool")
  (?*m* addItem "Awesome")
  (?*f* add ?*m*))


(deffunction add-behaviours ()
  (?*f* addWindowListener
        (new WindowListener frame-handler (engine)))
  (?*m* addItemListener
        (new ItemListener menu-handler (engine))))

(deffunction show-frame ()
  (?*f* validate)
  (?*f* pack)
  (?*f* show))

(deffunction frame-handler (?event)
  (if (= (?event getID) (get-member ?event WINDOW_CLOSING)) then 
    (call (get ?event source) dispose)
    (call System exit 0)))

(deffunction menu-handler (?event)
  (printout t "You chose: " (call (get ?event item) toString) crlf))

;; ******************************
;; Run the program

(create-frame)
(add-widgets)
(add-behaviours)
(show-frame)

