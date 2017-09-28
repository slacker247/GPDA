;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw.clp
;; Simple example of using the jess.awt.Canvas class to draw in a
;; Jess GUI without writing any Java code!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; painter(java.awt.Component, java.awt.Graphics)
;; Draw a red X on a blue background. The X goes between the 
;; component's corners.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction painter (?canvas ?graph)
  (bind ?x (get-member (call ?canvas getSize) width))
  (bind ?y (get-member (call ?canvas getSize) height))
  (?graph setColor (get-member java.awt.Color blue))
  (?graph fillRect 0 0 ?x ?y)
  (?graph setColor (get-member java.awt.Color red))
  (?graph drawLine 0 0 ?x ?y)
  (?graph drawLine ?x 0 0 ?y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closer(java.awt.event.WindowEvent)
;; Your generic window-closing event handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction closer (?event)
   (if (= (call ?event getID) (get-member ?event WINDOW_CLOSING)) then 
     (call (get ?event source) dispose)
     (call java.lang.System exit 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create some components. Note how we use the special Canvas subclass
;; and tell it which function to call to paint itself. We also install
;; a handler so the window responds to close events.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction create-components ()  
  (bind ?f (new java.awt.Frame "Drawing Demo"))
  (?f addWindowListener (new jess.awt.WindowListener closer (engine)))  
  (bind ?c (new jess.awt.Canvas painter (engine)))
  (?f add "Center" ?c)
  (?c setSize 100 100)
  (?f pack)
  (?f show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put things together and display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-components)

