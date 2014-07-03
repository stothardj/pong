(ns pong.menu)

(defrecord Menu [options selected])

(defn menu
  "Given an alternating sequence of keywords to strings create a menu object.
   Do not use a map as then ordering will be lost."
  [& options]
  {:pre [(even? (count options))]}
  (map->Menu {:options (partition 2 options)
    :selected 0}))

(defn- menu-change [menu f]
  (update-in menu [:selected] #(mod (f %) (count (:options menu)))))

(defn menu-next
  "Returns a menu with the next option selected. Wraps."
  [menu]
  (menu-change menu inc))

(defn menu-prev
  "Returns a menu with the prev option selected. Wraps."
  [menu]
  (menu-change menu dec))

;; (extend-type Menu
;;   ISeq
;;   (first [menu]))
