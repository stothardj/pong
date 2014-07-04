(ns pong.menu)

(defrecord Menu [options selected])

(defn menu
  "Given an alternating sequence of keywords to strings create a menu object."
  [& options]
  {:pre [(even? (count options))]}
  (map->Menu {:options (partition 2 options)
    :selected 0}))

(defn- change [menu f]
  (update-in menu [:selected] #(mod (f %) (count (:options menu)))))

(defn select-next
  "Returns a menu with the next option selected. Wraps."
  [menu]
  (change menu inc))

(defn select-prev
  "Returns a menu with the prev option selected. Wraps."
  [menu]
  (change menu dec))

(defn options
  "Return a convenient sequence for iterating over. Attaches metadata to indicate selected item."
  [menu]
  (map-indexed #(with-meta %2 {:selected (= (:selected menu) %)}) (:options menu)))

(defn options-indexed
  "Similar to options, but also includes the index on each item."
  [menu]
  (map-indexed (fn [idx itm] [idx itm]) (options menu)))

(defn selected
  "Returns the item which is currently selected."
  [menu]
  (nth (:options menu) (:selected menu)))
