(import ./zipper :as z)

(defn has-children?
  ``
  Returns true if `node` can have children.
  Returns false if `node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (has-children? [:number @{} "8"])
  # =>
  false

  )

(defn zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [tree]
  (defn branch?
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (has-children? a-node))))
  #
  (defn children
    [a-node]
    (if (branch? a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node
    [a-node children]
    [(first a-node) @{} ;children])
  #
  (z/zipper tree
            :branch? branch?
            :children children
            :make-node make-node))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (zip root-node))

  the-node
  # =>
  root-node

  (merge {} the-state)
  # =>
  @{}

  )

(defn attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers.
  ``
  [zloc]
  (get (z/node zloc) 1))

(comment

  (type (import ./location :as l))
  # =>
  :table

  )

(comment

  (-> (l/ast "(+ 1 3)")
      zip
      z/down
      attrs)
  # =>
  @{:bc 1 :bl 1 :ec 8 :el 1}

  )

(defn zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [tree]
  (-> (zip tree)
      z/down))

(comment

  #(import ./location :as l)

  (-> (l/ast "(+ 1 3)")
      zip-down
      z/node)
  # =>
  '(:tuple @{:bc 1 :bl 1
             :ec 8 :el 1}
           (:symbol @{:bc 2 :bl 1
                      :ec 3 :el 1} "+")
           (:whitespace @{:bc 3 :bl 1
                          :ec 4 :el 1} " ")
           (:number @{:bc 4 :bl 1
                      :ec 5 :el 1} "1")
           (:whitespace @{:bc 5 :bl 1
                          :ec 6 :el 1} " ")
           (:number @{:bc 6 :bl 1
                      :ec 7 :el 1} "3"))

  )

(defn right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (z/right zloc)]
    (if (pred right-sib)
      right-sib
      (right-until right-sib pred))))

(comment

  (-> [:code @{}
       [:tuple @{}
        [:comment @{} "# hi there"] [:whitespace @{} "\n"]
        [:symbol @{} "+"] [:whitespace @{} " "]
        [:number @{} "1"] [:whitespace @{} " "]
        [:number @{} "2"]]]
      zip-down
      z/down
      (right-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      z/node)
  # =>
  [:symbol @{} "+"]

  (-> [:code @{}
       [:tuple @{}
        [:keyword @{} ":a"]]]
      zip-down
      z/down
      (right-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true)))
  # =>
  nil

  )

# wsc == whitespace, comment
(defn right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (right-until zloc
               |(match (z/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (l/ast "(:a)")
      zip-down
      z/down
      right-skip-wsc)
  # =>
  nil

  )

(defn left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (z/left zloc)]
    (if (pred left-sib)
      left-sib
      (left-until left-sib pred))))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      right-skip-wsc
      (left-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> [:code @{}
       [:tuple @{}
        [:keyword @{} ":a"]]]
      zip-down
      z/down
      (left-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true)))
  # =>
  nil

  )

(defn left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (left-until zloc
               |(match (z/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      right-skip-wsc
      left-skip-wsc
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  )

