# bl - begin line
# bc - begin column
# el - end line
# ec - end column
(defn make-attrs
  [& items]
  (zipcoll [:bl :bc :el :ec]
           items))

(defn atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column)
                           ,peg-form
                           (line) (column)))
        ,|[node-type (make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column)))
        ,|[node-type (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
           ;(slice $& 2 -4)]))

(defn collection-node
  [node-type open-delim close-delim]
  # to avoid issues when transforming this file
  (def replace_ (symbol "replace"))
  ~(cmt
     (capture
       (sequence
         (line) (column)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (,replace_ (sequence (line) (column))
                              ,|(string/format
                                  "line: %p column: %p missing %p for %p"
                                  $0 $1 close-delim node-type))))
         (line) (column)))
     ,|[node-type (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
        ;(slice $& 2 -4)]))

(def loc-grammar
  ~@{:main (sequence (line) (column)
                     (some :input)
                     (line) (column))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(atom-node :whitespace
                             '(choice (some (set " \0\f\t\v"))
                                      (choice "\r\n"
                                              "\r"
                                              "\n")))
     # :whitespace
     # (cmt (capture (sequence (line) (column)
     #                         (choice (some (set " \0\f\t\v"))
     #                                 (choice "\r\n"
     #                                         "\r"
     #                                         "\n"))
     #                         (line) (column)))
     #      ,|[:whitespace (make-attrs ;(slice $& 0 -2)) (last $&)])
     #
     :comment ,(atom-node :comment
                          '(sequence "#"
                                     (any (if-not (set "\r\n") 1))))
     #
     :form (choice # reader macros
                   :fn
                   :quasiquote
                   :quote
                   :splice
                   :unquote
                   # collections
                   :array
                   :bracket-array
                   :tuple
                   :bracket-tuple
                   :table
                   :struct
                   # atoms
                   :number
                   :constant
                   :buffer
                   :string
                   :long-buffer
                   :long-string
                   :keyword
                   :symbol)
     #
     :fn ,(reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(reader-macro-node :quasiquote "~")
     #
     :quote ,(reader-macro-node :quote "'")
     #
     :splice ,(reader-macro-node :splice ";")
     #
     :unquote ,(reader-macro-node :unquote ",")
     #
     :array ,(collection-node :array "@(" ")")
     # :array
     # (cmt
     #   (capture
     #     (sequence
     #       (line) (column)
     #       "@("
     #       (any :input)
     #       (choice ")"
     #               (error
     #                 (replace (sequence (line) (column))
     #                          ,|(string/format
     #                              "line: %p column: %p missing %p for %p"
     #                              $0 $1 ")" :array))))
     #       (line) (column)))
     #   ,|[:array (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #      ;(slice $& 2 -4)])
     #
     :tuple ,(collection-node :tuple "(" ")")
     #
     :bracket-array ,(collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(collection-node :bracket-tuple "[" "]")
     #
     :table ,(collection-node :table "@{" "}")
     #
     :struct ,(collection-node :struct "{" "}")
     #
     :number ,(atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(atom-node :buffer
                         '(sequence `@"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :escape (sequence "\\"
                       (choice (set `"'0?\abefnrtvz`)
                               (sequence "x" (2 :h))
                               (sequence "u" (4 :h))
                               (sequence "U" (6 :h))
                               (error (constant "bad escape"))))
     #
     :string ,(atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(atom-node :long-string
                              :long-bytes)
     #
     :long-bytes {:main (drop (sequence :open
                                        (any (if-not :close 1))
                                        :close))
                  :open (capture :delim :n)
                  :delim (some "`")
                  :close (cmt (sequence (not (look -1 "`"))
                                        (backref :n)
                                        (capture (backmatch :n)))
                              ,=)}
     #
     :long-buffer ,(atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match loc-grammar " ") 2)
  # =>
  '(:whitespace @{:bc 1 :bl 1 :ec 2 :el 1} " ")

  (get (peg/match loc-grammar "true?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 6 :el 1} "true?")

  (get (peg/match loc-grammar "nil?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 5 :el 1} "nil?")

  (get (peg/match loc-grammar "false?") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "false?")

  (get (peg/match loc-grammar "# hi there") 2)
  # =>
  '(:comment @{:bc 1 :bl 1 :ec 11 :el 1} "# hi there")

  (get (peg/match loc-grammar "1_000_000") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 10 :el 1} "1_000_000")

  (get (peg/match loc-grammar "8.3") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "8.3")

  (get (peg/match loc-grammar "1e2") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "1e2")

  (get (peg/match loc-grammar "0xfe") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "0xfe")

  (get (peg/match loc-grammar "2r01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "2r01")

  (get (peg/match loc-grammar "3r101&01") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 9 :el 1} "3r101&01")

  (get (peg/match loc-grammar "2:u") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 4 :el 1} "2:u")

  (get (peg/match loc-grammar "-8:s") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 5 :el 1} "-8:s")

  (get (peg/match loc-grammar "1e2:n") 2)
  # =>
  '(:number @{:bc 1 :bl 1 :ec 6 :el 1} "1e2:n")

  (get (peg/match loc-grammar "printf") 2)
  # =>
  '(:symbol @{:bc 1 :bl 1 :ec 7 :el 1} "printf")

  (get (peg/match loc-grammar ":smile") 2)
  # =>
  '(:keyword @{:bc 1 :bl 1 :ec 7 :el 1} ":smile")

  (get (peg/match loc-grammar `"fun"`) 2)
  # =>
  '(:string @{:bc 1 :bl 1 :ec 6 :el 1} "\"fun\"")

  (get (peg/match loc-grammar "``long-fun``") 2)
  # =>
  '(:long-string @{:bc 1 :bl 1 :ec 13 :el 1} "``long-fun``")

  (get (peg/match loc-grammar "@``long-buffer-fun``") 2)
  # =>
  '(:long-buffer @{:bc 1 :bl 1 :ec 21 :el 1} "@``long-buffer-fun``")

  (get (peg/match loc-grammar `@"a buffer"`) 2)
  # =>
  '(:buffer @{:bc 1 :bl 1 :ec 12 :el 1} "@\"a buffer\"")

  (get (peg/match loc-grammar "@[8]") 2)
  # =>
  '(:bracket-array @{:bc 1 :bl 1
                     :ec 5 :el 1}
                   (:number @{:bc 3 :bl 1
                              :ec 4 :el 1} "8"))

  (get (peg/match loc-grammar "@{:a 1}") 2)
  # =>
  '(:table @{:bc 1 :bl 1
             :ec 8 :el 1}
           (:keyword @{:bc 3 :bl 1
                       :ec 5 :el 1} ":a")
           (:whitespace @{:bc 5 :bl 1
                          :ec 6 :el 1} " ")
           (:number @{:bc 6 :bl 1
                      :ec 7 :el 1} "1"))

  (get (peg/match loc-grammar "~x") 2)
  # =>
  '(:quasiquote @{:bc 1 :bl 1
                  :ec 3 :el 1}
                (:symbol @{:bc 2 :bl 1
                           :ec 3 :el 1} "x"))

  (get (peg/match loc-grammar "' '[:a :b]") 2)
  # =>
  '(:quote @{:bc 1 :bl 1
             :ec 11 :el 1}
           (:whitespace @{:bc 2 :bl 1
                          :ec 3 :el 1} " ")
           (:quote @{:bc 3 :bl 1
                     :ec 11 :el 1}
                   (:bracket-tuple @{:bc 4 :bl 1
                                     :ec 11 :el 1}
                                   (:keyword @{:bc 5 :bl 1
                                               :ec 7 :el 1} ":a")
                                   (:whitespace @{:bc 7 :bl 1
                                                  :ec 8 :el 1} " ")
                                   (:keyword @{:bc 8 :bl 1
                                               :ec 10 :el 1} ":b"))))

  )

(def loc-top-level-ast
  (put (table ;(kvs loc-grammar))
       :main ~(sequence (line) (column)
                        :input
                        (line) (column))))

(defn par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc tree el ec]
             (peg/match loc-top-level-ast src start)]
      @[:code (make-attrs bl bc el ec) tree]
      @[:code])
    (if-let [captures (peg/match loc-grammar src start)]
      (let [[bl bc] (slice captures 0 2)
            [el ec] (slice captures -3)
            trees (array/slice captures 2 -3)]
        (array/insert trees 0
                      :code (make-attrs bl bc el ec)))
      @[:code])))

# XXX: backward compatibility
(def ast par)

(comment

  (par "(+ 1 1)")
  # =>
  '@[:code @{:bc 1 :bl 1
             :ec 8 :el 1}
     (:tuple @{:bc 1 :bl 1
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
                        :ec 7 :el 1} "1"))]

  )

(defn gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (gen* elt buf))
    #
    :buffer
    (buffer/push-string buf (in an-ast 2))
    :comment
    (buffer/push-string buf (in an-ast 2))
    :constant
    (buffer/push-string buf (in an-ast 2))
    :keyword
    (buffer/push-string buf (in an-ast 2))
    :long-buffer
    (buffer/push-string buf (in an-ast 2))
    :long-string
    (buffer/push-string buf (in an-ast 2))
    :number
    (buffer/push-string buf (in an-ast 2))
    :string
    (buffer/push-string buf (in an-ast 2))
    :symbol
    (buffer/push-string buf (in an-ast 2))
    :whitespace
    (buffer/push-string buf (in an-ast 2))
    #
    :array
    (do
      (buffer/push-string buf "@(")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (gen* elt buf)))
    ))

(defn gen
  [an-ast]
  (let [buf @""]
    (gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def code gen)

(comment

  (gen
    [:code])
  # =>
  ""

  (gen
    '(:whitespace @{:bc 1 :bl 1
                    :ec 2 :el 1} " "))
  # =>
  " "

  (gen
    '(:buffer @{:bc 1 :bl 1
                :ec 12 :el 1} "@\"a buffer\""))
  # =>
  `@"a buffer"`

  (gen
    '@[:code @{:bc 1 :bl 1
               :ec 8 :el 1}
       (:tuple @{:bc 1 :bl 1
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
                          :ec 7 :el 1} "1"))])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (gen (par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (gen (par src))))

    )

  )

########################################################################

# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (butlast @[:a :b :c])
  # =>
  @[:a :b]

  (butlast [:a])
  # =>
  []

  )

(defn rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (rest [:a :b :c])
  # =>
  [:b :c]

  (rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (tuple-push nil :a)
  # =>
  [:a]

  (tuple-push @[] :a)
  # =>
  [:a]

  )

(defn to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (to-entries {})
  # =>
  @[]

  (to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (rest xs) xs]))

(comment

  (first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )

########################################################################

(defn zipper
  ``
  Returns a new zipper consisting of two elements:

  * `a-root` - the passed in root node.
  * `state` - table of info about node's z-location in the tree with keys:
    * `:ls` - left siblings
    * `:pnodes` - path of nodes from root to current z-location
    * `:pstate` - parent node's state
    * `:rs` - right siblings
    * `:changed?` - indicates whether "editing" has occured

  `state` has a prototype table with four functions:

  * :branch? - fn that tests if a node is a branch (has children)
  * :children - fn that returns the child nodes for the given branch.
  * :make-node - fn that takes a node + children and returns a new branch
    node with the same.
  * :make-state - fn for creating a new state
  ``
  [a-root branch?-fn children-fn make-node-fn]
  #
  (defn make-state_
    [&opt ls_ rs_ pnodes_ pstate_ changed?_]
    (table/setproto @{:ls ls_
                      :pnodes pnodes_
                      :pstate pstate_
                      :rs rs_
                      :changed? changed?_}
                    @{:branch? branch?-fn
                      :children children-fn
                      :make-node make-node-fn
                      :make-state make-state_}))
  #
  [a-root (make-state_)])

(comment

  # XXX

  )

# ds - data structure
(defn ds-zip
  ``
  Returns a zipper for nested data structures (tuple/array/table/struct),
  given a root data structure.
  ``
  [ds]
  (zipper ds
          |(or (dictionary? $) (indexed? $))
          to-entries
          (fn [p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (ds-zip a-node))

  the-node
  # =>
  a-node

  # merge is used to "remove" the prototype table of `st`
  (merge {} the-state)
  # =>
  @{}

  )

(defn node
  "Returns the node at `zloc`."
  [zloc]
  (get zloc 0))

(comment

  (node (ds-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn state
  "Returns the state for `zloc`."
  [zloc]
  (get zloc 1))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (-> (ds-zip [:a [:b [:x :y]]])
             state))
  # =>
  @{}

  )

(defn branch?
  ``
  Returns true if the node at `zloc` is a branch.
  Returns false otherwise.
  ``
  [zloc]
  (((state zloc) :branch?) (node zloc)))

(comment

  (branch? (ds-zip [:a :b [:x :y]]))
  # =>
  true

  )

(defn children
  ``
  Returns children for a branch node at `zloc`.
  Otherwise throws an error.
  ``
  [zloc]
  (if (branch? zloc)
    (((state zloc) :children) (node zloc))
    (error "Called `children` on a non-branch zloc")))

(comment

  (children (ds-zip [:a :b [:x :y]]))
  # =>
  [:a :b [:x :y]]

  )

(defn make-state
  ``
  Convenience function for calling the :make-state function for `zloc`.
  ``
  [zloc &opt ls rs pnodes pstate changed?]
  (((state zloc) :make-state) ls rs pnodes pstate changed?))

(comment

  # merge is used to "remove" the prototype table of `st`
  (merge {}
         (make-state (ds-zip [:a :b [:x :y]])))
  # =>
  @{}

  )

(defn down
  ``
  Moves down the tree, returning the leftmost child z-location of
  `zloc`, or nil if there are no children.
  ``
  [zloc]
  (when (branch? zloc)
    (let [[z-node st] zloc
          [k rest-kids kids]
          (first-rest-maybe-all (children zloc))]
      (when kids
        [k
         (make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (node (down (ds-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (ds-zip [:a :b [:x :y]])
      down
      branch?)
  # =>
  false

  (try
    (-> (ds-zip [:a])
        down
        children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               ds-zip
               down
               state))
    #
    '@{:ls ()
       :pnodes ((:a (:b (:x :y))))
       :pstate @{}
       :rs ((:b (:x :y)))})
  # =>
  true

  )

(defn right
  ``
  Returns the z-location of the right sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st
        [r rest-rs rs] (first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs)
      [r
       (make-state zloc
                   (tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (ds-zip [:a :b])
      down
      right
      node)
  # =>
  :b

  (-> (ds-zip [:a])
      down
      right)
  # =>
  nil

  )

(defn make-node
  ``
  Returns a branch node, given `zloc`, `a-node` and `kids`.
  ``
  [zloc a-node kids]
  (((state zloc) :make-node) a-node kids))

(comment

  (make-node (ds-zip [:a :b [:x :y]])
             [:a :b] [:x :y])
  # =>
  [:x :y]

  )

(defn up
  ``
  Moves up the tree, returning the parent z-location of `zloc`,
  or nil if at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs
         :changed? changed?} st]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(make-node zloc pnode [;ls z-node ;rs])
           (make-state zloc
                       (get pstate :ls)
                       (get pstate :rs)
                       (get pstate :pnodes)
                       (get pstate :pstate)
                       true)]
          [pnode pstate])))))

(comment

  (def m-zip
    (ds-zip [:a :b [:x :y]]))

  (deep=
    (-> m-zip
        down
        up)
    m-zip)
  # =>
  true

  (deep=
    (-> m-zip
        down
        right
        right
        down
        up
        up)
    m-zip)
  # =>
  true

  )

# XXX: used by `root` and `df-next`
(defn end?
  "Returns true if `zloc` represents the end of a depth-first walk."
  [zloc]
  (= :end (state zloc)))

(defn root
  ``
  Moves all the way up the tree for `zloc` and returns the node at
  the root z-location.
  ``
  [zloc]
  (if (end? zloc)
    (node zloc)
    (if-let [p (up zloc)]
      (root p)
      (node zloc))))

(comment

  (def a-zip
    (ds-zip [:a :b [:x :y]]))

  (node a-zip)
  # =>
  (-> a-zip
      down
      right
      right
      down
      root)

  )

(defn df-next
  ``
  Moves to the next z-location, depth-first.  When the end is
  reached, returns a special z-location detectable via `end?`.
  Does not move if already at the end.
  ``
  [zloc]
  #
  (defn recur
    [a-loc]
    (if (up a-loc)
      (or (right (up a-loc))
          (recur (up a-loc)))
      [(node a-loc) :end]))
  #
  (if (end? zloc)
    zloc
    (or (and (branch? zloc) (down zloc))
        (right zloc)
        (recur zloc))))

(comment

  (def a-zip
    (ds-zip [:a :b [:x]]))

  (node (df-next a-zip))
  # =>
  :a

  (-> a-zip
      df-next
      df-next
      node)
  # =>
  :b

  (-> a-zip
      df-next
      df-next
      df-next
      df-next
      df-next
      end?)
  # =>
  true

  )

(defn replace
  "Replaces existing node at `zloc` with `a-node`, without moving."
  [zloc a-node]
  (let [[_ st] zloc]
    [a-node
     (make-state zloc
                 (get st :ls)
                 (get st :rs)
                 (get st :pnodes)
                 (get st :pstate)
                 true)]))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      (replace :w)
      root)
  # =>
  [:w :b [:x :y]]

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      right
      down
      (replace :w)
      root)
  # =>
  [:a :b [:w :y]]

  )

(defn edit
  ``
  Replaces the node at `zloc` with the value of `(f node args)`,
  where `node` is the node associated with `zloc`.
  ``
  [zloc f & args]
  (replace zloc
           (apply f (node zloc) args)))

(comment

  (-> (ds-zip [1 2 [8 9]])
      down
      (edit inc)
      root)
  # =>
  [2 2 [8 9]]

  (-> (ds-zip [1 2 [8 9]])
      down
      (edit inc)
      right
      (edit inc)
      right
      down
      (edit dec)
      right
      (edit dec)
      root)
  # =>
  [2 3 [7 8]]

  )

(defn insert-child
  ``
  Inserts `child` as the leftmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (replace zloc
           (make-node zloc
                      (node zloc)
                      [child ;(children zloc)])))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      (insert-child :c)
      root)
  # =>
  [:c :a :b [:x :y]]

  )

(defn append-child
  ``
  Appends `child` as the rightmost child of the node at `zloc`,
  without moving.
  ``
  [zloc child]
  (replace zloc
           (make-node zloc
                      (node zloc)
                      [;(children zloc) child])))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      (append-child :c)
      root)
  # =>
  [:a :b [:x :y] :c]

  )

(defn rightmost
  ``
  Returns the z-location of the rightmost sibling of the node at
  `zloc`, or the current node's z-location if there are none to the
  right.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? rs)
             (not (empty? rs)))
      [(last rs)
       (make-state zloc
                   (tuple-push ls z-node ;(butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      rightmost
      node)
  # =>
  [:x :y]

  )

(defn remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.
  Throws an error if called at the root z-location.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls
         :pnodes pnodes
         :pstate pstate
         :rs rs} st]
    #
    (defn recur
      [a-zloc]
      (if-let [child (and (branch? a-zloc) (down a-zloc))]
        (recur (rightmost child))
        a-zloc))
    #
    (if (not (empty? st))
      (if (pos? (length ls))
        (recur [(last ls)
                (make-state zloc
                            (butlast ls)
                            rs
                            pnodes
                            pstate
                            true)])
        [(make-node zloc (last pnodes) rs)
         (make-state zloc
                     (get pstate :ls)
                     (get pstate :rs)
                     (get pstate :pnodes)
                     (get pstate :pstate)
                     true)])
      (error "Called `remove` at root"))))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      remove
      node)
  # =>
  :a

  (try
    (remove (ds-zip [:a :b [:x :y]]))
    ([e] e))
  # =>
  "Called `remove` at root"

  )

(defn left
  ``
  Returns the z-location of the left sibling of the node
  at `zloc`, or nil if there is no such sibling.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (when (and (not (empty? st))
               (indexed? ls)
               (not (empty? ls)))
      [(last ls)
       (make-state zloc
                   (butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (ds-zip [:a :b :c])
      down
      right
      right
      left
      node)
  # =>
  :b

  (-> (ds-zip [:a])
      down
      left)
  # =>
  nil

  )

(defn df-prev
  ``
  Moves to the previous z-location, depth-first.
  If already at the root, returns nil.
  ``
  [zloc]
  #
  (defn recur
    [a-zloc]
    (if-let [child (and (branch? a-zloc)
                        (down a-zloc))]
      (recur (rightmost child))
      a-zloc))
  #
  (if-let [left-loc (left zloc)]
    (recur left-loc)
    (up zloc)))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      df-prev
      node)
  # =>
  :a

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      right
      down
      df-prev
      node)
  # =>
  [:x :y]

  )

(defn insert-right
  ``
  Inserts `a-node` as the right sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (make-state zloc
                   ls
                   [a-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-right` at root"))))

(comment

  (def a-zip
    (ds-zip [:a :b [:x :y]]))

  (-> a-zip
      down
      (insert-right :z)
      root)
  # =>
  [:a :z :b [:x :y]]

  (try
    (insert-right a-zip :e)
    ([e] e))
  # =>
  "Called `insert-right` at root"

  )

(defn insert-left
  ``
  Inserts `a-node` as the left sibling of the node at `zloc`,
  without moving.
  ``
  [zloc a-node]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (not (empty? st))
      [z-node
       (make-state zloc
                   (tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (ds-zip [:a :b [:x :y]]))

  (-> a-zip
      down
      (insert-left :z)
      root)
  # =>
  [:z :a :b [:x :y]]

  (try
    (insert-left a-zip :e)
    ([e] e))
  # =>
  "Called `insert-left` at root"

  )

(defn rights
  "Returns siblings to the right of `zloc`."
  [zloc]
  (when-let [st (state zloc)]
    (get st :rs)))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      rights)
  # =>
  [:b [:x :y]]

  )

(defn lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (ds-zip [:a :b])
      down
      lefts)
  # =>
  []

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      right
      lefts)
  # =>
  [:a :b]

  )

(defn leftmost
  ``
  Returns the z-location of the leftmost sibling of the node at `zloc`,
  or the current node's z-location if there are no siblings to the left.
  ``
  [zloc]
  (let [[z-node st] zloc
        {:ls ls :rs rs} st]
    (if (and (not (empty? st))
             (indexed? ls)
             (not (empty? ls)))
      [(first ls)
       (make-state zloc
                   []
                   [;(rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      leftmost
      node)
  # =>
  :a

  (-> (ds-zip [:a :b [:x :y]])
      down
      rightmost
      leftmost
      node)
  # =>
  :a

  )

(defn path
  "Returns the path of nodes that lead to `zloc` from the root node."
  [zloc]
  (when-let [st (state zloc)]
    (get st :pnodes)))

(comment

  (path (ds-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (ds-zip [:a :b [:x :y]])
      down
      path)
  # =>
  [[:a :b [:x :y]]]

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      right
      down
      path)
  # =>
  [[:a :b [:x :y]] [:x :y]]

  )

(defn right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (right zloc)]
    (if (pred right-sib)
      right-sib
      (right-until right-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      ds-zip
      down
      right
      down
      (right-until |(match (node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      node)
  # =>
  [:symbol "+"]

  )

(defn left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (left zloc)]
    (if (pred left-sib)
      left-sib
      (left-until left-sib pred))))

(comment

  (-> [:code
       [:tuple
        [:comment "# hi there"] [:whitespace "\n"]
        [:symbol "+"] [:whitespace " "]
        [:number "1"] [:whitespace " "]
        [:number "2"]]]
      ds-zip
      down
      right
      down
      rightmost
      (left-until |(match (node $)
                     [:comment]
                     false
                     #
                     [:whitespace]
                     false
                     #
                     true))
      node)
  # =>
  [:number "1"]

  )

(defn search-from
  ``
  Successively call `pred` on z-locations starting at `zloc`
  in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (if (pred zloc)
    zloc
    (when-let [next-zloc (df-next zloc)]
      (when (end? next-zloc)
        (break nil))
      (search-from next-zloc pred))))

(comment

  (-> (ds-zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :b
                      true))
      node)
  # =>
  :b

  (-> (ds-zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :d
                      true)))
  # =>
  nil

  (-> (ds-zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :a
                      true))
      node)
  # =>
  :a

  )

(defn search-after
  ``
  Successively call `pred` on z-locations starting after
  `zloc` in depth-first order.  If a call to `pred` returns a
  truthy value, return the corresponding z-location.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when (end? zloc)
    (break nil))
  (when-let [next-zloc (df-next zloc)]
    (if (pred next-zloc)
      next-zloc
      (search-after next-zloc pred))))

(comment

  (-> (ds-zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :b
                       true))
      left
      node)
  # =>
  :a

  (-> (ds-zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :d
                       true)))
  # =>
  nil

  (-> (ds-zip [:a [:b :c [2 [3 :smile] 5]]])
      (search-after |(match (node $)
                       [_ :smile]
                       true))
      down
      node)
  # =>
  3

  )

(defn unwrap
  ``
  If the node at `zloc` is a branch node, "unwrap" its children in
  place.  If `zloc`'s node is not a branch node, do nothing.

  Throws an error if `zloc` corresponds to a top-most container.
  ``
  [zloc]
  (unless (branch? zloc)
    (break zloc))
  #
  (when (empty? (state zloc))
    (error "Called `unwrap` at root"))
  #
  (def kids (children zloc))
  (var i (dec (length kids)))
  (var curr-zloc zloc)
  (while (<= 0 i) # right to left
    (set curr-zloc
         (insert-right curr-zloc (get kids i)))
    (-- i))
  # try to end up at a sensible spot
  (set curr-zloc
       (remove curr-zloc))
  (if-let [ret-zloc (right curr-zloc)]
    ret-zloc
    curr-zloc))

(comment

  (-> (ds-zip [:a :b [:x :y]])
      down
      right
      right
      unwrap
      root)
  # =>
  [:a :b :x :y]

  (-> (ds-zip [:a :b [:x :y]])
      down
      unwrap
      root)
  # =>
  [:a :b [:x :y]]

  (-> (ds-zip [[:a]])
      down
      unwrap
      root)
  # =>
  [:a]

  (-> (ds-zip [[:a :b] [:x :y]])
      down
      down
      remove
      unwrap
      root)
  # =>
  [:b [:x :y]]

  (try
    (-> (ds-zip [:a :b [:x :y]])
        unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn wrap
  ``
  Replace nodes from `start-zloc` through `end-zloc` with a single
  node of the same type as `wrap-node` containing the nodes from
  `start-zloc` through `end-zloc`.

  If `end-zloc` is not specified, just wrap `start-zloc`.

  The caller is responsible for ensuring the value of `end-zloc`
  is somewhere to the right of `start-zloc`.  Throws an error if
  an inappropriate value is specified for `end-zloc`.
  ``
  [start-zloc wrap-node &opt end-zloc]
  (default end-zloc start-zloc)
  #
  # 1. collect all nodes to wrap
  #
  (def kids @[])
  (var cur-zloc start-zloc)
  (while (and cur-zloc
              # XXX: expensive?
              (not (deep= (node cur-zloc)
                          (node end-zloc)))) # left to right
    (array/push kids (node cur-zloc))
    (set cur-zloc (right cur-zloc)))
  (when (nil? cur-zloc)
    (error "Called `wrap` with invalid value for `end-zloc`."))
  # also collect the last node
  (array/push kids (node end-zloc))
  #
  # 2. replace locations that will be removed with non-container nodes
  #
  (def dummy-node
    (make-node start-zloc wrap-node (tuple)))
  (set cur-zloc start-zloc)
  # trying to do this together in step 1 is not straight-forward
  # because the desired exiting condition for the while loop depends
  # on cur-zloc becoming end-zloc -- if `replace` were to be used
  # there, the termination condition never gets fulfilled properly.
  (for i 0 (dec (length kids)) # left to right again
    (set cur-zloc
         (-> (replace cur-zloc dummy-node)
             right)))
  (set cur-zloc
       (replace cur-zloc dummy-node))
  #
  # 3. remove all relevant locations
  #
  (def new-node
    (make-node start-zloc wrap-node (tuple ;kids)))
  (for i 0 (dec (length kids)) # right to left
    (set cur-zloc
         (remove cur-zloc)))
  # 4. put the new container node into place
  (replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (ds-zip [:a [:b] :c :x])
        down
        right))

  (node start-zloc)
  # =>
  [:b]

  (-> (wrap start-zloc [])
      root)
  # =>
  [:a [[:b]] :c :x]

  (def end-zloc
    (right start-zloc))

  (node end-zloc)
  # =>
  :c

  (-> (wrap start-zloc [] end-zloc)
      root)
  # =>
  [:a [[:b] :c] :x]

  (try
    (-> (wrap end-zloc [] start-zloc)
        root)
    ([e] e))
  # =>
  "Called `wrap` with invalid value for `end-zloc`."

  )

########################################################################

(defn has-children?
  ``
  Returns true if `a-node` can have children.
  Returns false if `a-node` cannot have children.
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
  [a-tree]
  (defn branch?_
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (has-children? a-node))))
  #
  (defn children_
    [a-node]
    (if (branch?_ a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node_
    [a-node kids]
    [(first a-node) (get a-node 1) ;kids])
  #
  (zipper a-tree branch?_ children_ make-node_))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (zip root-node))

  the-node
  # =>
  root-node

  # merge is used to "remove" the prototype table of `st`
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
  (get (node zloc) 1))

(comment

  (-> (par "(+ 1 3)")
      zip
      down
      attrs)
  # =>
  @{:bc 1 :bl 1 :ec 8 :el 1}

  )

(defn zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [a-tree]
  (-> (zip a-tree)
      down))

(comment

  (-> (par "(+ 1 3)")
      zip-down
      node)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "+"]
   [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
   [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
   [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
   [:number @{:bc 6 :bl 1 :ec 7 :el 1} "3"]]

  (-> (par "(/ 1 8)")
      zip-down
      root)
  # =>
  @[:code @{:bc 1 :bl 1 :ec 8 :el 1}
    [:tuple @{:bc 1 :bl 1 :ec 8 :el 1}
            [:symbol @{:bc 2 :bl 1 :ec 3 :el 1} "/"]
            [:whitespace @{:bc 3 :bl 1 :ec 4 :el 1} " "]
            [:number @{:bc 4 :bl 1 :ec 5 :el 1} "1"]
            [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
            [:number @{:bc 6 :bl 1 :ec 7 :el 1} "8"]]]

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
               |(match (node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  (-> (par (string "(# hi there\n"
                   "+ 1 2)"))
      zip-down
      down
      right-skip-wsc
      node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (par "(:a)")
      zip-down
      down
      right-skip-wsc)
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
              |(match (node $)
                 [:whitespace]
                 false
                 #
                 [:comment]
                 false
                 #
                 true)))

(comment

  (-> (par (string "(# hi there\n"
                   "+ 1 2)"))
      zip-down
      down
      right-skip-wsc
      right-skip-wsc
      left-skip-wsc
      node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (par "(:a)")
      zip-down
      down
      left-skip-wsc)
  # =>
  nil

  )

# ws == whitespace
(defn right-skip-ws
  ``
  Try to move right from `zloc`, skipping over whitespace
  nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (right-until zloc
               |(match (node $)
                  [:whitespace]
                  false
                  #
                  true)))

(comment

  (-> (par (string "( # hi there\n"
                   "+ 1 2)"))
      zip-down
      down
      right-skip-ws
      node)
  # =>
  [:comment @{:bc 3 :bl 1 :ec 13 :el 1} "# hi there"]

  (-> (par "(:a)")
      zip-down
      down
      right-skip-ws)
  # =>
  nil

  )

(defn left-skip-ws
  ``
  Try to move left from `zloc`, skipping over whitespace
  nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (left-until zloc
              |(match (node $)
                 [:whitespace]
                 false
                 #
                 true)))

(comment

  (-> (par (string "(# hi there\n"
                   "+ 1 2)"))
      zip-down
      down
      right
      right
      left-skip-ws
      node)
  # =>
  [:comment @{:bc 2 :bl 1 :ec 12 :el 1} "# hi there"]

  (-> (par "(:a)")
      zip-down
      down
      left-skip-ws)
  # =>
  nil

  )

