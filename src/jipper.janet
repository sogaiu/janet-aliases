(comment import ./helpers :prefix "")
# based on code by corasaurus-hex

# `slice` doesn't necessarily preserve the input type

# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn h/butlast
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 0 -2)
      (array/slice indexed 0 -2))))

(comment

  (h/butlast @[:a :b :c])
  # =>
  @[:a :b]

  (h/butlast [:a])
  # =>
  []

  )

(defn h/rest
  [indexed]
  (if (empty? indexed)
    nil
    (if (tuple? indexed)
      (tuple/slice indexed 1 -1)
      (array/slice indexed 1 -1))))

(comment

  (h/rest [:a :b :c])
  # =>
  [:b :c]

  (h/rest @[:a])
  # =>
  @[]

  )

# XXX: can pass in array - will get back tuple
(defn h/tuple-push
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (h/tuple-push [:a :b] :c)
  # =>
  [:a :b :c]

  (h/tuple-push nil :a)
  # =>
  [:a]

  (h/tuple-push @[] :a)
  # =>
  [:a]

  )

(defn h/to-entries
  [val]
  (if (dictionary? val)
    (pairs val)
    val))

(comment

  (sort (h/to-entries {:a 1 :b 2}))
  # =>
  @[[:a 1] [:b 2]]

  (h/to-entries {})
  # =>
  @[]

  (h/to-entries @{:a 1})
  # =>
  @[[:a 1]]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (h/to-entries [:a :b :c])
  # =>
  [:a :b :c]

  )

# XXX: when xs is empty, "all" becomes nil
(defn h/first-rest-maybe-all
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (h/rest xs) xs]))

(comment

  (h/first-rest-maybe-all [:a :b])
  # =>
  [:a [:b] [:a :b]]

  (h/first-rest-maybe-all @[:a])
  # =>
  [:a @[] @[:a]]

  (h/first-rest-maybe-all [])
  # =>
  [nil nil nil]

  # XXX: is this what we want?
  (h/first-rest-maybe-all nil)
  # =>
  [nil nil nil]

  )


(comment import ./location :prefix "")
# bl - begin line
# bc - begin column
# bp - begin position
# el - end line
# ec - end column
# ep - end position
(defn l/make-attrs
  [& items]
  (zipcoll [:bl :bc :bp :el :ec :ep]
           items))

(defn l/atom-node
  [node-type peg-form]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,peg-form
                           (line) (column) (position)))
        ,|[node-type (l/make-attrs ;(slice $& 0 -2)) (last $&)]))

(defn l/reader-macro-node
  [node-type sigil]
  ~(cmt (capture (sequence (line) (column) (position)
                           ,sigil
                           (any :non-form)
                           :form
                           (line) (column) (position)))
        ,|[node-type (l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
           ;(slice $& 3 -5)]))

(defn l/collection-node
  [node-type open-delim close-delim]
  ~(cmt
     (capture
       (sequence
         (line) (column) (position)
         ,open-delim
         (any :input)
         (choice ,close-delim
                 (error
                   (replace (sequence (line) (column) (position))
                            ,|(string/format
                                (string "line: %p column: %p position: %p "
                                        "missing %p for %p")
                                $0 $1 $2 close-delim node-type))))
         (line) (column) (position)))
     ,|[node-type (l/make-attrs ;(slice $& 0 3) ;(slice $& -5 -2))
        ;(slice $& 3 -5)]))

(def l/loc-grammar
  ~@{:main (sequence (line) (column) (position)
                     (some :input)
                     (line) (column) (position))
     #
     :input (choice :non-form
                    :form)
     #
     :non-form (choice :whitespace
                       :comment)
     #
     :whitespace ,(l/atom-node :whitespace
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
     :comment ,(l/atom-node :comment
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
     :fn ,(l/reader-macro-node :fn "|")
     # :fn (cmt (capture (sequence (line) (column)
     #                             "|"
     #                             (any :non-form)
     #                             :form
     #                             (line) (column)))
     #          ,|[:fn (make-attrs ;(slice $& 0 2) ;(slice $& -4 -2))
     #             ;(slice $& 2 -4)])
     #
     :quasiquote ,(l/reader-macro-node :quasiquote "~")
     #
     :quote ,(l/reader-macro-node :quote "'")
     #
     :splice ,(l/reader-macro-node :splice ";")
     #
     :unquote ,(l/reader-macro-node :unquote ",")
     #
     :array ,(l/collection-node :array "@(" ")")
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
     :tuple ,(l/collection-node :tuple "(" ")")
     #
     :bracket-array ,(l/collection-node :bracket-array "@[" "]")
     #
     :bracket-tuple ,(l/collection-node :bracket-tuple "[" "]")
     #
     :table ,(l/collection-node :table "@{" "}")
     #
     :struct ,(l/collection-node :struct "{" "}")
     #
     :number ,(l/atom-node :number
                         ~(drop (sequence (cmt (capture (some :num-char))
                                               ,scan-number)
                                          (opt (sequence ":" (range "AZ" "az"))))))
     #
     :num-char (choice (range "09" "AZ" "az")
                       (set "&+-._"))
     #
     :constant ,(l/atom-node :constant
                           '(sequence (choice "false" "nil" "true")
                                      (not :name-char)))
     #
     :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                        (set "!$%&*+-./:<?=>@^_"))
     #
     :buffer ,(l/atom-node :buffer
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
     :string ,(l/atom-node :string
                         '(sequence `"`
                                    (any (choice :escape
                                                 (if-not "\"" 1)))
                                    `"`))
     #
     :long-string ,(l/atom-node :long-string
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
     :long-buffer ,(l/atom-node :long-buffer
                              '(sequence "@" :long-bytes))
     #
     :keyword ,(l/atom-node :keyword
                          '(sequence ":"
                                     (any :name-char)))
     #
     :symbol ,(l/atom-node :symbol
                         '(some :name-char))
     })

(comment

  (get (peg/match l/loc-grammar " ") 3)
  # =>
  [:whitespace @{:bl 1 :el 1 :bc 1 :bp 0 :ec 2 :ep 1} " "]

  (get (peg/match l/loc-grammar "true?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 6 :ep 5} "true?"]

  (get (peg/match l/loc-grammar "nil?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 5 :ep 4} "nil?"]

  (get (peg/match l/loc-grammar "false?") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :bp 0 :ec 7 :ep 6} "false?"]

  (get (peg/match l/loc-grammar "# hi there") 3)
  # =>
  [:comment @{:bl 1 :el 1 :bc 1 :bp 0 :ec 11 :ep 10} "# hi there"]

  (get (peg/match l/loc-grammar "1_000_000") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 10 :ep 9} "1_000_000"]

  (get (peg/match l/loc-grammar "8.3") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :bp 0 :ec 4 :ep 3} "8.3"]

  (get (peg/match l/loc-grammar "1e2") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "1e2"]

  (get (peg/match l/loc-grammar "0xfe") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "0xfe"]

  (get (peg/match l/loc-grammar "2r01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "2r01"]

  (get (peg/match l/loc-grammar "3r101&01") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 8 :bp 0 :ec 9} "3r101&01"]

  (get (peg/match l/loc-grammar "2:u") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 3 :bp 0 :ec 4} "2:u"]

  (get (peg/match l/loc-grammar "-8:s") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5} "-8:s"]

  (get (peg/match l/loc-grammar "1e2:n") 3)
  # =>
  [:number @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "1e2:n"]

  (get (peg/match l/loc-grammar "printf") 3)
  # =>
  [:symbol @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} "printf"]

  (get (peg/match l/loc-grammar ":smile") 3)
  # =>
  [:keyword @{:bl 1 :el 1 :bc 1 :ep 6 :bp 0 :ec 7} ":smile"]

  (get (peg/match l/loc-grammar `"fun"`) 3)
  # =>
  [:string @{:bl 1 :el 1 :bc 1 :ep 5 :bp 0 :ec 6} "\"fun\""]

  (get (peg/match l/loc-grammar "``long-fun``") 3)
  # =>
  [:long-string @{:bl 1 :el 1 :bc 1 :ep 12 :bp 0 :ec 13} "``long-fun``"]

  (get (peg/match l/loc-grammar "@``long-buffer-fun``") 3)
  # =>
  [:long-buffer
   @{:bl 1 :el 1 :bc 1 :bp 0 :ec 21 :ep 20}
   "@``long-buffer-fun``"]

  (get (peg/match l/loc-grammar `@"a buffer"`) 3)
  # =>
  [:buffer @{:bl 1 :el 1 :bc 1 :ep 11 :bp 0 :ec 12} "@\"a buffer\""]

  (get (peg/match l/loc-grammar "@[8]") 3)
  # =>
  [:bracket-array @{:bl 1 :el 1 :bc 1 :ep 4 :bp 0 :ec 5}
   [:number @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} "8"]]

  (get (peg/match l/loc-grammar "@{:a 1}") 3)
  # =>
  [:table @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:keyword @{:bl 1 :el 1 :bc 3 :ep 4 :bp 2 :ec 5} ":a"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]

  (get (peg/match l/loc-grammar "~x") 3)
  # =>
  [:quasiquote @{:bl 1 :el 1 :bc 1 :ep 2 :bp 0 :ec 3}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "x"]]

  (get (peg/match l/loc-grammar "' '[:a :b]") 3)
  # =>
  [:quote @{:bl 1 :el 1 :bc 1 :ep 10 :bp 0 :ec 11}
   [:whitespace @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} " "]
   [:quote @{:bl 1 :el 1 :bc 3 :ep 10 :bp 2 :ec 11}
    [:bracket-tuple @{:bl 1 :el 1 :bc 4 :ep 10 :bp 3 :ec 11}
     [:keyword @{:bl 1 :el 1 :bc 5 :ep 6 :bp 4 :ec 7} ":a"]
     [:whitespace @{:bl 1 :el 1 :bc 7 :ep 7 :bp 6 :ec 8} " "]
     [:keyword @{:bl 1 :el 1 :bc 8 :ep 9 :bp 7 :ec 10} ":b"]]]]

  )

(def l/loc-top-level-ast
  (put (table ;(kvs l/loc-grammar))
       :main ~(sequence (line) (column) (position)
                        :input
                        (line) (column) (position))))

(defn l/par
  [src &opt start single]
  (default start 0)
  (if single
    (if-let [[bl bc bp tree el ec ep]
             (peg/match l/loc-top-level-ast src start)]
      @[:code (l/make-attrs bl bc bp el ec ep) tree]
      @[:code])
    (if-let [captures (peg/match l/loc-grammar src start)]
      (let [[bl bc bp] (slice captures 0 3)
            [el ec ep] (slice captures -4)
            trees (array/slice captures 3 -4)]
        (array/insert trees 0
                      :code (l/make-attrs bl bc bp el ec ep)))
      @[:code])))

# XXX: backward compatibility
(def l/ast l/par)

(comment

  (l/par "(+ 1 1)")
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "1"]]]

  )

(defn l/gen*
  [an-ast buf]
  (case (first an-ast)
    :code
    (each elt (drop 2 an-ast)
      (l/gen* elt buf))
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
        (l/gen* elt buf))
      (buffer/push-string buf ")"))
    :bracket-array
    (do
      (buffer/push-string buf "@[")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf))
      (buffer/push-string buf "]"))
    :bracket-tuple
    (do
      (buffer/push-string buf "[")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf))
      (buffer/push-string buf "]"))
    :tuple
    (do
      (buffer/push-string buf "(")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf))
      (buffer/push-string buf ")"))
    :struct
    (do
      (buffer/push-string buf "{")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf))
      (buffer/push-string buf "}"))
    :table
    (do
      (buffer/push-string buf "@{")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf))
      (buffer/push-string buf "}"))
    #
    :fn
    (do
      (buffer/push-string buf "|")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf)))
    :quasiquote
    (do
      (buffer/push-string buf "~")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf)))
    :quote
    (do
      (buffer/push-string buf "'")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf)))
    :splice
    (do
      (buffer/push-string buf ";")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf)))
    :unquote
    (do
      (buffer/push-string buf ",")
      (each elt (drop 2 an-ast)
        (l/gen* elt buf)))
    ))

(defn l/gen
  [an-ast]
  (let [buf @""]
    (l/gen* an-ast buf)
    # XXX: leave as buffer?
    (string buf)))

# XXX: backward compatibility
(def l/code l/gen)

(comment

  (l/gen [:code])
  # =>
  ""

  (l/gen [:whitespace @{:bc 1 :bl 1 :bp 0
                      :ec 2 :el 1 :ep 1} " "])
  # =>
  " "

  (l/gen [:buffer @{:bc 1 :bl 1 :bp 0
                  :ec 12 :el 1 :ep 11} "@\"a buffer\""])
  # =>
  `@"a buffer"`

  (l/gen @[:code @{:bc 1 :bl 1 :bp 0
                 :ec 8 :el 1 :ep 7}
         [:tuple @{:bc 1 :bl 1 :bp 0
                   :ec 8 :el 1 :ep 7}
                 [:symbol @{:bc 2 :bl 1 :bp 1
                            :ec 3 :el 1 :ep 2} "+"]
                 [:whitespace @{:bc 3 :bl 1 :bp 2
                                :ec 4 :el 1 :ep 3} " "]
                 [:number @{:bc 4 :bl 1 :bp 3
                            :ec 5 :el 1 :ep 4} "1"]
                 [:whitespace @{:bc 5 :bl 1 :bp 4
                                :ec 6 :el 1 :ep 5} " "]
                 [:number @{:bc 6 :bl 1 :bp 5
                            :ec 7 :el 1 :ep 6} "1"]]])
  # =>
  "(+ 1 1)"

  )

(comment

  (def src "{:x  :y \n :z  [:a  :b    :c]}")

  (l/gen (l/par src))
  # =>
  src

  )

(comment

  (comment

    (let [src (slurp (string (os/getenv "HOME")
                             "/src/janet/src/boot/boot.janet"))]
      (= (string src)
         (l/gen (l/par src))))

    )

  )


(def version "2026-03-19_04-53-23")

# exports
(def par l/par)
(def gen l/gen)

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

(defn indexed-zip
  ``
  Returns a zipper for nested indexed data structures (tuples
  or arrays), given a root data structure.
  ``
  [indexed]
  (zipper indexed
          indexed?
          h/to-entries
          (fn [_p xs] xs)))

(comment

  (def a-node
    [:x [:y :z]])

  (def [the-node the-state]
    (indexed-zip a-node))

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

  (node (indexed-zip [:a :b [:x :y]]))
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
         (-> (indexed-zip [:a [:b [:x :y]]])
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

  (branch? (indexed-zip [:a :b [:x :y]]))
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

  (children (indexed-zip [:a :b [:x :y]]))
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
         (make-state (indexed-zip [:a :b [:x :y]])))
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
          (h/first-rest-maybe-all (children zloc))]
      (when kids
        [k
         (make-state zloc
                     []
                     rest-kids
                     (if (not (empty? st))
                       (h/tuple-push (get st :pnodes) z-node)
                       [z-node])
                     st
                     (get st :changed?))]))))

(comment

  (node (down (indexed-zip [:a :b [:x :y]])))
  # =>
  :a

  (-> (indexed-zip [:a :b [:x :y]])
      down
      branch?)
  # =>
  false

  (try
    (-> (indexed-zip [:a])
        down
        children)
    ([e] e))
  # =>
  "Called `children` on a non-branch zloc"

  (deep=
    #
    (merge {}
           (-> [:a [:b [:x :y]]]
               indexed-zip
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
        [r rest-rs rs_] (h/first-rest-maybe-all rs)]
    (when (and (not (empty? st)) rs_)
      [r
       (make-state zloc
                   (h/tuple-push ls z-node)
                   rest-rs
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (indexed-zip [:a :b])
      down
      right
      node)
  # =>
  :b

  (-> (indexed-zip [:a])
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

  (make-node (indexed-zip [:a :b [:x :y]])
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
    (indexed-zip [:a :b [:x :y]]))

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
    (indexed-zip [:a :b [:x :y]]))

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
    (indexed-zip [:a :b [:x]]))

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

  (-> (indexed-zip [:a :b [:x :y]])
      down
      (replace :w)
      root)
  # =>
  [:w :b [:x :y]]

  (-> (indexed-zip [:a :b [:x :y]])
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

  (-> (indexed-zip [1 2 [8 9]])
      down
      (edit inc)
      root)
  # =>
  [2 2 [8 9]]

  (-> (indexed-zip [1 2 [8 9]])
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

  (-> (indexed-zip [:a :b [:x :y]])
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

  (-> (indexed-zip [:a :b [:x :y]])
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
                   (h/tuple-push ls z-node ;(h/butlast rs))
                   []
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (indexed-zip [:a :b [:x :y]])
      down
      rightmost
      node)
  # =>
  [:x :y]

  )

(defn remove
  ``
  Removes the node at `zloc`, returning the z-location that would have
  preceded it in a depth-first walk.  Throws an error if called at the
  root z-location.
  ``
  [zloc]
  (let [[_z-node st] zloc
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
                            (h/butlast ls)
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

  (-> (indexed-zip [:a :b [:x :y]])
      down
      right
      remove
      node)
  # =>
  :a

  (try
    (remove (indexed-zip [:a :b [:x :y]]))
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
                   (h/butlast ls)
                   [z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))])))

(comment

  (-> (indexed-zip [:a :b :c])
      down
      right
      right
      left
      node)
  # =>
  :b

  (-> (indexed-zip [:a])
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

  (-> (indexed-zip [:a :b [:x :y]])
      down
      right
      df-prev
      node)
  # =>
  :a

  (-> (indexed-zip [:a :b [:x :y]])
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
    (indexed-zip [:a :b [:x :y]]))

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
                   (h/tuple-push ls a-node)
                   rs
                   (get st :pnodes)
                   (get st :pstate)
                   true)]
      (error "Called `insert-left` at root"))))

(comment

  (def a-zip
    (indexed-zip [:a :b [:x :y]]))

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

  (-> (indexed-zip [:a :b [:x :y]])
      down
      rights)
  # =>
  [:b [:x :y]]

  (-> (indexed-zip [:a :b])
      down
      right
      rights)
  # =>
  []

  )

(defn lefts
  "Returns siblings to the left of `zloc`."
  [zloc]
  (if-let [st (state zloc)
           ls (get st :ls)]
    ls
    []))

(comment

  (-> (indexed-zip [:a :b])
      down
      lefts)
  # =>
  []

  (-> (indexed-zip [:a :b [:x :y]])
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
                   [;(h/rest ls) z-node ;rs]
                   (get st :pnodes)
                   (get st :pstate)
                   (get st :changed?))]
      zloc)))

(comment

  (-> (indexed-zip [:a :b [:x :y]])
      down
      leftmost
      node)
  # =>
  :a

  (-> (indexed-zip [:a :b [:x :y]])
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

  (path (indexed-zip [:a :b [:x :y]]))
  # =>
  nil

  (-> (indexed-zip [:a :b [:x :y]])
      down
      path)
  # =>
  [[:a :b [:x :y]]]

  (-> (indexed-zip [:a :b [:x :y]])
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
      indexed-zip
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
      indexed-zip
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

  (-> (indexed-zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :b
                      true))
      node)
  # =>
  :b

  (-> (indexed-zip [:a :b :c])
      down
      (search-from |(match (node $)
                      :d
                      true)))
  # =>
  nil

  (-> (indexed-zip [:a :b :c])
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

  (-> (indexed-zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :b
                       true))
      left
      node)
  # =>
  :a

  (-> (indexed-zip [:b :a :b])
      down
      (search-after |(match (node $)
                       :d
                       true)))
  # =>
  nil

  (-> (indexed-zip [:a [:b :c [2 [3 :smile] 5]]])
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

  (-> (indexed-zip [:a :b [:x :y]])
      down
      right
      right
      unwrap
      root)
  # =>
  [:a :b :x :y]

  (-> (indexed-zip [:a :b [:x :y]])
      down
      unwrap
      root)
  # =>
  [:a :b [:x :y]]

  (-> (indexed-zip [[:a]])
      down
      unwrap
      root)
  # =>
  [:a]

  (-> (indexed-zip [[:a :b] [:x :y]])
      down
      down
      remove
      unwrap
      root)
  # =>
  [:b [:x :y]]

  (try
    (-> (indexed-zip [:a :b [:x :y]])
        unwrap)
    ([e] e))
  # =>
  "Called `unwrap` at root"

  )

(defn eq?
  ``
  Compare two zlocs, `a-zloc` and `b-zloc`, for equality.
  ``
  [a-zloc b-zloc]
  (and (= (length (lefts a-zloc)) (length (lefts b-zloc)))
       (= (path a-zloc) (path b-zloc))))

(comment

  (def iz (indexed-zip [:a :b :c :b]))

  (eq? (-> iz down right)
       (-> iz down right right right))
  # =>
  false

  (eq? (-> iz down right)
       (-> iz down right right right left left))
  # =>
  true

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
              (not (eq? cur-zloc end-zloc))) # left to right
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
  (repeat (dec (length kids)) # left to right again
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
  (repeat (dec (length kids)) # right to left
    (set cur-zloc
         (remove cur-zloc)))
  # 4. put the new container node into place
  (replace cur-zloc new-node))

(comment

  (def start-zloc
    (-> (indexed-zip [:a [:b] :c :x])
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
  and column numbers along with 0-based positions.
  ``
  [zloc]
  (get (node zloc) 1))

(comment

  (-> (par "(+ 1 3)")
      zip
      down
      attrs)
  # =>
  @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}

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
  [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
   [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "+"]
   [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
   [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
   [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
   [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "3"]]

  (-> (par "(/ 1 8)")
      zip-down
      root)
  # =>
  @[:code @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
    [:tuple @{:bl 1 :el 1 :bc 1 :ep 7 :bp 0 :ec 8}
     [:symbol @{:bl 1 :el 1 :bc 2 :ep 2 :bp 1 :ec 3} "/"]
     [:whitespace @{:bl 1 :el 1 :bc 3 :ep 3 :bp 2 :ec 4} " "]
     [:number @{:bl 1 :el 1 :bc 4 :ep 4 :bp 3 :ec 5} "1"]
     [:whitespace @{:bl 1 :el 1 :bc 5 :ep 5 :bp 4 :ec 6} " "]
     [:number @{:bl 1 :el 1 :bc 6 :ep 6 :bp 5 :ec 7} "8"]]]

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
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

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
  [:symbol @{:bl 2 :el 2 :bc 1 :ep 13 :bp 12 :ec 2} "+"]

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
  [:comment @{:bl 1 :el 1 :bc 3 :ep 12 :bp 2 :ec 13} "# hi there"]

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
  [:comment @{:bl 1 :el 1 :bc 2 :ep 11 :bp 1 :ec 12} "# hi there"]

  (-> (par "(:a)")
      zip-down
      down
      left-skip-ws)
  # =>
  nil

  )

