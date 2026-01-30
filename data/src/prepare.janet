(import ./common :as c)
(import ./jipper :as j)
(import ./utils :as u)

(defn valid-sym-name?
  [sym-name]
  (peg/match
    ~(sequence (some (choice (range "09" "AZ" "az" "\x80\xFF")
                             (set "!$%&*+-./:<?=>@^_")))
               -1)
    sym-name))

(comment

  (valid-sym-name? "hello")
  # =>
  @[]

  (valid-sym-name? "(hi)")
  # =>
  nil

  )

(def non-call-things
  {"def" 1 "def-" 1
   "var" 1 "var-" 1})

(def call-things
  {"defn" 1 "defn-" 1
   "defmacro" 1 "defmacro-" 1
   "varfn" 1})

# XXX: defglobal, varglobal, and defdyn are not handled
(def def-things
  (merge non-call-things call-things))

(def destruct-types
  (invert [:tuple :bracket-tuple
           :array :bracket-array
           :struct :table]))

# XXX: does not handle arbitrarily nested destructuring
(defn analyze-defish
  [acc a-zloc]
  (when-let [b-zloc (j/right-skip-wsc a-zloc)]
    (def [_ _ def-type] (j/node a-zloc))
    (def [_ loc _] (j/node (j/up a-zloc)))
    (match (j/node b-zloc)
      [:symbol name-loc name]
      (array/push acc {:name name
                       :type def-type
                       :loc loc
                       :name-loc name-loc})
      #
      [node-type _ & rest]
      (when (get destruct-types node-type)
        (array/concat acc
                      (keep (fn [[node-type node-loc node-value]]
                              (when (= :symbol node-type)
                                {:name node-value
                                 :type def-type
                                 :loc loc
                                 :name-loc node-loc}))
                            rest)))))
  #
  acc)

(defn find-top-level-syms
  [zloc]
  (var cur-zloc zloc)
  (def sym-zlocs @[])
  #
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol
              (match (j/node child-zloc) [:symbol _ name]
                (when (get def-things name)
                  (array/push sym-zlocs child-zloc))))))
    (set cur-zloc (j/right cur-zloc)))
  #
  sym-zlocs)

(defn tweak-import-forms
  [zloc]
  (var cur-zloc zloc)
  (set cur-zloc (j/zip-down (j/root cur-zloc)))
  (while (not (j/end? cur-zloc))
    (when-let [i-zloc (match (j/node cur-zloc)
                        # XXX: assumes no whitespace or comments
                        #      before `import` symbol
                        [:tuple _ [:symbol _ "import"]]
                        cur-zloc)
               i-node (j/node i-zloc)]
      (def i-tbl (c/analyze-import i-node))
      (def i-path (get i-tbl :path))
      (assertf i-path "import form lacks a path: %n" (j/gen i-node))
      (set cur-zloc (j/replace cur-zloc
                               [:tuple @{}
                                [:symbol @{} "import"]
                                [:whitespace @{} " "]
                                [:symbol @{} i-path]
                                [:whitespace @{} " "]
                                [:keyword @{} ":prefix"]
                                [:whitespace @{} " "]
                                [:string @{} `""`]])))
    (set cur-zloc (j/df-next cur-zloc)))
  #
  cur-zloc)

(defn sym-at-or-before?
  [a-loc b-loc]
  (def {:bl a-bl :bc a-bc :ec a-ec :el a-el} a-loc)
  (def {:bl b-bl :bc b-bc :ec b-ec :el b-el} b-loc)
  (assertf (and (= a-bl a-el) (= b-bl b-el))
           "symbols must start and end on the same line: %n %n"
           a-loc b-loc)
  (cond
    (not= a-bl b-bl)
    (< a-bl b-bl)
    # XXX: check that this really should not be <
    (def le (<= a-ec b-bc))
    le
    #
    (and (= a-bc b-bc) (= a-ec b-ec))
    true
    #
    (errorf "locations should not overlap: %n %n" a-loc b-loc)))

(comment

  (sym-at-or-before? {:bl 1 :bc 1 :ec 1 :el 1}
                     {:bl 2 :bc 2 :ec 2 :el 2})
  # =>
  true

  (sym-at-or-before? {:bl 2 :bc 2 :ec 2 :el 2}
                     {:bl 1 :bc 1 :ec 1 :el 1})
  # =>
  false

  (sym-at-or-before? {:bl 1 :bc 1 :ec 8 :el 1}
                     {:bl 1 :bc 1 :ec 8 :el 1})
  # =>
  true

  (sym-at-or-before? {:bl 1 :bc 1 :ec 8 :el 1}
                     {:bl 1 :bc 8 :ec 10 :el 1})
  # =>
  true

  (def [ok? result]
    (protect (sym-at-or-before? {:bl 1 :bc 1 :el 1 :ec 3}
                                {:bl 1 :bc 2 :el 1 :ec 7})))

  (and (not ok?)
       (string/has-prefix? "locations should not" result))
  # =>
  true

  )

(defn rename
  [prefix in-path out-path]
  (u/maybe-dump :call "rename" :in-path in-path :out-path out-path)
  (def prefix-str (string prefix "/"))
  #
  (def src (slurp in-path))
  (def tree (j/par src))
  (var cur-zloc nil)
  #
  (set cur-zloc
       (try (j/zip-down tree)
         ([e] (eprintf e)
              (eprintf "failed to create zipper from file: %s" in-path)
              (os/exit 1))))
  # find top-level symbols
  (def sym-zlocs (find-top-level-syms cur-zloc))
  (def def-descs (reduce analyze-defish @[] sym-zlocs))
  (def sym-tbl (tabseq [{:name name :name-loc name-loc} :in def-descs]
                 name name-loc))
  # if it worked before, it should work again without error
  (set cur-zloc (j/zip-down tree))
  # may be rename using found top-level symbols
  (while (not (j/end? cur-zloc))
    (when-let [found
               (match (j/node cur-zloc) [:symbol curr-loc name]
                 (when-let [def-name-loc (get sym-tbl name)
                            _ (sym-at-or-before? def-name-loc curr-loc)]
                   name))]
      (def new-name (string prefix-str found))
      (set cur-zloc (j/replace cur-zloc [:symbol @{} new-name])))
    (set cur-zloc (j/df-next cur-zloc)))
  #
  cur-zloc)

(defn prepare-imported
  [in-dir obj-path prefixes opts]
  (u/maybe-dump :call "prepare-imported" :in-dir in-dir
                :obj-path obj-path :prefixes prefixes :opts opts)
  (def {:sep sep} (u/get-os-bits))
  (eachp [path prefix] prefixes
    (def [dir fname] (u/split-path path))
    (def ipath path)
    (def [dir rest] (u/diff-path in-dir ipath))
    (assertf (= in-dir dir)
             "expected in-dir = dir, but: %s %s" in-dir dir)
    (def [subdir fname] (u/split-path rest))
    # subdir includes sep at front
    (def obj-dir (string obj-path subdir))
    (os/mkdir obj-dir)
    (def opath (string obj-dir sep fname))
    # rename some names
    (def zloc (rename prefix ipath opath))
    # tweak import forms
    (def t-zloc (tweak-import-forms zloc))
    # save
    (spit opath (j/gen (j/root t-zloc)))))

(defn prepare-start
  [start-path in-name obj-path opts]
  (u/maybe-dump :call "prepare-start" :start-path start-path
                :in-name in-name :obj-path obj-path :opts opts)
  (def {:sep sep} (u/get-os-bits))
  (def {:start-file-perm perm} opts)
  (def in-src (slurp start-path))
  (def in-tree (j/par in-src))
  (var cur-zloc nil)
  (set cur-zloc
       (try (j/zip-down in-tree)
         ([e] (eprintf e)
              (eprintf "zipper creation failed for file: %s" start-path)
              (os/exit 1))))
  (def in-out-path (string obj-path sep in-name))
  # only tweak import forms
  (spit in-out-path
        (j/gen (j/root (tweak-import-forms cur-zloc))))
  (when perm
    (os/chmod in-out-path perm))
  #
  in-out-path)

