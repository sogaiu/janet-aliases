(import ./jipper :as j)
(import ./utils :as u)

(defn is-import?
  [zloc]
  (def node (j/node zloc))
  (when (not= :tuple (get node 0))
    (break false))
  #
  (def head-zloc (j/down zloc))
  (when (not head-zloc)
    (break false))
  #
  (def [head-type _ _] (j/node head-zloc))
  (def first-non-wsc-zloc
    (cond
      (= :symbol head-type)
      head-zloc
      #
      (or (= :whitespace head-type)
          (= :comment head-type))
      (j/right-skip-wsc head-zloc)
      #
      nil))
  (when (not first-non-wsc-zloc)
    (break false))
  #
  (def [fnw-type _ fnw-value] (j/node first-non-wsc-zloc))
  (when (and (= :symbol fnw-type)
             (= "import" fnw-value))
    (u/maybe-dump :is-import? (j/gen (j/node zloc)))
    true))

(comment

  (def zloc (j/zip-down (j/par `(import ./args :prefix "")`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
   [:string @{:bc 24 :bl 1 :ec 26 :el 1} `""`]]

  (is-import? zloc)
  # =>
  true

  (def zloc (j/zip-down (j/par `( import ./args :as a)`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
   [:whitespace @{:bc 2 :bl 1 :ec 3 :el 1} " "]
   [:symbol @{:bc 3 :bl 1 :ec 9 :el 1} "import"]
   [:whitespace @{:bc 9 :bl 1 :ec 10 :el 1} " "]
   [:symbol @{:bc 10 :bl 1 :ec 16 :el 1} "./args"]
   [:whitespace @{:bc 16 :bl 1 :ec 17 :el 1} " "]
   [:keyword @{:bc 17 :bl 1 :ec 20 :el 1} ":as"]
   [:whitespace @{:bc 20 :bl 1 :ec 21 :el 1} " "]
   [:symbol @{:bc 21 :bl 1 :ec 22 :el 1} "a"]]

  (is-import? zloc)
  # =>
  true

  )

(defn analyze-import
  [node]
  (def parsed
    (try (parse (j/gen node))
      ([e] (errorf "failed to parse node: %n" node))))
  (assertf (tuple? parsed) "expected tuple, found: %n: for: %n"
           (type parsed) parsed)
  #
  (table :path (string (get parsed 1))
         ;(tuple/slice parsed 2)))

(comment

  (analyze-import [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
                   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
                   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
                   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
                   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
                   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
                   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
                   [:string @{:bc 24 :bl 1 :ec 26 :el 1} "\"\""]])
  # =>
  @{:path "./args" :prefix ""}

  (analyze-import [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
                   [:whitespace @{:bc 2 :bl 1 :ec 3 :el 1} " "]
                   [:symbol @{:bc 3 :bl 1 :ec 9 :el 1} "import"]
                   [:whitespace @{:bc 9 :bl 1 :ec 10 :el 1} " "]
                   [:symbol @{:bc 10 :bl 1 :ec 16 :el 1} "./args"]
                   [:whitespace @{:bc 16 :bl 1 :ec 17 :el 1} " "]
                   [:keyword @{:bc 17 :bl 1 :ec 20 :el 1} ":as"]
                   [:whitespace @{:bc 20 :bl 1 :ec 21 :el 1} " "]
                   [:symbol @{:bc 21 :bl 1 :ec 22 :el 1} "a"]])
  # =>
  @{:as 'a :path "./args"}

  )

(defn make-version-def-form
  [&opt name time]
  (default name "version")
  #
  (string/format `(def %s "%s")` name (u/dt-stamp time)))

(comment

  (make-version-def-form nil 0)
  # =>
  "(def version \"1970-01-01_00-00-00\")"

  (make-version-def-form "my-stamp" "1234567890")
  # =>
  "(def my-stamp \"2009-02-13_23-31-30\")"

  )

(defn is-version-def?
  [zloc]
  (def node (j/node zloc))
  (when (not= :tuple (get node 0))
    (break false))
  #
  (def head-zloc (j/down zloc))
  (when (not head-zloc)
    (break false))
  #
  (def [head-type _ _] (j/node head-zloc))
  (def first-non-wsc-zloc
    (cond
      (= :symbol head-type)
      head-zloc
      #
      (or (= :whitespace head-type)
          (= :comment head-type))
      (j/right-skip-wsc head-zloc)
      #
      nil))
  (when (not first-non-wsc-zloc)
    (break false))
  #
  (def [fnw-type _ fnw-value] (j/node first-non-wsc-zloc))
  (when (not (and (= :symbol fnw-type)
                  (= "def" fnw-value)))
    (break false))
  #
  (def second-non-wsc-zloc (j/right-skip-wsc first-non-wsc-zloc))
  (when (not second-non-wsc-zloc)
    (break false))
  #
  (def [nnw-type _ nnw-value] (j/node second-non-wsc-zloc))
  (when (and (= :symbol nnw-type)
             (= "version" nnw-value))
    (u/maybe-dump :is-version-def? (j/gen (j/node zloc)))
    true))

(comment

  (def zloc (j/zip-down (j/par `(def version "DEVEL")`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 22 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 5 :el 1} "def"]
   [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
   [:symbol @{:bc 6 :bl 1 :ec 13 :el 1} "version"]
   [:whitespace @{:bc 13 :bl 1 :ec 14 :el 1} " "]
   [:string @{:bc 14 :bl 1 :ec 21 :el 1} "\"DEVEL\""]]

  (is-version-def? zloc)
  # =>
  true

  )

