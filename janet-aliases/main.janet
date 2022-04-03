(import ./location :as l)
(import ./zipper :as j)
(import ./loc-jipper :as j)
(import ./vendor/walk-dir :as wd)

(defn collect
  [zloc pred]
  (def results @[])
  (var curr-zloc zloc)
  #
  (while (not (j/end? curr-zloc))
    (when (pred curr-zloc)
      (array/push results (j/node curr-zloc)))
    (if-let [next-zloc (j/df-next curr-zloc)]
      (set curr-zloc next-zloc)
      (break nil)))
  results)

(comment

  (def src
    ``
    (import ./location :as l)

    (def a 1)

    (import ./zipper :as z)
    ``)

  (collect (-> (l/ast src)
               j/zip-down)
           |(match (j/node $)
              [:tuple _ [:symbol _ "import"]]
              true))
  # =>
  '@[(:tuple @{:bc 1 :bl 1 :ec 26 :el 1}
             (:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import")
             (:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " ")
             (:symbol @{:bc 9 :bl 1 :ec 19 :el 1} "./location")
             (:whitespace @{:bc 19 :bl 1 :ec 20 :el 1} " ")
             (:keyword @{:bc 20 :bl 1 :ec 23 :el 1} ":as")
             (:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " ")
             (:symbol @{:bc 24 :bl 1 :ec 25 :el 1} "l"))
     (:tuple @{:bc 1 :bl 5 :ec 24 :el 5}
             (:symbol @{:bc 2 :bl 5 :ec 8 :el 5} "import")
             (:whitespace @{:bc 8 :bl 5 :ec 9 :el 5} " ")
             (:symbol @{:bc 9 :bl 5 :ec 17 :el 5} "./zipper")
             (:whitespace @{:bc 17 :bl 5 :ec 18 :el 5} " ")
             (:keyword @{:bc 18 :bl 5 :ec 21 :el 5} ":as")
             (:whitespace @{:bc 21 :bl 5 :ec 22 :el 5} " ")
             (:symbol @{:bc 22 :bl 5 :ec 23 :el 5} "z"))]

  (def src
    ``
    (import freja/default-hotkeys :prefix "" :export true)
    ``)

  (collect (-> (l/ast src)
               j/zip-down)
           |(match (j/node $)
              [:tuple _ [:symbol _ "import"]]
              true))
  # =>
  '@[(:tuple @{:bc 1 :bl 1 :ec 55 :el 1}
             (:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import")
             (:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " ")
             (:symbol @{:bc 9 :bl 1 :ec 30 :el 1} "freja/default-hotkeys")
             (:whitespace @{:bc 30 :bl 1 :ec 31 :el 1} " ")
             (:keyword @{:bc 31 :bl 1 :ec 38 :el 1} ":prefix")
             (:whitespace @{:bc 38 :bl 1 :ec 39 :el 1} " ")
             (:string @{:bc 39 :bl 1 :ec 41 :el 1} "\"\"")
             (:whitespace @{:bc 41 :bl 1 :ec 42 :el 1} " ")
             (:keyword @{:bc 42 :bl 1 :ec 49 :el 1} ":export")
             (:whitespace @{:bc 49 :bl 1 :ec 50 :el 1} " ")
             (:constant @{:bc 50 :bl 1 :ec 54 :el 1} "true"))]

  )

(defn collect-imports
  [src]
  (collect (-> (l/ast src)
               j/zip-down)
           |(match (j/node $)
              [:tuple _ [:symbol _ "import"]]
              true)))

(comment

  (def src
    ``
    (import ./location :as l)

    (def a 1)

    (import ./zipper :as z)
    ``)

  (collect-imports src)
  # =>
  '@[(:tuple @{:bc 1 :bl 1 :ec 26 :el 1}
             (:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import")
             (:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " ")
             (:symbol @{:bc 9 :bl 1 :ec 19 :el 1} "./location")
             (:whitespace @{:bc 19 :bl 1 :ec 20 :el 1} " ")
             (:keyword @{:bc 20 :bl 1 :ec 23 :el 1} ":as")
             (:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " ")
             (:symbol @{:bc 24 :bl 1 :ec 25 :el 1} "l"))
     (:tuple @{:bc 1 :bl 5 :ec 24 :el 5}
             (:symbol @{:bc 2 :bl 5 :ec 8 :el 5} "import")
             (:whitespace @{:bc 8 :bl 5 :ec 9 :el 5} " ")
             (:symbol @{:bc 9 :bl 5 :ec 17 :el 5} "./zipper")
             (:whitespace @{:bc 17 :bl 5 :ec 18 :el 5} " ")
             (:keyword @{:bc 18 :bl 5 :ec 21 :el 5} ":as")
             (:whitespace @{:bc 21 :bl 5 :ec 22 :el 5} " ")
             (:symbol @{:bc 22 :bl 5 :ec 23 :el 5} "z"))]

  )

(defn import-node-to-table
  [node]
  (match node
    [:tuple span
     [:symbol _ "import"]
     [:whitespace _ _]
     [:symbol _ path]
     & rest]
    (do
      (def tbl @{:_import-path path
                 :_span span})
      (def n-elts (length rest))
      (when (pos? n-elts)
        (var idx 0)
        (while (< idx n-elts)
          (assert (match (get rest idx)
                    [:whitespace _ _]
                    true)
                  (string/format "Unexpected node for whitespace: %p"
                                 (get rest idx)))
          (++ idx)
          (def name
            (match (get rest idx)
              [:keyword _ kwd]
              (keyword (string/slice kwd 1))))
          (assert name (string/format "Unexpected node for name: %p"
                                 (get rest idx)))
          (++ idx)
          (assert (match (get rest idx)
                    [:whitespace _ _]
                    true)
                  (string/format "Unexpected node for whitespace: %p"
                                 (get rest idx)))
          (++ idx)
          (def value
            (match (get rest idx)
              [_ _ val]
              val))
          (assert value
                  (string/format "Unexpected node for value: %p"
                                 (get rest idx)))
          (put tbl name value)
          (++ idx))
        (assert (= idx n-elts)
                (string/format "Unexpected index: %p" idx)))
      tbl)))

(comment

  (def node
    '(:tuple @{:bc 1 :bl 1 :ec 26 :el 1}
             (:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import")
             (:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " ")
             (:symbol @{:bc 9 :bl 1 :ec 19 :el 1} "./location")
             (:whitespace @{:bc 19 :bl 1 :ec 20 :el 1} " ")
             (:keyword @{:bc 20 :bl 1 :ec 23 :el 1} ":as")
             (:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " ")
             (:symbol @{:bc 24 :bl 1 :ec 25 :el 1} "l")))

  (import-node-to-table node)
  # =>
  @{:_import-path "./location"
    :_span @{:bc 1 :bl 1 :ec 26 :el 1}
    :as "l"}

  (def node
    '(:tuple @{:bc 1 :bl 1 :ec 55 :el 1}
             (:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import")
             (:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " ")
             (:symbol @{:bc 9 :bl 1 :ec 30 :el 1} "freja/default-hotkeys")
             (:whitespace @{:bc 30 :bl 1 :ec 31 :el 1} " ")
             (:keyword @{:bc 31 :bl 1 :ec 38 :el 1} ":prefix")
             (:whitespace @{:bc 38 :bl 1 :ec 39 :el 1} " ")
             (:string @{:bc 39 :bl 1 :ec 41 :el 1} "\"\"")
             (:whitespace @{:bc 41 :bl 1 :ec 42 :el 1} " ")
             (:keyword @{:bc 42 :bl 1 :ec 49 :el 1} ":export")
             (:whitespace @{:bc 49 :bl 1 :ec 50 :el 1} " ")
             (:constant @{:bc 50 :bl 1 :ec 54 :el 1} "true")))

  (import-node-to-table node)
  # =>
  @{:_import-path "freja/default-hotkeys"
    :_span @{:bc 1 :bl 1 :ec 55 :el 1}
    :export "true"
    :prefix "\"\""}

  )

(defn main
  [& args]
  (assert (pos? (length args))
          "need at least one argument, a directory path")
  #
  (def root-dir (get args 1))
  #
  (assert (= :directory
             (os/stat root-dir :mode))
          "argument was not a directory path")
  #
  (def file-paths @[])
  #
  (wd/just-files root-dir file-paths
                 |(when (string/has-suffix? ".janet" $) $))
  #
  (def tables @[])
  #
  (each fp file-paths
    (def src (slurp fp))
    (when (not (empty? src))
      (array/concat tables
                    (-?>> src
                          collect-imports
                          (map import-node-to-table)
                          (map |(put $ :_found-in fp))))))
  #
  (def grouped
    (group-by |(get $ :_import-path) tables))
  #
  (each import-path (->> grouped
                         keys
                         sort)
    (def data
      (get grouped import-path))
    (def results
      (->> data
           (filter |(get $ :as))
           (sort-by |(get $ :as))))
    #
    (when (< 1 (length results))
      (print import-path)
      (each tbl results
        (printf "  %p %p"
                (get tbl :as)
                (get tbl :_found-in)))
      (print))))

(comment

  (def src
    ``
    (import ./file-handling)

    (import freja/file-handling :as fh)
    ``)

  # project root
  (def pr
    "/home/user/src/freja")

  # JANET_PATH
  (def jp
    "/home/user/.local/lib/janet")

  @{# path of imported module as found in module/cache
    "freja/file-handling.janet" # note: not full path
    # array of tables
    @[@{:_found-in (string pr "/freja/checkpoint.janet")
        :_import-path "./file-handling"
        :_span @{}}]
    # path of imported module as found in module/cache
    "freja/file-handling" # note: no file extension
    # array of tables
    @[@{:_found-in (string pr "/freja/checkpoint.janet")
        :_import-path "freja/file-handling"
        :_span @{}
        :as "fh"}
      @{:_found-in (string pr "/freja/editor.janet")
        :_import-path "freja/file-handling"
        :_span @{}
        :as "fh"}
      @{:_found-in (string pr "/freja/default-layout.janet")
        :_import-path "freja/file-handling"
        :_span @{}
        :as "fh"}
      @{:_found-in (string pr "/freja/default-hotkeys.janet")
        :_import-path "freja/file-handling"
        :_span @{}
        :as "fh"}
      @{:_found-in (string pr "/freja/find-file.janet")
        :_import-path "freja/file-handling"
        :_span @{}
        :as "fh"}]
    # path of imported module as found in module/cache
    "freja/event/subscribe.janet" # note: not full path
    # array of tables
    @[@{:_found-in (string pr "/freja/event/jaylib-to-events.janet")
        :_import-path "./subscribe"
        :_span @{}
        :as "s"}]}

  # for modules of "type" :preload, the associated path in `module/cache` does not
  # appear to be a full path, but rather the same as what is used as the
  # "path" argument to import, e.g. in the following form:
  #
  #   (import freja/state)
  #
  # the associated path in `module/cache` is "freja/state" (no file extension)
  #
  # in freja, for:
  #
  #   (import ./state)
  #
  # there may not be an associated path until control-L is invoked at least
  # once.  after such time, the associated path might be "freja/state.janet".
  # note the file extension.  the module's type in such a case is (always?)
  # :source.

  )

# given:
#
# * project root path
# * starting janet file?
# * JANET_PATH
#
# produce:
#
# * summary of aliases
#   * for each imported file:
#     * list all used aliases
#     * indicate whether there are inconsistent aliases

