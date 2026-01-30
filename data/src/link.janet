(import ./common :as c)
(import ./jipper :as j)
(import ./utils :as u)

# create single file of source from appropriately modified set of
# files (see the code in prepare), beginning with a starting janet
# file by:
#
# 1. create a zipper from in-path's content, then traverse to the
#    right, recording the corresponding source code to out-path unless
#    the encountered zloc has a node representing an import form.
#
# 2. if an import form is encountered, record a commented version of
#    it in out-path, and if the file the import form refers to has not
#    been visited, visit the file and continue recursively.
#
# 3. in the case of the starting file, if a `(def version ...)` form
#    is encountered, replace the `...` with a suitable string.

(defn traverse
  [a-path misc]
  (def {:out-file out-file :sep sep :seen seen
        :is-starting-file? is-starting-file?} misc)
  (def [a-dir a-name] (u/split-path a-path))
  (def full-path (os/realpath (string (os/cwd) sep a-name)))
  (when (in seen full-path) (break))
  #
  (put seen full-path true)
  (var zloc
    (try (-> (slurp full-path)
             j/par
             j/zip-down)
      ([e] (errorf "failed to prepare zloc from: %s" full-path))))
  (while zloc
    (def cur-node (j/node zloc))
    (cond
      (c/is-import? zloc)
      (let [i-tbl (c/analyze-import cur-node)
            commented (-> zloc
                          (j/insert-child [:whitespace {} " "])
                          (j/insert-child [:symbol {} "comment"])
                          j/node
                          j/gen)]
        (file/write out-file commented "\n")
        (def i-path (get i-tbl :path))
        # parse import path
        (def [dir name] (u/split-path i-path))
        (def cur-dir (os/cwd))
        #
        (os/cd dir)
        (traverse (string i-path ".janet")
                  (merge misc {:is-starting-file? false}))
        (os/cd cur-dir))
      #
      (and is-starting-file? (c/is-version-def? zloc))
      (let [vd-form (c/make-version-def-form)]
        (file/write out-file vd-form))
      #
      (file/write out-file (j/gen cur-node)))
    (set zloc (j/right zloc))))

(defn link
  [in-path out-path &opt opts]
  (u/maybe-dump :call "link" :in-path in-path :out-path out-path
                :opts opts)
  (def {:sep sep} (u/get-os-bits))
  (def {:add-shebang add-shebang} opts)
  # assumes paths are full paths...
  # XXX: could check if we had abspath?
  (def [dir-path file-path] (u/split-path in-path))
  # remember which files have already been "imported"
  (def seen @{})
  # for restoring the current working directory (cwd)
  (def old-dir (os/cwd))
  # need to operate relative to in-path's dir
  (os/cd dir-path)
  #
  (defer (os/cd old-dir)
    (with [out-file (file/open out-path :w)]
      (when add-shebang
        (file/write out-file (u/make-shebang) "\n\n"))
      (traverse in-path {:out-file out-file :seen seen :sep sep
                         :is-starting-file? true})
      (file/flush out-file))))

