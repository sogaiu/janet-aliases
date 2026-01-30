(defn wd/path-join
  [& parts]
  (string/join parts
               (dyn :path-fs-sep
                    (if (= :windows (os/which))
                      `\`
                      "/"))))

(comment

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (wd/path-join "/tmp" "test.txt")))
  # =>
  "/tmp/test.txt"

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (wd/path-join "/tmp" "foo" "test.txt")))
  # =>
  "/tmp/foo/test.txt"

  (do
    (def sep (dyn :path-fs-sep))
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep `\`)
      (wd/path-join "C:" "windows" "system32")))
  # =>
  `C:\windows\system32`

  )

(defn wd/path-ext
  [path]
  (let [results (string/find-all "." path)]
    (if-let [last-one (last results)]
      (string/slice path last-one)
      "")))

(comment

  (wd/path-ext "hello.janet")
  # =>
  ".janet"

  (wd/path-ext "bye")
  # =>
  ""

  )

(defn wd/is-dir?
  ``
  Returns true if `path` is a directory.  Otherwise, returns false.

  If optional argument `symlink` is true, return true for symlinks
  that resolve to directories.  The default value for `symlink`
  is false.
  ``
  [path &opt symlink]
  (default symlink false)
  (when-let [path path
             stat (if symlink
                    (os/stat path)
                    (os/lstat path))]
    (= :directory (stat :mode))))

(comment

  (wd/is-dir? (or (os/getenv "HOME")
               (os/getenv "USERPROFILE")))
  # =>
  true

 )

(defn wd/is-file?
  ``
  Returns true if `path` is an ordinary file (e.g. not a directory).
  Otherwise, returns false.

  If optional argument `symlink` is true, return true for symlinks
  that resolve to files.  The default value for `symlink` is false.
  ``
  [path &opt symlink]
  (default symlink false)
  (truthy?
    (when-let [path path
               mode-stat (if symlink
                           (os/stat path)
                           (os/lstat path :mode))]
      (= :file mode-stat))))

(comment

  (wd/is-file? (or (os/getenv "HOME")
                (os/getenv "USERPROFILE")))
  # =>
  false

  (let [name (string (gensym))]
    (if (os/stat name)
      true
      (do
        (spit name "hello")
        (def res
          (wd/is-file? name))
        (os/rm name)
        res)))
  # =>
  true

 )

(defn wd/just-files
  ``
  Recursively visit directory tree starting at `path`, accumulating
  file (not directory) paths by default into array `acc`.

  If optional argument `a-fn` is specified, instead accumulate only
  file paths for which `a-fn` applied to the file path returns a
  truthy result.

  If optional argument `symlink` is truthy, treat symlinks to
  directories as directories.  That is, follow symlinks that point to
  directories and descend into them looking for files.
  ``
  [path acc &opt a-fn symlink]
  (default a-fn identity)
  (default symlink false)
  (when (wd/is-dir? path symlink)
    (each thing (os/dir path)
      (def thing-path
        (wd/path-join path thing))
      (cond
        (and (wd/is-file? thing-path)
             (a-fn thing-path))
        (array/push acc thing-path)
        #
        (wd/is-dir? thing-path symlink)
        (wd/just-files thing-path acc a-fn symlink))))
  acc)

(comment

  (def acc @[])

  (wd/just-files (wd/path-join (os/getenv "HOME")
                         ".config")
              acc)

  )

(defn wd/just-dirs
  ``
  Recursively visit directory tree starting at `path`, accumulating
  directory paths by default into array `acc`.

  If optional argument `a-fn` is specified, instead accumulate only
  directory paths for which `a-fn` applied to the directory path
  returns a truthy result.

  If optional argument `symlink` is truthy, treat symlinks to
  directories as directories.  That is, follow symlinks that point to
  directories and descend into them looking for directories.
  ``
  [path acc &opt a-fn symlink]
  (default a-fn identity)
  (default symlink false)
  (when (wd/is-dir? path symlink)
    (each thing (os/dir path)
      (def thing-path
        (wd/path-join path thing))
      (when (wd/is-dir? thing-path symlink)
        (when (a-fn thing-path)
          (array/push acc thing-path))
        (wd/just-dirs thing-path acc a-fn symlink))))
  acc)

(comment

  (def acc @[])

  (wd/just-dirs (wd/path-join (os/getenv "HOME")
                        ".config")
             acc)

 )

(defn wd/visit-files
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered file (not directory) path.
  ``
  [path a-fn]
  (when (wd/is-dir? path)
    (each thing (os/dir path)
      (def thing-path (wd/path-join path thing))
      (cond
        (wd/is-file? thing-path)
        (a-fn thing-path)
        #
        (wd/is-dir? thing-path)
        (wd/visit-files thing-path a-fn)))))

(comment

  (wd/visit-files (wd/path-join (os/getenv "HOME")
                          ".config")
               |(eprint $))

 )

(defn wd/visit-dirs
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered directory path.
  ``
  [path a-fn]
  (when (wd/is-dir? path)
    (each thing (os/dir path)
      (def thing-path (wd/path-join path thing))
      (when (wd/is-dir? thing-path)
        (a-fn thing-path)
        (wd/visit-dirs thing-path a-fn)))))

(comment

  (wd/visit-dirs (wd/path-join (os/getenv "HOME")
                         ".config")
              |(eprint $))

 )

(defn wd/visit
  ``
  Recursively traverse directory tree starting at `path`, applying
  argument `a-fn` to each encountered path (file and directory).
  ``
  [path a-fn]
  (when (wd/is-dir? path)
    (each thing (os/dir path)
      (def thing-path (wd/path-join path thing))
      (when (or (wd/is-file? thing-path)
                (wd/is-dir? thing-path))
        (a-fn thing-path))
      (when (wd/is-dir? thing-path)
        (wd/visit thing-path a-fn)))))

(comment

  (wd/visit (wd/path-join (os/getenv "HOME")
                    ".config")
         |(eprint $))

 )

