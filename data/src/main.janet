(import ./args :as a)
(import ./link :as l)
(import ./prepare :as p)
(import ./study :as s)
(import ./utils :as u)

(def version "DEVEL")

(def usage
  ``
  Usage: jell [<start-path> [<out-path> [<obj-path>]]]
         jell [-h|--help]

  Create a single `.janet` file from multiple files [1].

  Parameters:

    <start-path>                 path to starting file
    <out-path>                   path to output file
    <obj-path>                   path to temp directory

  Defaults:

    <start-path>                 src/main.janet
    <out-path>                   j.out
    <obj-path>                   obj

  Options:

    -h, --help                   show this output

  Configuration (optional):

    .jell.jdn                    configuration file

  Example Invocations:

    Create a single `.janet` file from multiple files:

    $ jell src/main.janet output.janet

    Same but use `tmp/` as an intermediate file directory:

    $ jell src/main.janet output.janet tmp

    Create file, if a suitable configuration file exists:

    $ jell

    Without a suitable configuration file, show usage:

    $ jell

  Example `.jell.jdn` content:

    {:start-path "src/main.janet"
     :out-path "j.out"
     :obj-path "obj"}

  ---

  [1] There are a fair number of restrictions on the
      type of Janet code that can be handled.
  ``)

########################################################################

(defn get-full-paths
  [opts]
  (def {:sep sep} (u/get-os-bits))
  (def [start-path obj-path out-path]
    [(get opts :start-path)
     (get opts :obj-path)
     (get opts :out-path)])
  #
  (def cur-dir (os/cwd))
  #
  (assertf (= :file (os/stat start-path :mode))
           "expected an existing file for: %s" start-path)
  (def start-path (os/realpath (string cur-dir sep start-path)))
  #
  (when (not= :directory (os/stat obj-path :mode))
    (os/mkdir obj-path))
  (assertf (= :directory (os/stat obj-path :mode))
           "expected directory at: %s" obj-path)
  (def obj-path (os/realpath (string cur-dir sep obj-path)))
  #
  (def out-path
    (let [op (string cur-dir sep out-path)]
      (when (not (= :file (os/stat op :mode)))
        (u/touch op))
      #
      (assertf (= :file (os/stat op :mode))
               "expected file at %s" op)
      (if (u/abspath? op)
        op
        (os/realpath (string cur-dir sep op)))))
  #
  [start-path obj-path out-path])

(defn assimilate
  [opts]
  (def {:bs-land bs-land} (u/get-os-bits))
  (def [start-path obj-path out-path] (get-full-paths opts))
  (def perm
    (when (not bs-land) (os/stat start-path :permissions)))
  (put opts :start-file-perm perm)
  # study the input files starting at start-path
  (when (get opts :flycheck) (flycheck start-path))
  (def prefixes (s/study start-path))
  (def [in-dir in-name] (u/split-path start-path))
  (when (get opts :flycheck)
    (eachp [path _] prefixes
      (flycheck path)))
  # prepare imported files: rename names and tweak import forms
  (p/prepare-imported in-dir obj-path prefixes opts)
  # prepare starting file: tweak import forms
  (def in-path (p/prepare-start start-path in-name obj-path opts))
  # link
  (l/link in-path out-path opts)
  (when (not bs-land)
    (os/chmod out-path perm)))

########################################################################

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  (u/maybe-dump :opts opts)
  #
  (cond
    (get opts :show-help)
    (print usage)
    #
    (get opts :show-version)
    (print version)
    #
    (and (get opts :start-path)
         (get opts :obj-path)
         (get opts :out-path))
    (assimilate opts)
    #
    (do
      (eprint "please specify a start path and an output path")
      (eprint "Try jell -h for usage.")
      (os/exit 1))))

