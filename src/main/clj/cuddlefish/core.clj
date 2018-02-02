(ns cuddlefish.core
  "A cute little git wrapper."
  {:authors ["Colin Steele <cvillecsteele@gmail.com>"
             "Reid \"arrdem\" McKenzie <me@arrdem.com>"]
   :license "https://www.eclipse.org/legal/epl-v10.html" }
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]))

(def git-describe-pattern
  #"(?<tag>.*)-(?<ahead>\d+)-g(?<ref>[0-9a-f]*)(?<dirty>(-dirty)?)")

(defn- do-cmd [cmd]
  (str/trim (:out (apply sh cmd))))

(defn current-branch
  "Fetches the name of the current git branch using the configured `git`."
  [{:keys [git] :as config}]
  (let [cmd [git "rev-parse" "--abbrev-ref" "HEAD"]]
    (do-cmd cmd)))

(defn resolve-ref
  "Fetches the git ref of `ref`, being a tag or ref name using the configured `git`."
  [{:keys [git] :as config} ref]
  (let [cmd [git "rev-parse" "--verify" ref]]
    (do-cmd cmd)))

(defn ref-message
  "Fetches the message of the `ref-or-sha` from git-log, using the configured `git`."
  [{:keys [git] :as config} ref-or-sha]
  (let [cmd [git "log" "-1" ref-or-sha]]
    (:out (apply sh cmd))))

(defn ref-ts
  "Fetches the timestamp of the `ref-or-sha` from git-log, using the configured `git`."
  [{:keys [git] :as config} ref-or-sha]
  (let [cmd [git "log" "-1" "--pretty=%ct" ref-or-sha]]
    (do-cmd cmd)))

(defmacro let-groups
  "Let for binding groups out of a j.u.r.Pattern j.u.r.Matcher."
  {:style/indent [1]}
  [[bindings m] & body]
  (let [s (with-meta (gensym "matcher") {:tag java.util.regex.Matcher})]
    `(let [~s ~m
           ~@(mapcat identity
                     (for [b bindings]
                       `[~b (.group ~s ~(name b))]))]
       ~@body)))

(defn ensure-pattern
  "Given a string, compiles it to a j.u.Pattern."
  [x]
  (cond (string? x)
        (re-pattern x)

        (instance? java.util.regex.Pattern x)
        x

        :else
        (throw (IllegalArgumentException. "ensure-pattern requires a string or a j.u.r.Pattern!"))))

(defn- parse-describe
  "Implementation detail.

  Used to parse the output of git-describe, using the configured `describe-pattern`.

  Returns a map `{:tag, :ahead, :ahead?, :ref, :ref-short, :dirty?}`
  if the pattern matches, otherwise returns the empty map."
  [{:keys [describe-pattern] :as config} out]
  (let [pattern (ensure-pattern describe-pattern)
        matcher (re-matcher pattern out)]
    (if-not (.matches matcher)
      (do (binding [*out* *err*]
            (printf (str "Warning: cuddlefish couldn't match the current repo status:\n%s\n\n"
                         "Against pattern:\n%s\n\n")
                    (pr-str out) pattern)
            (.flush *out*))
          {})
      (let-groups [[tag ahead ref dirty] matcher]
        {:tag       tag
         :ahead     (Integer/parseInt ahead)
         :ahead?    (not= ahead "0")
         :ref       (resolve-ref config "HEAD")
         :ref-short ref
         :dirty?    (not= "" dirty)}))))

(defn describe
  "Uses git-describe to parse the status of the repository.

  Using the configured `git` and `describe-pattern` to parse the output.

  Returns a map `{:tag, :ahead, :ahead?, :ref, :ref-short, :dirty?}`
  if the pattern matches, otherwise returns the empty map."
  [{:keys [git] :as config}]
  (let [{:keys [exit out] :as child} (apply sh [git "describe" "--tags" "--dirty" "--long"])]
    (if-not (= exit 0)
      (binding [*out* *err*]
        (printf "Warning: lein-git-version git exited %d\n%s\n\n"
                exit child)
        (.flush *out*)
        {})
      (parse-describe config (str/trim out)))))

(defn status
  "Fetch the current git status, augmenting `#'describe`'s output
  with the message and timestamp of the last commit, if the repository
  isn't dirty."
  [config]
  (if-let [{:keys [dirty?] :as status} (describe config)]
    (cond->  status
      (not dirty?) (assoc :message (ref-message config "HEAD"))
      (not dirty?) (assoc :timestamp (ref-ts config "HEAD")))))
