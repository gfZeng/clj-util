
(set-env!
 :source-paths    #{"src"}
 :dependencies '[[adzerk/bootlaces "0.1.13" :scope "test"]])

(require '[adzerk.bootlaces :refer :all])

(def +version+ "2.0.3")
(bootlaces! +version+)


(task-options!
 push {:gpg-sign false}
 pom {:project  'clj-integral
      :version  +version+
      :url      "https://github.com/gfZeng/clj-util"
      :scm      {:url "https://github.com/gfZeng/Korma"}
      :license  {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})
