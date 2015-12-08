(defproject intro "0.2.3"
  :description "A pilot project to use Clojure for introductory computer science courses at the University of Minnesota - Morris"
  :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                 [quil "2.2.5"]
                 [inflections "0.9.14"]
                 ;[uncomplicate/clojurecl "0.3.0"]
                 [org.clojars.quil-firstclass-shapes/firstclassshapes "0.0.1"]]
  :plugins [[lein-autoexpect "1.0"]
            [lein-pprint "1.1.2"]]
  :java-source-paths ["src/java/connectFour/"]
  :main intro.core)
