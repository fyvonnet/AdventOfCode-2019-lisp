(defsystem "adventofcode-2019"
           :description "Advent of Code 2019 in LISP"
           :author "Franck YVONNET"
           :serial t
           :depends-on (:alexandria :cl-ppcre :iterate :queues :queues.simple-queue :trivia)
           :components ((:file "intcode")
                        (:file "aoc-coord")
                        (:file "day01")
                        (:file "day02")
                        (:file "day03")
                        (:file "day04")
                        (:file "day05")
                        (:file "day06")))
