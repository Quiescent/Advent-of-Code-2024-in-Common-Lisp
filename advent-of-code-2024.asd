(defsystem "advent-of-code-2024"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-ppcre :trivia :trivia.ppcre :arrow-macros :metabang-bind :neat-lambda)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "2024-day-7")
                 (:file "2024-day-6")
                 (:file "2024-day-5")
                 (:file "2024-day-4")
                 (:file "2024-day-3")
                 (:file "2024-day-2")
                 (:file "2024-day-1"))))
  :description "")

