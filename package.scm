;;
;; Package sxmml
;;

(define-gauche-package "sxmml"
  ;; Repository URL, e.g. github
  ;;  This URL uniquely identifies the package.
  :repository "https://github.com/sayzbrdg/sxmml.git"

  ;;
  :version "0.1"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "S-eXpression Music Macro Language\n\
                convert MML writen in sxmml to other drivers format."

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.15")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  :providing-modules (sxmml)

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Seiji Ohashi <sayzbrdg@gmail.com>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ()

  ;; Homepage URL, if any.
  ; :homepage "http://example.com/sxmml/"
  )
