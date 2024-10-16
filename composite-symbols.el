;;; composite-symbols.el --- Composite special symbols for programming -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014 Kirill Ignatiev <github.com/ikirill>
;;
;; Author: Kirill Ignatiev <github.com/ikirill>
;; Version: 0.1
;; Keywords: faces, convenience
;; URL: https://github.com/ikirill/composite-symbols
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (dash "2.19.1"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This mode replaces some special symbols with corresponding unicode
;; characters. For example, it will replace "<=" with <LESS-THAN> '<',
;; and "None" with <EMPTY SET> '∅' (with default settings, which can
;; be modified).
;;
;; To use:
;;
;; - `composite-symbols-mode' enables special symbols in some default
;;   programming languages. If your language is not supported, open an
;;   issue on github, or define your own configuration. If the mode
;;   replaces symbols that you do not want to see replaced, you can
;;   configure your own replacement rules.
;; - `composite-symbols-greek-mode' shows all non-ambiguous Greek
;;   letters that are spelled in English as the corresponding Greek
;;   letters.
;; - `composite-symbols-ignore-indentation' controls whether the mode
;;   is allowed to "break" indentation by changing lengths of lines.
;; - For a starting point configuring your own replacement rules, look
;;   at the definition of `composite-symbols-assign-arrow-mode'.
;;
;; Notes:
;;
;; - There is a fair amount of variability in what you might prefer to
;;   see from this package.  See definitions of variables like
;;   `composite-symbols-cc-rules' to see how to define your own sets of
;;   composite symbol replacements.
;;
;; - Many of these characters might be missing from your default
;;   fonts.  It may be helpful to install the package `unicode-fonts',
;;   or use set-fontset-font to update which font Emacs uses to
;;   display which characters (other, non-default fonts might have the
;;   missing characters).
;;
;;   The package unicode-fonts includes helpful suggestions about
;;   which extra fonts might need to be installed.
;;
;; - When defining new composite symbols, the mode
;;   `show-unicode-minor-mode' is quite helpful as it shows next to
;;   every hexadecimal literal the corresponding Unicode character.
;;
;; - This (programming-language-agnostic) package derives from the
;;   code that haskell-mode uses to fontify haskell code.
;;
;; - font-lock suppresses errors in the fontification process.  The
;;   package `font-lock-studio' can make debugging easier.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

;; {{{ Customizations

(defgroup composite-symbols nil
  "
This mode replaces some special symbols with corresponding unicode
characters. For example, it will replace \"<=\" with <LESS-THAN> '<',
and \"None\" with <EMPTY SET> '∅' (with default settings, which can
be modified)."
  :group 'faces
  :link '(function-link composite-symbols-mode))

(defcustom composite-symbols-ignore-indentation nil
  "Whether indentation can be ignored when composing symbols.

If non-nil, the resulting indentation will be based on
already-composed symbols, and so will be different."
  :group 'composite-symbols
  :type 'boolean
  :safe #'booleanp)

(defcustom composite-symbols-user-alist nil
  "An alist mapping modes to keywords that will be used by
composite-symbols-mode.

For examples, see:
 - `composite-symbols-defaults'
 - `composite-symbols-default-mode-alist'

To specify a character code, use `insert-char' (e.g., ?≠) or a
hex code such as #x2260."
  :group 'composite-symbols
  ;; NOTE This *must* be consistent with composite-symbols--compile-user
  :type
  '(alist
    :key-type (symbol :tag "Mode")
    :value-type
    (choice
     :tag ""
     (choice
      :tag "Default mode rules"
      (const :tag "C-style" composite-symbols-cc-rules)
      (const :tag "Haskell-style" composite-symbols-haskell-rules)
      (const :tag "Julia-style" composite-symbols-julia-rules)
      (const :tag "Lisp-style" composite-symbols-lisp-rules)
      (const :tag "Python-style" composite-symbols-python-rules)
      (symbol :tag "Symbol"))
     (repeat
      :tag "List of mode-specific composite symbol rules"
      (choice
       :tag ""
       (string :tag "Default rule symbol")
       (cons :tag "Custom rule"
             (string :tag "Name (replaced string)")
             (choice :tag "Rule"
                     (integer :tag "Replacement char code")
                     (list :tag "Detailed rule"
                           (string :tag "Match regex")
                           (integer :tag "Match group")
                           (integer :tag "Character code")
                           (choice :tag "Reject before"
                                   (const nil)
                                   (string :tag "Regex"))
                           (choice :tag "Reject after"
                                   (const nil)
                                   (string :tag "Regex"))))))))))
;; (makunbound 'composite-symbols-user-alist)

(defvar composite-symbols-ignored-faces
  '(font-lock-doc-face font-lock-string-face font-lock-comment-face)
  "Text fontified with these faces will not be touch by special symbols.")

;; }}}
;; {{{ Creating keywords for font-lock-keywords

(defun composite-symbols-keyword-with-spaces (match match-group char-spec)
  "Make a keyword as `composite-symbols-keyword',
specifying that a match must be surrounded with spaces or control
characters."
  (composite-symbols-keyword
   match match-group char-spec
   (rx (not (any control space)) point)
   (rx point (not (any control space)))))

(defun composite-symbols-keyword (match match-group char-spec &optional reject-before reject-after)
  "Return a keyword specification suitable for `font-lock-add-keywords'.

MATCH is a regexp matching the text to be replaced.

MATCH-GROUP is the group in MATCH that will be replaced.

CHAR-SPEC is the replacement character, passed directly to `compose-region'.

REJECT-BEFORE is a regexp such that if it matches just before
MATCH, then MATCH will not be replaced, e.g., \"[0-9]\\=\". Note
that this is not the same as adding REJECT-BEFORE to the regex
MATCH, because everything up to the match might be already
fontified, in which case font-lock will start looking for a match
too far to the right.

REJECT-AFTER is a regexp such that if it matches just after MATCH,
then MATCH will not be replaced."
  (when (not match) (error "composite-symbols-keyword: match cannot be nil"))
  (when (not char-spec)
    (error "composite-symbols-keyword: char-spec cannot be nil"))
  (when (numberp char-spec)
    (setq char-spec (decode-char 'ucs char-spec)))
  (if (and (not reject-before) (not reject-after))
      (if (= match-group 0)
          `(,match (0 (composite-symbols--compose ,char-spec) prepend))
        `(,match (,match-group (composite-symbols--compose ,char-spec ,match-group) prepend)))
    `(,match (,match-group
              (composite-symbols--compose-check
               ,char-spec ,match-group ,reject-before ,reject-after) prepend))))

(defun composite-symbols--breaks-indentation (start end)
  "Whether the replacing the given length of text with one symbol
would break indentation.

Works by assuming that any line indented to the right of the end
of the symbol is indented relative to the current line."
  (and (> end (1+ start))
       (save-excursion
         (goto-char end)
         (let ((here (current-column)))
           (skip-syntax-forward " ")
           (when (= 0 (forward-line))
             (back-to-indentation)
             (< here (current-column)))))))

(defun composite-symbols--invalid-face (pos)
  "Check whether face at POS should cause the symbol there to be ignored."
  (let ((face (get-text-property pos 'face)))
    (if (and face (listp face))
        ;; This is necessary because sometimes face is a face composed
        ;; of more than one face
        (-any-p (lambda (it) (memq it composite-symbols-ignored-faces)) face)
      (memq face composite-symbols-ignored-faces))))

(defun composite-symbols--compose (char-spec &optional group)
  "Replace current match group with the character given by CHAR-SPEC.

The match group is GROUP, which defaults to 0.

This function is supposed to evaluate to facespec, as described
in `font-lock-keywords'."
  (unless group (setq group 0))
  (let* ((start (match-beginning group))
         (end (match-end group)))
    (when (and (not (composite-symbols--invalid-face start))
               (or composite-symbols-ignore-indentation
                   (not (composite-symbols--breaks-indentation start end))))
      (compose-region start end char-spec)))
  nil)

(defun composite-symbols--compose-check
    (char-spec &optional match-group reject-before reject-after)
  "Replace current match with given character using `compose-region'.
See also `composite-symbols-keyword', which uses this.

CHAR-SPEC is passed directly to `compose-region'.x

MATCH-GROUP is the matched regex group that will be replaced.

REJECT-BEFORE is a regex; if it matches when point is placed at
the beginning of match, the match will not be replaced.

REJECT-AFTER is like REJECT-BEFORE, but point is placed at the
end of the current match."
  (unless match-group (setq match-group 0))
  (let* ((start (match-beginning match-group))
         (end (match-end match-group)))
    (when (and (not (composite-symbols--invalid-face start))
               (or composite-symbols-ignore-indentation
                   (not (composite-symbols--breaks-indentation start end)))
               (not
                (and reject-before
                     (save-match-data
                       (save-excursion
                         (goto-char start)
                         (if (functionp reject-before)
                             (funcall reject-before)
                           (re-search-backward reject-before
                                               (line-beginning-position) t))))))
               (not
                (and reject-after
                     (save-match-data
                       (save-excursion
                         (goto-char end)
                         (if (functionp reject-after)
                             (funcall reject-after)
                           (re-search-forward reject-after
                                              (line-end-position) t)))))))
      (compose-region start end char-spec))
    nil))

(defvar composite-symbols-defaults)

(defun composite-symbols--compose-default (&optional lang)
  "Compose a symbol based on its default value.

It is assumed the match-group is 0. If supplied, LANG is used to
look up the default replacement symbol."
  (let* ((start (match-beginning 0))
         (end (match-end 0)))
    ;; (when (eq lang :greek) (message "Matched %s" (match-string 0)))
    (when (and (not (composite-symbols--invalid-face start))
               (or composite-symbols-ignore-indentation
                   (not (composite-symbols--breaks-indentation start end)))
               ;; FIXME There is a bug with greek-mode where in C++
               ;; and Haskell the word boundary regexes \\<, \\> don't
               ;; seem to work as expected. This does a similar thing.
               (not (and (eq lang :greek)
                         (or (save-excursion
                               (goto-char end)
                               (looking-at-p "[[:alpha:]]"))
                             (and (> start (point-min))
                                  (save-excursion
                                    (goto-char (1- start))
                                    (looking-at-p "[[:alpha:]]")))))))
      (let* ((lab (match-string 0))
             (ass (composite-symbols--default-lookup lab lang)))
        (compose-region start end (if (consp ass) (cl-caddr ass) ass)))))
  nil)

;; }}}
;; {{{ Appending lists of keywords

(defun composite-symbols-append (&rest kw-lists)
  "Given a list KW-LISTS of keyword lists, merge all that can be merged.

This is like `append', but it merges regular expression for
keywords with identical highlighters.

So given (\"a\" (0 (highlight))) (\"b\" (0 (highlight)))
the output should be (\"a\\|b\" (0 (highlight)))

The order between any two keywords one of which is merged may be
broken."
  (let ((work (apply 'append kw-lists)) sofar no-merge lookup)
    ;; This is quite messy
    (dolist (kw work)
      ;; (message "Keyword: %s" (prin1-to-string kw))
      ;; Here we expect kw to be (STRING (0 HIGHLIGHTER))
      ;; sofar is list of elements (0 HIGHLIGHTER) STRING-1 STRING-2 ...
      (cond
       ((not (and (stringp (car kw))
                (numberp (cl-caadr kw))
                (= 0 (cl-caadr kw))))
        (push kw no-merge))
       ((setq lookup (assoc (cdr kw) sofar))
        (push (car kw) (cdr lookup)))
       (t
        (push (cons (cdr kw) (list (car kw))) sofar)))
      ;; (message "sofar: %s\nno-merge: %s\nlookup: %s" sofar no-merge lookup)
      )
    (append
     no-merge
     (mapcar (lambda (elt)
               (if (= 1 (length (cdr elt)))
                   (cons (cadr elt) (car elt))
                 (cons (mapconcat 'identity (cdr elt) "\\|") (car elt))))
             sofar))))

;; }}}
;; {{{ Default string to character mappings

(defvar composite-symbols-defaults
  ;; See the docstring below
  ;; '((nil ...) (:c++ ...) (:python ...) ...)
  `((nil                   ; Map strings to integer character codes
     ("!" . #xac)          ; This is not suitable for C++, for example
     ("~=" . #xac)
     ("!=" . #x2262)
     ("/=" . #x2262)
     ("==" . #x2261)
     ("&&" . #x2227)
     ("||" . #x2228)
     (">=" . #x2265)
     ("<=" . #x2264)
     ("not" . #xac)
     ("and" . #x2227)
     ("or" . #x2228)

     ("->" . #x2192)
     ("=>" . #x21d2)
     ("<-" . #x2190)
     ("::" . #x2237)
     ("~" . #x223c)

     ("nullptr" . #x2205)
     ("null" . #x2205)
     ("NULL" . #x2205)
     ("None" . #x2205)
     ("undefined" . #x27c2)
     ("nothing" . #x2205)
     ("Int" . #x2124)
     ("Float64" . #x211d)
     ("Complex" . #x2102)
     ("lambda" . #x3bb)
     ("Bool" . #X1D539)

     ;; Don't use both as they are ambiguous
     (".." . ("\\.\\." 0 #x2025))
     ("..." . ("\\.\\.\\." 0 #x2026))

     ("<:" . #x22d6))

    (:symbols                ; special strings that have symbol syntax
     (">=" . ("\\_<>=\\_>" 0 #x2265))
     ("<=" . ("\\_<<=\\_>" 0 #x2264)))

    (:c++
     ("!" "!" 0 #xac nil "\\==")
     ;; Handle move constructors
     ;; ("&&" "&&" 0 #x2227 ,(rx (any alnum ?_) point)) ; not on T && x
     ("&&" "&&" 0 #x2227
      ,(lambda ()
         (skip-syntax-backward "-")
         (unless (bolp)
           (eq (get-char-property (1- (point)) 'face) 'font-lock-type-face))))
     ;; handle "while (x --> 0);"
     ("->" "->" 0 #x2192 "-\\=" "\\=>")
     ;; Do not require <> to have symbol syntax
     ;; Handle template arguments like S<T<U>>
     (">>" ">>" 0 #x226b "[[:alnum:]_]\\=")
     ("<<" "<<" 0 #x226a)
     ;; Avoid >>=, <<=
     (">=" ">=" 0 #x2265 ">\\=")
     ("<=" "<=" 0 #x2264 "<\\="))

    (:python
     ("not" "\\_<not\\_>" 0 #xac "\\_<is *\\=")
     ("<<" "<<" 0 #x226a)
     (">>" ">>" 0 #x226b))

    (:haskell       ; many symbols that are useless in other languages
     (".." . ("\\.\\." 0 #x2025))
     ("-<" . ("-<" 0 #x2919))
     (">-" . (">-" 0 #x291a))
     ("<*>" . ("<\\*>" 0 #x229b))
     (">>" . (">>" 0 #X226B ">\\=" "\\=[>=]"))
     ("<<" . ("<<" 0 #X226A "[<=]\\=" "\\=<"))
     (">>=" . (">>=" 0 #X291C))
     ("=<<" . ("=<<" 0 #X291B))
     (">>>" . (">>>" 0 #X22D9))
     ("<<<" . ("<<<" 0 #X22D8))
     ("***" . ("\\*\\*\\*" 0 #X2042))
     ("++" . ("\\+\\+" 0 #X29FA "\\+\\=" "\\=\\+"))
     ("+++" . ("\\+\\+\\+" 0 #X29FB))
     ("|||" . #X2AF4)
     ("elem" . #X2208)
     ("notElem" . #X2209)
     ("union" . #X222A)
     ("intersect" . #X2229)
     ("msum" . #X2295)
     ("Integer" . #X2124)
     ("Ratio Integer" . #X211A)
     ("Double" . #X211D)
     ("Bool" . #X1D539))

    (:julia
     ("!" "!" 0 #xac "\\(?:\\s_\\|\\sw\\)\\=" "\\==")
     ("!=" "!=" 0 #x2262 nil "\\==")
     ("|>" . #x25b7)
     ("function" . #x3bb)
     ("macro" . #x3bc))

    (:greek                     ; All fancy non-English alphabets
     ;; NOTE Gamma and GAMMA would be indistinguishable, so only Gamma
     ;; is supported. Same for all other capital letters.  Capital
     ;; letters such as Alpha that already appear in English do not get
     ;; defaults.
     ("Gamma" . #x393)
     ("Delta" . #x394)
     ("Theta" . #x398)
     ("Lambda" . #x39b)
     ("Xi" . #x39e)
     ("Pi" . #x3a0)
     ("Sigma" . #x3a3)
     ("Upsilon" . #x3d2)
     ("Phi" . #x3a6)
     ("Psi" . #x3a8)
     ("Omega" . #x3a9)
     ("alpha" . #x3b1)
     ("beta" . #x3b2)
     ("gamma" . #x3b3)
     ("delta" . #x3b4)
     ("epsilon" . #x3b5)
     ("zeta" . #x3b6)
     ("eta" . #x3b7)
     ("theta" . #x3b8)
     ("vartheta" . #x3d1)
     ("kappa" . #x3ba)
     ("lambda" . #x3bb)
     ("mu" . #x3bc)
     ("nu" . #x3bd)
     ("xi" . #x3be)
     ("pi" . #x3c0)
     ("varpi" . #x3d6)
     ("rho" . #x3c1)
     ("varrho" . #x3f1)
     ("varsigma" . #x3c2)
     ("sigma" . #x3c3)
     ("tau" . #x3c4)
     ("upsilon" . #x3c5)
     ("phi" . #x3d5)
     ("varphi" . #x3c6)
     ("chi" . #x3c7)
     ("psi" . #x3c8)
     ("omega" . #x3c9)

     ;; NOTE Aren't these backward English letters properly called Cyrillic?
     ;; NOTE Also RussianR will confuse those who expect it to be RussianIA.
     ("RussianR" . #x42f)
     ("RussianZ" . #x417)
     ("RussianZH" . #x416)
     ("RussianE" . #x42d)))
  "An alist that maps strings to replacement rules, of the form

  ((nil (STRING . CHAR-SPEC)
        (STRING . (REGEX MATCH-GROUP CHAR-SPEC))
        (STRING . (REGEX MATCH-GROUP CHAR-SPEC REJECT-BEFORE REJECT-AFTER)))
   (:symbols ...)
   (:c++ ...)
   (:python ...)
   (:greek ...))

The \"nil\" top-level tag corresponds to characters that can be
safely replaced directly without any extra checks. This is
insufficient for some purposes. C++ identifiers might be
surrounded with symbol/word-start/end regexes, and special chars
will be replaced without looking at surroundings.

The \"symbols\" tag corresponds to special symbols that are
considered symbols by their languages, not punctuation. (In C++
\">=\" consists of punctuation.)

CHAR-SPEC can be an integer or anything else (passed to
`compose-region' directly).")

;; }}}
;; {{{ Making default keywords

(defun composite-symbols--default-lookup (label lang)
  (cdr (assoc label
              (cdr (assoc lang composite-symbols-defaults)))))

(defun composite-symbols-from-defaults-noopt (names &optional lang)
  "Return the default keywords for each string in NAMES.

If LANG is non-nil, `composite-symbols-defaults-extra' will also
be examined.

Same as `composite-symbols-from-defaults', but with no attempt at
optimizations."
  (mapcar
   (lambda (n)
     (let ((d (composite-symbols--default-lookup n lang)))
       (unless d (error "Missing default character: %s" n))
       (cond
        ((integerp d)
         (if (string-match-p "[[:alpha:]_][[:alnum:]_]*" n)
             (composite-symbols-keyword (format "\\_<%s\\_>" n) 0 d)
           (composite-symbols-keyword n 0 d)))
        ((and (listp d) (<= 3 (length d)) (<= (length d) 5))
         (composite-symbols-keyword (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d) (nth 4 d)))
        ;; ((and (listp d) (= 3 (length d)))
        ;;  (composite-symbols-keyword (car d) (cadr d) (caddr d)))
        ;; ((and (listp d) (<= 4 (length d)))
        ;;  (composite-symbols-keyword (car d) (cadr d)))
        (t
         (error "Invalid default character: %s" d)))))
   names))

(defun composite-symbols-from-defaults (labels &optional lang word-separators)
  "Return the default keywords for each string in LABELS.

Keywords for multiple labels will be merged if possible, as
WORD-SEPARATORS, which defaults to 'symbols (see
`regexp-opt'). It can also be 'no-parens if no parens should be
asked from `regexp-opt'.

If LANG is non-nil, `composite-symbols-defaults-extra' will also
be examined.

This is the same as `composite-symbols-from-defaults-noopt', but
the different regexps are merged together, so that
`font-lock-keywords' will be shorter."
  (setq word-separators (or word-separators 'symbols))
  (when (eq word-separators 'no-parens) (setq word-separators nil))
  (let (merge-opt merge-rest merge-not merged-regex)
    ;; Check for bad labels that are missing from defaults
    (let ((bad-labels
           (cl-remove-if
            (lambda (lab) (composite-symbols--default-lookup lab lang))
            labels)))
      (when bad-labels
        (error "Missing defaults for labels in lang %s: %s"
               lang (prin1-to-string bad-labels))))
    ;; Merge identifier labels with plain characters together using regexp-opt
    ;; Merge the result with all non-identifier labels with plain characters
    ;; The result is fewer font-lock-keywords
    (setq labels (sort labels 'string<))
    (setq labels (nreverse labels))
    (while labels
      (let* ((lab (car labels))
             (ass (composite-symbols--default-lookup lab lang))
             (is-plain (or (not (consp ass))
                           (and (= 0 (cadr ass))
                                (string-match-p (car ass) lab)
                                (= 3 (length ass))))))
        (cond ((not is-plain) (push lab merge-not))
              ((consp ass) (push (car ass) merge-rest))
              ((string-match-p "[[:alpha:]_][[:alnum:]_]*" lab)
               (push lab merge-opt))
              (t (push lab merge-rest))))
      (setq labels (cdr labels)))
    (when (or merge-opt merge-rest)
      (setq merged-regex
            (concat (when merge-opt (regexp-opt merge-opt word-separators))
                    (when merge-rest
                      (concat (when merge-opt "\\|")
                              (mapconcat 'identity merge-rest "\\|"))))))
    (if merged-regex
        (append
         `((,merged-regex (0 (composite-symbols--compose-default ,lang) prepend)))
         (composite-symbols-from-defaults-noopt merge-not lang))
      (composite-symbols-from-defaults-noopt merge-not lang))))

;; }}}
;; {{{ Default sets of characters

(defvar composite-symbols-member-access
  (list
   (composite-symbols-keyword "\\." 0 #X2219
                   "[^<]\\<[+-]?[0-9]+\\=" "\\=\\([0-9]\\|h.?.?>\\)"))
  "Use ?∙ (#x2219) for member access.
This includes checks to make sure the dot is not part of a number.")

(defvar composite-symbols-low-asterisk
  `(,(composite-symbols-keyword "\\*" 0 #x204e))
  "In some fonts, the low-asterisk looks better for multiplication,
because when asterisk is used for multiplication it sometimes
appears a little too high.")

(defvar composite-symbols-assign-arrow
  ;; (list (composite-symbols-keyword-with-spaces "=" 0 #x27f5))
  (list (composite-symbols-keyword
         "="
         0
         ;; #x2190 ; leftwards arrow
         #x27f5 ; long leftwards arrow
         ;; Reject equals-sign as a possible assignment operator when
         ;; it has anything non-space, non-control to the left or
         ;; right of it.
         "[^[:space:][:cntrl:]]\\="
         "\\=[^[:space:][:cntrl:]]"))
  "Replace assignment operator with left arrow, as in some mathematical notations.")

;; }}}
;; {{{ Greek alphabet characters

(defvar composite-symbols-greek-rules
  (composite-symbols-from-defaults
   '("Gamma"
     "Delta"
     "Theta"
     "Lambda"
     "Xi"
     "Pi"
     "Sigma"
     "Upsilon"
     "Phi"
     "Psi"
     "Omega"
     "alpha"
     "beta"
     "gamma"
     "delta"
     "epsilon"
     "zeta"
     "eta"
     "theta"
     "vartheta"
     "kappa"
     "lambda"
     "mu"
     "nu"
     "xi"
     "pi"
     "varpi"
     "rho"
     "varrho"
     "varsigma"
     "sigma"
     "tau"
     "upsilon"
     "phi"
     "varphi"
     "chi"
     "psi"
     "omega"

     "RussianR"
     "RussianZ"
     "RussianZH"
     "RussianE"
     )
   :greek
   ;; FIXME There is a bug here, see composite-symbols--compose-default
   'no-parens)
  "Greek alphabet, plus four odd Russian letters.")

;; }}}
;; {{{ Alphabet generation function

(defun composite-symbols--char-code (char-name)
  "Return the character code for the given character name CHAR-NAME."
  (cdr (assoc (upcase char-name) (ucs-names))))

(defun composite-symbols-make-alphabet
    (format-string letter-list &optional insert)
  "For given letters, return corresponding entries in `composite-symbols-defaults'.

FORMAT-STRING is a format string that makes Unicode character
names from letter names.

LETTER-LIST is a list of names of letters.

If INSERT is given, the result is not returned but inserted at
point instead for debugging.

Each element of LETTER-LIST is inserted into FORMAT-STRING with
`format', and the result should be a valid unicode character
name, passed to `insert-char'.

Execute these directly to generate greek alphabet character list:

    (composite-symbols-make-alphabet
     \"greek capital letter %s\"
     (mapcar 'symbol-name '(Gamma Delta Theta Lambda Xi Pi Sigma Upsilon Phi Psi Omega))
     t)

    (composite-symbols-make-alphabet
     \"greek small letter %s\"
     (mapcar 'symbol-name
       '(alpha beta gamma delta epsilon zeta eta theta
         vartheta kappa lambda mu nu xi pi varpi rho
         varrho varsigma sigma tau upsilon phi varphi
         chi psi omega))
     t)"
  (mapcar
   (lambda (letter)
     (let* ((letter-string (upcase (format format-string letter)))
            (result (assoc letter-string (ucs-names))))
       (when result
         (setq result (cdr result))
         (if insert
             (insert (format "\n(\"%s\" . #x%x)" letter result))
           `(,letter . ,result)))))
   letter-list))

;; }}}
;; {{{ Default sets of characters (per language type)

(defvar composite-symbols-cc-rules
  (composite-symbols-append
   ;; composite-symbols-logical
   ;; This is tricky because it's a bit too close to the assignment operator.
   ;; (composite-symbols-from-defaults '("=="))
   ;; And "NOT EQUAL TO" is too small and too close to EQUAL TO
   ;; (list (composite-symbols-keyword "!=" 0 #x2260))
   (composite-symbols-from-defaults
    '("!=" "||" "and" "or" "not" "::" "nullptr" "NULL"))
   (composite-symbols-from-defaults
    '("!"  ">=" "<=" "&&" "->" ">>" "<<") :c++)
   composite-symbols-member-access
   ;; composite-symbols-low-asterisk
   )
  "Standard logical, comparison, member-access characters.
Also namespace access, right arrow, nullptr and NULL.")

(defvar composite-symbols-python-rules
  (append
   (composite-symbols-from-defaults
    '("&&" "||" "<=" ">=" "!=" "and" "or" "None" "lambda" "->"))
   (composite-symbols-from-defaults
    '(">>" "<<" "not") :python)
   composite-symbols-member-access
   ;; composite-symbols-low-asterisk
   )
  "Standard logical, comparison, member-access characters.
None is shown as the empty set, but it could also be shown as ⟂.")

(defvar composite-symbols-lisp-rules
  (composite-symbols-from-defaults
   '("/=" "<=" ">=" "and" "not" "or" "lambda"))
  "Standard symbols for lisp.")

(defvar composite-symbols-haskell-rules
  (composite-symbols-from-defaults
   '("-<" ">-" "<*>" ">>=" "=<<" ">>" "<<" ">>>" "<<<" "***" "++" "+++" "|||"
     ".." ; ranges in lists
     "elem" "notElem" "union" "intersect" "msum"
     "Integer" "Ratio Integer" "Double" "Bool")
   :haskell)
  "Lots of characters for `haskell-mode' of varying craziness.

See also the package
hackage.haskell.org/package/base-unicode-symbols because I took
many of these symbols from there.

This conflicts with haskell-mode's own characters, so this should
take precedence.")

(defvar composite-symbols-julia-rules
  (append
   (composite-symbols-from-defaults
    '("..." "::" "<:" "nothing" "Float64" "Int" "Complex" "Bool"
      "&&" "||" "=>"))
   (composite-symbols-from-defaults
    '("<<" ">>" "->" ">=" "<=")
    :c++)
   (composite-symbols-from-defaults
    ;; No "!" because it’s quite overloaded especially by DataFrames.jl
    '("function" "macro" "!=" "|>" "!")
    :julia))
  "Special symbols for julia.")

(defvar composite-symbols-known-rule-names
  '(cc python haskell lisp julia)
  "List of already known rules for comp")

(defvar composite-symbols-js-rules
  (append
   (composite-symbols-from-defaults
    '("||" "&&" "=>"))
   (composite-symbols-from-defaults
    '("!=") :julia)))

(defvar composite-symbols-default-mode-alist
  ;; FIXME This list is *very* incomplete and untested
  `(;; C-style
    (c-mode . composite-symbols-cc-rules)
    (c++-mode . composite-symbols-cc-rules)
    (glsl-mode . composite-symbols-cc-rules)
    (objc-mode . composite-symbols-cc-rules)
    (sh-mode . composite-symbols-cc-rules)
    (perl-mode . composite-symbols-cc-rules)
    (java-mode . composite-symbols-cc-rules)
    (ess-mode . composite-symbols-cc-rules)
    (ruby-mode . composite-symbols-cc-rules)
    (javascript-mode . composite-symbols-cc-rules)
    (js-mode . composite-symbols-js-rules)

    (python-mode . composite-symbols-python-rules)

    (haskell-mode . composite-symbols-haskell-rules)
    (tuareg-mode . composite-symbols-haskell-rules)
    (sml-mode . composite-symbols-haskell-rules)
    (fsharp-mode . composite-symbols-haskell-rules)

    (emacs-lisp-mode . composite-symbols-lisp-rules)
    (scheme-mode . composite-symbols-lisp-rules)
    (lisp-mode . composite-symbols-lisp-rules)
    (clojure-mode . composite-symbols-lisp-rules)

    (julia-mode . composite-symbols-julia-rules))
  "An alist mapping modes to their default sets of composite symbol rules.")

;; }}}
;; {{{ User-specified keywords

(defun composite-symbols--compile-user (kw-list)
  (let (kw-defaults kw-user)
    (dolist (kw kw-list)
      (if (composite-symbols--default-lookup kw nil)
          (push kw kw-defaults)
        (push kw kw-user)))
    (append
     (composite-symbols-from-defaults kw-defaults)
     (mapcar
      (lambda (spec)
        (cond
         ((stringp spec)
          (composite-symbols-from-defaults (list spec)))
         ((integerp (cdr spec))
          (composite-symbols-keyword (car spec) 0 (cdr spec)))
         (t
          (let ((match (nth 1 spec))
                (match-group (nth 2 spec))
                (unicode-hex (nth 3 spec))
                (reject-before (nth 4 spec))
                (reject-after (nth 5 spec)))
            (composite-symbols-keyword
             match match-group unicode-hex reject-before reject-after)))))
      kw-user))))

;; }}}
;; {{{ Mode definition

(defun composite-symbols--enable (kw-list)
  "Enable composition of symbols using the given keyword list.

KW-LIST is passed to `font-lock-add-keywords'."
  ;; We don't remove 'composition from this list down below
  ;; because we don't know if anything else is using it too.
  (add-to-list 'font-lock-extra-managed-props 'composition)
  (font-lock-add-keywords nil kw-list 'append)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    ;; font-lock-fontify-buffer shouldn't be used on later versions
    (with-no-warnings (font-lock-fontify-buffer))))

(defun composite-symbols--disable (kw-list)
  "Disable composition of symbols using the given keyword list.

KW-LIST is passed to `font-lock-remove-keywords'."
  (font-lock-remove-keywords nil kw-list)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer))))

;;;###autoload
(define-minor-mode composite-symbols-mode
  "This mode replaces some special symbols with corresponding unicode
characters. For example, it will replace \"<=\" with <LESS-THAN> '<',
and \"None\" with <EMPTY SET> '∅' (with default settings, which can
be modified).

Notes:

- `composite-symbols-ignore-indentation' controls whether the mode
  is allowed to \"break\" indentation by changing lengths of lines.

- There is a fair amount of variability in what you might prefer
  to see from this package.  See definitions of variables like
  `composite-symbols-cc-rules' to see how to define your own sets
  of composite symbol replacements.

- Many of these characters might be missing from your default
  fonts.  It may be helpful to install the package `unicode-fonts',
  or use set-fontset-font to update which font Emacs uses to
  display which characters (other, non-default fonts might have the
  missing characters).

  The package unicode-fonts includes helpful suggestions about
  which extra fonts might need to be installed.

- When defining new composite symbols, the mode
  `show-unicode-minor-mode' is quite helpful as it shows next to
  every hexadecimal literal the corresponding Unicode character.

- This (programming-language-agnostic) package derives from the
  code that haskell-mode uses to fontify haskell code."
  :group 'composite-symbols
  (let ((user-kw (cdr (assq major-mode composite-symbols-user-alist)))
        default-kw
        kw)
    (let ((check-mode major-mode))
      (while (and check-mode (not default-kw))
        (setq
         default-kw (cdr (assq check-mode composite-symbols-default-mode-alist))
         check-mode (get check-mode 'derived-mode-parent))))
    (when (and user-kw (listp user-kw))
      (setq user-kw (composite-symbols--compile-user user-kw))
      (when composite-symbols-mode
        (message "User keywords: %s" (prin1-to-string user-kw))))
    (setq kw (or user-kw default-kw))
    (when (symbolp kw) (setq kw (symbol-value kw)))
    (when (and (not kw) composite-symbols-mode (called-interactively-p 'interactive))
      (let* ((presets (mapcar #'symbol-name composite-symbols-known-rule-names))
             (s (ido-completing-read "Select composite-symbols rule: " presets nil t)))
        (setq kw (symbol-value (intern (concat "composite-symbols-" s "-rules"))))))
    (when (not kw)
      ;; ;; Warn in case this is a misconfiguration problem.
      ;; (when (and major-mode
      ;;          (get major-mode 'derived-mode-parent))
      ;;   (message "Composite-symbols: mode %s is not known." major-mode))
      (setq-local composite-symbols-mode nil))
    (cond
     (composite-symbols-mode
      (composite-symbols--enable kw))
     (t
      (composite-symbols--disable kw)))))

;;;###autoload
(define-minor-mode composite-symbols-assign-arrow-mode
  "Replace the assignment operators in C++ with left arrows."
  :group 'composite-symbols
  (if composite-symbols-assign-arrow-mode
      (composite-symbols--enable composite-symbols-assign-arrow)
    (composite-symbols--disable composite-symbols-assign-arrow)))

;;;###autoload
(define-minor-mode composite-symbols-greek-mode
  "Similar to `composite-symbols-mode', this mode displays all
words that are spelled as greek letters with the corresponding
letters."
  :group 'composite-symbols
  (cond
   (composite-symbols-greek-mode
    (composite-symbols--enable composite-symbols-greek-rules))
   (t
    (composite-symbols--disable composite-symbols-greek-rules))))

;; }}}
;; {{{ Show unicode mode

(defun show-unicode-hex--hook (&optional start end _prior-length)
  "Rescan a given portion of buffer for changes to hex sequences.

For meaning of START, END, _PRIOR-LENGTH, see `after-change-functions'."
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  ;; Need to include full matches when start, end don't overlap it fully.
  (setq end (save-excursion (goto-char end) (line-end-position))
        start (save-excursion (goto-char start) (line-beginning-position)))
  (remove-overlays start end 'category 'show-unicode-hex-mode)
  (save-match-data
    (save-excursion
      (let ((hex-regex
             ;; (rx "#" (group "x" (group (>= 2 (char hex-digit)))))
             "#\\(x\\([[:xdigit:]]\\{2,\\}\\)\\)")
            o char-string)
        (goto-char start)
        (while (and (< (point) end)
                    (search-forward-regexp hex-regex end t nil))
          (setq
           o (make-overlay (match-beginning 1) (match-beginning 1) nil t t)
           char-string
           (propertize
            (format "[%c]" (string-to-number (match-string 2) 16))
            'face (get 'show-unicode-hex-mode 'face)))
          ;; (message "Found: %s (%s)" (match-string 1) char-string)
          (overlay-put o 'category 'show-unicode-hex-mode)
          (overlay-put o 'before-string char-string))))))

;;;###autoload
(define-minor-mode show-unicode-hex-mode
  "Display characters next to literal hexadecimal values.


For every hex number such as #x2140, display the corresponding
unicode character as #[C]x????. The display is \"immaterial\" and
nothing in text is actually modified.

Notes:

- It is recommended to install the package unicode-fonts
  (github.com/rolandwalker/unicode-fonts), if you are going to be
  using many uncommon Unicode symbols."
  :group 'composite-symbols
  ;; Default overlay properties
  (put 'show-unicode-hex-mode 'evaporate t)
  (put 'show-unicode-hex-mode 'intangible t)
  (put 'show-unicode-hex-mode 'face 'font-lock-string-face)
  (cond
   (show-unicode-hex-mode
    (show-unicode-hex--hook)
    (add-hook 'after-change-functions 'show-unicode-hex--hook t t))
   (t
    (remove-overlays (point-min) (point-max) 'category 'show-unicode-hex-mode)
    (remove-hook 'after-change-functions 'show-unicode-hex--hook))))

;; }}}

(provide 'composite-symbols)
;; Local Variables:
;; eval: (when (fboundp 'show-unicode-hex-mode) (show-unicode-hex-mode))
;; byte-compile-warnings: (not cl-functions)
;; coding: utf-8-unix
;; End:
;;; composite-symbols.el ends here
