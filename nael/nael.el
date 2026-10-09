;;; nael.el --- Major mode for Lean  -*- lexical-binding: t; -*-

;; Copyright © 2013-2014 Microsoft Corp.
;; Copyright © 2014-2015 Soonho Kong
;; Copyright © 2024 Free Software Foundation, Inc.
;; Copyright © 2025 Mekeor Melire

;; Author: Adam Topaz <topaz@ualberta.ca>
;;         Akira Komamura <akira.komamura@gmail.com>
;;         Bao Zhiyuan <bzy_sustech@foxmail.com>
;;         Daniel Selsam <daniel.selsam@protonmail.com>
;;         Gabriel Ebner <gebner@gebner.org>
;;         Henrik Böving <hargonix@gmail.com>
;;         Hongyu Ouyang <oyhy0214@163.com>
;;         Jakub Bartczuk <bartczukkuba@gmail.com>
;;         Leonardo de Moura <leonardo@microsoft.com>
;;         Mauricio Collares <mauricio@collares.org>
;;         Mekeor Melire <mekeor@posteo.de>
;;         Philip Kaludercic <philipk@posteo.net>
;;         Richard Copley <buster@buster.me.uk>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;;         Siddharth Bhat <siddu.druid@gmail.com>
;;         Simon Hudon <simon.hudon@gmail.com>
;;         Soonho Kong <soonhok@cs.cmu.edu>
;;         Tomáš Skřivan <skrivantomas@seznam.cz>
;;         Wojciech Nawrocki <wjnawrocki@protonmail.com>
;;         Yael Dillies <yael.dillies@gmail.com>
;;         Yury G. Kudryashov <urkud@urkud.name>
;; Keywords: languages
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: Apache-2.0 AND GPL-3.0-only
;; URL: https://codeberg.org/mekeor/nael
;; Version: 0.9.0

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.  To be precise, it is licensed under Apache-2.0,
;; see LICENSE.APACHE2, and sublicensed under GPL3.

;;; Commentary:

;; `nael-mode' is a major mode for the Lean language.  It is based on
;; Emacs' built-in tree-sitter support and thus requires the Lean
;; grammar `tree-sitter-lean' to be installed.  Nael offers to install
;; it for you; see `nael-grammar-recipe'.  For more information, read
;; the manual:
;;
;; - As Org file `README.org' in the `nael' folder or repository.
;;
;; - After installation, as local Info manual per C-h i d nael RET or
;;   (info "(nael) Top")
;;
;; - As HTML webpage at <https://codeberg.org/mekeor/nael>.

;;; Code:

(require 'jsonrpc)
(require 'project)
(require 'rx)
(require 'seq)
(require 'treesit)

;; Avoid strict loading of optional features, files or dependencies.
(defvar lsp-managed-mode-hook)
(declare-function flymake-goto-next-error "flymake"
                  (&optional n filter interactive))
(declare-function lsp "lsp-mode" (&optional arg))
(declare-function nael-abbrev-configure "nael-abbrev" ())
(declare-function nael-abbrev-help "nael-abbrev" (&optional beg end))
(declare-function nael-eglot-configure-when-initialized "nael-eglot"
                  (_))
(declare-function nael-eglot-configure-when-managed "nael-eglot" ())
(declare-function nael-lsp-configure-when-managed "nael-lsp" ())

;; `treesit-ensure-installed' and `treesit-outline-predicate' were
;; introduced in Emacs 31.  Both are used guardedly.
(declare-function treesit-ensure-installed "treesit" (language))
(defvar treesit-outline-predicate)

(defgroup nael nil
  "Major mode for Lean."
  :group 'languages
  :link '(emacs-library-link :tag "Source Lisp File" "nael.el")
  :link '(url-link :tag "Website" "https://codeberg.org/mekeor/nael")
  :prefix "nael-"
  :tag "Nael")

;;;; Grammar:

;; (defcustom nael-grammar-recipe
;;   '(lean "https://github.com/Julian/tree-sitter-lean"
;;          :commit "916c44f4d89c970f66dd34a49e7ce848f4d29e72")
;;   "Recipe for installing the Lean tree-sitter grammar.

;; Nael appends this to `treesit-language-source-alist', so an entry you
;; have added there yourself takes precedence.  The revision is pinned on
;; purpose: tree-sitter grammars rename and restructure nodes between
;; releases, which silently breaks the font-lock and indentation queries
;; below.  See `treesit-language-source-alist' for the format."
;;   :type '(cons symbol (plist :value-type string))
;;   :group 'nael)

;; (add-to-list 'treesit-language-source-alist nael-grammar-recipe t)

(defconst nael--grammar-probe
  (concat
   ;; Declarations and their name fields.
   "(declaration (theorem name: (identifier))) "
   ;; Lean 4 module system, added in tree-sitter-lean 0.2.
   "(moduleTk) (public_section) "
   ;; Indentation-sensitive blocks, which rely on the external scanner.
   "(where_decls) (do_block) (interpolation)")
  "Query exercising grammar features that Nael's queries depend upon.

Compiling it is a cheap way to tell a compatible grammar from an
incompatible one, which otherwise manifests as font-lock and
indentation quietly doing nothing.")

(defun nael-grammar-ready-p ()
  "Return non-nil when a usable Lean tree-sitter grammar is installed.

Offer to install the grammar per `nael-grammar-recipe' if it is missing,
and warn if the installed grammar is too old for Nael's queries."
  (and (if (fboundp 'treesit-ensure-installed)
           (treesit-ensure-installed 'lean)
         ;; Emacs 30 has no `treesit-ensure-installed'.
         (or (treesit-ready-p 'lean t)
             (and (y-or-n-p "Install the Lean tree-sitter grammar? ")
                  (progn (treesit-install-language-grammar 'lean)
                         (treesit-ready-p 'lean t)))))
       ;; `treesit-ready-p' also rejects buffers that are too large.
       (treesit-ready-p 'lean)
       (or (treesit-query-valid-p 'lean nael--grammar-probe)
           (ignore
            (lwarn 'nael :warning "\
The installed Lean tree-sitter grammar is incompatible with Nael.  \
Reinstall it with M-x treesit-install-language-grammar RET lean RET.")))))

;;;; Syntax:

(defvar nael-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Parentheses:
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments:
    (modify-syntax-entry ?/  ". 14nb" table)
    (modify-syntax-entry ?-  ". 123"  table)
    (modify-syntax-entry ?\n ">"      table)

    ;; Words:
    (mapc
     (lambda (character) (modify-syntax-entry character "w" table))
     '(;; Latin characters:
       ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u
       ?v ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
       ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ;; Digits
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ;; Greek (and Coptic) characters:
       ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;; ?λ
       ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ ?φ ?χ ?ψ ?ω ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ
       ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ ?Ϥ ?ϥ
       ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ
       ?ϻ ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ
       ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ ?ἦ ?ἧ ?Ἠ
       ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ
       ?Ἶ ?Ἷ ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ
       ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ
       ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
       ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ
       ?ᾒ ?ᾓ ?ᾔ ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ ?ᾣ ?ᾤ ?ᾥ ?ᾦ
       ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά
       ?ᾼ ?᾽ ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ
       ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
       ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ
       ?Ώ ?ῼ ?´ ?῾
       ;; Mathematical characters:
       ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔
       ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩
       ?K ?Å ?ℬ ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻ ?ℼ ?ℽ ?ℾ
       ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊ ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏
       ;; Subscripts:
       ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀ ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ
       ?ₛ ?ₜ ?' ?_ ?! ??))

    ;; Escaped identifiers, e.g. «foo bar», are symbols, not comments.
    ;; Tree-sitter lexes them as part of an `identifier' node.
    (modify-syntax-entry ?« "_" table)
    (modify-syntax-entry ?» "_" table)

    ;; Operators:
    (mapc
     (lambda (character) (modify-syntax-entry character "." table))
     '(?# ?$ ?% ?& ?* ?+ ?< ?= ?> ?@ ?^ ?| ?~ ?:))

    ;; Whitespace:
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)

    ;; Strings:
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "/"  table)

    table)
  "Syntax table used in `nael-mode'.

Tree-sitter does the parsing, but Emacs still consults the syntax table
for `syntax-ppss', paragraph filling, `comment-dwim' and friends.")

;;;; Tokens:

;; The following lists enumerate every anonymous token of the Lean
;; tree-sitter grammar, partitioned by role.  They are kept exhaustive
;; on purpose: a token that belongs to no list is a token that never
;; gets fontified.  To regenerate them, partition the entries of
;; `src/node-types.json' whose `named' property is false.

(defconst nael--keywords
  '("abbrev" "add_decl_doc" "alias" "assert_exists"
    "assert_not_exists" "assert_not_imported" "attribute" "axiom"
    "behavior" "builtin_initialize" "by" "by_cases" "case" "cases"
    "check_assertions" "class" "constant" "declare_syntax_cat" "def"
    "deriving" "do" "elab_rules" "else" "end" "example" "exists"
    "export" "extends" "for" "forall" "from" "fun" "generalizing"
    "grind_pattern" "have" "haveI" "if" "import" "in" "induction"
    "inductive" "infix" "infixl" "infixr" "initialize"
    "initialize_simps_projections" "instance" "lemma" "let" "letI"
    "library_note" "local" "macro" "macro_rules" "match" "mut" "name"
    "namespace" "noncomputable" "nonrec" "notation" "obtain" "opaque"
    "open" "partial" "postfix" "prefix" "priority" "private"
    "protected" "rec" "recommended_spelling" "register_builtin_option"
    "register_error_explanation" "register_option" "scoped" "section"
    "set" "set_option" "show" "structure" "suffices" "syntax" "then"
    "theorem" "to_dual_insert_cast" "to_dual_insert_cast_fun"
    "unif_hint" "universe" "universes" "unsafe" "using" "variable"
    "where" "with" "Π" "Σ" "Σ'" "λ" "∀" "∀ᵐ" "∃" "∃ᵐ" "∏" "∐" "∑" "∫"
    "∫⁻" "⋀" "⋁" "⋂" "⋃" "⨁" "⨂" "⨅" "⨆" "⨍")
  "Keyword tokens of the Lean tree-sitter grammar.

This includes binder heads such as `∀' and the big-operator notations,
which Lean spells as prefix keywords rather than operators.")

(defconst nael--directives
  '("#check" "#check_failure" "#eval" "#exit" "#print"
    "#print_axioms" "#reduce" "#synth" "#version")
  "Tokens of the Lean tree-sitter grammar that start a `#'-command.")

(defconst nael--operators
  '("!" "!=" "#" "$" "%" "&" "&&" "&&&" "''" "*" "+" "++" "+ᵥ" ",*"
    ",+" ",?" "-" "->" "-ᵥ" "/" "//" ":" "::" "::ᵣ" "::ₘ" ":=" "<"
    "<$>" "<&&>" "<&>" "<*>" "<-" "<->" "<;>" "<<" "<<<" "<=" "<|"
    "<|>" "<||>" "=" "=<<" "==" "=>" ">" ">=" ">>" ">>=" ">>>" "?" "@"
    "@&" "\\" "^" "^^^" "|>" "||" "|||" "~" "¬" "×" "×'" "×ˢ" "×ₗ" "ˣ"
    "ᵀ" "ᵐᵒᵖ" "ᵒᵈ" "ᵒᵖ" "ᶜ" "†" "•" "⁺" "⁻¹" "⁻¹'" "←" "↑" "→" "→*"
    "→+" "→+*" "→ᵃ" "→ₐ" "→ₑ" "→ₑ*" "→ₑ+" "→ₒ" "→ₗ" "→ₙ*" "→ₙ+"
    "→ₙ+*" "↓" "↔" "↠" "↥" "↦" "↪" "↪*" "↪+" "↪+*" "↪o" "↾" "⇑" "⇒"
    "⇔" "⇨" "∂" "∆" "∈" "∉" "∗" "∘" "∘ₗ" "∙" "∣" "∤" "∧" "∨" "∩" "∪"
    "≃" "≃*" "≃+" "≃+*" "≃ᵃ" "≃ᵤ" "≃ₐ" "≃ₒ" "≃ₗ" "≃ₘ" "≅" "≈" "≉" "≌"
    "≍" "≠" "≡" "≢" "≤" "≥" "≪≫" "≫" "≺" "≼" "⊂" "⊃" "⊆" "⊇" "⊑" "⊒"
    "⊓" "⊔" "⊕" "⊕ₗ" "⊗" "⊗ₖ" "⊗ₛ" "⊗ₜ" "⊙" "⊚" "⊛" "⊞" "⊠" "⊣" "⊨"
    "⊻" "⊼" "⋆" "⋖" "⋗" "⋙" "▸" "◁" "◫" "✝" "⟶" "⟹" "⥤" "⨯" "⬝" "⬝ᵥ"
    "￢")
  "Operator tokens of the Lean tree-sitter grammar.")

(defconst nael--brackets
  '("#[" "(" ")" ".{" "@[" "[" "]" "{" "{{" "}" "}}" "‹" "›" "⌈" "⌉"
    "⌉₊" "⌊" "⌋" "⌋₊" "⟦" "⟧" "⟨" "⟩" "⟪" "⟫" "⦃" "⦄")
  "Bracket tokens of the Lean tree-sitter grammar.")

(defconst nael--delimiters
  '("," "." "..." ";" "|")
  "Delimiter tokens of the Lean tree-sitter grammar.")

;;;; Node types:

(defconst nael--declaration-regexp
  (rx bos (or "abbrev" "axiom" "constant" "def" "example" "inductive"
              "instance" "opaque" "structure" "theorem")
      eos)
  "Regular expression matching nodes that declare a Lean constant.

These are the children of a `declaration' node; the `declaration' itself
additionally carries attributes and modifiers.")

(defconst nael--block-regexp
  (rx bos (or "by" "do_block") eos)
  "Regular expression matching indentation-delimited block nodes.")

(defconst nael--wrapper-regexp
  (rx bos (or "open" "set_option") eos)
  "Regular expression matching commands that take another command as body.

These are the `in' forms, as in `set_option … in' and `open … in', whose
body is a command in its own right and stays in the column of the
command that qualifies it:

  set_option maxHeartbeats 1000000 in
  theorem foo : True := trivial")

(defconst nael--where-regexp
  (rx bos (or "where_struct" "where_decls") eos)
  "Regular expression matching `where'-clause nodes.")

(defconst nael--bracket-node-regexp
  (rx bos (or "anon_ctor" "anon_ctor_binder" "anon_subterm" "array_lit"
              "attributes" "ceil_lit" "dbl_angle_lit" "explicit_binder"
              "floor_lit" "implicit_binder" "instance_binder"
              "interpolation" "list_lit" "norm_lit" "paren"
              "quotation" "quotient_lit" "range_lit" "set_builder"
              "strict_implicit_binder" "struct_lit" "subtype_lit"
              "tuple_binder")
      eos)
  "Regular expression matching nodes delimited by a pair of brackets.")

(defconst nael--continuation-regexp
  (rx bos (or "app" "binary_op" "unary_op") eos)
  "Regular expression matching nodes that may continue onto a new line.")

(defconst nael--closer-regexp
  (rx bos (any "])}" "⌉" "⌋" "⟧" "⟩" "⟫" "⦄" "›") eos)
  "Regular expression matching closing-bracket tokens.

The two-character closers `}}', `⌉₊' and `⌋₊' are matched by their first
character, which is all `node-is' ever looks at.")

(defconst nael--text-regexp
  (rx bos (or "block_comment" "doc_comment" "interpolated_str"
              "line_comment" "module_doc_comment" "raw_string"
              "str_lit")
      eos)
  "Regular expression matching nodes whose content is prose or text.")

;;;; Font-Lock:

(defvar nael--font-lock-settings
  (treesit-font-lock-rules
   :language 'lean
   :feature 'comment
   '([(line_comment) (block_comment)] @font-lock-comment-face
     [(doc_comment) (module_doc_comment)] @font-lock-doc-face)

   :language 'lean
   :feature 'definition
   '(;; Declarations of functions, theorems and other constants.
     (abbrev name: (identifier) @font-lock-function-name-face)
     (alias_cmd name: (identifier) @font-lock-function-name-face)
     (axiom name: (identifier) @font-lock-function-name-face)
     (constant name: (identifier) @font-lock-function-name-face)
     (def name: (identifier) @font-lock-function-name-face)
     (instance name: (identifier) @font-lock-function-name-face)
     (opaque name: (identifier) @font-lock-function-name-face)
     (syntax_cmd name: (identifier) @font-lock-function-name-face)
     (theorem name: (identifier) @font-lock-function-name-face)
     (where_aux_def name: (identifier) @font-lock-function-name-face)

     ;; Declarations of types and their constructors.
     (ctor name: (identifier) @font-lock-type-face)
     (ctor_alt name: (identifier) @font-lock-type-face)
     (declare_syntax_cat_cmd name: (identifier) @font-lock-type-face)
     (inductive name: (identifier) @font-lock-type-face)
     (structure name: (identifier) @font-lock-type-face)

     ;; Structure fields and their instantiations.
     (field name: (identifier) @font-lock-property-name-face)
     (struct_field name: (identifier) @font-lock-property-name-face)

     ;; Namespaces, sections and modules.
     (end name: (identifier) @font-lock-type-face)
     (export namespace: (identifier) @font-lock-type-face)
     (import name: (identifier) @font-lock-type-face)
     (namespace name: (identifier) @font-lock-type-face)
     (notation_decl_cmd namespace: (identifier) @font-lock-type-face)
     (open namespace: (identifier) @font-lock-type-face)
     (open scoped: (identifier) @font-lock-type-face)
     (section name: (identifier) @font-lock-type-face)

     ;; Bindings of local variables, hypotheses and parameters.
     (anon_ctor_binder (identifier) @font-lock-variable-name-face)
     (binders (identifier) @font-lock-variable-name-face)
     (block_assign name: (identifier) @font-lock-variable-name-face)
     (by_cases name: (identifier) @font-lock-variable-name-face)
     (explicit_binder name: (identifier)
                      @font-lock-variable-name-face)
     (have name: (identifier) @font-lock-variable-name-face)
     (implicit_binder name: (identifier)
                      @font-lock-variable-name-face)
     (instance_binder name: (identifier)
                      @font-lock-variable-name-face)
     (let name: (identifier) @font-lock-variable-name-face)
     (register_cmd name: (identifier) @font-lock-variable-name-face)
     (set_option name: (identifier) @font-lock-variable-name-face)
     (strict_implicit_binder name: (identifier)
                             @font-lock-variable-name-face)
     (suffices name: (identifier) @font-lock-variable-name-face)
     (tuple_binder (identifier) @font-lock-variable-name-face)
     (universe name: (identifier) @font-lock-variable-name-face)

     ;; Goals selected by the `case' tactic.
     (tactic_case name: (identifier) @font-lock-constant-face))

   :language 'lean
   :feature 'keyword
   `([,@nael--keywords] @font-lock-keyword-face

     ;; Keywords that the grammar wraps in a same-named rule.
     [(all) (meta) (moduleTk) (mutual) (prelude) (public)]
     @font-lock-keyword-face

     ;; `sorry' and `admit' leave a hole in a proof.
     (sorry) @font-lock-warning-face

     ;; Escape hatches that abort or trace at run-time.  These are
     ;; ordinary identifiers as far as the grammar is concerned.
     ((identifier) @font-lock-warning-face
      (:match ,(rx bos (or "assert" "dbgTrace" "panic" "unreachable"))
              @font-lock-warning-face)))

   :language 'lean
   :feature 'preprocessor
   `([,@nael--directives] @font-lock-preprocessor-face
     (attributes name: (identifier) @font-lock-preprocessor-face)
     (attribute_cmd name: (identifier) @font-lock-preprocessor-face))

   :language 'lean
   :feature 'string
   '([(interpolated_str) (raw_string) (str_lit)]
     @font-lock-string-face)

   :language 'lean
   :feature 'escape-sequence
   ;; `:override' is required: the enclosing string literal has already
   ;; been fontified by the `string' feature above.
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'lean
   :feature 'string-interpolation
   :override t
   '((interpolation ["{" "}"] @font-lock-misc-punctuation-face)
     (interpolation (identifier) @font-lock-variable-use-face))

   :language 'lean
   :feature 'number
   '([(num_lit) (scientific_lit)] @font-lock-number-face
     (proj field: (num_lit) @font-lock-number-face))

   :language 'lean
   :feature 'constant
   '([(bot_const) (char_lit) (empty_const) (false_const) (goal_const)
      (name_lit) (top_const) (true_const)]
     @font-lock-constant-face
     (dot_ident name: (identifier) @font-lock-constant-face))

   :language 'lean
   :feature 'builtin
   '([(antiquot) (cdot) (hole) (named_hole) (rest_pat) (star)
      (synth_hole)]
     @font-lock-builtin-face)

   :language 'lean
   :feature 'type
   '([(prop_const) (sort_const) (type_const)] @font-lock-type-face

     ;; Ascribed types of binders, fields and value declarations.  A
     ;; theorem's "type" is a proposition rather than a type, so those
     ;; are deliberately left out.
     (abbrev type: (identifier) @font-lock-type-face)
     (constant type: (identifier) @font-lock-type-face)
     (ctor_alt type: (identifier) @font-lock-type-face)
     (def type: (identifier) @font-lock-type-face)
     (explicit_binder type: (identifier) @font-lock-type-face)
     (field type: (identifier) @font-lock-type-face)
     (implicit_binder type: (identifier) @font-lock-type-face)
     (instance_binder type: (identifier) @font-lock-type-face)
     (instance_binder type: (app fn: (identifier)
                                 @font-lock-type-face))
     (opaque type: (identifier) @font-lock-type-face)
     (strict_implicit_binder type: (identifier)
                             @font-lock-type-face)

     ;; Type classes named by `deriving' and universe levels.
     (deriving_cmd class: (identifier) @font-lock-type-face)
     (universe_app level: (identifier) @font-lock-type-face))

   :language 'lean
   :feature 'function
   '((app fn: (identifier) @font-lock-function-call-face)
     (app fn: (proj field: (identifier)
                    @font-lock-function-call-face))
     (attribute_cmd target: (identifier)
                    @font-lock-function-call-face)
     (deriving_cmd target: (identifier) @font-lock-type-face)
     (export only: (identifier) @font-lock-function-call-face)
     (grind_pattern_cmd name: (identifier)
                        @font-lock-function-call-face)
     (induction elim: (identifier) @font-lock-function-call-face)
     (open only: (identifier) @font-lock-function-call-face))

   :language 'lean
   :feature 'property
   '((paren arg_name: (identifier) @font-lock-property-use-face)
     (proj field: (identifier) @font-lock-property-use-face))

   :language 'lean
   :feature 'variable
   '((identifier) @font-lock-variable-use-face)

   :language 'lean
   :feature 'operator
   `([,@nael--operators] @font-lock-operator-face)

   :language 'lean
   :feature 'bracket
   `([,@nael--brackets] @font-lock-bracket-face)

   :language 'lean
   :feature 'delimiter
   `([,@nael--delimiters] @font-lock-delimiter-face))
  "`treesit-font-lock-settings' for `nael-mode'.")

(defvar nael-font-lock-feature-list
  '((comment definition)
    (keyword preprocessor string)
    (builtin constant escape-sequence number string-interpolation
             type)
    (bracket delimiter function operator property variable))
  "`treesit-font-lock-feature-list' for `nael-mode'.

With the default `treesit-font-lock-level' of 3, the features of the
fourth list are disabled.  Enable or disable individual features with
`treesit-font-lock-recompute-features'.")

;;;; Indentation:

(defcustom nael-indent-offset 2
  "Number of spaces for each indentation step in `nael-mode'."
  :type 'natnum
  :safe #'natnump
  :group 'nael)

(defcustom nael-indent-signature-offset 4
  "Number of spaces to indent a declaration's signature by, when wrapped.

Lean's style guide indents the continuation of a wrapped signature twice
as far as the declaration's body, so that the two can be told apart:

  theorem foo (n : Nat) :
      0 < n + 1 := by
    omega

Set this to the value of `nael-indent-offset' to indent both alike."
  :type 'natnum
  :safe #'natnump
  :group 'nael)

(defconst nael--opener-regexp
  (rx (or (seq symbol-start
               (or "by" "do" "else" "from" "then" "where" "with")
               symbol-end)
          "," "(" "[" "{" ":=" "<-" "=>" "←" "↦" "⟨"))
  "Regular expression matching tokens that open a continuation line.

Used by `nael--indent-fallback' only, hence the deliberately small set:
these are the line endings after which Lean code is indented further.")

(defun nael--indent-keep (&rest _)
  "Return nil, which leaves the indentation of the current line alone.

Tree-sitter skips a line whose offset is nil.  This is the right answer
whenever the parse tree cannot tell a genuine continuation line from a
statement that the grammar merely failed to recognize: Lean's whitespace
is significant, so guessing wrong changes what the code means."
  nil)

(defun nael--indent-signature-p (node parent &rest _)
  "Return non-nil if NODE continues the signature of declaration PARENT."
  (and (equal (treesit-node-field-name node) "type")
       (string-match-p nael--declaration-regexp
                       (or (treesit-node-type parent) ""))))

(defun nael--indent-text-p (node parent bol &rest _)
  "Return non-nil if the line at BOL continues a comment or a string.

A line in the middle of a multi-line comment or string literal has no
NODE of its own, since the enclosing token started earlier; in that case
PARENT is the token itself."
  (let ((node (or node parent (treesit-node-at bol))))
    (and node
         (string-match-p nael--text-regexp (treesit-node-type node))
         (< (treesit-node-start node) bol))))

(defun nael--indent-blank-p (_node _parent bol &rest _)
  "Return non-nil if the line at BOL holds nothing but whitespace."
  (save-excursion
    (goto-char bol)
    (looking-at-p "[[:blank:]]*$")))

(defconst nael--indent-island-regexp
  (rx bos (or "declaration" "module") eos)
  "Regular expression matching nodes that bound the search for a parse error.

A `declaration' is parsed independently of what surrounds it, so an error
outside one says nothing about the structure within it.  Treating these
nodes as islands is what keeps a single unparsable construct from
disabling indentation for a whole file; see `nael--indent-error-p'.")

(defun nael--indent-error-p (node parent bol &rest _)
  "Return non-nil if the line at BOL lies in a region the parser rejected.

The Lean grammar covers the language approximately — `for' and `while'
loops, for example, are not modeled at all — so stretches of perfectly
valid Lean end up under an ERROR node.  There is no tree to reason about
there, and a wrong guess would cascade down the rest of the buffer, so
such lines are left exactly as their author wrote them.

Only errors that actually enclose the line count.  The search stops at
the nearest node of `nael--indent-island-regexp', because tree-sitter
recovers from an error by widening the node that contains it, often as
far as the whole buffer: one unmodelled `for' loop in a 10000-line file
otherwise leaves every line in it, declarations included, untouched."
  (unless (nael--indent-blank-p node parent bol)
    (let ((node (or node parent))
          (error-node nil))
      (while (and node
                  (not error-node)
                  (not (string-match-p nael--indent-island-regexp
                                       (treesit-node-type node))))
        (if (equal (treesit-node-type node) "ERROR")
            (setq error-node node)
          (setq node (treesit-node-parent node))))
      error-node)))

(defun nael--indent-toplevel-p (_node parent &rest _)
  "Return non-nil if PARENT encloses the current line at the top level.

That is the case when PARENT is the root of the parse tree, which is
normally a `module'.  A parse error anywhere in the buffer can make the
root an ERROR instead, and can nest further ERROR nodes beneath it, so
ERROR nodes on the way up to the root are stepped over: a command sits in
column zero no matter how badly the text around it parsed."
  (let ((node parent))
    (while (and node
                (treesit-node-parent node)
                (equal (treesit-node-type node) "ERROR"))
      (setq node (treesit-node-parent node)))
    (and node (null (treesit-node-parent node)))))

(defun nael--indent-damaged-p (_node parent &rest _)
  "Return non-nil if PARENT, the construct around the current line, is broken.

A construct with a parse error directly inside it has no structure left
to indent from, because its remaining parts may well have been attached
to the wrong node.  Leave such lines alone, just as `nael--indent-error-p'
does with the error itself."
  (seq-find (lambda (child)
              (or (equal (treesit-node-type child) "ERROR")
                  (treesit-node-check child 'missing)))
            (treesit-node-children parent)))

(defun nael--indent-fallback (_node _parent bol &rest _)
  "Return the column to indent the line at BOL to, ignoring the parse tree.

Tree-sitter cannot help on an empty line, since there is no node to
anchor against.  Continue the indentation of the previous non-empty
line instead, one step further if that line ends in an opening token."
  (save-excursion
    (goto-char bol)
    (while (and (zerop (forward-line -1))
                (looking-at-p "[[:blank:]]*$")))
    (back-to-indentation)
    (let ((indent (current-column)))
      (goto-char (line-end-position))
      (skip-chars-backward "[:blank:]")
      (if (looking-back nael--opener-regexp
                       (line-beginning-position))
          (+ indent nael-indent-offset)
        indent))))

(defconst nael--statement-parent-regexp
  (rx bos (or "app" "by" "declaration" "do_block" "match" "match_alt"
              "module" "struct_lit" "tactic_case" "where_decls"
              "where_struct")
      eos)
  "Regular expression matching nodes whose children can begin a statement.

A child of such a node that opens a line of its own begins something
new — a command, a structure field, a tactic — rather than continuing a
construct begun on an earlier line, and so is a sound place to indent
from; see `nael--indent-anchor'.

The grammar deliberately does not model Lean's tactic vocabulary, so a
sequence of tactics shows up as a chain of `app' nodes.  That is why
`app' belongs here, even though its children are a function and its
arguments in every other context.")

(defconst nael--anchor-regexp
  (rx bos (or "by" "do_block" "match" "where_decls" "where_struct") eos)
  "Regular expression matching nodes that set an indentation base.

Unlike the nodes of `nael--statement-parent-regexp', these do so even
when their own parent is in the middle of a construct, because a block
that opens a line indents its contents relative to itself.")

(defun nael--indent-standalone-p (node)
  "Return non-nil if NODE is the first thing on its line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (looking-back (rx bol (* whitespace))
                  (line-beginning-position))))

(defun nael--indent-anchor (_node parent &rest _)
  "Return the position of the statement enclosing PARENT.

Like the `standalone-parent' preset, this walks up from PARENT until it
finds a node that begins its own line.  Unlike that preset, it walks on
past a node that merely continues a construct begun on an earlier line,
which is what tells a block's body apart from a wrapped signature:

  theorem foo (n : Nat) :
      0 < n + 1 := by
    omega

The `0 < …' of the signature does begin its own line, yet `omega' is
part of the statement that `theorem' begins, and indents from there."
  (let ((node parent)
        (anchor nil))
    (while (and node (not anchor))
      (when (and (nael--indent-standalone-p node)
                 (or (string-match-p nael--anchor-regexp
                                     (treesit-node-type node))
                     ;; A child of the root always begins a statement,
                     ;; whether the root parsed as a `module' or, after an
                     ;; error, as an ERROR; see `nael--indent-toplevel-p'.
                     (nael--indent-toplevel-p nil (treesit-node-parent node))
                     (string-match-p
                      nael--statement-parent-regexp
                      (or (treesit-node-type (treesit-node-parent node))
                          "module"))))
        (setq anchor (treesit-node-start node)))
      (setq node (treesit-node-parent node)))
    (or anchor (point-min))))

;; Note that the regular expressions of `node-is', `parent-is' and
;; `n-p-gp' are not anchored, so each of them has to anchor itself.

(defvar nael-indent-rules
  `((lean
     ;; Prose is the author's to lay out, not ours.
     (nael--indent-text-p column-0 nael--indent-keep)

     ;; Likewise whatever the parser could not make sense of.
     (nael--indent-error-p column-0 nael--indent-keep)

     ;; An empty line has no node to anchor against.  Anchor at column
     ;; zero and let the offset compute the whole column.
     (nael--indent-blank-p column-0 nael--indent-fallback)

     ;; Any other line that no node begins — one starting in the middle
     ;; of a token, say — is none of our business either.
     (no-node column-0 nael--indent-keep)

     ;; A closing bracket aligns with the line that opened it.
     ((node-is ,nael--closer-regexp) parent-bol 0)

     ;; `then' and `else' align with the `if' they belong to.
     ((node-is ,(rx bos (or "then" "else") eos)) parent-bol 0)

     ;; Commands always start in column zero, error or no error.
     (nael--indent-toplevel-p column-0 0)

     ;; A command qualified by `set_option … in' or `open … in' keeps the
     ;; column of the qualifier rather than indenting under it.
     ((parent-is ,nael--wrapper-regexp) parent-bol 0)

     ;; Below this point every rule reasons about the shape of the
     ;; parse tree, so stop here if that shape is not to be trusted.
     (nael--indent-damaged-p column-0 nael--indent-keep)

     ;; A `where' clause that opens its own line aligns with the
     ;; declaration it closes over, not with the fields it introduces.
     ((node-is ,nael--where-regexp) nael--indent-anchor 0)

     ;; The `|' of a `match ... with', or of an `induction'/`cases'
     ;; tactic, aligns with the keyword that introduces it, whereas the
     ;; equations of a `def' are indented below it.
     ((n-p-gp ,(rx bos "match_alt" eos)
              ,(rx bos (or "induction" "match") eos) nil)
      parent-bol 0)
     ((n-p-gp nil ,(rx bos "match_alt" eos) nil)
      parent-bol nael-indent-offset)

     ;; Tactic blocks (`by') and monadic blocks (`do').  Both usually
     ;; open at the end of a line, so anchor on the statement that
     ;; encloses them; see `nael--indent-anchor'.
     ((parent-is ,nael--block-regexp)
      nael--indent-anchor nael-indent-offset)

     ;; Binders and result types continue a declaration's signature.
     (nael--indent-signature-p
      nael--indent-anchor nael-indent-signature-offset)
     ((parent-is ,(rx bos "binders" eos))
      nael--indent-anchor nael-indent-signature-offset)

     ;; `where' clauses, i.e. structure fields and auxiliary
     ;; definitions.
     ((parent-is ,nael--where-regexp)
      nael--indent-anchor nael-indent-offset)

     ;; Bodies, structure fields and constructors of a declaration.
     ((parent-is ,nael--declaration-regexp)
      nael--indent-anchor nael-indent-offset)

     ;; Attributes and modifiers sit on the declaration's own column.
     ((parent-is ,(rx bos "declaration" eos)) parent-bol 0)

     ;; Terms within a pair of brackets.
     ((parent-is ,nael--bracket-node-regexp)
      parent-bol nael-indent-offset)

     ;; Leave continuation lines as they are; see `nael--indent-keep'.
     ((parent-is ,nael--continuation-regexp)
      parent-bol nael--indent-keep)

     (catch-all parent-bol nael-indent-offset)))
  "`treesit-simple-indent-rules' for `nael-mode'.")

;;;; Navigation:

(defconst nael--defun-regexp
  (rx bos "declaration" eos)
  "Regular expression matching nodes that `nael-mode' treats as defuns.

The `declaration' node wraps the declaration proper together with its
attributes, doc-modifiers and visibility markers, all of which belong to
the defun.")

(defun nael--named-node-p (node)
  "Return non-nil if NODE is a named node.

Many keyword tokens of the Lean grammar share their type name with the
rule they introduce: both the `def' command and the literal `def' that
opens it have node type \"def\".  Matching node types by regular
expression alone therefore picks up bare keywords as well."
  (treesit-node-check node 'named))

(defconst nael--imenu-settings
  `(("Namespace" ,(rx bos (or "namespace" "section") eos)
     nael--named-node-p nil)
    ("Type" ,(rx bos (or "inductive" "structure") eos)
     nael--named-node-p nil)
    ("Theorem" ,(rx bos "theorem" eos)
     nael--named-node-p nil)
    ("Definition"
     ,(rx bos (or "abbrev" "axiom" "constant" "def" "instance"
                  "opaque")
          eos)
     nael--named-node-p nil))
  "`treesit-simple-imenu-settings' for `nael-mode'.")

(defconst nael--outline-regexp
  (rx bos (or "declaration" "namespace" "section") eos)
  "Regular expression matching nodes that `nael-mode' outlines.")

(defun nael-outline-p (node)
  "`treesit-outline-predicate' for `nael-mode'.

Return non-nil if NODE should become a heading of `outline-minor-mode'."
  (and (nael--named-node-p node)
       (string-match-p nael--outline-regexp
                       (treesit-node-type node))))

(defun nael--node-name (node field)
  "Return the text of NODE's named FIELD child, or nil if it has none.

Only named children are considered, because the grammar labels the colon
of a type ascription with the same `type' field as the type itself."
  (when-let* ((child (seq-find
                      (lambda (child)
                        (equal (treesit-node-field-name child) field))
                      (treesit-node-children node t))))
    (treesit-node-text child t)))

(defun nael-defun-name (node)
  "`treesit-defun-name-function' for `nael-mode'.

Return the name that NODE declares, as a string, or nil if it declares
none."
  (pcase (treesit-node-type node)
    ;; Look inside the wrapper for the declaration proper, which the
    ;; grammar always puts last.
    ("declaration"
     (nael-defun-name (treesit-node-child node -1 t)))
    ("example" "example")
    ;; An anonymous instance is identified by the class it inhabits.
    ("instance" (or (nael--node-name node "name")
                    (nael--node-name node "type")))
    (_ (nael--node-name node "name"))))

;;;; Auxiliary Functions and Commands:

(defun nael-comment-insert ()
  "`comment-insert-comment-function' for `nael-mode'."
  (interactive)
  (if (save-excursion (beginning-of-line)
                      (looking-at-p "[[:blank:]]*$"))
      (progn
        ;; Respect users who set `comment-start' to "--".
        (insert comment-start " ")
        ;; Respect users who set `comment-end' to "".
        (unless (length= comment-end 0)
          (save-excursion
            (insert " " comment-end))))
    (end-of-line)
    (unless (looking-back "[[:blank:]]" (1- (point)))
      (insert " "))
    (insert "-- ")))

(defun nael-fill-paragraph (&optional justify)
  "Fill comment paragraph at point.  Maybe JUSTIFY."
  (interactive)
  (when (save-excursion (nth 4 (syntax-ppss (point))))
    (let* ((com-beg (save-excursion
                      (re-search-backward "[/-]-" nil t)
                      (match-beginning 0)))
           (multi (eq (char-after com-beg) ?/)))
      (if multi
          (let* ((par-beg (save-excursion
                            (re-search-backward paragraph-start nil t)
                            (match-beginning 0)))
                 (beg (max com-beg par-beg))
                 (com-end (if multi "-/" "$"))
                 (com-end (save-excursion
                            ;; If cursor is at -|/, then move to |-/,
                            ;; so that `re-search-forward' can locate
                            ;; comment ending.
                            (and (eq (char-before) ?-)
                                 (eq (char-after) ?/)
                                 (backward-char))
                            (re-search-forward com-end nil t)
                            (match-end 0)))
                 (par-end (save-excursion
                            (search-forward paragraph-separate nil t)
                            (match-end 0)))
                 (end (min com-end par-end)))
            (fill-region beg end justify))
        ;; `fill-comment-paragraph' fills prefixed comments well, when
        ;; configured correctly.
        (let ((comment-start "--") (comment-end ""))
          ;; For some reason, "" is used as fill-prefix by
          ;; `fill-comment-paragraph' when point is at --|.  Avoid
          ;; this misbehavior by moving point forward one char.
          (and (not (eolp))
               (looking-back "--" (max (- (point) 2) (point-min)))
               (forward-char))
          (fill-comment-paragraph justify))))))

;;;; Preparation:

;; Our goal is to avoid loading `nael-abbrev' / `abbrev', `nael-eglot'
;; / `eglot' and `nael-lsp' / `lsp', until the user calls one of their
;; autoloaded commands.  We are lucky that `post-self-insert-hook' is
;; strictly loaded and that `eglot-server-initialized-hook',
;; `eglot-managed-mode-hook' as well as `lsp-managed-mode-hook' are
;; all initialized to nil, in usual Emacs manner.  Thus, it's fine to
;; call `add-hook' on them, even if they have not been defined as
;; variables yet.

;; We could introduce a hook, with all of
;; `nael-prepare-{abbrev,eglot,lsp}' being default members of it,
;; which we could run in the beginning of the definition-body of
;; `nael-mode'.  As mentioned, though, it's uncommon in Emacs to have
;; hooks initialized with non-nil values and some common functions
;; like `add-hook' rely on this practice.  Thus, we use boolean flags
;; instead.

(defcustom nael-prepare-abbrev t
  "Whether `abbrev-mode' should be prepared for `nael-mode'."
  :type 'boolean
  :group 'nael)

(defun nael-prepare-abbrev ()
  "Prepare `abbrev-mode' for `nael-mode'.

Expand symbol-including abbreviations when adequate character inserted."
  (interactive)
  (when nael-prepare-abbrev
    (add-hook 'abbrev-mode-hook
              #'nael-abbrev-configure nil 'local)))

(defcustom nael-prepare-eglot t
  "Whether `eglot' should be prepared for `nael-mode'."
  :type 'boolean
  :group 'nael)

(defun nael-prepare-eglot ()
  "Prepare `eglot' for `nael-mode'."
  (interactive)
  (when nael-prepare-eglot
    ;; We want to add an entry to `eglot-server-programs' but we want
    ;; to avoid stricly loading `eglot' here.  Unfortunately, Eglot
    ;; doesn't offer any hook that'd be run before it accesses
    ;; `eglot-server-programs'.  We have no choice but
    ;; `with-eval-after-load'.
    (with-eval-after-load 'eglot
      (require 'nael-eglot))
    (add-hook 'eglot-server-initialized-hook
              #'nael-eglot-configure-when-initialized nil 'local)
    (add-hook 'eglot-managed-mode-hook
              #'nael-eglot-configure-when-managed nil 'local)))

(defcustom nael-prepare-lsp t
  "Whether `lsp-mode' should be prepared for `nael-mode'."
  :type 'boolean
  :group 'nael)

(defun nael-prepare-lsp ()
  "Prepare `lsp-mode' for `nael-mode'.

Note that if you call `lsp-mode' inside a buffer majored by `nael-mode',
it is unguardedly assumed that you have `nael-lsp' package installed and
that either you have `nael-lsp' loaded, or `nael-lsp-autoloads', or at
least evaluated an autoload statement for
`nael-lsp-configure-when-managed'."
  (interactive)
  (when nael-prepare-lsp
    ;; The `lsp-language-id-configuration' variable needs to be
    ;; modified so early, that hooks don't work.  We have no choice
    ;; but `with-eval-after-load'.
    (with-eval-after-load 'lsp-mode
      (require 'nael-lsp))
    (add-hook 'lsp-managed-mode-hook
              #'nael-lsp-configure-when-managed nil 'local)))

;; Let's use the same interface (a configure-function, a
;; prepare-option and -function) for Flymake too because we don't load
;; or invoke it in `nael-mode' itself.

(defun nael-flymake-configure ()
  "Use Flymake to jump to errors."
  (interactive)
  (setq-local next-error-function
              #'flymake-goto-next-error))

(defcustom nael-prepare-flymake t
  "Whether `flymake-mode' should be prepared for `nael-mode'."
  :type 'boolean
  :group 'nael)

(defun nael-prepare-flymake ()
  "Prepare `flymake-mode' for `nael-mode'."
  (interactive)
  (when nael-prepare-flymake
    (add-hook 'flymake-mode-hook
              #'nael-flymake-configure nil 'local)))

;;;; Mode:

(defcustom nael-mode-hook nil
  "Hook run when entering `nael-mode'."
  :options '(abbrev-mode eglot-ensure imenu-add-menubar-index lsp)
  :type 'hook
  :group 'nael)

(defvar-keymap nael-mode-map
  "<remap> <display-local-help>" #'eldoc-doc-buffer
  "C-c C-a" #'abbrev-mode
  "C-c C-c" #'project-compile
  "C-c C-e" #'eglot
  "C-c C-k" #'nael-abbrev-help)

;;;###autoload
(define-derived-mode nael-mode prog-mode "Nael"
  "Major mode for Lean, powered by tree-sitter.

\\{nael-mode-map}"
  :syntax-table nael-mode-syntax-table
  ;; Preparations:
  (nael-prepare-abbrev)
  (nael-prepare-eglot)
  (nael-prepare-lsp)
  ;; Paragraphs and filling:
  (setq-local paragraph-start
              "[[:blank:]]*$")
  (setq-local paragraph-separate
              "[[:blank:]]*$")
  (setq-local fill-paragraph-function
              #'nael-fill-paragraph)
  ;; Comments:
  (setq-local comment-end
              "-/")
  (setq-local comment-end-skip
              "[[:space:]]*-/")
  (setq-local comment-insert-comment-function
              #'nael-comment-insert)
  (setq-local comment-padding
              1)
  (setq-local comment-quote-nested ;; Comments may be nested.
              nil)
  (setq-local comment-start
              "/-")
  (setq-local comment-start-skip
              "/-[[:space:]]*")
  (setq-local comment-style
              'multi-line)
  (setq-local comment-use-syntax
              t)
  (setq-local parse-sexp-ignore-comments
              t)
  ;; Compile:
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local compile-command
              "lake build ")
  ;; Flymake:
  (nael-flymake-configure)
  ;; Tree-sitter:
  (when (nael-grammar-ready-p)
    (setq treesit-primary-parser (treesit-parser-create 'lean))
    ;; Font-lock:
    (setq-local treesit-font-lock-settings
                nael--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                nael-font-lock-feature-list)
    ;; Indentation:
    (setq-local treesit-simple-indent-rules
                nael-indent-rules)
    ;; Lean's whitespace is significant and never tabulated.
    (setq-local indent-tabs-mode nil)
    ;; Navigation:
    (setq-local treesit-defun-type-regexp
                nael--defun-regexp)
    (setq-local treesit-defun-name-function
                #'nael-defun-name)
    (setq-local treesit-thing-settings
                `((lean
                   (list ,nael--bracket-node-regexp)
                   (text ,nael--text-regexp))))
    ;; Imenu:
    (setq-local treesit-simple-imenu-settings
                nael--imenu-settings)
    ;; Outline:
    (when (boundp 'treesit-outline-predicate)
      (setq-local treesit-outline-predicate
                  #'nael-outline-p))
    (treesit-major-mode-setup)))

;; Lean language specification requires UTF-8 encoding.
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

;;;; Association:

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "\\.lean\\'" 'nael-mode))

;;;###autoload
(when (fboundp 'derived-mode-add-parents)
  (derived-mode-add-parents 'nael-mode '(lean-ts-mode)))

(add-to-list 'project-vc-extra-root-markers
             "lakefile.lean")

(add-to-list 'project-vc-extra-root-markers
             "lakefile.toml")

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes
               (cons "lean" 'nael)))

;; If the code that requires `markdown-mode' grows, we will extract it
;; into a new package that depends on it.  But a single expression is
;; not worth a package.
(with-eval-after-load 'markdown-mode
  (add-to-list 'markdown-code-lang-modes
               (cons "lean" 'nael-mode)))

(provide 'nael)

;;; nael.el ends here
