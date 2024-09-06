;; Code to setup tree-sitter grammar in Emacs for various languages.
;; Copied from https://github.com/mickeynp/combobulate/blob/master/.ts-setup.els
;; Added java

(let ((treesit-language-source-alist
       '(
	 ;;(css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
         ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
         ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
         ;; (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	 ;; (java . ("https://github.com/tree-sitter/java-tree-sitter" "v0.23.0"))
         ;; (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
         ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
         ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
         ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
         ;; (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	 (kotlin . ("https://github.com/tree-sitter/kotlin-tree-sitter" "v0.23.0"))
	 )))
  (message "* Installing tree-sitter grammars...")
  (pcase-dolist (`(,lang . ,rest) treesit-language-source-alist)
    (message "** Installing %s..." lang)
    (treesit-install-language-grammar lang)))
