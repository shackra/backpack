;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((jinx-languages . "en")))
 (yaml-ts-mode . ((eglot-workspace-configuration . (:yaml (
							   :format (:enable t)
							   :validate t
							   :hover t
							   :completion t
							   :schemas (
								     :https\://www.schemastore.org/github-workflow.json "/.github/workflows/ci.yml"
								     )
							   :schemaStore (:enable t))
							  :editor (:tabSize 4))))))
