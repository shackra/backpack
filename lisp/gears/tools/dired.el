;; -*- lexical-binding: t; -*-

(leaf dired
  :doc "the built-in Emacs file manager with sane defaults"
  :unless (gearp! :tools -dired)
  :tag "builtin"
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x 4 C-j" . dired-jump-other-window)
   (:dired-mode-map
    ("(" . dired-hide-details-mode)
    ("C-c C-e" . wdired-change-to-wdired-mode)))
  :custom
  (dired-listing-switches                       .	"-alh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer     .	t)
  (dired-dwim-target                            .	t)
  (dired-auto-revert-buffer                     .	#'dired-buffer-stale-p)
  (dired-recursive-copies                       .	'always)
  (dired-recursive-deletes                      .	'top)
  (dired-create-destination-dirs                .	'ask)
  (dired-vc-rename-file                         .	t)
  (dired-isearch-filenames                      .	t)
  (dired-clean-confirm-killing-deleted-buffers  .	nil)
  (dired-hide-details-hide-symlink-targets      .	nil)
  (dired-omit-files                              .	"^\\.[^.]+$")
  (dired-omit-extensions                         .	'(".elc" ".o" ".pyc" ".class" ".aux" ".log" ".dvi"))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (unless (gearp! :tools dired -details)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))
  (with-eval-after-load 'dired
    (require 'dired-x)))

(leaf diredfl
  :doc "extra font-lock highlighting for dired"
  :unless (gearp! :tools -dired)
  :ensure (diredfl :ref "fe72d2e42ee18bf6228bba9d7086de4098f18a70")
  :hook (dired-mode-hook . diredfl-mode))

(leaf nerd-icons-dired
  :doc "file type icons in dired listings"
  :when (gearp! :tools dired icons)
  :ensure (nerd-icons-dired :ref "104acd8879528b8115589f35f1bbcbe231ad732f")
  :hook (dired-mode-hook . nerd-icons-dired-mode))

(leaf dired-narrow
  :doc "live filtering of dired listings"
  :when (gearp! :tools dired narrow)
  :ensure (dired-narrow
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-narrow.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))

(leaf dired-collapse
  :doc "collapse single-child directory chains"
  :when (gearp! :tools dired collapse)
  :ensure (dired-collapse
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-collapse.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48")
  :hook (dired-mode-hook . dired-collapse-mode))

(leaf dired-subtree
  :doc "expand subdirectories inline with tree indentation"
  :when (gearp! :tools dired subtree)
  :ensure (dired-subtree
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-subtree.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48")
  :bind (:dired-mode-map
         (("TAB" . dired-subtree-toggle)
          ("S-TAB" . dired-subtree-cycle)))
  :config
  (setq dired-subtree-use-backgrounds nil))

(leaf dired-rainbow
  :doc "colorize dired listings by file extension"
  :when (gearp! :tools dired rainbow)
  :ensure (dired-rainbow
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-rainbow.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))

(leaf dired-filter
  :doc "ibuffer-like filtering for dired; replaces dired-omit-mode"
  :unless (gearp! :tools dired -filter)
  :ensure (dired-filter
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-filter.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48")
  :hook (dired-mode-hook . dired-filter-mode)
  :custom
  (dired-filter-stack . '((omit)))
  :bind (:dired-mode-map ("," . dired-filter-mode)))

(leaf dired-open
  :doc "open files with external programs"
  :when (gearp! :tools dired open)
  :ensure (dired-open
           :host github :repo "Fuco1/dired-hacks"
           :files ("dired-open.el")
           :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))

(leaf fd-dired
  :doc "use fd instead of find for dired-find"
  :when (gearp! :tools dired fd)
  :ensure (fd-dired
           :host github :repo "yqrashawn/fd-dired"
           :ref "c5b31aec25d20c3219e2635b82efeba532895278")
  :doctor ("fd" . "fast find alternative for dired-find"))

(leaf diff-hl
  :doc "VC status indicators in the dired fringe"
  :when (gearp! :tools dired vc)
  :ensure (diff-hl :ref "b965e19e6e7f9933199e421849a49229207c1c9f")
  :hook (dired-mode-hook . diff-hl-dired-mode))

(leaf dirvish
  :doc "modern dired replacement with built-in extensions"
  :when (gearp! :tools dired dirvish)
  :ensure (dirvish
           :host github :repo "alexluigit/dirvish"
           :files (:defaults "extensions/*.el")
           :ref "d877433f957a363ad78b228e13a8e5215f2d6593")
  :bind
  (("C-x d" . dirvish)
   ("C-x C-j" . dirvish-dwim-target-dirvish)
   (:dirvish-mode-map
    ("C-c C-e" . wdired-change-to-wdired-mode)))
  :config
  (dirvish-override-dired-mode 1)
  (setq dirvish-attributes
        '(vc-state file-time file-size)
        dirvish-mode-line-format
        '(" " dirvish-mode-line-sort " " dirvish-mode-line-filter " " dirvish-mode-line-stats))
  (leaf dirvish-side
    :doc "side panel file manager"
    :when (gearp! :tools dired dirvish)
    :bind (("s-d" . dirvish-side))))
