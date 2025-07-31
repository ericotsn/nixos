;;; -*- lexical-binding: t; -*-

(require 'exwm)

(setq exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
	([?\s-w] . exwm-workspace-switch)
	([?\s-&] . (lambda (cmd)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command cmd nil cmd)))
	,@(mapcar (lambda (i)
		    `(,(kbd (format "s-%d" i)) .
		      (lambda ()
			(interactive)
			(exwm-workspace-switch-create ,i))))
		  (number-sequence 0 9))))

(setq exwm-randr-workspace-monitor-plist '(0 "Virtual-1"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
	    (start-process-shell-command
	     "xrandr" nil "xrandr --output Virtual-1 --auto")))
(exwm-randr-mode)

(exwm-systemtray-mode)

(exwm-wm-mode)
