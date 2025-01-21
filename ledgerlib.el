(require 'dash)

(cl-defstruct ledgerlib-entry
  ""
  file
  line
  date
  ;; _?
  description
  transactions)

(cl-defstruct ledgerlib-tx
  ""
  line
  account
  amount
  ;;_?
  )

;; TODO: Error handling on bad commands

(defvar ledgerlib-time-units-calendar
  '(("W" . 7.0)
    ("M" . 4.0)
    ("Q" . 3.0)))

(defvar ledgerlib-time-units-standard-workdays
  '(("W" . 5.0)
    ("M" . 4.0)
    ("Q" . 3.0)))

(defvar ledgerlib-time-units-standard-workdays-convenient
  `(("W" . 5.0)
    ("Q" . ,(* 4.0 3.0))))

(cl-defun ledgerlib-days-to-time-units (days &optional units)
  (cl-loop
   with units = (or units ledgerlib-time-units-calendar)
   with result = (abs days)
   with sign = (signum days)
   with result-label = "d"
   for (label . step) in units
   as current = (/ result step)
   while (> (floor current) 0)
   do (setq result current
            result-label label)
   finally return
   ;; Drop any empty remainder
   (let ((result (* result sign)))
     (format
      (cond
       ((> (cadr (cl-floor result)) 0) "%.2g%s")
       ((= result 0) "%d")
       (t "%d%s"))
      result result-label))))

(cl-defun ledgerlib-days-to-workday-units (days)
  ""
  (ledgerlib-days-to-time-units
   days
   ledgerlib-time-units-standard-workdays))

(cl-defun ledgerlib-days-to-convenient-units (days)
  ""
  (ledgerlib-days-to-time-units
   days
   ledgerlib-time-units-standard-workdays-convenient))

(cl-defun ledgerlib-process-raw (raw &key calendar-days)
  ""
  (cl-loop
   for (file line time _ description . txs) in raw
   collect (make-ledgerlib-entry
            :file file
            :line line
            :date (format-time-string "%F" time)
            :description description
            :transactions (cl-loop
                           for (line account amount _) in txs
                           ;; TODO: Support other amount representations
                           as amount = (pcase amount
                                         ((rx (seq (zero-or-one "-") (+ digit) "s"))
                                          (/ (cl-parse-integer (substring amount 0 -1))
                                             60.0 60.0 (if calendar-days 24.0 8.0)))
                                         (_ amount))
                           collect (make-ledgerlib-tx
                                    :line line
                                    :account account
                                    :amount amount)))))

(cl-defun ledgerlib-to-raw (data)
  ""
  (cl-loop
   for entry in data collect
   (pcase-let (((cl-struct ledgerlib-entry file line date description transactions) entry))
     `(,file
       ,line
       ,date
       ,description
       ,(cl-loop
         for tx in transactions collect
         (pcase-let (((cl-struct ledgerlib-tx line account amount) tx))
           `(,line
             ,account
             ,amount)))))))

;;;###autoload
(cl-defun ledgerlib-cmd (cmd &key no-process calendar-days)
  ""
  (let* ((cmd (concat "ledger emacs " cmd))
         (raw (shell-command-to-string cmd)))
    (if no-process
        (read raw)
      (ledgerlib-process-raw (read raw)))))

(cl-defun ledgerlib--make-unit-converter (units)
  (lambda (days)
    (pcase units
      ('nil days)
      ((pred functionp)
       (funcall units days))
      ((pred listp)
       (ledgerlib-days-to-time-units days units))
      ('work
       (ledgerlib-days-to-time-units days ledgerlib-time-units-standard-workdays))
      ('convenient
       (ledgerlib-days-to-time-units days ledgerlib-time-units-standard-workdays-convenient))
      (_ days))))

;;;###autoload
(cl-defun ledgerlib-balances (data &key with-total calendar-days units)
  ""
  (let ((data (if (stringp data)
                  (ledgerlib-cmd data :calendar-days calendar-days)
                data))
        (converter (ledgerlib--make-unit-converter units)))
    (cl-loop
     with totals = (make-hash-table :test 'equal)
     with sum = 0
     for entry in data
     do (pcase-let* (((cl-struct ledgerlib-entry date description transactions) entry))
          (--map
           (pcase-let* (((cl-struct ledgerlib-tx account amount) it))
             (cl-incf sum amount)
             (puthash
              account
              (+ (gethash account totals 0) amount)
              totals))
           transactions))

     finally return
     `(,@(cl-loop
          for account in (sort (hash-table-keys totals) 'string<)
          collect `(,account
                    ,(funcall converter (gethash account totals))))

       ,@(when with-total
           `(("TOTAL" ,(funcall converter sum))))))))

;;;###autoload
(cl-defun ledgerlib-register (data &key calendar-days units)
  ""
  (let ((data (if (stringp data)
                  (ledgerlib-cmd data :calendar-days calendar-days)
                data))
        (converter (ledgerlib--make-unit-converter units)))
    (--sort
     (string< (car it) (car other))
     (cl-loop
      for entry in data append
      (pcase-let* (((cl-struct ledgerlib-entry date description transactions) entry))
        (--map
         (pcase-let* (((cl-struct ledgerlib-tx account amount) it))
           `(,date ,description ,account ,(funcall converter amount)))
         transactions))))))

;;
(provide 'ledgerlib)
