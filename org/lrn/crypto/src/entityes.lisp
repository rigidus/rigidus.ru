;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::entityes][entityes]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

(define-entity asset "Сущность ассета"
  ((version int)
   (asset_id string)
   (name string)
   (descr text)
   (quantity (or db-null int))
   (decimals int)
   (owner (or db-null text))
   (script (or db-null text))))

(make-asset-table)

;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

(define-entity account "Сущность аккаунта"
  ((version int)
   (account_id string)
   (name string)
   (descr text)
   (script (or db-null text))))

(make-account-table)


;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

(define-entity transaction-issue-asset "Сущность транзакции выпуска ассета"
  ((id serial)
   (version int)
   (sender (or db-null string))
   (fee_asset (or db-null string))
   (fee_value (or db-null string))
   (asset string)))

(make-transaction-issue-asset-table)
;; entityes ends here
