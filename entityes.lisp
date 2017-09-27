;; [[file:doc.org::*Конечный автомат разбора орг-файлов][event_entity]]
;; ;; А тут мы вызываем генератор кода и записываем результат в файл
;; (in-package #:moto)

;; <!-<gen_parser()>>

;; (defparameter *fsm-test* "

;; -*- mode: org; fill-column: 95 -*-

;;    - third
;; - first
;;  - second

;; ")
;; ;; @title Клеточные автоматы
;; ;; @category emacs

;; ;; Это могло бы быть интересным на следующем ITGM и достаточно
;; ;; функциональным для FProg-комьюнити.
;; ;; Особенно если описать все на хаскеле -
;; ;; заодно хороший повод в нем наконец разобраться.

;; ;; Можно было бы рассмотреть:
;; ;; - Жизнь Конвея
;; ;; - WireWorld Брайаном Сильверманом (особенно интересен в
;; ;;   в отношении применимости к проекту симуляции электронных
;; ;;   схем.
;; ;; - Алгоритм волновой трассировки Ли для поиска путей
;; ;;   (например, в двумерных играх, или для трассировки печатных плат,
;; ;;   что даже лучше)

;; ;; * Иерархическая структура

;; ;;   Планирую выложить целый раздел с практическими примерами
;; ;;   реализации основных часто используемых идей, чтобы не объяснять
;; ;;   базовые концепции.
;; ;;   К примеру, динамического программирования

;; ;; ** Ближайшие планы

;; ;;    Практика создания языков предметной области
;; ;;    - Слишком сложно чтобы быть правдой
;; ;;    - Построение визуальных представлений

;; ;;    Как устроены распределенные виртуальные машины
;; ;;    - Блокчейн и все-все-все
;; ;;    - Распределенное состояние

;; ;; ** Отдаленные планы

;; ;;    - Анализ изображений с использованием сверточных нейронных сетей
;; ;;    - Краткое введение в практику использования клеточных автоматов
;; ;;    - Регистры сдвига с линейной обратной связью (LFSR) и их применение
;; ;;    - Автоматическое построение оптимальных путей в графе
;; ;;    - Автоматизация доказательства корректности протоколов распределенного обмена сообщениями в P2P сетях
;; ;; ")
;; ;; "

;; (define-condition fsm-unknown-input (condition)
;;   ((unexpected :initarg :unexpected :reader unexpected)
;;    (state      :initarg :state      :reader state))
;;   (:report (lambda (condition stream)
;;              (format stream "Unexpected character ~@C in state :~A"
;;                      (unexpected condition)
;;                      (state condition)))))

;; (defmacro dbg-state ()
;;   `(format t "~%:~A [~A] (~A)~%" state cur *fsm-test*))

;; (defmacro set-empty (name)
;;   `(setf ,name ""))

;; (defmacro cur+ (name)
;;   `(setf ,name (concatenate 'string ,name (string cur))))

;; (defmacro out-empty-string ()
;;   `(setf rs (concatenate 'string rs (string #\Newline) "<br />") ))

;; (defmacro open-ul ()
;;   `(setf rs (concatenate 'string rs (string #\Newline) "<ul>")))

;; (defmacro close-ul ()
;;   `(setf rs (concatenate 'string rs (string #\Newline) "</ul>")))

;; (defmacro close-li ()
;;   `(setf rs (concatenate 'string rs (string #\Newline) "<li>"
;;                          (format nil "[~D]" space-cnt)
;;                          (subseq acc-li 0 (- (length acc-li) 1)) "</li>")))

;; (defmacro ret ()
;;   `(equal cur #\Newline))

;; (defmacro spc ()
;;   `(equal cur #\Space))

;; (defmacro star ()
;;   `(equal cur #\*))

;; (defmacro tire ()
;;   `(equal cur #\-))

;; (defmacro not-ret ()
;;   `(not (equal cur #\Newline)))

;; (let ((pos 0)
;;       (state :start)
;;       (space-cnt 0)
;;       (acc-li)
;;       (rs ""))
;;   (defun get-next ()
;;     (prog1 (setf cur (coerce (subseq *fsm-test* pos (+ 1 pos)) 'character))
;;       (format t "~c" cur)
;;       (incf pos)))
;;   (defun fsm ()
;;     (tagbody
;;        revert
;;        (let ((cur (get-next)))
;;          (ecase state
;;            (:start (progn
;;                      (dbg-state)
;;                      (setf space-cnt 0)
;;                      (cond ((ret)                (progn
;;                                                    (out-empty-string)
;;                                                    (setf state :start) (go revert)))
;;                            ((spc)                (progn
;;                                                    (incf space-cnt)
;;                                                    (setf state :space) (go revert)))
;;                            ((tire)               (progn
;;                                                    (setf state :minus) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            (:space (progn
;;                      (dbg-state)
;;                      (cond ((spc)                (progn
;;                                                    (incf space-cnt)
;;                                                    (setf state :space) (go revert)))
;;                            ((ret)                (progn
;;                                                    (out-empty-string)
;;                                                    (setf state :start) (go revert)))
;;                            ((tire)               (progn
;;                                                    (setf state :minus) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            (:minus (progn
;;                      (dbg-state)
;;                      (cond ((star)               (progn
;;                                                    (setf state :mode) (go revert)))
;;                            ((spc)                (progn
;;                                                    (open-ul)
;;                                                    (setf state :ul) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            (:mode  (progn
;;                      (dbg-state)
;;                      (cond ((not-ret)            (progn
;;                                                    (setf state :mode) (go revert)))
;;                            ((ret)                (progn
;;                                                    (setf state :start) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            (:ul    (progn
;;                      (dbg-state)
;;                      (set-empty acc-li)
;;                      (cond ((not-ret)            (progn
;;                                                    (cur+ acc-li)
;;                                                    (setf state :li) (go revert)))
;;                            ((ret)                (progn
;;                                                    (close-ul)
;;                                                    (setf state :start) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            (:li    (progn
;;                      (dbg-state)
;;                      (cur+ acc-li)
;;                      (cond ((not-ret)            (progn
;;                                                    (setf state :li) (go revert)))
;;                            ((ret)                (progn
;;                                                    (close-li)
;;                                                    (setf state :ul) (go revert)))
;;                            (t (error 'fsm-unknown-input :unexpected cur :state state)))))
;;            ))
;;        (go revert)))
;;     (handler-case (fsm)
;;       (SB-KERNEL:BOUNDING-INDICES-BAD-ERROR () rs))
;;     rs)
;; event_entity ends here
