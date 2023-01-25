(ns dsl.core
  (:require [clojure.walk :as walk]
            [clojure.core.match :refer [match]]))

(def cal (java.util.Calendar/getInstance))
(def today (java.util.Date.))
(def yesterday (do (.add cal java.util.Calendar/DATE -1) (.getTime cal)))
(def tomorrow (do (.add cal java.util.Calendar/DATE 2) (.getTime cal)))

;; Поддерживаемые операции:
;; > >= < <=
;; Функция принимает на вход три аргумента. Она должна определить,
;; являются ли второй и третий аргумент датами. Если являются,
;; то из дат необходимо взять date.getTime и сравнить их по этому числу.
;; Если получены не даты, то выполнить операцию op в обычном порядке:
;; (op d1 d2).
(defn d-op [op d1 d2]
  (let [date-mode
        (->> [d1 d2]
             (map type)
             (every? #(= % java.util.Date)))]
    (if date-mode
      (op (.getTime d1) (.getTime d2))
      (op d1 d2))))

;; Пример вызова:
;; (d-add today '+ 1 'day)
;; Функция должна на основе своих параметров создать новую дату.
;; Дата создается при помощи календаря, например так:
;; (def cal (java.util.Calendar/getInstance))
;; (.add cal java.util.Calendar/DATE 2)
;; (.getTime cal)
;; Во-первых, необходимо на основе 'op' и 'num' определить количество, на
;; которое будем изменять дату. 'Op' может принимать + и -, соответственно
;; нужно будет не изменять либо изменить знак числа 'num'.
;; Во-вторых, необходимо узнать период, на который будем изменять дату.
;; Например, если получили 'day, то в вызове (.add cal ...) будем использовать
;; java.util.Calendar/DATE. Если получили 'months, то java.util.Calendar/MONTH.
;; И так далее.
;; Результат работы функции - новая дата, получаемая из календаря так: (.getTime cal)
(defn d-add [date op num period]
  (let [positive (= op '+)
        val      (if positive num (* num -1))
        unit     (match period
                        (:or 'second 'seconds) java.util.Calendar/SECOND
                        (:or 'minute 'minutes) java.util.Calendar/MINUTE
                        (:or 'hour 'hours)     java.util.Calendar/HOUR
                        (:or 'day 'days)       java.util.Calendar/DATE
                        (:or 'week 'weeks)     java.util.Calendar/WEEK_OF_MONTH
                        (:or 'month 'months)   java.util.Calendar/MONTH
                        (:or 'year 'years)     java.util.Calendar/YEAR)]
    (.setTime cal date)
    (.add cal unit val) 
    (.getTime cal)))

;; Можете использовать эту функцию для того, чтобы определить,
;; является ли список из 4-х элементов тем самым списком, который создает новую дату,
;; и который нужно обработать функцией d-add.
(defn is-d-op? [code]
  (if (not (list? code))
    false 
    (let [op (first code)]
      (and (= (count code) 3)
           (contains? #{'> '>= '< '<=} op)))))

(defn is-d-add? [code]
  (if (not (list? code))
    false
    (let [op (second code)
          period (last code)]
      (and (= (count code) 4)
        (or (= '+ op)
            (= '- op))
          (contains? #{'day 'days 'week 'weeks 'month 'months 'year 'years
                      'hour 'hours 'minute 'minutes 'second 'seconds} period )))))

;; В code содержится код-как-данные. Т.е. сам code -- коллекция, но его содержимое --
;; нормальный код на языке Clojure.
;; Нам необходимо пройтись по каждому элементу этого кода, найти все списки из 3-х элементов,
;; в которых выполняется сравнение, и подставить вместо этого кода вызов d-op;
;; а для списков из четырех элементов, в которых создаются даты, подставить функцию d-add.
(defmacro with-datetime [& code]
  (conj
    (walk/postwalk 
      (fn [line]
        (cond (is-d-add? line) (let [[d o v p] line]
                                 `(d-add ~d (quote ~o) ~v (quote ~p)))
              (is-d-op? line)  `(d-op ~@line)
              :else            line))
      code)
    'do))

(defn one [] 1)

(with-datetime
    (if (> today tomorrow) (println "Time goes wrong"))
    (if (<= yesterday today) (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly"))))
