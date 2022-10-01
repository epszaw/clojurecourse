(ns task02.query
  (:use [task02 helpers db]
        [clojure.string :as str]
        [clojure.core.match :only [match]]))

;; Функция выполняющая парсинг запроса переданного пользователем
;;
;; Синтаксис запроса:
;; SELECT table_name [WHERE column comp-op value] [ORDER BY column] [LIMIT N] [JOIN other_table ON left_column = right_column]
;;
;; - Имена колонок указываются в запросе как обычные имена, а не в виде keywords. В
;;   результате необходимо преобразовать в keywords
;; - Поддерживаемые операторы WHERE: =, !=, <, >, <=, >=
;; - Имя таблицы в JOIN указывается в виде строки - оно будет передано функции get-table для получения объекта
;; - Значение value может быть либо числом, либо строкой в одинарных кавычках ('test').
;;   Необходимо вернуть данные соответствующего типа, удалив одинарные кавычки для строки.
;;
;; - Ключевые слова --> case-insensitive
;;
;; Функция должна вернуть последовательность со следующей структурой:
;;  - имя таблицы в виде строки
;;  - остальные параметры которые будут переданы select
;;
;; Если запрос нельзя распарсить, то вернуть nil

;; Примеры вызова:
;; > (parse-select "select student")
;; ("student")
;; > (parse-select "select student where id = 10")
;; ("student" :where #<function>)
;; > (parse-select "select student where id = 10 limit 2")
;; ("student" :where #<function> :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2")
;; ("student" :where #<function> :order-by :id :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2 join subject on id = sid")
;; ("student" :where #<function> :order-by :id :limit 2 :joins [[:id "subject" :sid]])
;; > (parse-select "werfwefw")
;; nil

(defn make-where-function [col op val]
  (let [k (keyword col)
        v (if (re-find #"'.+'" val) val (Integer/parseInt val))]
    (match [op]
         ["!="] (fn [d]
                  (if-not (get d k) v))
         ["="]  (fn [d]
                  (if (= (get d k) v) val))
         :else  (fn [d]
                  ((eval (read-string op)) (get d k) v)))))

(defn parse-select [^String sel-string]
  (let [sel                (str/split sel-string #"\s")
        restore-sel-string (fn [sel] (str/join " " sel))]
    (match sel
           ["select" tbl & rest] (concat (list tbl) (parse-select (restore-sel-string rest)))
           ["where" col op val & rest] (concat (list :where (make-where-function col op val)) (parse-select (restore-sel-string rest)))
           ["order" "by" col & rest] (concat (list :order-by (keyword col)) (parse-select (restore-sel-string rest)))
           ["limit" lim & rest] (concat (list :limit (Integer/parseInt lim)) (parse-select (restore-sel-string rest)))
           ["join" tbl "on" col1 "=" col2] (list :joins [[(keyword col1) tbl (keyword col2)]])
           :else nil)))

;; Выполняет запрос переданный в строке.  Бросает исключение если не удалось распарсить запрос

;; Примеры вызова:
;; > (perform-query "select student")
;; ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "select student order by year")
;; ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})
;; > (perform-query "select student where id > 1")
;; ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "not valid")
;; exception...
(defn perform-query [^String sel-string]
  (if-let [query (parse-select sel-string)]
    (apply select (get-table (first query)) (rest query))
    (throw (IllegalArgumentException. (str "Can't parse query: " sel-string)))))

(perform-query "select student")
(perform-query "select student order by year")
(perform-query "select student where id > 1")
