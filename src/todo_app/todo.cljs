(ns todo_app.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; todo-item
;; 1/key
;; 2/value
;; 3/done - boolean yes or no 
;;
;; using vector for todo items
;;

(def todo-entry-key (reagent/atom  0))
(def todo-entry-val (reagent/atom ""))
(def todo-items (reagent/atom [] ))
(def todo-deleted-items (atom []))


(defn discard-key-from-vector [v n]
  (apply vector (filter (fn [x] (not (= (get x :key) n)))
                        v)))

;; (defn flip-done-item [v item]
;;   (let [the-key (get item :key)]
;;   (conj (apply vector (filter (fn [x] (not (= (get x :key) the-key)))
;;                               v))
;;         {:key (get item :key)
;;          :val (get item :val)
;;          :done (not (get item :done))})))



;; count number items 
(defn count-all-tasks []
  (count @todo-items))


;; count number items where done 
(defn count-completed-tasks []
  (count (filter
          #(get % :done)
          @todo-items)))

;; count number items where not done 
(defn count-outstanding-tasks []
  (count (filter
          #(not (get % :done))
          @todo-items)))


;; [flip-done-item]
;; takes a list of items and a single item
;; if match of single item in those items it will logically flip the done
;; otherwise leave item copied
;; assumes only things in map are key,val,done
;; really what doing is changing one property of one item in a collection
;;
(defn flip-done-item [v-items item]
  (let [k (get item :key)]
    (mapv
     (fn [an-item]
       (let [ek (get an-item :key)
             ev (get an-item :val)
             ed (get an-item :done)]         
       (if (= k ek)
         {:key ek :val ev :done (not ed)}
         {:key ek :val ev :done ed})))
     v-items)))



(defn change-item-text [v-items item txt]
  (let [k (get item :key)]
    (mapv
     (fn [an-item]
       (let [ek (get an-item :key)
             ev (get an-item :val)
             ed (get an-item :done)]         
       (if (= k ek)
         {:key ek :val txt :done ed}
         {:key ek :val ev :done ed})))
     v-items)))



;; each todo item has an ability to be altered through its own display
(defn specialized-todo-entry-component [item todo-items]
  [:input {:type "text"
           :value (get item :val)
           :style {:position "relative"}
           :on-change #(reset! todo-items
                               (change-item-text @todo-items item (-> % .-target .-value)))
           :placeholder (if (get item :done)
                          "something done ..."
                          "something to do ...")
           }])





;; no need for key up because the entry is changed directly in the page
           ;; :on-key-up #(and
           ;;              (not (= "" (-> % .-target .-value)))
           ;;              (if (= "Enter" (.-key %))
           ;;                [
           ;;                 (js/alert (-> % .-target .-value))
           ;;                 ;;(reset! todo-entry-val "")
           ;;                 ]
           ;;                ))



(defn erase-item [item]
  (reset! todo-items
          (discard-key-from-vector @todo-items (get item :key))))




;; [todo-items-component]
;; 1/ got an unordered list , css style none
;; 2/ using conj , add ul header to list of todo items
;; each todo item is a list element
;; 3/ button to flip if todo item has been done
;; 4/ todo item text as 
;; 5/ delete button to
;;
;; replace delete button with bin image png
;;
(defn todo-items-component []
  (conj [:ul {:style {:list-style "none"}}]
        (for [item @todo-items]
          ^{:key (get item :key)}
          
          [:li
           
           (if (get item :done)
             {:style {:text-decoration "line-through" :background-color "aquamarine"}}
             {:style {:text-decoration "line-through" :background-color "aquamarine"}})
           
           [:div
            
            (if (get item :done)
             {:style {:background-color "darkkhaki" :margin-top "10px"}}
             {:style {:background-color "aquamarine" :margin-top "10px"}})
            
            (if (get item :done)
              [:img  {:src "img/tick2.png" :width "25" :height "25" :style {:position "relative"}
                      :on-click #(reset! todo-items (flip-done-item @todo-items item))}]
              [:img  {:src "img/tick.png" :width "25" :height "25" :style {:position "relative"}
                      :on-click #(reset! todo-items (flip-done-item @todo-items item))}])

            ;;(get item :val)

            [specialized-todo-entry-component item todo-items]

            [:img  {:src "img/bin.png" :width "25" :height "25" :style {:position "relative" :padding-top "7px"}
                    :on-click #(erase-item item)}]
             ]
            ]
           )
          )
        )
  


;; [todo-add-item x]
;; set todo-items to be conjoin of todo-items and new todo-item
;; increment todo-entry-key
(defn todo-add-item [x]
  (let [new-item {:key @todo-entry-key
                 :val x ;;@todo-entry-val
                 :done false}]
    (reset! todo-items  (conj @todo-items  new-item))
  (swap! todo-entry-key inc)))


;; if want to add a new item that has already been completed say
;; (todo-add-item2 "bought apples" true)
;; to mean we have bought apples 
(defn todo-add-item2 [x d]
  (let [new-item {:key @todo-entry-key
                 :val x ;;@todo-entry-val
                 :done d}]
    (reset! todo-items  (conj @todo-items new-item))
  (swap! todo-entry-key inc)))



;; [todo-entry-component]
;; create a text box
;; 1/ displayed value taken as reagent atom todo-entry-val
;; 2/ as text box changes, change todo-entry-val to same value
;; 3/ after ENTER key is pressed and entry is not empty string
;; then add that item to the todo list and clear text box
(defn todo-entry-component []
  [:input {:type "text"
           :value @todo-entry-val
           :on-change #(reset! todo-entry-val (-> % .-target .-value))
           :placeholder "something to do..."
           :on-key-up #(and
                        (not (= "" (-> % .-target .-value)))
                        (if (= "Enter" (.-key %))
                          [
                           (todo-add-item (-> % .-target .-value))
                           (reset! todo-entry-val "")
                           ]
                          ))
           }])




;; [todo-app]
;; shows a header To Do Application
;; displays stats on completed tasks
;; shows text box to enter new tasks
;; shows the todo items
(defn todo-app []
  [:div
   [:h1 "To Do Application todo.cljs"] 
   [:p (str "you have "
            (count-outstanding-tasks) " tasks outstanding "
            " and completed " (count-completed-tasks) " in a total of "
            (count-all-tasks) " tasks.") ]
   [todo-entry-component]
   [todo-items-component]
   ])


;; use reagent to render application
(reagent/render-component [todo-app]
                          (. js/document (getElementById "app")))

;; set title of document
(set! (.-title js/document) "ToDo Application")

;; populate todo app with some data
(doseq [a ["apples" "bananas" "pears"]]
  (todo-add-item a))

(todo-add-item2 "" false)
(todo-add-item2 "" true)


;;(.log js/console "hello world!")

;;(set! (.bgcolor js/document) "blue")

;; 
(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

