(ns clj-bootstrap.components
  (require [hiccup.core :as h]
           [hiccup.def :as def]
           [hiccup.element :as elem]
           [hiccup.util :as util])
  ;; just for testing...DEL
  (use [compojure core response]
       [ring.adapter.jetty :only [run-jetty]]
       [ring.util.response]
       [ring.middleware file file-info stacktrace reload])
  (require [compojure.route :as route]
           [hiccup.page :as page]))


;; TODO:buttons

;; navbar

(def/defelem navbar-inner [& content]
  [:div.navbar-inner content])

(def/defelem container [& content]
  [:div.container content])

(def/defelem navbar [content & [loc]]
  (let [navbar-class
        (fn [loc]
          (cond (= loc :top)
                "navbar navbar-fixed-top"
                (= loc :bottom)
                "navbar navbar-fixed-bottom"
                :else
                "navbar"))]
    [:div {:class (navbar-class loc)} content]))

(defmacro defnavbar [& [loc]]
  (let [name# (symbol (if loc (str "navbar-fixed-" (util/to-str loc)) "navbar-float"))]
    `(def/defhtml ~name# [& ~'content]
       (-> ~'content
           (container)
           (navbar-inner)
           (navbar ~loc)))))

(defnavbar)
(defnavbar :top)
(defnavbar :bottom)

(def/defelem divider-vertical []
  [:li.divider-vertical])

(def/defelem divider-horizontal []
  [:li.divider-horizontal])

(def/defelem brand [url & content]
  ((def/wrap-attrs elem/link-to) {:class "brand"} url content))

(def/defelem nav-links [coll & [active]]
  [:ul {:class "nav"} (for [x coll]
             (if (= x active)
               [:li.active x]
               [:li x]))])

(defn wrap-attrs-with-merge-fn
  "Add an optional attribute argument to a function that returns a element vector."
  [func & [f]]
  (let [merge-fn (or (partial merge-with f) merge)]
    (fn [& args]
      (if (map? (first args))
        (let [[tag & body] (apply func (rest args))]
          (if (map? (first body))
            (apply vector tag (merge-fn (first body) (first args)) (rest body))
            (apply vector tag (first args) body)))
        (apply func args)))))

(defn- merge-class [x y]
  (str x " " y))

(defn pull-left [elem]
  (partial (wrap-attrs-with-merge-fn elem merge-class) {:class "pull-left"}))

(defn pull-right [elem]
  (partial (wrap-attrs-with-merge-fn elem merge-class) {:class "pull-right"}))

(def/defelem navbar-form [content]
  [:form {:class "navbar-form"} content])

(def/defelem navbar-search [content]
  [:form {:class "navbar-search"} content])

(def/defelem navbar-form-input [& [span]]
  [:input {:type "text" :class span}])

(def/defelem navbar-search-input [& [span]]
  [:input {:type "text" :class (str span " " "search-query") :placeholder "Search"}])


;; Just for testing...remove

(def header-includes
  {:bootstrap-responsive (page/include-css "/css/bootstrap-responsive.css")
   :bootstrap (page/include-css "/css/bootstrap.css")})

(def/defhtml page-header [& incls]
  (page/html5
   (h/html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:meta {:name "description" :content ""}]
     [:title "kokous"]
     (map #(get header-includes %) incls)])))

(def/defhtml page [& content]
  (page-header :bootstrap :bootstrap-responsive)
  [:body content])

(defroutes test-routes
  (GET "/" [] (page (h/html [:h1 "HELLO"])))
  (route/not-found (h/html [:h1 "NOT FOUND"])))

(defn test-app
  []
  (-> test-routes
      (wrap-reload '(clj-bootstrap.components))
      (wrap-file "resources/public")
      (wrap-stacktrace)))

(defn start-server
  [& _]
  (run-jetty (test-app) {:port 8083 :join? false}))

(def *server* (agent (start-server)))

(defn restart-server []
  (send *server* (fn [a] (.stop a)))
  (send *server* start-server))
