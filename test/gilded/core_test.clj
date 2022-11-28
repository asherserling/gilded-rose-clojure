(ns gilded.core-test
  (:require [clojure.test :refer [deftest is]]
            [gilded.core :as x]))

(defn make-store-update-n-times [updater fixture n]
  (let [store (x/make-store fixture)
        update! (fn [_]
                  (updater store)
                  (x/item-seq store))]
    (take n (iterate update! nil))))

(deftest test-with-original-fixture
  ;; removed conjured item; behavioral discrepancy expected 
  (let [original-fixture [{:name "+5 Dexterity Vest", :quality 20, :sell-in 10}
                          {:name "Aged Brie", :quality 0, :sell-in 2}
                          {:name "Elixir of the Mongoose", :quality 7, :sell-in 5}
                          {:name "Sulfuras, Hand of Ragnaros", :quality 80, :sell-in 0}
                          {:name "Sulfuras, Hand of Ragnaros", :quality 80, :sell-in -1}
                          {:name "Backstage passes to a TAFKAL80ETC concert", :quality 20, :sell-in 15}
                          {:name "Backstage passes to a TAFKAL80ETC concert", :quality 49, :sell-in 10}
                          {:name "Backstage passes to a TAFKAL80ETC concert", :quality 49, :sell-in 5}]]
    (is (= (make-store-update-n-times x/original-update-quality! original-fixture 100)
           (make-store-update-n-times x/update-quality! original-fixture 100)))))

(deftest regular-item-decreases-by-1-before- 
  (let [item {:name "foo" :quality 10 :sell-in 2}
        updated-item (x/update-item item)]
    (is (= (:quality updated-item) 9))))

(deftest regular-item-decreases-by-2-after-sell-date
  (let [item (x/update-item {:name "foo" :quality 10 :sell-in -1})]
    (is (= (:quality item) 8))))

(deftest make-quality-degrader-degrades-one-unit-when-not-passed-sell-date
  (let [degrader (x/make-quality-degrader 1)
        item {:quality 10 :sell-in 1}
        new-quality (degrader item)]
    (is (= 9 new-quality))))

(deftest quality-of-item-never-negative
  (let [item {:quality 0 :sell-in -1}
        after-a-long-time (nth (iterate x/update-item item) 1000)]
    (is (not (< (:quality after-a-long-time) 0)))))

(def some-aged-brie {:name "Aged Brie" :quality 10 :sell-in 1})

(deftest aged-brie-increases-in-quality
  (let [updated-item (x/update-item some-aged-brie)]
    (is (= (:quality updated-item) 11))))

(deftest aged-brie-doubles-increase-in-quality-after-sell-in
  (let [updated-item (x/update-item {:name "Aged Brie" :quality 0 :sell-in -1})]
    (is (= (:quality updated-item) 2))))

(deftest quality-never-more-than-50
  (let [item (nth (iterate x/update-item some-aged-brie) 1000)]
    (is (not (> (:quality item) 50)))))

(defn get-updated-conjured-quality-for-days [days initial-quality]
  (let [example-items (map
                       (fn [day] {:name "Conjured Mana Cake"
                                  :quality initial-quality
                                  :sell-in day})
                       days)]
    (map (comp :quality x/update-item) example-items)))

(deftest conjured-items-degrade-two-before-sell-date
  (let [updated-qualities (get-updated-conjured-quality-for-days 
                           [2 3 4 5 6 7 8 9 10] 
                           10)]
    (is (every? #(= 8 %) updated-qualities))))

(deftest conjured-items-degrade-four-after-sell-date
  (let [update-qualities (get-updated-conjured-quality-for-days
                          [0 -1 -2 -3 -4 -5 -6]
                          10)] 
    (is (every? #(= 6 %) update-qualities))))
