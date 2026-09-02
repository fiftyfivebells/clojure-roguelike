(ns roguelike.monster-test
  (:require [clojure.test :refer [deftest is testing]]
            [roguelike.monster :as monster]
            [roguelike.monster.catalog :as catalog]
            [roguelike.rng :as rng]
            [roguelike.ui.appearance.monster :as appearance]))

;;; Helpers

(defn- all-species-in-genus
  [[_genus-key genus]]
  (keys (:species genus)))

(def ^:private all-catalog-species
  (into #{} (mapcat all-species-in-genus) catalog/genera))

(defn- resolved-templates
  "Every species pulled through the public template fn, as {species template}."
  []
  (into {} (map (juxt identity monster/template)) all-catalog-species))

;;; Resolution correctness

(deftest every-catalog-species-resolves-to-a-template
  (testing "every species in catalog/genera appears in the resolved templates"
    (doseq [species all-catalog-species]
      (is (some? (monster/template species))
          (str species " missing from resolved templates")))))

(deftest resolved-templates-carry-their-genus-and-species
  (testing "each resolved template's genus/species keys match where it was defined"
    (doseq [[genus-key {:keys [species]}] catalog/genera
            species-key (keys species)]
      (let [tmpl (monster/template species-key)]
        (is (= genus-key (:monster/genus tmpl)))
        (is (= species-key (:monster/species tmpl)))))))

(deftest genus-level-keys-are-inherited-by-every-species
  (testing "a genus-level key like :monster/flags is present on every species in that genus"
    (doseq [[_genus-key {:keys [species] :as genus}] catalog/genera
            :let [inherited (dissoc genus :species)]
            species-key (keys species)
            [k v] inherited]
      (is (= v (get (monster/template species-key) k))
          (str species-key " missing inherited " k)))))

(deftest species-keys-are-namespaced-to-their-own-genus
  (testing "a species key's keyword namespace matches the genus it's defined under"
    ;; catches a copy-pasted genus block whose species keys weren't renamed,
    ;; which would otherwise produce a working monster filed under the wrong
    ;; genus with the wrong flags
    (doseq [[genus-key {:keys [species]}] catalog/genera
            species-key (keys species)]
      (is (= (name genus-key) (namespace species-key))
          (str species-key " is not namespaced to " genus-key)))))

(deftest no-species-key-is-lost-in-the-flatten
  (testing "the sum of species counts per genus equals the resolved map's count"
    ;; duplicate keys across genera would silently vanish without this check
    (let [expected (reduce + (map (comp count :species val) catalog/genera))]
      (is (= expected (count all-catalog-species)))
      (is (= expected (count (resolved-templates)))))))

;;; Required keys

(deftest every-resolved-template-has-the-required-keys
  (testing "every species has :depth, :group/size, :sight/radius, :hp, :hp-variance, :to-hit, :armor-class, :attacks"
    (doseq [[species tmpl] (resolved-templates)]
      (doseq [k [:depth :group/size :sight/radius :hp :hp-variance :to-hit :armor-class :attacks]]
        (is (contains? tmpl k) (str species " missing " k))))))

;;; Catalog sanity

(deftest group-size-is-a-valid-lo-hi-pair
  (testing ":group/size is [lo hi] with lo <= hi and lo >= 1"
    (doseq [[species tmpl] (resolved-templates)]
      (let [[lo hi] (:group/size tmpl)]
        (is (<= 1 lo) (str species " group/size lo below 1"))
        (is (<= lo hi) (str species " group/size lo above hi"))))))

(deftest depth-is-a-non-negative-integer
  (testing "0-indexed depths are valid; depth 0 is the first dungeon level"
    (doseq [[species tmpl] (resolved-templates)]
      (is (integer? (:depth tmpl)) (str species " depth is not an integer"))
      (is (>= (:depth tmpl) 0) (str species " depth is negative")))))

(deftest every-blow-has-a-valid-shape
  (testing "each attack has :method, :effect, and, where it deals damage, :dice >= 1 and :sides >= 1"
    (doseq [[species tmpl] (resolved-templates)
            blow (:attacks tmpl)]
      (is (some? (:method blow)) (str species " blow missing :method"))
      (is (some? (:effect blow)) (str species " blow missing :effect"))
      (when (or (:dice blow) (:sides blow))
        (is (>= (:dice blow) 1) (str species " blow dice below 1"))
        (is (>= (:sides blow) 1) (str species " blow sides below 1"))))))

;;; Appearance drift
;; this test deliberately crosses from a content namespace (catalog) into a
;; UI namespace (appearance), since it's the only place that knows both sides

(def ^:private appearance-genera
  @#'appearance/genus->appearance)

(def ^:private appearance-species
  @#'appearance/species->appearance)

(deftest appearance-genus-keys-match-the-catalog
  (testing "the set of genus keys in the appearance tables equals the catalog's"
    (is (= (set (keys catalog/genera)) (set (keys appearance-genera))))))

(deftest appearance-species-keys-match-the-resolved-catalog
  (testing "the set of species keys in the appearance tables equals the resolved species"
    (is (= all-catalog-species (set (keys appearance-species))))))

(deftest no-species-key-is-lost-merging-appearance-tables-flat
  (testing "summed per-genus appearance species counts equal the flattened map's count"
    (let [expected (reduce + (map (comp count :species val) appearance-genera))]
      (is (= expected (count appearance-species))))))

;;; species-at-depth

(deftest species-at-depth-uses-the-depth-3-to-depth-plus-2-band
  (testing "boundary depths are included, one step outside is not"
    (let [depth 5
          in-band? (set (monster/species-at-depth depth))]
      (doseq [[species tmpl] (resolved-templates)]
        (if (<= (- depth 3) (:depth tmpl) (+ depth 2))
          (is (contains? in-band? species) (str species " should be in band at depth " depth))
          (is (not (contains? in-band? species)) (str species " should not be in band at depth " depth)))))))

(deftest species-at-depth-is-never-empty-across-the-catalogs-depth-range
  (testing "every depth from 0 through the deepest catalog depth has at least one eligible species"
    ;; an empty result means a level with no monsters and no error - a silent
    ;; failure worth catching at test time
    (let [max-depth (apply max (map :depth (vals (resolved-templates))))]
      (doseq [depth (range 0 (inc max-depth))]
        (is (seq (monster/species-at-depth depth))
            (str "no eligible species at depth " depth))))))

;;; instantiate

(deftest instantiate-produces-a-live-entity-shape
  (testing "the entity has :entity/type :monster, the given :pos, and equal current/max hp"
    (let [ctx {:rng-state (rng/make 42)}
          [_ entity] (monster/instantiate ctx :orc/goblin [3 4])]
      (is (= :monster (:entity/type entity)))
      (is (= [3 4] (:pos entity)))
      (is (= (:hp/current entity) (:hp/max entity))))))

(deftest instantiate-strips-spawn-only-keys
  (testing "spawn-only keys don't survive onto the instance"
    (let [ctx {:rng-state (rng/make 42)}
          [_ entity] (monster/instantiate ctx :orc/goblin [3 4])]
      (doseq [k [:depth :group/size :hp :hp-variance]]
        (is (not (contains? entity k)) (str k " leaked onto the instance"))))))

(deftest instantiate-keeps-genus-and-species
  (testing "genus and species survive onto the instance for the renderer and message layer"
    (let [ctx {:rng-state (rng/make 42)}
          [_ entity] (monster/instantiate ctx :orc/goblin [3 4])]
      (is (= :orc (:monster/genus entity)))
      (is (= :orc/goblin (:monster/species entity))))))

(deftest instantiate-hp-varies-within-the-variance-band
  (testing "hp across seeds stays within the [base - spread, base + spread] band and varies"
    (let [base (:hp (monster/template :orc/hill-orc))
          pct (:hp-variance (monster/template :orc/hill-orc))
          spread (int (* base pct))
          low (max 1 (- base spread))
          high (+ base spread)
          hps (map (fn [s]
                     (let [[_ entity] (monster/instantiate {:rng-state (rng/make s)}
                                                            :orc/hill-orc [0 0])]
                       (:hp/current entity)))
                   (range 1 30))]
      (is (every? #(<= low % high) hps))
      ;; the band is narrow (a handful of integers), so requiring every draw
      ;; to be distinct would be flaky by pigeonhole; more than one distinct
      ;; value across 30 seeds is enough to show the roll is actually happening
      (is (< 1 (count (distinct hps)))
          "no variance at all across 30 seeds would suggest the roll isn't happening"))))

(deftest instantiate-advances-the-ctx
  (testing "the returned ctx differs from the input, since the hp roll consumes randomness"
    (let [ctx {:rng-state (rng/make 42)}
          [ctx' _] (monster/instantiate ctx :orc/goblin [0 0])]
      (is (not= ctx ctx')))))
