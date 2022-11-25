(ns clj-diff.test.custom-comparator
  (:use [clj-diff.core]
        [clojure.test]))

(defn ignore-case [a b]
  (cond
    (and (char? a) (char? b)) (= (Character/toLowerCase ^char a) (Character/toLowerCase ^char b))
    (and (string? a) (string? b)) (= (.toLowerCase ^String a) (.toLowerCase ^String b))
    :else (= a b)))

(deftest diff-test
  (let [t (fn [a b] (edit-distance (diff a b {:compare ignore-case})))]

    (is (= (edit-distance (diff "aaaabbcc" "AAAAbBcC"))
          12))
    (is (= (edit-distance (diff "aaaabbcc" "AAAAbBcCz"))
          13))
    (is (= (edit-distance (diff "aaaabbcc" "AAAAbBcC" {:compare ignore-case}))
          0))
    (is (= (edit-distance (diff "aaaabbcc" "AAAAbBcCz" {:compare ignore-case}))
          1))))

(deftest patch-test
  (is (= (patch "aaaabbcc"
           (diff "aaaabbcc" "AAAAbBcC" {:compare ignore-case}))
        "aaaabbcc"))

  (is (= (patch "aaaabbcc"
           (diff "aaaabbcc" "AAAAbBcCz" {:compare ignore-case}))
        "aaaabbccz"))

  (is (= (patch "aaaabbcc"
           (diff "aaaabbcc" "AAAAbBcCZ" {:compare ignore-case}))
        "aaaabbccZ")))


(deftest edit-distance-test
  (is (= (edit-distance (diff "aba" "aBa"))
        2))
  (is (= (edit-distance (diff "aba" "aBa" {:compare ignore-case}))
         0))
  (is (= (edit-distance (diff "aBcaBBa" "cbabac"))
        7))
  (is (= (edit-distance (diff "aBcaBBa" "cbabac" {:compare ignore-case}))
         5))
  (is (= (edit-distance (diff "nBP8GaFHVls2dI8h9aK1FWdRgevf43"
                              "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v"
                          {:compare ignore-case}))
         44)))

(deftest longest-common-subseq-test
  (are [a b _ d] (= (longest-common-subseq a b {:compare ignore-case}) d)
       "aba" "aBa"         => "aba"
       "aBa" "aba"         => "aBa"
       "aba" "Ada"         => "aa"
       "abA" "ada"         => "aA"
       "abca" "aCa"        => "aca"
       "abCa" "aca"        => "aCa"
       "abma" "Aca"        => "aa"
       "Abma" "aca"        => "Aa"
       "kitten" "sitting"  => "ittn"
       "Saturday" "Sunday" => "Suday"
       "gumbo" "gambol"    => "gmbo"
       "nBP8GaFHVls2dI8h9aK1FWdRgevf43" "925BCPcYhT5hs8L9T3K2T5C7U3Lz5v" =>
       "BPHs89Kv"))

(deftest longest-common-subseq-seq-test
  (are [a b _ d] (= (longest-common-subseq (seq a) (seq b) {:compare ignore-case}) d)
       "kitTen" "sitting"  => [\i \t \T \n]
       "Saturday" "SuNday" => [\S \u \d \a \y]
       "gUMbo" "gamBOl"    => [\g \M \b \o]))
