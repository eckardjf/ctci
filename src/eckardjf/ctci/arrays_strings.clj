(ns eckardjf.ctci.arrays-strings
  (:import (java.util BitSet Arrays)))

(set! *warn-on-reflection* true)

;; Is Unique:
;;
;; Implement an algorithm to determine if a string has all unique characters.
;;
;; What if you cannot use additional data structures?

;; O(n)
(defn unique-chars? [s]
  (if (> (count s) 128)
    false
    (let [bs (BitSet. 128)]
      (loop [remaining (map int s)]
        (if (empty? remaining)
          true
          (let [[x & xs] remaining]
            (if (.get bs x)
              false
              (do
                (.set bs x)
                (recur xs)))))))))

(comment
  (unique-chars? "abc")
  (unique-chars? "aaa")
  )

;; O(n^2)
(defn unique-chars-in-place? [s]
  (loop [i 0]
    (if (= i (dec (count s)))
      true
      (if (loop [j (inc i)]
            (if (= j (count s))
              false
              (if (= (nth s i) (nth s j))
                true
                (recur (inc j)))))
        false
        (recur (inc i))))))

(comment
  (unique-chars-in-place? "abc")
  (unique-chars-in-place? "aaa")
  )

;; O(n log n)
(defn unique-chars-in-place-sort? [^chars arr]
  (Arrays/sort arr)
  (loop [i 0
         j 1]
    (if (= j (alength arr))
      true
      (if (= (aget arr i) (aget arr j))
        false
        (recur (inc i) (inc j))))))

(comment
  (unique-chars-in-place-sort? (char-array "abc"))
  (unique-chars-in-place-sort? (char-array "aaa"))
  )

;; Check Permutation:
;;
;; Given two strings, write a method to decide if one is a permutation of the other.

(defn permutation-sort? [s1 s2]
  (if (not= (count s1) (count s2))
    false
    (= (sort s1) (sort s2))))

(comment
  (permutation-sort? "abc" "bca")
  (permutation-sort? "abc" "bc")
  )

(defn permutation? [s1 s2]
  (if (not= (count s1) (count s2))
    false
    (let [letters (int-array 128)]
      (doseq [x (map int s1)]
        (aset letters x (inc (aget letters x))))
      (loop [remaining (map int s2)]
        (if (empty? remaining)
          true
          (let [[x & xs] remaining]
            (aset letters x (dec (aget letters x)))
            (if (< (aget letters x) 0)
              false
              (recur xs))))))))

(comment
  (permutation? "abc" "bca")
  (permutation? "abc" "bc")
  )

;; URLify:
;;
;; Write a method to replace all spaces in a string with '%20:

;; You may assume that the string has sufficient space at the end to hold the additional characters,
;; and that you are given the "true" length of the string.
;;
;; (Note: If implementing in Java, please use a character array so that you can perform this operation in place.)

(defn urlify [^chars arr len]
  (let [num-spaces (reduce (fn [n c]
                             (if (= \space c) (inc n) n))
                           0 (take len arr))
        true-length (+ len (* 2 num-spaces))]
    (when (< true-length (alength arr))
      (aset arr true-length (char 0)))
    (loop [i (dec len)
           j (dec true-length)]
      (when-not (neg? i)
        (let [c (aget arr i)]
          (if (= \space c)
            (do
              (aset arr j \0)
              (aset arr (dec j) \2)
              (aset arr (- j 2) \%)
              (recur (dec i) (- j 3)))
            (do
              (aset arr j c)
              (recur (dec i) (dec j)))))))))

(comment
  (let [arr (char-array "Mr John Smith         ")]
    (urlify arr 13)
    (apply str (take-while #(not= (char 0) %) arr)))
  )

;; Palindrome Permutation:
;;
;; Given a string, write a function to check if it is a permutation of a palindrome.
;; A palindrome is a word or phrase that is the same forwards and backwards.
;; A permutation is a rearrangement of letters.
;; The palindrome does not need to be limited to just dictionary words.

(defn letter? [^Character c]
  (Character/isLetter c))

(defn lower-case [^Character c]
  (Character/toLowerCase c))

(defn palindrome-permutation-easy? [s]
  (->> s
       (eduction (map lower-case) (filter letter?))
       (frequencies)
       (vals)
       (filter odd?)
       (count)
       (>= 1)))

(comment
  (palindrome-permutation-easy? "Tact Coa")
  )

(defn palindrome-permutation? [s]
  (let [bs (BitSet. 128)]
    (loop [i 0]
      (if (= i (count s))
        ;; this should call Long/bitCount once
        (<= (.cardinality bs) 1)
        (let [c (nth s i)]
          (when (letter? c)
            (.flip bs (int (lower-case c))))
          (recur (inc i)))))))

(comment
  (palindrome-permutation? "Tact Coa")
  )

;; One Away:
;;
;; There are three types of edits that can be performed on strings:
;; insert a character, remove a character, or replace a character.
;;
;; Given two strings, write a function to check if they are one edit (or zero edits) away.

(defn one-insert-away? [s1 s2]
  (loop [i1 0
         i2 0
         diff false]
    (if (= i1 (count s1))
      true
      (if (not= (nth s1 i1) (nth s2 i2))
        (if diff
          false
          (if (not= (nth s1 i1) (nth s2 (inc i2)))
            false
            (recur (inc i1) (+ 2 i2) true)))
        (recur (inc i1) (inc i2) diff)))))

(defn one-replace-away? [s1 s2]
  (loop [i 0
         diff false]
    (if (= i (count s1))
      true
      (if (not= (nth s1 i) (nth s2 i))
        (if diff
          false
          (recur (inc i) true))
        (recur (inc i) diff)))))

(defn one-edit-away? [s1 s2]
  (if (> (Math/abs (- (count s1) (count s2))) 1)
    false
    (cond
      (= (count s1) (count s2)) (one-replace-away? s1 s2)
      (< (count s1) (count s2)) (one-insert-away? s1 s2)
      :else (one-insert-away? s2 s1))))

(comment
  (one-replace-away? "pale" "bale")
  (one-insert-away? "ple" "pale")
  (one-edit-away? "pale" "bale")
  (one-edit-away? "ple" "pale")
  )

;; String Compression:
;;
;; Implement a method to perform basic string compression using the counts of repeated characters.
;; For example, the string aabcccccaaa would become a2b1c5a3.
;;
;; If the "compressed" string would not become smaller than the original string,
;; your method should return the original string.
;;
;; You can assume the string has only uppercase and lowercase letters (a - z).

(defn compress-easy [s]
  (->> s
       (partition-by identity)
       (mapcat (juxt first count))
       (apply str)))

(comment
  (compress-easy "aabcccccaaa"))

(defn compress [s]
  (let [sb (StringBuilder.)]
    (loop [i 0]
      (if (= i (count s))
        (.toString sb)
        (let [n (loop [j (inc i)]
                  (if (or (= j (count s))
                          (not= (nth s i) (nth s j)))
                    (- j i)
                    (recur (inc j))))]
          (.append sb (nth s i))
          (.append sb n)
          (recur (+ i n)))))))

(comment
  (compress "aabcccccaaa")
  )

;; Rotate Matrix:
;;
;; Given an image represented by an NxN matrix, where each pixel in the image is 4 bytes,
;; write a method to rotate the image by 90 degrees. Can you do this in place?




;; Zero Matrix:
;;
;; Write an algorithm such that if an element in an MxN matrix is 0, its entire row and column are set to O.



;; String Rotation:
;;
;; Assume you have a method isSubstring which checks if one word is a substring of another. Given two strings,
;; s1 and s2, write code to check if s2 is a rotation of s1 using only one call to isSubstring
;; (e.g.,"waterbottle"is a rotation of"erbottlewat").
