package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences ={
   val lc = w toLowerCase
   val gb = lc groupBy(c => c)
   val m = gb map(c => (c._1,c._2.length))
   m.toList.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString.filterNot(c => c.isWhitespace))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy(wordOccurrences(_))


  /** Returns all the anagrams of a given word.
    * LINK: https://twitter.github.io/scala_school/collections.html
    * */
  def wordAnagrams(word: Word): List[Word] = {
    val f = dictionaryByOccurrences.filter((m: (Occurrences, List[Word])) => m._1 == wordOccurrences(word))
    val m = f.map(_._2)
    if(m.iterator.hasNext) m.iterator.next() else List()
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   *
   *  LINKS: http://alvinalexander.com/scala/scala-for-comprehension-syntax-for-yield-loop-examples
    *
    *  TODO: Has to be extended for then 2 sets!
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def combinationsAcc(acc: Occurrences): List[Occurrences] = acc match{
      case h :: t => if(h._2 > 1) List(acc) ::: combinationsAcc(List((h._1, (h._2 - 1)))) else List(acc)
    }
    val ofl = occurrences.foldLeft(List[List[(Char, Int)]]())((r,c) => r ::: combinationsAcc(List(c)))
    val t = for(a <- ofl; b <- ofl.tail.tail; if(a.head._1 != b.head._1))yield List(a.head,b.head)
    List() +: (ofl ::: t)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   *  Example:
   *   val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
   *   val y = List(('r', 1))
   *
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = x diff y

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
    *
    *  LINK: https://www.tutorialspoint.com/scala/scala_strings.htm
    *
    *  TODO: Does not work right.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    if(sentence.isEmpty) return  List(Nil)

    val so: Occurrences = sentenceOccurrences(sentence)
    val cb: List[Occurrences] = combinations(so)

    def concat(elem: Occurrences, acc: Word): Word = elem match{
      case Nil => acc
      case h :: t => concat(t, h._1 + acc)
    }
    val tt: List[Word] = for(a <- cb; if(!a.isEmpty))yield concat(a, "").drop(0)
    for(a <- tt)yield wordAnagrams(a).filterNot(l => l.isEmpty)
  }
}
