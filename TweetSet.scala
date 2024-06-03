package objsets

import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet {
  
    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

    def isEmpty: Boolean

  
    def union(that: TweetSet): TweetSet
  
  
    def mostRetweeted: Tweet
  
  
    def descendingByRetweet: TweetList
  
 
    def incl(tweet: Tweet): TweetSet

  
    def remove(tweet: Tweet): TweetSet

  
    def contains(tweet: Tweet): Boolean

  
    def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

    def contains(tweet: Tweet): Boolean = false

    def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

    def remove(tweet: Tweet): TweetSet = this
    
    def foreach(f: Tweet => Unit): Unit = ()

    override def isEmpty: Boolean = true

    override def union(that: TweetSet): TweetSet = that

    override def mostRetweeted: Tweet = throw new NoSuchElementException

    override def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      val subTrees = left.filterAcc(p, acc).union(right.filterAcc(p, acc))
      if (p(elem)) subTrees.incl(elem)
      else subTrees
    }
  

    def contains(x: Tweet): Boolean ={
        if (x.text < elem.text) left.contains(x)
        else if (elem.text < x.text) right.contains(x)
        else true
    }

    def incl(x: Tweet): TweetSet = {
        if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
        else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
        else this
     }

    def remove(tw: Tweet): TweetSet =
        if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
        else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
        else left.union(right)

    def foreach(f: Tweet => Unit): Unit = {
        f(elem)
        left.foreach(f)
        right.foreach(f)
    }

    override def isEmpty: Boolean = false

 
    override def union(that: TweetSet): TweetSet = {
        right.union(left.union(that.incl(elem)))
    }

    override def mostRetweeted: Tweet = {
        val moreRetweeted = right.union(left).filter(t => t.retweets > elem.retweets)
        if (moreRetweeted.isEmpty) elem
        else moreRetweeted.mostRetweeted
    }
  
    override def descendingByRetweet: TweetList = {
        def loop(source: TweetSet, result: TweetList): TweetList = {
          if (source.isEmpty) result
          else {
        val headTweet = source.mostRetweeted
        new Cons(headTweet, loop(source.remove(headTweet), result))
      }
    }
        loop(this, Nil)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}