/**
 * cse250.pa3.Trie.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:haiyizha
 * Person#:50287269
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import cse250.examples.graph.AdjacencyListGraph
import cse250.examples.types._

import scala.util.Sorting
import scala.util.control.Breaks
import cse250.examples.list._
import cse250.examples.types.immutable.ListADT
import org.graalvm.compiler.word.Word



class Trie {
  /** The graph to store the trie. */
  val _storageGraph = new AdjacencyListGraph[Int,Char]
  /** The root of the trie. */
  val _trieRoot = _storageGraph.insertVertex(0)

  var inlist = new LectureArrayList[LectureArrayList[String]]

  /** Inserts the given word into the trie graph. */
  def insert(word: String): Unit ={
    /** Inserts the given word into the trie graph. */
    var check3 = 0
    var ill = 0
    for(i<- 0 until inlist.length){
      if(inlist.apply(i).apply(0)== word){
        var ab = inlist.apply(i).apply(1).toInt + 1
        inlist.apply(i).update(1,ab.toString)
        check3 = 1
      }
    }
    if(check3 ==0){
      var my = new LectureArrayList[String]
      my.insert(0,word)
      my.insert(1,1.toString)
      inlist.insert(0,my)
      inlist.apply(0).update(0,word)
      inlist.apply(0).update(1,1.toString)

    }
    if(word == ""){
      _trieRoot._elem += 1}
      if(word.length == 1) {
        var check = 0
        val iter = _storageGraph.incidentEdges(_trieRoot)
        while (iter.hasNext){
          var ch = iter.next
          if(ch._elem==word(0)){
            ch.opposite(_trieRoot)._elem+=1
            check += 1
            return}}
        if(check == 0){
          _storageGraph.insertEdge(_trieRoot,_storageGraph.insertVertex(1),word(0))}}
    else{
        var check = 0
        var check2 = 0
        var a = _trieRoot
        var b = a
        for(i<-0 until word.length){
          check =  0
          b = a
          val iter = _storageGraph.edges
            while(iter.hasNext){
              var node =iter.next()
              if(node._elem==word(i)){
                if(node.endVertices(0)==b){
                  if(i == word.length-1){
                  node.opposite(a)._elem+=1
                }
                  b = a
                  a = node.opposite(a)
                  check += 1
                  check2 += 1
                }
              }
            }
          if(check == 0){
            if(i == word.length-1){
              var c = _storageGraph.insertVertex(1)
              _storageGraph.insertEdge(a,c,word(i))
              a = c
            }
            else{
              var c = _storageGraph.insertVertex(0)
              _storageGraph.insertEdge(a,c,word(i))
              a = c
            }
          }
        }

      }


  }

  /** Returns the number of times the given word was inserted. */
  def count(word: String): Int = {
    if(word == ""){
      return _trieRoot._elem
    }
    else{
      for(i<-0 until inlist.length){
        if(inlist.apply(i).apply(0)==word){
          return (inlist.apply(i).apply(1)).toInt
        }
      }
    }
    return 15
  }

  /** Returns the number of words stored within. */
  def length: Int = {
    return inlist.length
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[String] = new Iterator[String] {

    var currPos = inlist.length-1

    override def hasNext: Boolean = (currPos != -1)

    override def next(): String = {
      val retval = inlist.apply(currPos).apply(0)
      currPos -= 1
      retval
    }
  }

  /** Returns a sequence of all words of a given length ordered alphabetically. */
  def allWordsOfLength(length: Int): Seq[String] = {
    val pq: collection.mutable.PriorityQueue[String] = collection.mutable.PriorityQueue()
    var result: Seq[String] = Seq()
    var result2: Seq[String] = Seq()

    val iter = this.iterator
    while(iter.hasNext){
      pq.enqueue(iter.next())
    }
    result = pq.clone().dequeueAll

    for(i<-0 until result.length){
      if(result(i).length == length){
        result2 = result2 :+ result(i)
      }
    }
    return result2.reverse

  }

  /** Returns a sequence containing the k most inserted words.*/
  def mostCommon(k: Int): Seq[String] ={
    val pq: collection.mutable.PriorityQueue[String] = collection.mutable.PriorityQueue()
    var result: Seq[String] = Seq()
    var result2: Seq[LectureArrayList[String]] = Seq()
    var mylist = inlist
    if(k>=this.length){
      mylist
    }
    else{
      while(mylist.length != k){
        var index = 0
        for(i <- 0 until mylist.length){
          if(mylist.apply(index).apply(1).toInt > mylist.apply(i).apply(1).toInt){
            index = i
          }
        }
        mylist.remove(index)
      }
    }

    for(i<- 0 until mylist.length){
      result2 = result2 :+ mylist.apply(i)
    }
    println(mylist)
    val comparator:(LectureArrayList[String],LectureArrayList[String]) =>Boolean = (a:LectureArrayList[String], b:LectureArrayList[String]) =>if(a.apply(1).toInt == b.apply(1).toInt){a.apply(0) < b.apply(0)}else {a.apply(1).toInt < b.apply(1).toInt}
    result2 = result2.sortWith(comparator)
    println(result2)
    for(i <- 0 until result2.length){
      result = result :+ result2.apply(i).apply(0)
    }
    return result
  }

  /** Returns a sequence containing the k most inserted words that start with the given prefix. */
  def mostCommonWithPrefix(prefix: String, k: Int): Seq[String] = {
    var myl = new LectureArrayList[LectureArrayList[String]]
    var num = 0
    if(prefix == ""){
      return this.mostCommon(k)
    }
    else{
      for(i<-0 until this.length){
        var check = 0
        for(a<-0 until prefix.length){
          if(inlist.apply(i).apply(0).length<prefix.length){
            check += 1
          }
          else{
            if(inlist.apply(i).apply(0)(a) != prefix(a)){
              check += 1
            }
          }
        }
        if(check == 0){
          myl.insert(num,inlist.apply(i))
          num +=1
        }
      }
      var result: Seq[String] = Seq()
      var result2: Seq[LectureArrayList[String]] = Seq()
      if(k>=myl.length){
        myl
      }
      else{
        while(myl.length != k){
          var index = 0
          for(i <- 0 until myl.length){
            if(myl.apply(index).apply(1).toInt > myl.apply(i).apply(1).toInt){
              index = i
            }
          }
          myl.remove(index)
        }
      }

      for(i<- 0 until myl.length){
        result2 = result2 :+ myl.apply(i)
      }
      val comparator:(LectureArrayList[String],LectureArrayList[String]) =>Boolean = (a:LectureArrayList[String], b:LectureArrayList[String]) =>if(a.apply(1).toInt == b.apply(1).toInt){a.apply(0) < b.apply(0)}else {a.apply(1).toInt < b.apply(1).toInt}
      result2 = result2.sortWith(comparator)
      println(result2)
      for(i <- 0 until result2.length){
        result = result :+ result2.apply(i).apply(0)
      }
      println(result)
      return result
    }
  }
}
