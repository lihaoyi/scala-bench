package bench

import scala.collection.mutable

case class Benchmark(name: String,
                     cases: Benchmark.Case[_]*)
object Benchmark{
  case class Case[T](name: String,
                     initializer: Int => T)
                    (callback: T => Any){
    def run(n: Int) = callback(initializer(n))
  }
  def pair[T](t: => T) = (t, t)
  val nullO: Object = null
  def obj = new Object()
  val benchmarks = Seq(
    Benchmark(
      "construct",
      Case("List", n=>n){ n =>
        var b = List.empty[Object]
        var i = 0
        while(i < n){
          b = obj :: b
          i += 1
        }
        b
      },
      Case("Vector", n=>n){ n =>
        var b = Vector.empty[Object]
        var i = 0
        while(i < n){
          b = obj +: b
          i += 1
        }
        b
      },
      Case("Set", n=>n){ n =>
        var b = Set.empty[Object]
        var i = 0
        while(i < n){
          b = b + obj
          i += 1
        }
        b
      },
      Case("Map", n=>n){ n =>
        var b = Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b = b + (obj -> obj)
          i += 1
        }
        b
      },
      Case("Array", n=>n){ n =>
        var b = new Array[Object](0)
        var i = 0
        while(i < n){
          b = b :+ obj
          i += 1
        }
        b
      },
      Case("Array-prealloc", n=>n){ n =>
        val b = new Array[Object](n)
        var i = 0
        while(i < n){
          b(i) = obj
          i += 1
        }
        b
      },
      Case("m.Buffer", n=>n){ n =>
        val b = mutable.Buffer.empty[Object]
        var i = 0
        while(i < n){
          b.append(obj)
          i += 1
        }
        b
      },
      Case("m.Set", n=>n){ n =>
        val b = mutable.Set.empty[Object]
        var i = 0
        while(i < n){
          b.add(obj)
          i += 1
        }
        b
      },
      Case("m.Map", n=>n){ n =>
        val b = mutable.Map.empty[Object, Object]
        var i = 0
        while(i < n){
          b.put(obj, obj)
          i += 1
        }
        b
      }
    ),
    Benchmark(
      "concat",
      Case("List", x => pair(List.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("Vector", x => pair(Vector.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("Set", x => pair(Array.fill(x)(obj).toSet)){ case (a, b) =>
        a ++ b
      },
      Case("Map", x => pair(Array.fill(x)(obj -> obj).toMap)){ case (a, b) =>
        a ++ b
      },
      Case("Array", x => pair(Array.fill(x)(obj))){ case (a, b) =>
        a ++ b
      },
      Case("m.Buffer", x => pair(mutable.Buffer.fill(x)(obj))){ case (a, b) =>
        a.appendAll(b)
      },
      Case("m.Set", x => pair(Array.fill(x)(obj).to[mutable.Set])){ case (a, b) =>
        a ++= b
      },
      Case("m.Map", x => pair(mutable.Map(Array.fill(x)(obj -> obj):_*))){ case (a, b) =>
        a ++= b
      }
    ),
    Benchmark(
      "deconstruct",
      Case("List.tail", List.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("Vector.tail", Vector.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("Vector.init", Vector.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.init
        x
      },
      // SLOW
      Case("Set.tail", Array.fill(_)(obj).toSet){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("Set.-", Array.fill(_)(obj).toSet){ a =>
        var x = a
        while(x.nonEmpty) x = x - x.head
        x
      },
      // SLOW
      //        Case("Map.tail", Array.fill(_)(obj -> obj).toMap){ a =>
      //          var x = a
      //          while(x.nonEmpty) x = x.tail
      //          x
      //        },
      Case("Map.-", Array.fill(_)(obj -> obj).toMap){ a =>
        var x = a
        while(x.nonEmpty) x = x.-(x.head._1)
        x
      },
      Case("Array.tail", Array.fill(_)(obj)){ a =>
        var x = a
        while(x.nonEmpty) x = x.tail
        x
      },
      Case("m.Buffer.remove", x => mutable.Buffer.fill(x)(obj)){ a =>
        while (a.nonEmpty) a.remove(a.length-1)
        a
      },
      Case("m.Set.remove", Array.fill(_)(obj).to[mutable.Set]){ a =>
        while (a.nonEmpty) a.remove(a.head)
        a
      },
      Case("m.Map.remove", x => mutable.Map(Array.fill(x)(obj -> obj):_*)){ a =>
        while (a.nonEmpty) a.remove(a.head._1)
        a
      }
    ),
    Benchmark(
      "foreach",
      Case("List", List.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        while(i < 100) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Vector", Vector.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        while(i < 100) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Set", Array.fill(_)(obj).toSet){ a =>
        var i = 0
        var last = nullO
        while(i < 100) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Map", Array.fill(_)(obj -> obj).toMap){ a =>
        var i = 0
        var last = nullO
        while(i < 100) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Array", Array.fill(_)(obj)){ a =>
        var i = 0
        var last = nullO
        while(i < 100) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("Array-while", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var last = nullO
        var i = 0
        while(i < 10) {
          var j = 0
          while(j < n){
            last = a(j)
            j += 1
          }
          i += 1
        }
        last
      },
      Case("m.Buffer", x => mutable.Buffer.fill(x)(obj)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("m.Set", Array.fill(_)(obj).to[mutable.Set]){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      },
      Case("m.Map", x => mutable.Map(Array.fill(x)(obj -> obj):_*)){ a =>
        var last = nullO
        var i = 0
        while(i < 10) {
          a.foreach(last = _)
          i += 1
        }
        last
      }
    ),
    Benchmark(
      "lookup",
      Case("List", x => x -> List.fill(x)(obj)){ case (n, a) =>
        var i = 0
        var last = nullO
        while(i < n){
          last = a(i)
          i += 1
        }
        last
      },
      Case("Vector", x => x -> Vector.fill(x)(obj)){ case (n, a) =>
        var i = 0
        var last = nullO
        while(i < n){
          last = a(i)
          i += 1
        }
        last
      },
      Case("Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.toSet
      }){ case (keys, a) =>
        var i = 0
        val n = keys.length
        var last = false
        while(i < n){
          last = a(keys(i))
          i += 1
        }
        last
      },
      Case("Map", x => {
        val r = Array.fill(x)(obj -> obj).toMap
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var i = 0
        val n = keys.length
        var last = nullO
        while(i < n){
          last = a(keys(i))
          i += 1
        }
        last
      },
      Case("Array", x => x -> Array.fill(x)(obj)){ case (n, a) =>
        var i = 0
        var last = nullO
        while(i < n){
          last = a(i)
          i += 1
        }
        last
      },
      Case("m.Buffer", x => x -> mutable.Buffer.fill(x)(obj)){ case (n, a) =>
        var i = 0
        var last = nullO
        while(i < n){
          last = a(i)
          i += 1
        }
        last
      },
      Case("m.Set", x => {
        val r = Array.fill(x)(obj)
        r -> r.to[mutable.Set]
      }){ case (keys, a) =>
        var last = false
        val n = keys.length
        var i = 0
        while(i < n){
          last = a(keys(i))
          i += 1
        }
        last
      },
      Case("m.Map", x => {
        val r = mutable.Map(Array.fill(x)(obj -> obj):_*)
        r.keysIterator.toArray -> r
      }){ case (keys, a) =>
        var last = nullO
        var i = 0
        val n = keys.length
        while(i < n){
          last = a(keys(i))
          i += 1
        }
        last
      }
    )
  )

}
