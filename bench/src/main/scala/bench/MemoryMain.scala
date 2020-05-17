package bench
import java.text.NumberFormat
import java.util.Locale

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue
import scala.collection.{SortedSet, mutable}
import scala.reflect.ClassTag
object MemoryMain{
  def main(args: Array[String]): Unit = {
    def obj = new Object()
    def nums[T:ClassTag](n: Int, f: Int => T) = (0 until n).iterator.map(f).toArray
    val collections = Seq[(String, Int => AnyRef)](
      ("Vector",          n => Vector(nums(n, _ => obj):_*)),
      ("Array",           n => Array(nums(n, _ => obj):_*)),
      ("List",            n => List(nums(n, _ => obj):_*)),
      ("Set",             n => Set(nums(n, _ => obj):_*)),
      ("Map",             n => Map(nums(n, _ => (obj, obj)):_*)),

      ("SortedSet", n => SortedSet(nums(n, x=>x):_*)),
      ("Queue",     n => Queue(nums(n, _ => obj):_*)),

      ("m.Map",       n =>mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*)),
        ("m.Buffer",    n =>mutable.Buffer(nums(n, _ => obj):_*)),
        ("m.Set",       n =>mutable.Set(nums(n, _ => obj):_*)),
      ("m.Queue",     n =>mutable.Queue(nums(n, _ => obj):_*)),
      ("m.PriQueue",  n =>mutable.PriorityQueue(nums(n, x=>x):_*)),
      ("m.SortedSet", n =>mutable.SortedSet(nums(n, x=>x):_*)),

      ("String",  "1" * _),

      ("ArrayBoolean",  n => nums(n, _ % 2 == 0)),
      ("ArrayByte",     n => nums(n, _.toByte)),
      ("ArrayShort",    n => nums(n, _.toShort)),
      ("ArrayInt",      n => nums(n, _.toInt)),
      ("ArrayLong",     n => nums(n, _.toLong)),

      ("BoxArrayBoolean", n => nums(n, x => (x % 2 == 0).asInstanceOf[AnyRef])),
      ("BoxArrayByte",    n => nums(n, _.toByte.asInstanceOf[AnyRef])),
      ("BoxArrayShort",   n => nums(n, _.toShort.asInstanceOf[AnyRef])),
      ("BoxArrayInt",     n => nums(n, _.toInt.asInstanceOf[AnyRef])),
      ("BoxArrayLong",    n => nums(n, _.toLong.asInstanceOf[AnyRef]))

//      ("j.List",    nums(_, _.toLong.asInstanceOf[AnyRef]).toBuffer.asJava: java.util.List[AnyRef]),
//      ("j.Map",       n => mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*).asJava: java.util.Map[AnyRef, AnyRef]),
//      ("j.Set",     nums(_, _ => obj).to[mutable.Set].asJava: java.util.Set[AnyRef])
    )
    val sizes = Seq(0, 1, 4, 16, 64, 256, 1024, 4069, 16192, 65536, 262144, 1048576)
    val results = for((name, factory) <- collections) yield {
      val numbers = for(n <- sizes) yield DeepSize(factory(n))
      (name, numbers)
    }

    def printRow[I: Integral](name: String, items: Seq[I]) = {
      val width = 15
      println(
        name.padTo(width, ' ') +
        items.map(NumberFormat.getNumberInstance(Locale.US).format)
             .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    printRow("Size", sizes)
    println()
    for((name, numbers) <- results){
      printRow(name, numbers)
    }
  }
}

