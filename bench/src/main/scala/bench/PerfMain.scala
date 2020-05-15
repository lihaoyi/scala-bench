package bench

import java.text.NumberFormat
import java.util.Locale

import better.files.File
import org.json4s.native.Serialization

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue
import scala.collection.{SortedSet, mutable}
import org.json4s._

object PerfMain{

  def main(args: Array[String]): Unit = {

    def printRow[I: Integral](name: String, items: Seq[I]) = {
      val width = 15
      println(
        name.padTo(width, ' ') +
        items.map(NumberFormat.getNumberInstance(Locale.US).format)
             .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    // How large the collections will be in each benchmark
    val sizes = (0 to 25).map(Math.pow(2, _).toInt)
    // How many times to repeat each benchmark
    val repeats = 7
    // How long each benchmark runs, in millis
    val duration = 2000
    // How long a benchmark can run before we stop incrementing it
    val cutoff = 400 * 1000 * 1000

    printRow("Size", sizes)
    val output = mutable.Map.empty[(String, String, Long), mutable.Buffer[Long]]
    val cutoffSizes = mutable.Map.empty[(String, String), Int]
    for(i <- 1 to repeats){
      println("Run " + i)
      for(benchmark <- Benchmark.benchmarks){
        println()
        println(benchmark.name)
        println()
        for (bench <- benchmark.cases){
          val key = benchmark.name -> bench.name


          val times =
            for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield{
              val buf = output.getOrElseUpdate((benchmark.name, bench.name, size), mutable.Buffer())
              def handle(run: Boolean) = {
                System.gc()

                val start = System.currentTimeMillis()
                var count = 0
                while(System.currentTimeMillis() - start < duration){
                  if (run) bench.run(size)
                  else bench.initializer(size)
                  count += 1
                }
                val end = System.currentTimeMillis()
                (count, end - start)
              }
              val (initCounts, initTime) = handle(run = false)
              val (runCounts, runTime) = handle(run = true)
              val res = ((runTime.toDouble / runCounts - initTime.toDouble / initCounts) * 1000000).toLong
              buf.append(res)
              if (res > cutoff) {
                cutoffSizes(key) = math.min(
                  cutoffSizes.getOrElse(key, Int.MaxValue),
                  size
                )
              }
              res
            }
          printRow(bench.name, times)
        }
      }
    }
    implicit val formats = Serialization.formats(NoTypeHints)
    File(s"target/results${util.Properties.versionNumberString}.json").write(
      Serialization.write(output.mapValues(_.toList).toMap)
    )
  }
}

