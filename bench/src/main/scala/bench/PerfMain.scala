package bench

import java.text.NumberFormat
import java.util.Locale

import better.files.File
import org.json4s._
import org.json4s.native.Serialization

import scala.collection.mutable

object PerfMain{

  def main(args: Array[String]): Unit = {

    def printRow[I](name: String, items: Seq[I]) = {
      val width = 25
      println(
        name.padTo(width, ' ') +
        items.map(NumberFormat.getNumberInstance(Locale.US).format)
             .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    // How large the collections will be in each benchmark
    val sizes = (4 to 22).map(Math.pow(2, _).toInt)
    // How many times to repeat each benchmark
    val repeats = 3
    // How long each benchmark runs, in millis
    val duration = 500
    // How long a benchmark can run before we stop incrementing it
    val cutoff = 200 * 1000 * 1000

    printRow("Size", sizes)
    val output = mutable.Map.empty[(String, String, Double), mutable.Buffer[Double]]
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

                val start = System.nanoTime()
                var count = 0
                while(System.nanoTime() - start < duration * 1000000 ){
                  if (run) bench.run(size)
                  else bench.initializer(size)
                  count += 1
                }
                val end = System.nanoTime()
                (count, end - start)
              }
              val (initCounts, initTime) = handle(run = false)
              val (runCounts, runTime) = handle(run = true)
              val res = ((runTime.toDouble / runCounts - initTime.toDouble / initCounts))
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
    File(s"target/results${util.Properties.versionNumberString}2.json").write(
      Serialization.write(output.map{case (key, value) => key.productIterator.mkString("|") -> value.toList}.toMap)
    )
  }
}

