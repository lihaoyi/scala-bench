package bench
import java.math.{MathContext, RoundingMode}
import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale

import ammonite.ops._
import pprint.Config.Colors._
/**
  * Created by haoyi on 9/26/16.
  */
object AnalyzeMain {
  def main(args: Array[String]): Unit = {
    val results = upickle.default.read[Map[(String, String, Long), Vector[Long]]](
      read! pwd/'target/"results.json"
    )
    val grouped: Map[String, Map[String, Map[Long, (Long, String)]]] = {
      results.groupBy{case ((bench, coll, size), res) => bench }
        .map{ case (bench, rest) =>
          bench -> rest.groupBy{case ((bench, coll, size), res) => coll}
            .map{ case (coll, rest) =>
              coll -> rest.groupBy { case ((bench, coll, size), res) => size }
                .mapValues{ items =>
                  val divisor = (bench, coll) match{
                    case ("lookup", _) => 100
                    case ("foreach", "List-while" | "Array-while" | "m.Buffer") => 100
                    case ("foreach", _) => 10
                    case _ => 1
                  }
                  val sorted = items.toVector.flatMap{case ((bench, coll, size), res) => res}.sorted
                  val middling = sorted.drop(1).dropRight(1).map(_ / divisor)
                  val mean = middling.sum / middling.length
                  val stdDev = math.sqrt(middling.map(x => (x-mean) * (x-mean)).sum / middling.length).toLong
                  val accuracy = math.max(1, math.pow(10, math.log10(stdDev).toInt).toInt)

                  val stdDevStr = if (stdDev == 0.0) "0%"
                  else new DecimalFormat("0.0").format(stdDev * 100.0 / math.abs(mean)) + "%"

                  (mean / accuracy * accuracy, stdDevStr)
                }
            }
        }
    }

    val width = 15
    pprint.pprintln(grouped)
    print("|:" + "-" * width + "-|")
    for(size <- Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)){
      print("-"*width + ":|")
    }
    println()
    for((bench, items) <- grouped){
      print("| " + " "*width + " |")
      for(size <- Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)){
        print(" "*width + " |")
      }
      println()
      print("| " +("**"+bench+"**").padTo(width, ' ') + " |")
      for(size <- Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)){
        print(("**" + NumberFormat.getNumberInstance(Locale.US).format(size) + "**").reverse.padTo(width, ' ').reverse + " |")
      }
      println()
      print("| " + " "*width + " |")
      for(size <- Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)){
        print(" "*width + " |")
      }
      println()

      for((coll, items) <- items){
        print("| ")
        print(coll.padTo(width, ' '))
        print(" |")
        for(size <- Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)){
          items.get(size) match{
            case Some((mean, stdDev)) =>
//              val ranges = Seq(
//                1000000000 -> "s",
//                1000000 -> "ms",
//                1000 -> "us",
//                1 -> "ns"
//              )
//              val (div, suffix) = ranges.find(_._1 < math.abs(mean)).getOrElse(1 -> "ns")
              val (div, suffix) = (1, "")
//              val mathContext = new MathContext(2, RoundingMode.DOWN)
//              val bigDecimal = new java.math.BigDecimal(mean * 1.0 / div, mathContext)

//              print((bigDecimal.toPlainString() + suffix + " ± " + stdDev).reverse.padTo(width, ' ').reverse + " |")
              print((NumberFormat.getNumberInstance(Locale.US).format(mean) + " ± " + stdDev).reverse.padTo(width, ' ').reverse + " |")
            case None =>
              print(" " * width + " |")
          }

        }
        println()
      }

    }
  }
}
