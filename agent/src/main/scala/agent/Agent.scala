package agent

import java.lang.instrument.Instrumentation
object Agent{
  private var instrumentation: Instrumentation = null
  def premain(args: String, inst: Instrumentation): Unit = {
    instrumentation = inst
  }
  def getObjectSize(o: Any) = instrumentation.getObjectSize(o)
}

