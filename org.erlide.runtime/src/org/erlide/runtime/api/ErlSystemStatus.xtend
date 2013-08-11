package org.erlide.runtime.api

import com.ericsson.otp.erlang.OtpErlangAtom
import com.ericsson.otp.erlang.OtpErlangException
import com.ericsson.otp.erlang.OtpErlangList
import com.ericsson.otp.erlang.OtpErlangLong
import com.ericsson.otp.erlang.OtpErlangObject
import com.ericsson.otp.erlang.OtpErlangPid
import com.ericsson.otp.erlang.OtpErlangTuple
import java.util.List
import org.erlide.util.erlang.ErlUtils
import org.erlide.util.erlang.TermParserException
import org.erlide.util.ErlLogger

class ErlSystemStatus {

  val MemoryStatus memory
  val List<ProcessStatus> processes
  val List<String> names

  new(OtpErlangTuple tuple) {
    val OtpErlangObject[] pps = (tuple.elementAt(0)as OtpErlangList).elements()
    processes = newArrayList()
    for (item : pps) {
      processes.add(new ProcessStatus(item as OtpErlangTuple))
    }
    memory = new MemoryStatus(tuple.elementAt(1)  as OtpErlangList)
    names = ((tuple.elementAt(2)) as OtpErlangList).elements.map[(it as OtpErlangAtom).atomValue]
  }

  def String prettyPrint() '''
    Memory:
    «memory.prettyPrint»
    ----------
    Registered processes:
    «names»
    ----------
    «FOR status : processes»
      «status.prettyPrint»
      ----------
    «ENDFOR»
    ----------
  '''

}

class MemoryStatus {

  long total
  long processes
  long processes_used
  long system
  long atom
  long atom_used
  long binary
  long code
  long ets
  long low
  long maximum

  new(OtpErlangList input) {
    for (item : input.elements()) {
      try {
        val bind = ErlUtils::match("{K:a, V:i}", item);
        val key = bind.getAtom("K");
        val value = bind.getLong("V");
        switch key {
          case "total":
            total = value
          case "processes":
            processes = value
          case "processes_used":
            processes_used = value
          case "system":
            system = value
          case "atom":
            atom = value
          case "atom_used":
            atom_used = value
          case "binary":
            binary = value
          case "code":
            code = value
          case "ets":
            ets = value
          case "low":
            low = value
          case "maximum":
            maximum = value
        }
      } catch (OtpErlangException e) {
        ErlLogger.error(e);
      } catch (TermParserException e) {
        ErlLogger.error(e);
      }
    }
  }

  def String prettyPrint() '''
             total: «String::format("%,10d", total)»
         processes: «String::format("%,10d", processes)»
    processes_used: «String::format("%,10d", processes_used)»
            system: «String::format("%,10d", system)»
              atom: «String::format("%,10d", atom)»
         atom_used: «String::format("%,10d", atom_used)»
       binary: «String::format("%,10d", binary)»
         code: «String::format("%,10d", code)»
          ets: «String::format("%,10d", ets)»
          low: «String::format("%,10d", low)»
           maximum: «String::format("%,10d", maximum)»
  '''

}

class ProcessStatus {
  OtpErlangPid pid
  long memory
  long heap_size
  long stack_size
  long total_heap_size
  OtpErlangObject binary
  String registered_name
  OtpErlangObject stacktrace

  new(
    OtpErlangTuple input
  ) {
    for (item : input.elements()) {
      try {
        val bind = ErlUtils::match("{K:a, V}", item);
        val key = bind.getAtom("K");
        val value = bind.get("V");
        switch key {
          case "memory":
            memory = (value as OtpErlangLong).longValue()
          case "heap_size":
            heap_size = (value as OtpErlangLong).longValue()
          case "stack_size":
            stack_size = (value as OtpErlangLong).longValue()
          case "total_heap_size":
            total_heap_size = (value as OtpErlangLong).longValue()
          case "binary":
            binary = value
          case "name":
            if (value instanceof OtpErlangAtom) {
              registered_name = (value as OtpErlangAtom).atomValue()
            } else {
              registered_name = ""
            }
          case "current_stacktrace":
            stacktrace = value
          case "pid":
            pid = value as OtpErlangPid
        }
      } catch (Exception e) {
        ErlLogger::error(">>>>>>> bad term in system status: " + input);
      }
    }
  }

  def String prettyPrint() '''
    Process «pid» («registered_name»)
         memory: «String::format("%,10d", memory)»
      heap_size: «String::format("%,10d", heap_size)»
     stack_size: «String::format("%,10d", stack_size)»
     total_heap: «String::format("%,10d", total_heap_size)»
         binary: «binary»
     stacktrace: «stacktrace»
  '''

}
