package org.erlide.runtime;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.erlide.runtime.MemoryStatus;
import org.erlide.runtime.ProcessStatus;

@SuppressWarnings("all")
public class ErlSystemStatus {
  private final MemoryStatus memory;
  
  private final List<ProcessStatus> processes;
  
  public ErlSystemStatus(final OtpErlangTuple tuple) {
    OtpErlangObject _elementAt = tuple.elementAt(0);
    final OtpErlangObject[] pps = ((OtpErlangList) _elementAt).elements();
    ArrayList<ProcessStatus> _newArrayList = CollectionLiterals.<ProcessStatus>newArrayList();
    this.processes = _newArrayList;
    for (final OtpErlangObject item : pps) {
      ProcessStatus _processStatus = new ProcessStatus(((OtpErlangTuple) item));
      this.processes.add(_processStatus);
    }
    OtpErlangObject _elementAt_1 = tuple.elementAt(1);
    MemoryStatus _memoryStatus = new MemoryStatus(((OtpErlangList) _elementAt_1));
    this.memory = _memoryStatus;
  }
  
  public String prettyPrint() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("Memory:");
    _builder.newLine();
    String _prettyPrint = this.memory.prettyPrint();
    _builder.append(_prettyPrint, "");
    _builder.newLineIfNotEmpty();
    _builder.append("----------");
    _builder.newLine();
    {
      for(final ProcessStatus status : this.processes) {
        String _prettyPrint_1 = status.prettyPrint();
        _builder.append(_prettyPrint_1, "");
        _builder.newLineIfNotEmpty();
        _builder.append("----------");
        _builder.newLine();
      }
    }
    _builder.append("----------");
    _builder.newLine();
    return _builder.toString();
  }
}
