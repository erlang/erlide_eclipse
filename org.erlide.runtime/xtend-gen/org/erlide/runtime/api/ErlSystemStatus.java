package org.erlide.runtime.api;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.ArrayList;
import java.util.List;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.runtime.api.MemoryStatus;
import org.erlide.runtime.api.ProcessStatus;

@SuppressWarnings("all")
public class ErlSystemStatus {
  private final MemoryStatus memory;
  
  private final List<ProcessStatus> processes;
  
  private final List<String> names;
  
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
    OtpErlangObject _elementAt_2 = tuple.elementAt(2);
    OtpErlangObject[] _elements = ((OtpErlangList) _elementAt_2).elements();
    final Function1<OtpErlangObject,String> _function = new Function1<OtpErlangObject,String>() {
      public String apply(final OtpErlangObject it) {
        String _atomValue = ((OtpErlangAtom) it).atomValue();
        return _atomValue;
      }
    };
    List<String> _map = ListExtensions.<OtpErlangObject, String>map(((List<OtpErlangObject>)Conversions.doWrapArray(_elements)), _function);
    this.names = _map;
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
    _builder.append("Registered processes:");
    _builder.newLine();
    _builder.append(this.names, "");
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
