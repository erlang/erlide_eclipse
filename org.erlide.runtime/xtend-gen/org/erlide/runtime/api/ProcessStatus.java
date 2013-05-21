package org.erlide.runtime.api;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class ProcessStatus {
  private OtpErlangPid pid;
  
  private long memory;
  
  private long heap_size;
  
  private long stack_size;
  
  private long total_heap_size;
  
  private OtpErlangObject binary;
  
  private String registered_name;
  
  private OtpErlangObject stacktrace;
  
  public ProcessStatus(final OtpErlangTuple input) {
    OtpErlangObject[] _elements = input.elements();
    for (final OtpErlangObject item : _elements) {
      try {
        final Bindings bind = ErlUtils.match("{K:a, V}", item);
        final String key = bind.getAtom("K");
        final OtpErlangObject value = bind.get("V");
        boolean _matched = false;
        if (!_matched) {
          if (Objects.equal(key,"memory")) {
            _matched=true;
            long _longValue = ((OtpErlangLong) value).longValue();
            this.memory = _longValue;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"heap_size")) {
            _matched=true;
            long _longValue_1 = ((OtpErlangLong) value).longValue();
            this.heap_size = _longValue_1;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"stack_size")) {
            _matched=true;
            long _longValue_2 = ((OtpErlangLong) value).longValue();
            this.stack_size = _longValue_2;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"total_heap_size")) {
            _matched=true;
            long _longValue_3 = ((OtpErlangLong) value).longValue();
            this.total_heap_size = _longValue_3;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"binary")) {
            _matched=true;
            this.binary = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"name")) {
            _matched=true;
            if ((value instanceof OtpErlangAtom)) {
              String _atomValue = ((OtpErlangAtom) value).atomValue();
              this.registered_name = _atomValue;
            } else {
              this.registered_name = "";
            }
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"current_stacktrace")) {
            _matched=true;
            this.stacktrace = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"pid")) {
            _matched=true;
            this.pid = ((OtpErlangPid) value);
          }
        }
      } catch (final Throwable _t) {
        if (_t instanceof Exception) {
          final Exception e = (Exception)_t;
          String _plus = (">>>>>>> bad term in system status: " + input);
          ErlLogger.error(_plus);
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
  }
  
  public String prettyPrint() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("Process ");
    _builder.append(this.pid, "");
    _builder.append(" (");
    _builder.append(this.registered_name, "");
    _builder.append(")");
    _builder.newLineIfNotEmpty();
    _builder.append("     ");
    _builder.append("memory: ");
    String _format = String.format("%,10d", Long.valueOf(this.memory));
    _builder.append(_format, "     ");
    _builder.newLineIfNotEmpty();
    _builder.append("  ");
    _builder.append("heap_size: ");
    String _format_1 = String.format("%,10d", Long.valueOf(this.heap_size));
    _builder.append(_format_1, "  ");
    _builder.newLineIfNotEmpty();
    _builder.append(" ");
    _builder.append("stack_size: ");
    String _format_2 = String.format("%,10d", Long.valueOf(this.stack_size));
    _builder.append(_format_2, " ");
    _builder.newLineIfNotEmpty();
    _builder.append(" ");
    _builder.append("total_heap: ");
    String _format_3 = String.format("%,10d", Long.valueOf(this.total_heap_size));
    _builder.append(_format_3, " ");
    _builder.newLineIfNotEmpty();
    _builder.append("     ");
    _builder.append("binary: ");
    _builder.append(this.binary, "     ");
    _builder.newLineIfNotEmpty();
    _builder.append(" ");
    _builder.append("stacktrace: ");
    _builder.append(this.stacktrace, " ");
    _builder.newLineIfNotEmpty();
    return _builder.toString();
  }
}
