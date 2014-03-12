package org.erlide.runtime.api;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Objects;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.TermParserException;

@SuppressWarnings("all")
public class MemoryStatus {
  private long total;
  
  private long processes;
  
  private long processes_used;
  
  private long system;
  
  private long atom;
  
  private long atom_used;
  
  private long binary;
  
  private long code;
  
  private long ets;
  
  private long low;
  
  private long maximum;
  
  public MemoryStatus(final OtpErlangList input) {
    OtpErlangObject[] _elements = input.elements();
    for (final OtpErlangObject item : _elements) {
      try {
        final Bindings bind = ErlUtils.match("{K:a, V:i}", item);
        final String key = bind.getAtom("K");
        final long value = bind.getLong("V");
        boolean _matched = false;
        if (!_matched) {
          if (Objects.equal(key,"total")) {
            _matched=true;
            this.total = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"processes")) {
            _matched=true;
            this.processes = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"processes_used")) {
            _matched=true;
            this.processes_used = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"system")) {
            _matched=true;
            this.system = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"atom")) {
            _matched=true;
            this.atom = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"atom_used")) {
            _matched=true;
            this.atom_used = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"binary")) {
            _matched=true;
            this.binary = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"code")) {
            _matched=true;
            this.code = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"ets")) {
            _matched=true;
            this.ets = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"low")) {
            _matched=true;
            this.low = value;
          }
        }
        if (!_matched) {
          if (Objects.equal(key,"maximum")) {
            _matched=true;
            this.maximum = value;
          }
        }
      } catch (final Throwable _t) {
        if (_t instanceof OtpErlangException) {
          final OtpErlangException e = (OtpErlangException)_t;
          ErlLogger.error(e);
        } else if (_t instanceof TermParserException) {
          final TermParserException e_1 = (TermParserException)_t;
          ErlLogger.error(e_1);
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
  }
  
  public String prettyPrint() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("         ");
    _builder.append("total: ");
    String _format = String.format("%,10d", Long.valueOf(this.total));
    _builder.append(_format, "         ");
    _builder.newLineIfNotEmpty();
    _builder.append("     ");
    _builder.append("processes: ");
    String _format_1 = String.format("%,10d", Long.valueOf(this.processes));
    _builder.append(_format_1, "     ");
    _builder.newLineIfNotEmpty();
    _builder.append("processes_used: ");
    String _format_2 = String.format("%,10d", Long.valueOf(this.processes_used));
    _builder.append(_format_2, "");
    _builder.newLineIfNotEmpty();
    _builder.append("        ");
    _builder.append("system: ");
    String _format_3 = String.format("%,10d", Long.valueOf(this.system));
    _builder.append(_format_3, "        ");
    _builder.newLineIfNotEmpty();
    _builder.append("          ");
    _builder.append("atom: ");
    String _format_4 = String.format("%,10d", Long.valueOf(this.atom));
    _builder.append(_format_4, "          ");
    _builder.newLineIfNotEmpty();
    _builder.append("     ");
    _builder.append("atom_used: ");
    String _format_5 = String.format("%,10d", Long.valueOf(this.atom_used));
    _builder.append(_format_5, "     ");
    _builder.newLineIfNotEmpty();
    _builder.append("   ");
    _builder.append("binary: ");
    String _format_6 = String.format("%,10d", Long.valueOf(this.binary));
    _builder.append(_format_6, "   ");
    _builder.newLineIfNotEmpty();
    _builder.append("     ");
    _builder.append("code: ");
    String _format_7 = String.format("%,10d", Long.valueOf(this.code));
    _builder.append(_format_7, "     ");
    _builder.newLineIfNotEmpty();
    _builder.append("      ");
    _builder.append("ets: ");
    String _format_8 = String.format("%,10d", Long.valueOf(this.ets));
    _builder.append(_format_8, "      ");
    _builder.newLineIfNotEmpty();
    _builder.append("      ");
    _builder.append("low: ");
    String _format_9 = String.format("%,10d", Long.valueOf(this.low));
    _builder.append(_format_9, "      ");
    _builder.newLineIfNotEmpty();
    _builder.append("       ");
    _builder.append("maximum: ");
    String _format_10 = String.format("%,10d", Long.valueOf(this.maximum));
    _builder.append(_format_10, "       ");
    _builder.newLineIfNotEmpty();
    return _builder.toString();
  }
}
