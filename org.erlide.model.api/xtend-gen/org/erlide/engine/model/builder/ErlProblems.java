package org.erlide.engine.model.builder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.io.InputStream;
import java.util.List;
import java.util.Scanner;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.erlide.engine.model.builder.ProblemData;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class ErlProblems {
  private final List<ProblemData> data = CollectionLiterals.<ProblemData>newArrayList();
  
  private ErlProblems() {
    this.load();
  }
  
  public void load() {
    try {
      Class<? extends ErlProblems> _class = this.getClass();
      final ClassLoader loader = _class.getClassLoader();
      final InputStream input = loader.getResourceAsStream("org/erlide/engine/model/builder/errors.data");
      try {
        Scanner _scanner = new Scanner(input);
        final Scanner s = _scanner.useDelimiter("\\A");
        String _xifexpression = null;
        boolean _hasNext = s.hasNext();
        if (_hasNext) {
          _xifexpression = s.next();
        } else {
          _xifexpression = "";
        }
        final String src = _xifexpression;
        try {
          final OtpErlangObject source0 = ErlUtils.parse(src);
          final OtpErlangList source = ((OtpErlangList) source0);
          OtpErlangObject[] _elements = source.elements();
          for (final OtpErlangObject category0 : _elements) {
            {
              final OtpErlangTuple category = ((OtpErlangTuple) category0);
              OtpErlangObject _elementAt = category.elementAt(0);
              final String categoryName = ((OtpErlangAtom) _elementAt).atomValue();
              OtpErlangObject _elementAt_1 = category.elementAt(1);
              OtpErlangObject[] _elements_1 = ((OtpErlangList) _elementAt_1).elements();
              for (final OtpErlangObject item0 : _elements_1) {
                {
                  final OtpErlangTuple item = ((OtpErlangTuple) item0);
                  OtpErlangObject _elementAt_2 = item.elementAt(0);
                  final String tag = ((OtpErlangAtom) _elementAt_2).atomValue();
                  OtpErlangObject _elementAt_3 = item.elementAt(1);
                  String _stringValue = ((OtpErlangString) _elementAt_3).stringValue();
                  final String message = _stringValue.replaceAll("\\\\n", "\n");
                  int _arity = this.arity(message);
                  ProblemData _problemData = new ProblemData(categoryName, tag, message, _arity);
                  this.data.add(_problemData);
                }
              }
            }
          }
        } catch (final Throwable _t) {
          if (_t instanceof Exception) {
            final Exception e = (Exception)_t;
            ErlLogger.debug(e);
            throw e;
          } else {
            throw Exceptions.sneakyThrow(_t);
          }
        }
      } finally {
        input.close();
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public int arity(final String string) {
    int result = 0;
    byte[] _bytes = string.getBytes();
    for (final byte c : _bytes) {
      if ((c == 126)) {
        result = (result + 1);
      }
    }
    return result;
  }
  
  public static String quoteRegex(final String string) {
    final byte[] esc = "([{^$|)?*+.".getBytes();
    String result = string;
    for (final byte c : esc) {
      {
        final String r = ("\\" + Character.valueOf(((char) c)));
        final String v = ("\\\\" + Character.valueOf(((char) c)));
        String _replaceAll = result.replaceAll(r, v);
        result = _replaceAll;
      }
    }
    return result;
  }
  
  public void check() {
    final List<String> names = CollectionLiterals.<String>newArrayList();
    for (final ProblemData p : this.data) {
      {
        String _tag = p.getTag();
        String _plus = (_tag + "/");
        int _arity = p.getArity();
        String _plus_1 = (_plus + Integer.valueOf(_arity));
        boolean _contains = names.contains(_plus_1);
        if (_contains) {
          String _tag_1 = p.getTag();
          String _plus_2 = ("DOUBLE " + _tag_1);
          String _plus_3 = (_plus_2 + "/");
          int _arity_1 = p.getArity();
          String _plus_4 = (_plus_3 + Integer.valueOf(_arity_1));
          InputOutput.<String>println(_plus_4);
        }
        String _tag_2 = p.getTag();
        String _plus_5 = (_tag_2 + "/");
        int _arity_2 = p.getArity();
        String _plus_6 = (_plus_5 + Integer.valueOf(_arity_2));
        names.add(_plus_6);
      }
    }
  }
  
  public final static ErlProblems instance = new ErlProblems();
  
  public static ProblemData parse(final String msg) {
    return null;
  }
}
