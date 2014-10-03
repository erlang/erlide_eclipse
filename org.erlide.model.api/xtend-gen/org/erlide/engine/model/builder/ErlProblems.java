package org.erlide.engine.model.builder;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableList;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.erlide.engine.model.builder.ProblemData;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class ErlProblems {
  private final List<ProblemData> data = CollectionLiterals.<ProblemData>newArrayList();
  
  private final Map<String, ProblemData> tagMap = CollectionLiterals.<String, ProblemData>newHashMap();
  
  private ErlProblems() {
    this.load();
  }
  
  public void load() {
    try {
      Class<? extends ErlProblems> _class = this.getClass();
      final ClassLoader loader = _class.getClassLoader();
      final InputStream input = loader.getResourceAsStream("org/erlide/engine/model/builder/errors.data");
      try {
        String _name = Charsets.ISO_8859_1.name();
        final String src = Util.getInputStreamAsString(input, _name);
        try {
          final OtpErlangObject source0 = ErlUtils.parse(src);
          final OtpErlangList source = ((OtpErlangList) source0);
          OtpErlangObject[] _elements = source.elements();
          for (final OtpErlangObject item0 : _elements) {
            {
              final OtpErlangTuple item = ((OtpErlangTuple) item0);
              OtpErlangObject _elementAt = item.elementAt(0);
              final String tag = ((OtpErlangAtom) _elementAt).atomValue();
              OtpErlangObject _elementAt_1 = item.elementAt(1);
              String _stringValue = ((OtpErlangString) _elementAt_1).stringValue();
              final String message = _stringValue.replaceAll("\\\\n", "\n");
              final int myarity = ErlProblems.arity(message);
              final ProblemData problemData = new ProblemData(tag, message, myarity);
              this.data.add(problemData);
              String _tag = problemData.getTag();
              boolean _containsKey = this.tagMap.containsKey(_tag);
              if (_containsKey) {
                String _tag_1 = problemData.getTag();
                String _plus = ("duplicate problem tags are not allowed: \'" + _tag_1);
                String _plus_1 = (_plus + "\'");
                throw new IllegalStateException(_plus_1);
              }
              String _tag_2 = problemData.getTag();
              this.tagMap.put(_tag_2, problemData);
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
  
  public List<ProblemData> getData() {
    return ImmutableList.<ProblemData>copyOf(this.data);
  }
  
  public static int arity(final String string) {
    int result = 0;
    boolean escape = false;
    byte[] _bytes = string.getBytes();
    for (final byte c : _bytes) {
      {
        if (((!escape) && (c == 126))) {
          result = (result + 1);
        }
        escape = (c == 92);
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
  
  private static ErlProblems instance = null;
  
  public static ErlProblems getInstance() {
    boolean _tripleEquals = (ErlProblems.instance == null);
    if (_tripleEquals) {
      ErlProblems _erlProblems = new ErlProblems();
      ErlProblems.instance = _erlProblems;
    }
    return ErlProblems.instance;
  }
  
  public static ProblemData parse(final String msg) {
    ErlProblems _instance = ErlProblems.getInstance();
    for (final ProblemData p : _instance.data) {
      Pattern _pattern = p.getPattern();
      Matcher _matcher = _pattern.matcher(msg);
      boolean _matches = _matcher.matches();
      if (_matches) {
        return p;
      }
    }
    return null;
  }
  
  public ProblemData getProblem(final String message) {
    for (final ProblemData d : this.data) {
      {
        final List<String> args = d.getMessageArgs(message);
        boolean _tripleNotEquals = (args != null);
        if (_tripleNotEquals) {
          return d;
        }
      }
    }
    return null;
  }
}
