package org.erlide.engine.internal.model.root;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Objects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurationSerializer;
import org.erlide.engine.services.parsing.SimpleParserService;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class EmakeConfigurationSerializer implements ProjectConfigurationSerializer {
  public String encodeConfig(final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      final ErlangProjectProperties result = new ErlangProjectProperties();
      Path _path = new Path("ebin");
      result.setOutputDir(_path);
      result.setSourceDirs();
      IErlangEngine _instance = ErlangEngine.getInstance();
      SimpleParserService _simpleParserService = _instance.getSimpleParserService();
      final List<OtpErlangObject> content = _simpleParserService.parse(config);
      boolean _isEmpty = content.isEmpty();
      if (_isEmpty) {
        return result;
      }
      final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
        public void apply(final OtpErlangObject erl_opts) {
          try {
            final Bindings bindings = ErlUtils.match("{Src,Opts}", erl_opts);
            boolean _tripleNotEquals = (bindings != null);
            if (_tripleNotEquals) {
              final String src = bindings.getAtom("Src");
              String _xifexpression = null;
              boolean _contains = src.contains("/");
              if (_contains) {
                String[] _split = src.split("/");
                _xifexpression = IterableExtensions.<String>head(((Iterable<String>)Conversions.doWrapArray(_split)));
              } else {
                _xifexpression = "src";
              }
              final String path = _xifexpression;
              Collection<IPath> _sourceDirs = result.getSourceDirs();
              final ArrayList<IPath> sd = new ArrayList<IPath>(_sourceDirs);
              Path _path = new Path(path);
              sd.add(_path);
              result.setSourceDirs(sd);
              final Collection<OtpErlangObject> opts = bindings.getList("Opts");
              boolean _tripleNotEquals_1 = (opts != null);
              if (_tripleNotEquals_1) {
                final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
                  public void apply(final OtpErlangObject opt) {
                    try {
                      final Bindings b = ErlUtils.match("{Tag,Arg}", opt);
                      boolean _tripleNotEquals = (b != null);
                      if (_tripleNotEquals) {
                        EmakeConfigurationSerializer.this.parseOption(b, result);
                      }
                    } catch (Throwable _e) {
                      throw Exceptions.sneakyThrow(_e);
                    }
                  }
                };
                IterableExtensions.<OtpErlangObject>forEach(opts, _function);
              }
            }
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      IterableExtensions.<OtpErlangObject>forEach(content, _function);
      _xblockexpression = result;
    }
    return _xblockexpression;
  }
  
  public void parseOption(final Bindings b, final ErlangProjectProperties result) {
    try {
      String _atom = b.getAtom("Tag");
      boolean _matched = false;
      if (!_matched) {
        if (Objects.equal(_atom, "i")) {
          _matched=true;
          Collection<IPath> _includeDirs = result.getIncludeDirs();
          final List<IPath> incs = new ArrayList<IPath>(_includeDirs);
          String _string = b.getString("Arg");
          final Path inc = new Path(_string);
          boolean _contains = incs.contains(inc);
          boolean _not = (!_contains);
          if (_not) {
            incs.add(inc);
          }
          result.setIncludeDirs(incs);
        }
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
