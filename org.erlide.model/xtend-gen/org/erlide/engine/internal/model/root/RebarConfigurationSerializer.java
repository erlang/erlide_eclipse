package org.erlide.engine.internal.model.root;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
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
public class RebarConfigurationSerializer implements ProjectConfigurationSerializer {
  public String encodeConfig(final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      final ErlangProjectProperties result = new ErlangProjectProperties();
      Path _path = new Path("ebin");
      result.setOutputDir(_path);
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
            final Bindings bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts);
            boolean _tripleNotEquals = (bindings != null);
            if (_tripleNotEquals) {
              final Collection<OtpErlangObject> opts = bindings.getList("Opts");
              boolean _tripleNotEquals_1 = (opts != null);
              if (_tripleNotEquals_1) {
                final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
                  public void apply(final OtpErlangObject opt) {
                    try {
                      final Bindings b = ErlUtils.match("{Tag,Arg}", opt);
                      boolean _tripleNotEquals = (b != null);
                      if (_tripleNotEquals) {
                        RebarConfigurationSerializer.this.parseOption(result, b);
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
  
  public void parseOption(final ErlangProjectProperties result, final Bindings b) {
    try {
      String _atom = b.getAtom("Tag");
      boolean _matched = false;
      if (!_matched) {
        if (Objects.equal(_atom, "i")) {
          _matched=true;
          String _string = b.getString("Arg");
          final Path inc = new Path(_string);
          Collection<IPath> _includeDirs = result.getIncludeDirs();
          boolean _contains = _includeDirs.contains(inc);
          boolean _not = (!_contains);
          if (_not) {
            Collection<IPath> _includeDirs_1 = result.getIncludeDirs();
            final List<IPath> incs = CollectionLiterals.<IPath>newArrayList(((IPath[])Conversions.unwrapArray(_includeDirs_1, IPath.class)));
            incs.add(inc);
            result.setIncludeDirs(incs);
          }
        }
      }
      if (!_matched) {
        if (Objects.equal(_atom, "src_dirs")) {
          _matched=true;
          Collection<OtpErlangObject> _list = b.getList("Arg");
          final Function1<OtpErlangObject, Path> _function = new Function1<OtpErlangObject, Path>() {
            public Path apply(final OtpErlangObject it) {
              Path _xblockexpression = null;
              {
                final String s = ((OtpErlangString) it).stringValue();
                _xblockexpression = new Path(s);
              }
              return _xblockexpression;
            }
          };
          Iterable<Path> _map = IterableExtensions.<OtpErlangObject, Path>map(_list, _function);
          result.setSourceDirs(((IPath[])Conversions.unwrapArray(_map, IPath.class)));
        }
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
