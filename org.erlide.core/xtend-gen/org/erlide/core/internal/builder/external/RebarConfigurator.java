package org.erlide.core.internal.builder.external;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.erlide.engine.services.parsing.SimpleParserService;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class RebarConfigurator implements ProjectConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
      final ErlangProjectProperties result = _erlangProjectProperties;
      IErlangEngine _instance = ErlangEngine.getInstance();
      SimpleParserService _simpleParserService = _instance.getSimpleParserService();
      final List<OtpErlangObject> content = _simpleParserService.parse(config);
      boolean _isEmpty = content.isEmpty();
      if (_isEmpty) {
        return result;
      }
      final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
        public void apply(final OtpErlangObject it) {
          try {
            final Bindings bindings = ErlUtils.match("{erl_opts,Opts}", it);
            boolean _tripleNotEquals = (bindings != null);
            if (_tripleNotEquals) {
              final Collection<OtpErlangObject> opts = bindings.getList("Opts");
              final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
                public void apply(final OtpErlangObject it) {
                  try {
                    final Bindings b = ErlUtils.match("{Tag,Arg}", it);
                    String _atom = b.getAtom("Tag");
                    final String _switchValue = _atom;
                    boolean _matched = false;
                    if (!_matched) {
                      if (Objects.equal(_switchValue,"i")) {
                        _matched=true;
                        String _string = b.getString("Arg");
                        Path _path = new Path(_string);
                        result.setIncludeDirs(_path);
                      }
                    }
                    if (!_matched) {
                      if (Objects.equal(_switchValue,"src_dirs")) {
                        _matched=true;
                        Collection<OtpErlangObject> _list = b.getList("Arg");
                        final Function1<OtpErlangObject,Path> _function = new Function1<OtpErlangObject,Path>() {
                          public Path apply(final OtpErlangObject it) {
                            Path _xblockexpression = null;
                            {
                              final String s = ((OtpErlangString) it).stringValue();
                              Path _path = new Path(s);
                              _xblockexpression = (_path);
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
              };
              IterableExtensions.<OtpErlangObject>forEach(opts, _function);
            }
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      IterableExtensions.<OtpErlangObject>forEach(content, _function);
      _xblockexpression = (result);
    }
    return _xblockexpression;
  }
}
