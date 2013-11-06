package org.erlide.core.internal.builder.external;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.Collection;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.TermParser;

@SuppressWarnings("all")
public class RebarConfigurator implements ProjectConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    try {
      ErlangProjectProperties _xblockexpression = null;
      {
        ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
        final ErlangProjectProperties result = _erlangProjectProperties;
        TermParser _parser = TermParser.getParser();
        final OtpErlangObject content = _parser.parse(config);
        final Bindings bindings = ErlUtils.match("{erl_opts,Opts}", content);
        boolean _tripleEquals = (bindings == null);
        if (_tripleEquals) {
          return result;
        }
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
        _xblockexpression = (result);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
