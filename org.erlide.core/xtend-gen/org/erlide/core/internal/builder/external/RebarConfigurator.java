package org.erlide.core.internal.builder.external;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
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
      final Function2<Boolean,OtpErlangObject,Boolean> _function = new Function2<Boolean,OtpErlangObject,Boolean>() {
        public Boolean apply(final Boolean seenIncludes, final OtpErlangObject erl_opts) {
          try {
            Boolean _xblockexpression = null;
            {
              Boolean acc0 = seenIncludes;
              final Bindings bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts);
              Collection<OtpErlangObject> _list = null;
              if (bindings!=null) {
                _list=bindings.getList("Opts");
              }
              final Collection<OtpErlangObject> opts = _list;
              boolean _tripleNotEquals = (opts != null);
              if (_tripleNotEquals) {
                final Function2<Boolean,OtpErlangObject,Boolean> _function = new Function2<Boolean,OtpErlangObject,Boolean>() {
                  public Boolean apply(final Boolean seenIncludes1, final OtpErlangObject opt) {
                    try {
                      Boolean _xblockexpression = null;
                      {
                        Boolean acc = seenIncludes1;
                        final Bindings b = ErlUtils.match("{Tag,Arg}", opt);
                        boolean _tripleNotEquals = (b != null);
                        if (_tripleNotEquals) {
                          String _atom = b.getAtom("Tag");
                          final String _switchValue = _atom;
                          boolean _matched = false;
                          if (!_matched) {
                            if (Objects.equal(_switchValue,"i")) {
                              _matched=true;
                              ArrayList<IPath> _xifexpression = null;
                              if ((seenIncludes1).booleanValue()) {
                                Collection<IPath> _includeDirs = result.getIncludeDirs();
                                ArrayList<IPath> _arrayList = new ArrayList<IPath>(_includeDirs);
                                _xifexpression = _arrayList;
                              } else {
                                ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
                                _xifexpression = _newArrayList;
                              }
                              final List<IPath> incs = _xifexpression;
                              String _string = b.getString("Arg");
                              Path _path = new Path(_string);
                              incs.add(_path);
                              result.setIncludeDirs(incs);
                              acc = Boolean.valueOf(true);
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
                        }
                        _xblockexpression = (acc);
                      }
                      return _xblockexpression;
                    } catch (Throwable _e) {
                      throw Exceptions.sneakyThrow(_e);
                    }
                  }
                };
                Boolean _fold = IterableExtensions.<OtpErlangObject, Boolean>fold(opts, acc0, _function);
                acc0 = _fold;
              }
              _xblockexpression = (acc0);
            }
            return _xblockexpression;
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      IterableExtensions.<OtpErlangObject, Boolean>fold(content, Boolean.valueOf(false), _function);
      _xblockexpression = (result);
    }
    return _xblockexpression;
  }
}
