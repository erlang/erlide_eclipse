package org.erlide.core.internal.builder.external;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
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
import org.erlide.engine.model.root.ProjectConfigurationSerializer;
import org.erlide.engine.services.parsing.SimpleParserService;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class RebarConfigurator implements ProjectConfigurationSerializer {
  public String encodeConfig(final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
      final ErlangProjectProperties result = _erlangProjectProperties;
      Path _path = new Path("ebin");
      result.setOutputDir(_path);
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
              final Bindings bindings = ErlUtils.match("{erl_opts,Opts}", erl_opts);
              Boolean _xifexpression = null;
              boolean _tripleNotEquals = (bindings != null);
              if (_tripleNotEquals) {
                Boolean _xblockexpression_1 = null;
                {
                  final Collection<OtpErlangObject> opts = bindings.getList("Opts");
                  Boolean _xifexpression_1 = null;
                  boolean _tripleNotEquals_1 = (opts != null);
                  if (_tripleNotEquals_1) {
                    final Function2<Boolean,OtpErlangObject,Boolean> _function = new Function2<Boolean,OtpErlangObject,Boolean>() {
                      public Boolean apply(final Boolean seenIncludes_1, final OtpErlangObject opt) {
                        try {
                          boolean _xblockexpression = false;
                          {
                            final Bindings b = ErlUtils.match("{Tag,Arg}", opt);
                            boolean _xifexpression = false;
                            boolean _tripleNotEquals = (b != null);
                            if (_tripleNotEquals) {
                              boolean _parseOption = RebarConfigurator.this.parseOption(result, b, (seenIncludes_1).booleanValue());
                              _xifexpression = _parseOption;
                            } else {
                              _xifexpression = (seenIncludes_1).booleanValue();
                            }
                            _xblockexpression = (_xifexpression);
                          }
                          return Boolean.valueOf(_xblockexpression);
                        } catch (Throwable _e) {
                          throw Exceptions.sneakyThrow(_e);
                        }
                      }
                    };
                    Boolean _fold = IterableExtensions.<OtpErlangObject, Boolean>fold(opts, seenIncludes, _function);
                    _xifexpression_1 = _fold;
                  }
                  _xblockexpression_1 = (_xifexpression_1);
                }
                _xifexpression = _xblockexpression_1;
              } else {
                _xifexpression = seenIncludes;
              }
              _xblockexpression = (_xifexpression);
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
  
  public boolean parseOption(final ErlangProjectProperties result, final Bindings b, final boolean seenIncludes) {
    try {
      boolean _switchResult = false;
      String _atom = b.getAtom("Tag");
      final String _switchValue = _atom;
      boolean _matched = false;
      if (!_matched) {
        if (Objects.equal(_switchValue,"i")) {
          _matched=true;
          boolean _xblockexpression = false;
          {
            ArrayList<IPath> _xifexpression = null;
            if (seenIncludes) {
              Collection<IPath> _includeDirs = result.getIncludeDirs();
              ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList(((IPath[])Conversions.unwrapArray(_includeDirs, IPath.class)));
              _xifexpression = _newArrayList;
            } else {
              ArrayList<IPath> _newArrayList_1 = CollectionLiterals.<IPath>newArrayList();
              _xifexpression = _newArrayList_1;
            }
            final List<IPath> incs = _xifexpression;
            String _string = b.getString("Arg");
            Path _path = new Path(_string);
            final Path inc = _path;
            boolean _contains = incs.contains(inc);
            boolean _not = (!_contains);
            if (_not) {
              incs.add(inc);
            }
            result.setIncludeDirs(incs);
            _xblockexpression = (true);
          }
          _switchResult = _xblockexpression;
        }
      }
      if (!_matched) {
        if (Objects.equal(_switchValue,"src_dirs")) {
          _matched=true;
          boolean _xblockexpression_1 = false;
          {
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
            _xblockexpression_1 = (seenIncludes);
          }
          _switchResult = _xblockexpression_1;
        }
      }
      return _switchResult;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
