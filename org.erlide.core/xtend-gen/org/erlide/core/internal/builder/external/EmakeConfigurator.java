package org.erlide.core.internal.builder.external;

import com.ericsson.otp.erlang.OtpErlangObject;
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
public class EmakeConfigurator implements ProjectConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
      final ErlangProjectProperties result = _erlangProjectProperties;
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
      final Function2<Boolean,OtpErlangObject,Boolean> _function = new Function2<Boolean,OtpErlangObject,Boolean>() {
        public Boolean apply(final Boolean seenIncludes, final OtpErlangObject erl_opts) {
          try {
            Boolean _xblockexpression = null;
            {
              Boolean acc0 = seenIncludes;
              final Bindings bindings = ErlUtils.match("{Src,Opts}", erl_opts);
              boolean _tripleNotEquals = (bindings != null);
              if (_tripleNotEquals) {
                final String src = bindings.getAtom("Src");
                String _xifexpression = null;
                boolean _contains = src.contains("/");
                if (_contains) {
                  String[] _split = src.split("/");
                  String _head = IterableExtensions.<String>head(((Iterable<String>)Conversions.doWrapArray(_split)));
                  _xifexpression = _head;
                } else {
                  _xifexpression = "src";
                }
                final String path = _xifexpression;
                Collection<IPath> _sourceDirs = result.getSourceDirs();
                ArrayList<IPath> _arrayList = new ArrayList<IPath>(_sourceDirs);
                final ArrayList<IPath> sd = _arrayList;
                Path _path = new Path(path);
                sd.add(_path);
                result.setSourceDirs(sd);
                final Collection<OtpErlangObject> opts = bindings.getList("Opts");
                boolean _tripleNotEquals_1 = (opts != null);
                if (_tripleNotEquals_1) {
                  final Function2<Boolean,OtpErlangObject,Boolean> _function = new Function2<Boolean,OtpErlangObject,Boolean>() {
                    public Boolean apply(final Boolean seenIncludes1, final OtpErlangObject opt) {
                      try {
                        Boolean _xblockexpression = null;
                        {
                          Boolean acc = seenIncludes1;
                          final Bindings b = ErlUtils.match("{Tag,Arg}", opt);
                          boolean _tripleNotEquals = (b != null);
                          if (_tripleNotEquals) {
                            boolean _parseOption = EmakeConfigurator.this.parseOption(b, (acc).booleanValue(), result);
                            acc = Boolean.valueOf(_parseOption);
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
  
  public boolean parseOption(final Bindings b, final boolean seenIncludes, final ErlangProjectProperties result) {
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
              ArrayList<IPath> _arrayList = new ArrayList<IPath>(_includeDirs);
              _xifexpression = _arrayList;
            } else {
              ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
              _xifexpression = _newArrayList;
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
      return _switchResult;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}
