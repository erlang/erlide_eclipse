package org.erlide.core.internal.builder;

import com.google.common.base.Objects;
import java.util.Map;
import org.eclipse.core.resources.IProject;
import org.erlide.core.builder.IBuilder;
import org.erlide.core.internal.builder.EmakeBuilder;
import org.erlide.core.internal.builder.InternalBuilder;
import org.erlide.core.internal.builder.MakeBuilder;
import org.erlide.core.internal.builder.RebarBuilder;
import org.erlide.engine.model.root.OldErlangProjectProperties;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public class BuilderFactory {
  private final static String MAKE_BUILDER = "make";
  
  private final static String EMAKE_BUILDER = "emake";
  
  private final static String REBAR_BUILDER = "rebar";
  
  public IBuilder getBuilderFor(final IProject project) {
    IBuilder _xblockexpression = null;
    {
      OldErlangProjectProperties _oldErlangProjectProperties = new OldErlangProjectProperties(project);
      final OldErlangProjectProperties properties = _oldErlangProjectProperties;
      Map<String,String> _builderProperties = properties.getBuilderProperties();
      final String builder = _builderProperties.get("builder");
      IBuilder _switchResult = null;
      boolean _matched = false;
      if (!_matched) {
        if (Objects.equal(builder,BuilderFactory.MAKE_BUILDER)) {
          _matched=true;
          MakeBuilder _xblockexpression_1 = null;
          {
            ErlLogger.trace("builder", BuilderFactory.MAKE_BUILDER);
            MakeBuilder _makeBuilder = new MakeBuilder(project);
            _xblockexpression_1 = (_makeBuilder);
          }
          _switchResult = _xblockexpression_1;
        }
      }
      if (!_matched) {
        if (Objects.equal(builder,BuilderFactory.EMAKE_BUILDER)) {
          _matched=true;
          EmakeBuilder _xblockexpression_2 = null;
          {
            ErlLogger.trace("builder", BuilderFactory.EMAKE_BUILDER);
            EmakeBuilder _emakeBuilder = new EmakeBuilder(project);
            _xblockexpression_2 = (_emakeBuilder);
          }
          _switchResult = _xblockexpression_2;
        }
      }
      if (!_matched) {
        if (Objects.equal(builder,BuilderFactory.REBAR_BUILDER)) {
          _matched=true;
          RebarBuilder _xblockexpression_3 = null;
          {
            ErlLogger.trace("builder", BuilderFactory.REBAR_BUILDER);
            RebarBuilder _rebarBuilder = new RebarBuilder(project);
            _xblockexpression_3 = (_rebarBuilder);
          }
          _switchResult = _xblockexpression_3;
        }
      }
      if (!_matched) {
        InternalBuilder _xblockexpression_4 = null;
        {
          ErlLogger.trace("builder", "internal");
          InternalBuilder _internalBuilder = new InternalBuilder(project);
          _xblockexpression_4 = (_internalBuilder);
        }
        _switchResult = _xblockexpression_4;
      }
      _xblockexpression = (_switchResult);
    }
    return _xblockexpression;
  }
}
