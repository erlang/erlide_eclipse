package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.core.builder.IBuilder;
import org.erlide.core.internal.builder.ErlangToolExtensions;
import org.erlide.core.internal.builder.InternalBuilder;
import org.erlide.core.internal.builder.MakeBuilder;
import org.erlide.core.internal.builder.RebarBuilder;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public class BuilderFactory {
  public IBuilder getBuilderFor(final IProject project) {
    boolean _hasMakefile = ErlangToolExtensions.hasMakefile(project);
    if (_hasMakefile) {
      ErlLogger.debug("Use make builder");
      MakeBuilder _makeBuilder = new MakeBuilder(project);
      return _makeBuilder;
    }
    boolean _hasRebarConfig = ErlangToolExtensions.hasRebarConfig(project);
    if (_hasRebarConfig) {
      ErlLogger.debug("Use rebar builder");
      RebarBuilder _rebarBuilder = new RebarBuilder(project);
      return _rebarBuilder;
    }
    ErlLogger.debug("Use internal builder");
    InternalBuilder _internalBuilder = new InternalBuilder(project);
    return _internalBuilder;
  }
}
