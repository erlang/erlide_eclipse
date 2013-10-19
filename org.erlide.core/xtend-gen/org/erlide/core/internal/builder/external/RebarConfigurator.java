package org.erlide.core.internal.builder.external;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import java.io.File;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;

@SuppressWarnings("all")
public class RebarConfigurator implements BuilderConfigurator {
  public void createConfig(final IProject project, final IErlangProjectProperties info) {
    try {
      final CharSequence content = this.getConfigString(project, info);
      IPath _location = project.getLocation();
      IPath _append = _location.append("rebar.config");
      String _portableString = _append.toPortableString();
      File _file = new File(_portableString);
      Files.write(content, _file, Charsets.UTF_8);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public CharSequence getConfigString(final IProject project, final IErlangProjectProperties properties) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("%% coding: utf-8");
    _builder.newLine();
    _builder.newLine();
    return _builder;
  }
  
  public BuilderConfigParser getConfigParser() {
    return null;
  }
}
