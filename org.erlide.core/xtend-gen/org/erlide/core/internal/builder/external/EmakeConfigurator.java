package org.erlide.core.internal.builder.external;

import java.io.StringBufferInputStream;
import java.util.Collection;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public class EmakeConfigurator implements BuilderConfigurator {
  public void createConfig(final IProject project, final IErlangProjectProperties info) {
    final IFile config = project.getFile("Emakefile");
    try {
      StringConcatenation _builder = new StringConcatenation();
      {
        Collection<IPath> _sourceDirs = info.getSourceDirs();
        for(final IPath src : _sourceDirs) {
          _builder.append("{\'");
          String _portableString = src.toPortableString();
          _builder.append(_portableString, "");
          _builder.append("/*\',[");
          {
            Collection<IPath> _includeDirs = info.getIncludeDirs();
            for(final IPath inc : _includeDirs) {
              _builder.append("{i, \"");
              String _portableString_1 = inc.toPortableString();
              _builder.append(_portableString_1, "");
              _builder.append("\"},");
            }
          }
          _builder.append("]}.");
          _builder.newLineIfNotEmpty();
        }
      }
      final String template = _builder.toString();
      String _plus = (">> " + template);
      InputOutput.<String>println(_plus);
      StringBufferInputStream _stringBufferInputStream = new StringBufferInputStream(template);
      config.create(_stringBufferInputStream, true, null);
    } catch (final Throwable _t) {
      if (_t instanceof CoreException) {
        final CoreException e = (CoreException)_t;
        ErlLogger.error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  public BuilderConfigParser getConfigParser() {
    return null;
  }
}
