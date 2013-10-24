package org.erlide.core.internal.builder.external;

import java.util.Collection;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.ErlangProjectProperties;

@SuppressWarnings("all")
public class EmakeConfigurator implements BuilderConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
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
    return _builder.toString();
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    return null;
  }
  
  public String getConfigFile() {
    return "Emakefile";
  }
}
