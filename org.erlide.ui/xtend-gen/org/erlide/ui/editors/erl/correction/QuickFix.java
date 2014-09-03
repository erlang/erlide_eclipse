package org.erlide.ui.editors.erl.correction;

import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.ui.editors.erl.correction.QuickFixExecutor;

@Accessors
@SuppressWarnings("all")
public class QuickFix implements IMarkerResolution2 {
  private String label;
  
  private String description;
  
  private Image image;
  
  private List<String> tags;
  
  private QuickFixExecutor executor;
  
  private List<String> args;
  
  public QuickFix() {
  }
  
  public QuickFix(final QuickFix other) {
    String _label = other.getLabel();
    this.label = _label;
    String _description = other.getDescription();
    this.description = _description;
    Image _image = other.getImage();
    this.image = _image;
    List<String> _tags = other.getTags();
    this.tags = _tags;
    QuickFixExecutor _executor = other.getExecutor();
    this.executor = _executor;
  }
  
  public void run(final IMarker marker) {
    boolean _and = false;
    boolean _tripleNotEquals = (marker != null);
    if (!_tripleNotEquals) {
      _and = false;
    } else {
      boolean _exists = marker.exists();
      boolean _not = (!_exists);
      _and = _not;
    }
    if (_and) {
      return;
    }
    this.executor.run(marker, this);
  }
  
  public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
    return this.executor.appliesAt(invocationContext);
  }
  
  @Pure
  public String getLabel() {
    return this.label;
  }
  
  public void setLabel(final String label) {
    this.label = label;
  }
  
  @Pure
  public String getDescription() {
    return this.description;
  }
  
  public void setDescription(final String description) {
    this.description = description;
  }
  
  @Pure
  public Image getImage() {
    return this.image;
  }
  
  public void setImage(final Image image) {
    this.image = image;
  }
  
  @Pure
  public List<String> getTags() {
    return this.tags;
  }
  
  public void setTags(final List<String> tags) {
    this.tags = tags;
  }
  
  @Pure
  public QuickFixExecutor getExecutor() {
    return this.executor;
  }
  
  public void setExecutor(final QuickFixExecutor executor) {
    this.executor = executor;
  }
  
  @Pure
  public List<String> getArgs() {
    return this.args;
  }
  
  public void setArgs(final List<String> args) {
    this.args = args;
  }
}
