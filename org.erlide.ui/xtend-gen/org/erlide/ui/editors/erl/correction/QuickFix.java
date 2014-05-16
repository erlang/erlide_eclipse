package org.erlide.ui.editors.erl.correction;

import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.erlide.ui.editors.erl.correction.QuickFixExecutor;

@SuppressWarnings("all")
public class QuickFix implements IMarkerResolution2 {
  private String _label;
  
  public String getLabel() {
    return this._label;
  }
  
  public void setLabel(final String label) {
    this._label = label;
  }
  
  private String _description;
  
  public String getDescription() {
    return this._description;
  }
  
  public void setDescription(final String description) {
    this._description = description;
  }
  
  private Image _image;
  
  public Image getImage() {
    return this._image;
  }
  
  public void setImage(final Image image) {
    this._image = image;
  }
  
  private List<String> _tags;
  
  public List<String> getTags() {
    return this._tags;
  }
  
  public void setTags(final List<String> tags) {
    this._tags = tags;
  }
  
  private QuickFixExecutor _executor;
  
  public QuickFixExecutor getExecutor() {
    return this._executor;
  }
  
  public void setExecutor(final QuickFixExecutor executor) {
    this._executor = executor;
  }
  
  private List<String> _args;
  
  public List<String> getArgs() {
    return this._args;
  }
  
  public void setArgs(final List<String> args) {
    this._args = args;
  }
  
  public QuickFix() {
  }
  
  public QuickFix(final QuickFix other) {
    String _label = other.getLabel();
    this.setLabel(_label);
    String _description = other.getDescription();
    this.setDescription(_description);
    Image _image = other.getImage();
    this.setImage(_image);
    List<String> _tags = other.getTags();
    this.setTags(_tags);
    QuickFixExecutor _executor = other.getExecutor();
    this.setExecutor(_executor);
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
    QuickFixExecutor _executor = this.getExecutor();
    _executor.run(marker, this);
  }
  
  public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
    QuickFixExecutor _executor = this.getExecutor();
    return _executor.appliesAt(invocationContext);
  }
}
