package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.ui.editors.erl.correction.QuickFix;
import org.erlide.util.ErlLogger;

@Accessors
@SuppressWarnings("all")
public abstract class QuickFixExecutor implements ISafeRunnable {
  private IMarker marker;
  
  private QuickFix quickFix;
  
  public void handleException(final Throwable exception) {
    ErlLogger.error(exception);
  }
  
  public void run() throws Exception {
    MessageDialog.openInformation(null, "Erlang quick fix", "This quick-fix is not yet implemented");
  }
  
  public void run(final IMarker marker2, final QuickFix erlangQuickFix) {
    this.setMarker(marker2);
    this.setQuickFix(erlangQuickFix);
    SafeRunner.run(this);
  }
  
  public abstract boolean appliesAt(final IQuickAssistInvocationContext invocationContext);
  
  @Pure
  public IMarker getMarker() {
    return this.marker;
  }
  
  public void setMarker(final IMarker marker) {
    this.marker = marker;
  }
  
  @Pure
  public QuickFix getQuickFix() {
    return this.quickFix;
  }
  
  public void setQuickFix(final QuickFix quickFix) {
    this.quickFix = quickFix;
  }
}
