package org.erlide.cover.ui.actions;

import org.eclipse.jface.action.Action;
import org.erlide.cover.ui.annotations.EditorTracker;

public class ClearCoverageAction extends Action {
    
    public void run() {
        EditorTracker.getInstance().clearAllAnnotations();
    }
    
}
