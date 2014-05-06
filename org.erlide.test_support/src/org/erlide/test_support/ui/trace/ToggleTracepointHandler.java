package org.erlide.test_support.ui.trace;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.ide.ResourceUtil;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.ui.handlers.ErlangAbstractHandler;

public class ToggleTracepointHandler extends ErlangAbstractHandler {

    @Override
    protected void doAction(final ISelection sel, final ITextEditor textEditor) {
        System.out.println("TOGGLE TRACEPOINT ");

        final IFile file = ResourceUtil.getFile(textEditor.getEditorInput());
        try {
            final IMarker[] markers = file.findMarkers(
                    "org.erlide.test_support.tracingmarker", true, IResource.DEPTH_ONE);
            if (markers.length == 0) {
                System.out.println("create");
                final IMarker m = file
                        .createMarker("org.erlide.test_support.tracingmarker");
                m.setAttribute(IMarker.LINE_NUMBER, 5);
                m.setAttribute(IMarker.LOCATION, "5");
                m.setAttribute(IMarker.MESSAGE, "msg");
            } else {
                System.out.println("delete");
                markers[0].delete();
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        }
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    @Override
    public void setEnabled(final Object evaluationContext) {
        super.setEnabled(evaluationContext);
    }
}
