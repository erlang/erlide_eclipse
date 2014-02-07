package org.erlide.dialyzer.ui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.dialyzer.builder.DialyzerMarkerUtils;

public class RemoveDialyzerWarningsHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final ISelection selection = HandlerUtil.getCurrentSelection(event);
        if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            for (final Object o : structuredSelection.toArray()) {
                if (o instanceof IResource) {
                    final IResource resource = (IResource) o;
                    DialyzerMarkerUtils.removeDialyzerMarkersFor(resource);
                }
            }
        } else {
            DialyzerMarkerUtils.removeDialyzerMarkersFor(ResourcesPlugin.getWorkspace()
                    .getRoot());
        }
        return null;
    }

}
