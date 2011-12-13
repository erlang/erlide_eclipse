package org.erlide.ui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.services.builder.MarkerUtils;

public class RemoveDialyzerWarningsHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final ISelection selection = HandlerUtil.getCurrentSelection(event);
        if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
            final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            for (final Object o : structuredSelection.toArray()) {
                if (o instanceof IResource) {
                    final IResource resource = (IResource) o;
                    MarkerUtils.removeDialyzerMarkers(resource);
                }
            }
        } else {
            MarkerUtils.removeDialyzerMarkers(ResourcesPlugin.getWorkspace()
                    .getRoot());
        }
        return null;
    }

    @Override
    public void setEnabled(final Object evaluationContext) {
        final IEvaluationContext ec = (IEvaluationContext) evaluationContext;
        final Object sel = ec.getVariable("selection");
        if (sel instanceof ISelection) {
            final ISelection selection = (ISelection) sel;
            boolean enabled = false;
            if (selection instanceof IStructuredSelection
                    && !selection.isEmpty()) {
                final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
                for (final Object o : structuredSelection.toArray()) {
                    if (o instanceof IResource) {
                        final IResource resource = (IResource) o;
                        if (MarkerUtils.haveDialyzerMarkers(resource)) {
                            enabled = true;
                            break;
                        }
                    }
                }
            } else {
                enabled = MarkerUtils.haveDialyzerMarkers(ResourcesPlugin
                        .getWorkspace().getRoot());
            }
            setBaseEnabled(enabled);
        }
    }

}
