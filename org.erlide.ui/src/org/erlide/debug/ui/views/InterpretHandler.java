package org.erlide.debug.ui.views;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.ui.launch.DebugTab;

public class InterpretHandler extends AbstractHandler {

    private static final String INTERPRET_COMMAND_ID = "org.erlide.ui.interpret";
    private static final String DEINTERPRET_COMMAND_ID = "org.erlide.ui.deinterpret";

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final InterpretedModulesView view = (InterpretedModulesView) HandlerUtil
                .getActivePart(event);
        final String commandId = event.getCommand().getId();
        if (commandId.equals(INTERPRET_COMMAND_ID)) {
            final List<IErlModule> modules = DebugTab.getModulesFromAddModulesDialog(view
                    .getSite().getShell());
            for (final IErlModule module : modules) {
                view.interpretOrDeinterpret(module, true);
            }
        } else if (commandId.equals(DEINTERPRET_COMMAND_ID)) {
            final ISelection selection = HandlerUtil.getCurrentSelection(event);
            if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
                final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
                for (final Object o : structuredSelection.toArray()) {
                    final IErlModule module = (IErlModule) o;
                    view.interpretOrDeinterpret(module, false);
                }
            }
        } else {
            throw new ExecutionException("bad command id");
        }
        return null;
    }

    @Override
    public void setEnabled(final Object evaluationContext) {
        super.setEnabled(evaluationContext);
    }
}
