package org.erlide.ui.internal.util;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;

public final class CommandRunnerSelectionAdapter extends SelectionAdapter {

    private final String commandId;

    public CommandRunnerSelectionAdapter(final String commandId) {
        this.commandId = commandId;
    }

    @Override
    public void widgetSelected(final SelectionEvent e) {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final IHandlerService handlerService = (IHandlerService) workbench
                .getService(IHandlerService.class);
        try {
            handlerService.executeCommand(commandId, null);
        } catch (final Exception ex) {
            throw new RuntimeException(commandId + " not found");
        }

    }
}
