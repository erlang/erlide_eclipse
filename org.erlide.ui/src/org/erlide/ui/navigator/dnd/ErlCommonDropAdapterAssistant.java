package org.erlide.ui.navigator.dnd;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.ui.navigator.CommonDropAdapter;
import org.eclipse.ui.navigator.CommonDropAdapterAssistant;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;

public class ErlCommonDropAdapterAssistant extends CommonDropAdapterAssistant {

    final static String EXTENSION_POINT_ID = "org.erlide.ui.erlDndAdapter";
    protected IStatus status;

    void setStatus(final IStatus status) {
        this.status = status;
    }

    public ErlCommonDropAdapterAssistant() {
    }

    @Override
    public IStatus handleDrop(final CommonDropAdapter dropAdapter,
            final DropTargetEvent dropTargetEvent, final Object target) {
        status = Status.CANCEL_STATUS;
        try {
            final List<INavigatorDropHandler> handlers = ExtensionUtils.getExtensions(
                    EXTENSION_POINT_ID, INavigatorDropHandler.class);
            for (final INavigatorDropHandler handler : handlers) {
                final ISafeRunnable runnable = new ISafeRunnable() {

                    @Override
                    public void handleException(final Throwable exception) {
                        ErlLogger.error(exception);
                    }

                    @Override
                    public void run() throws Exception {
                        final IStatus theStatus = handler.handleDrop(dropAdapter,
                                dropTargetEvent, target);
                        setStatus(theStatus);
                    }
                };
                SafeRunner.run(runnable);
            }
        } catch (final Exception ex) {
            ErlLogger.warn(ex);
        }
        return status;
    }

    @Override
    public IStatus validateDrop(final Object target, final int operation,
            final TransferData transferType) {
        status = Status.CANCEL_STATUS;
        try {
            final IConfigurationElement[] config = Platform.getExtensionRegistry()
                    .getConfigurationElementsFor(EXTENSION_POINT_ID);
            for (final IConfigurationElement e : config) {
                final Object o = e.createExecutableExtension("class");
                if (o instanceof INavigatorDropHandler) {
                    final ISafeRunnable runnable = new ISafeRunnable() {

                        @Override
                        public void handleException(final Throwable exception) {
                            ErlLogger.error("Exception in client");
                        }

                        @Override
                        public void run() throws Exception {
                            final IStatus theStatus = ((INavigatorDropHandler) o)
                                    .validateDrop(target, operation, transferType);
                            setStatus(theStatus);
                        }
                    };
                    SafeRunner.run(runnable);
                }
            }
        } catch (final Exception ex) {
            ErlLogger.error(ex);
        }
        return status;
    }
}
