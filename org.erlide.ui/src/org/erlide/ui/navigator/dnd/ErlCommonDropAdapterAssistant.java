package org.erlide.ui.navigator.dnd;

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
import org.erlide.jinterface.ErlLogger;

public class ErlCommonDropAdapterAssistant extends CommonDropAdapterAssistant {

    final static String extensionPontId = "org.erlide.ui.erlDndAdapter";
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
            final IConfigurationElement[] config = Platform
                    .getExtensionRegistry().getConfigurationElementsFor(
                            extensionPontId);
            for (final IConfigurationElement e : config) {
                final Object o = e.createExecutableExtension("class");
                if (o instanceof INavigatorDropHandler) {
                    final ISafeRunnable runnable = new ISafeRunnable() {

                        @Override
                        public void handleException(final Throwable exception) {
                            ErlLogger.error(exception);
                        }

                        @Override
                        public void run() throws Exception {
                            final IStatus theStatus = ((INavigatorDropHandler) o)
                                    .handleDrop(dropAdapter, dropTargetEvent,
                                            target);
                            setStatus(theStatus);
                        }
                    };
                    SafeRunner.run(runnable);
                }
            }
        } catch (final Exception ex) {
            System.out.println(ex.getMessage());
        }
        return status;
    }

    @Override
    public IStatus validateDrop(final Object target, final int operation,
            final TransferData transferType) {
        status = Status.CANCEL_STATUS;
        try {
            final IConfigurationElement[] config = Platform
                    .getExtensionRegistry().getConfigurationElementsFor(
                            extensionPontId);
            for (final IConfigurationElement e : config) {
                final Object o = e.createExecutableExtension("class");
                if (o instanceof INavigatorDropHandler) {
                    final ISafeRunnable runnable = new ISafeRunnable() {

                        @Override
                        public void handleException(final Throwable exception) {
                            System.out.println("Exception in client");
                        }

                        @Override
                        public void run() throws Exception {
                            final IStatus theStatus = ((INavigatorDropHandler) o)
                                    .validateDrop(target, operation,
                                            transferType);
                            setStatus(theStatus);
                        }
                    };
                    SafeRunner.run(runnable);
                }
            }
        } catch (final Exception ex) {
            System.out.println(ex.getMessage());
        }
        return status;
    }
}
