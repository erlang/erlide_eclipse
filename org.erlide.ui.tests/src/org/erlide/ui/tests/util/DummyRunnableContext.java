package org.erlide.ui.tests.util;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * To be used from tests, when we don't care about canceling long running
 * operations
 *
 * @author Vlad
 *
 */
public class DummyRunnableContext implements IRunnableContext {

    public DummyRunnableContext() {
    }

    /*
     * @see org.eclipse.jface.operation.IRunnableContext#run(boolean, boolean,
     * org.eclipse.jface.operation.IRunnableWithProgress)
     */
    @Override
    public void run(final boolean fork, final boolean cancelable,
            final IRunnableWithProgress runnable) throws InvocationTargetException,
            InterruptedException {
        runnable.run(null);
    }

}
