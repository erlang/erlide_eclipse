package org.erlide.ui.tests.util;

import static com.google.common.collect.Iterables.toArray;
import static com.google.common.collect.Lists.newLinkedList;
import static java.util.Arrays.asList;

import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.junit.rules.MethodRule;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;

public class DisplayHelper implements MethodRule {

    private final Collection<Shell> capturedShells;

    private boolean displayOwner;
    private Display display;

    public DisplayHelper() {
        capturedShells = newLinkedList();
        capturedShells.addAll(asList(captureShells()));
    }

    public Display getDisplay() {
        if (display == null) {
            displayOwner = Display.getCurrent() == null;
            display = Display.getDefault();
        }
        return display;
    }

    public Shell[] getNewShells() {
        final Collection<Shell> newShells = newLinkedList();
        final Shell[] shells = captureShells();
        for (final Shell shell : shells) {
            if (!capturedShells.contains(shell)) {
                newShells.add(shell);
            }
        }
        return toArray(newShells, Shell.class);
    }

    public Shell createShell() {
        return createShell(SWT.NONE);
    }

    public Shell createShell(final int style) {
        return new Shell(getDisplay(), style);
    }

    public void ensureDisplay() {
        getDisplay();
    }

    public void flushPendingEvents() {
        while (Display.getCurrent() != null && !Display.getCurrent().isDisposed()
                && Display.getCurrent().readAndDispatch()) {
        }
    }

    public void dispose() {
        flushPendingEvents();
        disposeNewShells();
        disposeDisplay();
    }

    @Override
    public Statement apply(final Statement base, final FrameworkMethod method,
            final Object target) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                try {
                    base.evaluate();
                } finally {
                    dispose();
                }
            }
        };
    }

    private void disposeNewShells() {
        final Shell[] newShells = getNewShells();
        for (final Shell shell : newShells) {
            shell.dispose();
        }
    }

    private static Shell[] captureShells() {
        Shell[] result = new Shell[0];
        final Display currentDisplay = Display.getCurrent();
        if (currentDisplay != null) {
            result = currentDisplay.getShells();
        }
        return result;
    }

    private void disposeDisplay() {
        if (display != null && displayOwner) {
            if (display.isDisposed()) {
                display.dispose();
            }
            display = null;
        }
    }

}
