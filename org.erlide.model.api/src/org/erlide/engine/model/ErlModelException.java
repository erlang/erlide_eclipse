/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.engine.model;

import java.io.PrintStream;
import java.io.PrintWriter;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

/**
 * A checked exception representing a failure in the Erlang model. Erlang model
 * exceptions contain an Erlang-specific status object describing the cause of
 * the exception.
 * <p>
 * This class is not intended to be subclassed by clients. Instances of this
 * class are automatically created by the Erlang model when problems arise, so
 * there is generally no need for clients to create instances.
 * </p>
 *
 * @see IErlModelStatus
 * @see ErlModelStatusConstants
 */
public class ErlModelException extends CoreException {

    private static final long serialVersionUID = 1L;

    private final CoreException nestedCoreException;

    /**
     * Creates a Erlang model exception for a given <code>CoreException</code>.
     *
     * @param exception
     *            the <code>CoreException</code>
     */
    public ErlModelException(final CoreException exception, final IErlModelStatus status) {
        super(status);
        nestedCoreException = exception;
    }

    /**
     * Creates a Erlang model exception for an Erlang-specific status object.
     *
     * @param status
     *            the Erlang-specific status object
     */
    public ErlModelException(final IErlModelStatus status) {
        this(null, status);
    }

    public ErlModelException(final CoreException e) {
        this(e, new ErlModelStatus(e));
    }

    /**
     * Returns the underlying <code>Throwable</code> that caused the failure.
     *
     * @return the wrappered <code>Throwable</code>, or <code>null</code> if the
     *         direct case of the failure was at the Erlang model layer
     */
    public Throwable getException() {
        if (nestedCoreException == null) {
            return getStatus().getException();
        }
        return nestedCoreException;
    }

    /**
     * Returns the Erlang model status object for this exception. Equivalent to
     * <code>(IErlModelStatus) getStatus()</code>.
     *
     * @return a status object
     */
    public IErlModelStatus getErlangModelStatus() {
        final IStatus status = getStatus();
        if (status instanceof IErlModelStatus) {
            return (IErlModelStatus) status;
        }
        return null;
    }

    /**
     * Returns whether this exception indicates that an Erlang model element
     * does not exist. Such exceptions have a status with a code of
     * <code>IErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST</code>. This is a
     * convenience method.
     *
     * @return <code>true</code> if this exception indicates that a Erlang model
     *         element does not exist
     * @see IErlModelStatus#elementDoesNotExist()
     * @see ErlModelStatusConstants#ELEMENT_DOES_NOT_EXIST
     */
    public boolean isDoesNotExist() {
        final IErlModelStatus erlangModelStatus = getErlangModelStatus();
        return erlangModelStatus != null && erlangModelStatus.elementDoesNotExist();
    }

    /**
     * Prints this exception's stack trace to the given print stream.
     *
     * @param output
     *            the print stream
     */
    @Override
    public void printStackTrace(final PrintStream output) {
        synchronized (output) {
            super.printStackTrace(output);
            final Throwable throwable = getException();
            if (throwable != null) {
                output.print("Caused by: "); //$NON-NLS-1$
                throwable.printStackTrace(output);
            }
        }
    }

    /**
     * Prints this exception's stack trace to the given print writer.
     *
     * @param output
     *            the print writer
     */
    @Override
    public void printStackTrace(final PrintWriter output) {
        synchronized (output) {
            super.printStackTrace(output);
            final Throwable throwable = getException();
            if (throwable != null) {
                output.print("Caused by: "); //$NON-NLS-1$
                throwable.printStackTrace(output);
            }
        }
    }

    /*
     * Returns a printable representation of this exception suitable for
     * debugging purposes only.
     */
    @Override
    public String toString() {
        final StringBuilder buffer = new StringBuilder();
        buffer.append("Erlang Model Exception: "); //$NON-NLS-1$
        if (getException() != null) {
            if (getException() instanceof CoreException) {
                final CoreException c = (CoreException) getException();
                buffer.append("Core Exception [code "); //$NON-NLS-1$
                buffer.append(c.getStatus().getCode());
                buffer.append("] "); //$NON-NLS-1$
                buffer.append(c.getStatus().getMessage());
            } else {
                buffer.append(getException().toString());
            }
        } else {
            buffer.append(getStatus().toString());
        }
        return buffer.toString();
    }
}
