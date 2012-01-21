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
package org.erlide.core.model.root;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangCore;
import org.erlide.utils.Util;

/**
 * @see IErlModelStatus
 */

public class ErlModelStatus extends Status implements IErlModelStatus {
    public static final IErlElement[] NO_ELEMENTS = new IErlElement[0];

    /**
     * The elements related to the failure, or <code>null</code> if no elements
     * are involved.
     */
    protected IErlElement[] fElements = new IErlElement[0];

    /**
     * The path related to the failure, or <code>null</code> if no path is
     * involved.
     */
    protected IPath fPath;

    /**
     * The <code>String</code> related to the failure, or <code>null</code> if
     * no <code>String</code> is involved.
     */
    protected String fString;

    /**
     * Empty children
     */
    protected static final IStatus[] NO_CHILDREN = new IStatus[] {};

    protected IStatus[] children = NO_CHILDREN;

    /**
     * Singleton OK object
     */
    public static final IErlModelStatus VERIFIED_OK = new ErlModelStatus(OK,
            OK, Util.bind("status.OK")); //$NON-NLS-1$

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus() {
        // no code for an multi-status
        super(ERROR, ErlangCore.PLUGIN_ID, 0, "ErlModelStatus", null); //$NON-NLS-1$
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final int code) {
        super(ERROR, ErlangCore.PLUGIN_ID, code, "ErlModelStatus", null); //$NON-NLS-1$
        fElements = NO_ELEMENTS;
    }

    /**
     * Constructs an Erlang model status with the given corresponding elements.
     */
    public ErlModelStatus(final int code, final IErlElement[] elements) {
        super(ERROR, ErlangCore.PLUGIN_ID, code, "ErlModelStatus", null); //$NON-NLS-1$
        fElements = elements;
        fPath = null;
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final int code, final String string) {
        this(ERROR, code, string);
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final int severity, final int code,
            final String string) {
        super(severity, ErlangCore.PLUGIN_ID, code, "ErlModelStatus", null); //$NON-NLS-1$
        fElements = NO_ELEMENTS;
        fPath = null;
        fString = string;
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final int code, final Throwable throwable) {
        super(ERROR, ErlangCore.PLUGIN_ID, code, "ErlModelStatus", throwable); //$NON-NLS-1$
        fElements = NO_ELEMENTS;
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final int code, final IPath path) {
        super(ERROR, ErlangCore.PLUGIN_ID, code, "ErlModelStatus", null); //$NON-NLS-1$
        fElements = NO_ELEMENTS;
        fPath = path;
    }

    /**
     * Constructs an Erlang model status with the given corresponding element.
     */
    public ErlModelStatus(final int code, final IErlElement element) {
        this(code, new IErlElement[] { element });
    }

    /**
     * Constructs an Erlang model status with the given corresponding element
     * and string
     */
    public ErlModelStatus(final int code, final IErlElement element,
            final String string) {
        this(code, new IErlElement[] { element });
        fString = string;
    }

    /**
     * Constructs an Erlang model status with the given corresponding element
     * and path
     */
    public ErlModelStatus(final int code, final IErlElement element,
            final IPath path) {
        this(code, new IErlElement[] { element });
        fPath = path;
    }

    /**
     * Constructs an Erlang model status with the given corresponding element,
     * path and string
     */
    public ErlModelStatus(final int code, final IErlElement element,
            final IPath path, final String string) {
        this(code, new IErlElement[] { element });
        fPath = path;
        fString = string;
    }

    /**
     * Constructs an Erlang model status with no corresponding elements.
     */
    public ErlModelStatus(final CoreException coreException) {
        super(ERROR, ErlangCore.PLUGIN_ID,
                ErlModelStatusConstants.CORE_EXCEPTION,
                "ErlModelStatus", coreException); //$NON-NLS-1$
        fElements = NO_ELEMENTS;
    }

    protected int getBits() {
        final int severity = 1 << getCode() % 100 / 33;
        final int category = 1 << getCode() / 100 + 3;
        return severity | category;
    }

    /**
     * @see IStatus
     */
    @Override
    public IStatus[] getChildren() {
        return children;
    }

    /**
     * @see IErlModelStatus
     */
    @Override
    public IErlElement[] getElements() {
        return fElements;
    }

    /**
     * Returns the message that is relevant to the code of this status.
     */
    @Override
    public String getMessage() {
        final Throwable exception = getException();
        if (exception == null) {
            switch (getCode()) {
            case ErlModelStatusConstants.CORE_EXCEPTION:
                return Util.bind("status.coreException"); //$NON-NLS-1$

            case ErlModelStatusConstants.BUILDER_INITIALIZATION_ERROR:
                return Util.bind("build.initializationError"); //$NON-NLS-1$

            case ErlModelStatusConstants.BUILDER_SERIALIZATION_ERROR:
                return Util.bind("build.serializationError"); //$NON-NLS-1$

            case ErlModelStatusConstants.DEVICE_PATH:
                return Util.bind(
                        "status.cannotUseDeviceOnPath", getPath().toString()); //$NON-NLS-1$

            case ErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST:
                return Util
                        .bind("element.doesNotExist", fElements[0].toStringWithAncestors()); //$NON-NLS-1$

            case ErlModelStatusConstants.INDEX_OUT_OF_BOUNDS:
                return Util.bind("status.indexOutOfBounds"); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_CONTENTS:
                return Util.bind("status.invalidContents"); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_DESTINATION:
                return Util
                        .bind("status.invalidDestination", fElements[0].toStringWithAncestors()); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_ELEMENT_TYPES:
                final StringBuilder buff = new StringBuilder(
                        Util.bind("operation.notSupported")); //$NON-NLS-1$
                for (int i = 0; i < fElements.length; i++) {
                    if (i > 0) {
                        buff.append(", "); //$NON-NLS-1$
                    }
                    buff.append(fElements[i].toStringWithAncestors());
                }
                return buff.toString();

            case ErlModelStatusConstants.INVALID_NAME:
                return Util.bind("status.invalidName", fString); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_PATH:
                if (fString != null) {
                    return fString;
                }
                return Util
                        .bind("status.invalidPath", getPath() == null ? "null" : getPath().toString()); //$NON-NLS-1$ //$NON-NLS-2$

            case ErlModelStatusConstants.INVALID_PROJECT:
                return Util.bind("status.invalidProject", fString); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_RESOURCE:
                return Util.bind("status.invalidResource", fString); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_RESOURCE_TYPE:
                return Util.bind("status.invalidResourceType", fString); //$NON-NLS-1$

            case ErlModelStatusConstants.INVALID_SIBLING:
                if (fString != null) {
                    return Util.bind("status.invalidSibling", fString); //$NON-NLS-1$
                }
                return Util
                        .bind("status.invalidSibling", fElements[0].toStringWithAncestors()); //$NON-NLS-1$

            case ErlModelStatusConstants.IO_EXCEPTION:
                return Util.bind("status.IOException"); //$NON-NLS-1$

            case ErlModelStatusConstants.NAME_COLLISION:
                if (fElements != null && fElements.length > 0) {
                    // IErlElement element = elements[0];
                    // String name = element.getElementName();
                }
                if (fString != null) {
                    return fString;
                }
                return Util.bind("status.nameCollision", ""); //$NON-NLS-1$ //$NON-NLS-2$

            case ErlModelStatusConstants.NO_ELEMENTS_TO_PROCESS:
                return Util.bind("operation.needElements"); //$NON-NLS-1$

            case ErlModelStatusConstants.NULL_NAME:
                return Util.bind("operation.needName"); //$NON-NLS-1$

            case ErlModelStatusConstants.NULL_PATH:
                return Util.bind("operation.needPath"); //$NON-NLS-1$

            case ErlModelStatusConstants.NULL_STRING:
                return Util.bind("operation.needString"); //$NON-NLS-1$

            case ErlModelStatusConstants.PATH_OUTSIDE_PROJECT:
                return Util
                        .bind("operation.pathOutsideProject", fString, fElements[0].toStringWithAncestors()); //$NON-NLS-1$

            case ErlModelStatusConstants.READ_ONLY:
                final IErlElement element = fElements[0];
                final String name = element.getName();
                return Util.bind("status.readOnly", name); //$NON-NLS-1$

            case ErlModelStatusConstants.RELATIVE_PATH:
                return Util.bind(
                        "operation.needAbsolutePath", getPath().toString()); //$NON-NLS-1$

            case ErlModelStatusConstants.UPDATE_CONFLICT:
                return Util.bind("status.updateConflict"); //$NON-NLS-1$

            case ErlModelStatusConstants.NO_LOCAL_CONTENTS:
                return Util
                        .bind("status.noLocalContents", getPath().toString()); //$NON-NLS-1$
            }
            if (fString != null) {
                return fString;
            }
            return ""; // //$NON-NLS-1$
        }
        final String message = exception.getMessage();
        if (message != null) {
            return message;
        }
        return exception.toString();

    }

    /**
     * @see IErlModelStatus#getPath()
     */
    @Override
    public IPath getPath() {
        return fPath;
    }

    /**
     * @see IStatus#getSeverity()
     */
    @Override
    public int getSeverity() {
        if (children == NO_CHILDREN) {
            return super.getSeverity();
        }
        int severity = -1;
        for (final IStatus element : children) {
            final int childrenSeverity = element.getSeverity();
            if (childrenSeverity > severity) {
                severity = childrenSeverity;
            }
        }
        return severity;
    }

    /**
     * @see IErlModelStatus#getString()
     * @deprecated
     */
    @Override
    @Deprecated
    public String getString() {
        return fString;
    }

    /**
     * @see IErlModelStatus#isDoesNotExist()
     */
    @Override
    public boolean isDoesNotExist() {
        return getCode() == ErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST;
    }

    /**
     * @see IStatus#isMultiStatus()
     */
    @Override
    public boolean isMultiStatus() {
        return children != NO_CHILDREN;
    }

    /**
     * @see IStatus#isOK()
     */
    @Override
    public boolean isOK() {
        return getCode() == OK;
    }

    /**
     * @see IStatus#matches(int)
     */
    @Override
    public boolean matches(final int mask) {
        if (!isMultiStatus()) {
            return matches(this, mask);
        }
        for (final IStatus element : children) {
            if (matches((ErlModelStatus) element, mask)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Helper for matches(int).
     */
    protected boolean matches(final ErlModelStatus status, final int mask) {
        final int severityMask = mask & 0x7;
        final int categoryMask = mask & ~0x7;
        final int bits = status.getBits();
        return (severityMask == 0 || (bits & severityMask) != 0)
                && (categoryMask == 0 || (bits & categoryMask) != 0);
    }

    /**
     * Creates and returns a new <code>IErlModelStatus</code> that is a a
     * multi-status status.
     * 
     * @see IStatus#isMultiStatus()
     */
    public static IErlModelStatus newMultiStatus(final int code,
            final IErlModelStatus[] children) {
        final ErlModelStatus jms = new ErlModelStatus(code);
        jms.children = children;
        return jms;
    }

    /**
     * Creates and returns a new <code>IErlModelStatus</code> that is a a
     * multi-status status.
     * 
     * @see IStatus#isMultiStatus()
     */
    public static IErlModelStatus newMultiStatus(
            final IErlModelStatus[] children) {
        final ErlModelStatus jms = new ErlModelStatus();
        jms.children = children;
        return jms;
    }

    /**
     * Returns a printable representation of this exception for debugging
     * purposes.
     */
    @Override
    public String toString() {
        if (this == VERIFIED_OK) {
            return "ErlModelStatus[OK]"; //$NON-NLS-1$
        }
        final StringBuilder buffer = new StringBuilder();
        buffer.append("Erlang Model Status ["); //$NON-NLS-1$
        buffer.append(getMessage());
        buffer.append(']');
        return buffer.toString();
    }
}
