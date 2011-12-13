/*******************************************************************************
 * Copyright (c) 2000, 2008 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util;

import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;

/**
 * Implementation of a <code>ISelectionValidator</code> to validate the type of
 * an element. Empty selections are not accepted.
 */
public class TypedElementSelectionValidator implements
        ISelectionStatusValidator {

    private final IStatus fgErrorStatus = new StatusInfo(IStatus.ERROR, ""); //$NON-NLS-1$
    private final IStatus fgOKStatus = new StatusInfo();

    private final Class<?>[] fAcceptedTypes;
    private final boolean fAllowMultipleSelection;
    private final Collection<?> fRejectedElements;

    /**
     * @param acceptedTypes
     *            The types accepted by the validator
     * @param allowMultipleSelection
     *            If set to <code>true</code>, the validator allows multiple
     *            selection.
     */
    public TypedElementSelectionValidator(final Class<?>[] acceptedTypes,
            final boolean allowMultipleSelection) {
        this(acceptedTypes, allowMultipleSelection, null);
    }

    /**
     * @param acceptedTypes
     *            The types accepted by the validator
     * @param allowMultipleSelection
     *            If set to <code>true</code>, the validator allows multiple
     *            selection.
     * @param rejectedElements
     *            A list of elements that are not accepted
     */
    public TypedElementSelectionValidator(final Class<?>[] acceptedTypes,
            final boolean allowMultipleSelection,
            final Collection<?> rejectedElements) {
        Assert.isNotNull(acceptedTypes);
        fAcceptedTypes = acceptedTypes;
        fAllowMultipleSelection = allowMultipleSelection;
        fRejectedElements = rejectedElements;
    }

    /*
     * @see org.eclipse.ui.dialogs.ISelectionValidator#isValid(java.lang.Object)
     */
    @Override
    public IStatus validate(final Object[] elements) {
        if (isValid(elements)) {
            return fgOKStatus;
        }
        return fgErrorStatus;
    }

    private boolean isOfAcceptedType(final Object o) {
        for (int i = 0; i < fAcceptedTypes.length; i++) {
            if (fAcceptedTypes[i].isInstance(o)) {
                return true;
            }
        }
        return false;
    }

    private boolean isRejectedElement(final Object elem) {
        return fRejectedElements != null && fRejectedElements.contains(elem);
    }

    /**
     * @param elem
     *            the element to test
     * @return returns if the selected element is valid
     */
    protected boolean isSelectedValid(final Object elem) {
        return true;
    }

    private boolean isValid(final Object[] selection) {
        if (selection.length == 0) {
            return false;
        }

        if (!fAllowMultipleSelection && selection.length != 1) {
            return false;
        }

        for (int i = 0; i < selection.length; i++) {
            final Object o = selection[i];
            if (!isOfAcceptedType(o) || isRejectedElement(o)
                    || !isSelectedValid(o)) {
                return false;
            }
        }
        return true;
    }
}
