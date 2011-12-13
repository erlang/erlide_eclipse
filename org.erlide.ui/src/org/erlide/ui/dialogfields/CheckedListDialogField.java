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
package org.erlide.ui.dialogfields;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;

/**
 * A list with checkboxes and a button bar. Typical buttons are 'Check All' and
 * 'Uncheck All'. List model is independend of widget creation. DialogFields
 * controls are: Label, List and Composite containing buttons.
 */
public class CheckedListDialogField<Element> extends ListDialogField<Element> {

    private int fCheckAllButtonIndex;

    private int fUncheckAllButtonIndex;

    private List<Element> fCheckElements;

    public CheckedListDialogField(final IListAdapter<Element> adapter,
            final String[] customButtonLabels, final ILabelProvider lprovider) {
        super(adapter, customButtonLabels, lprovider);
        fCheckElements = new ArrayList<Element>();

        fCheckAllButtonIndex = -1;
        fUncheckAllButtonIndex = -1;
    }

    /**
     * Sets the index of the 'check' button in the button label array passed in
     * the constructor. The behaviour of the button marked as the check button
     * will then be handled internally. (enable state, button invocation
     * behaviour)
     */
    public void setCheckAllButtonIndex(final int checkButtonIndex) {
        Assert.isTrue(checkButtonIndex < fButtonLabels.length);
        fCheckAllButtonIndex = checkButtonIndex;
    }

    /**
     * Sets the index of the 'uncheck' button in the button label array passed
     * in the constructor. The behaviour of the button marked as the uncheck
     * button will then be handled internally. (enable state, button invocation
     * behaviour)
     */
    public void setUncheckAllButtonIndex(final int uncheckButtonIndex) {
        Assert.isTrue(uncheckButtonIndex < fButtonLabels.length);
        fUncheckAllButtonIndex = uncheckButtonIndex;
    }

    /*
     * @see ListDialogField#createTableViewer
     */
    @Override
    protected TableViewer createTableViewer(final Composite parent) {
        final Table table = new Table(parent, SWT.CHECK + getListStyle());
        final CheckboxTableViewer tableViewer = new CheckboxTableViewer(table);
        tableViewer.addCheckStateListener(new ICheckStateListener() {

            @Override
            public void checkStateChanged(final CheckStateChangedEvent e) {
                doCheckStateChanged(e);
            }
        });
        return tableViewer;
    }

    /*
     * @see ListDialogField#getListControl
     */
    @Override
    public Control getListControl(final Composite parent) {
        final Control control = super.getListControl(parent);
        if (parent != null) {
            ((CheckboxTableViewer) fTable).setCheckedElements(fCheckElements
                    .toArray());
        }
        return control;
    }

    /*
     * @see DialogField#dialogFieldChanged Hooks in to get element changes to
     * update check model.
     */
    @Override
    public void dialogFieldChanged() {
        for (int i = fCheckElements.size() - 1; i >= 0; i--) {
            if (!fElements.contains(fCheckElements.get(i))) {
                fCheckElements.remove(i);
            }
        }
        super.dialogFieldChanged();
    }

    private void checkStateChanged() {
        // call super and do not update check model
        super.dialogFieldChanged();
    }

    /**
     * Gets the checked elements.
     */
    public List<Object> getCheckedElements() {
        if (isOkToUse(fTableControl)) {
            // workaround for bug 53853
            final Object[] checked = ((CheckboxTableViewer) fTable)
                    .getCheckedElements();
            final ArrayList<Object> res = new ArrayList<Object>(checked.length);
            for (final Object element : checked) {
                res.add(element);
            }
            return res;
        }

        return new ArrayList<Object>(fCheckElements);
    }

    /**
     * Returns the number of checked elements.
     */
    public int getCheckedSize() {
        return fCheckElements.size();
    }

    /**
     * Returns true if the element is checked.
     */
    public boolean isChecked(final Element obj) {
        if (isOkToUse(fTableControl)) {
            return ((CheckboxTableViewer) fTable).getChecked(obj);
        }

        return fCheckElements.contains(obj);
    }

    /**
     * Sets the checked elements.
     */
    public void setCheckedElements(final Collection<? extends Element> list) {
        fCheckElements = new ArrayList<Element>(list);
        if (isOkToUse(fTableControl)) {
            ((CheckboxTableViewer) fTable).setCheckedElements(list.toArray());
        }
        checkStateChanged();
    }

    /**
     * Sets the checked state of an element.
     */
    public void setChecked(final Element element, final boolean state) {
        setCheckedWithoutUpdate(element, state);
        checkStateChanged();
    }

    /**
     * Sets the checked state of an element. No dialog changed listener is
     * informed.
     */
    public void setCheckedWithoutUpdate(final Element object,
            final boolean state) {
        if (state) {
            if (!fCheckElements.contains(object)) {
                fCheckElements.add(object);
            }
        } else {
            fCheckElements.remove(object);
        }
        if (isOkToUse(fTableControl)) {
            ((CheckboxTableViewer) fTable).setChecked(object, state);
        }
    }

    /**
     * Sets the check state of all elements
     */
    public void checkAll(final boolean state) {
        if (state) {
            fCheckElements = getElements();
        } else {
            fCheckElements.clear();
        }
        if (isOkToUse(fTableControl)) {
            ((CheckboxTableViewer) fTable).setAllChecked(state);
        }
        checkStateChanged();
    }

    @SuppressWarnings("unchecked")
    protected void doCheckStateChanged(final CheckStateChangedEvent e) {
        if (e.getChecked()) {
            fCheckElements.add((Element) e.getElement());
        } else {
            fCheckElements.remove(e.getElement());
        }
        checkStateChanged();
    }

    @Override
    public void replaceElement(final Element oldElement,
            final Element newElement) {
        final boolean wasChecked = isChecked(oldElement);
        super.replaceElement(oldElement, newElement);
        setChecked(newElement, wasChecked);
    }

    // ------ enable / disable management

    /*
     * @see ListDialogField#getManagedButtonState
     */
    @Override
    protected boolean getManagedButtonState(final ISelection sel,
            final int index) {
        if (index == fCheckAllButtonIndex) {
            return !fElements.isEmpty();
        } else if (index == fUncheckAllButtonIndex) {
            return !fElements.isEmpty();
        }
        return super.getManagedButtonState(sel, index);
    }

    /*
     * @see ListDialogField#extraButtonPressed
     */
    @Override
    protected boolean managedButtonPressed(final int index) {
        if (index == fCheckAllButtonIndex) {
            checkAll(true);
        } else if (index == fUncheckAllButtonIndex) {
            checkAll(false);
        } else {
            return super.managedButtonPressed(index);
        }
        return true;
    }

}
