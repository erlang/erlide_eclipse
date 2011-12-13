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
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.erlide.ui.util.PixelConverter;
import org.erlide.ui.util.SWTUtil;

/**
 * A list with a button bar. Typical buttons are 'Add', 'Remove', 'Up' and
 * 'Down'. List model is independend of widget creation. DialogFields controls
 * are: Label, List and Composite containing buttons.
 */
public class TreeListDialogField extends DialogField {

    protected TreeViewer fTree;

    protected ILabelProvider fLabelProvider;

    protected TreeViewerAdapter fTreeViewerAdapter;

    protected List<Object> fElements;

    protected ViewerSorter fViewerSorter;

    protected String[] fButtonLabels;

    private Button[] fButtonControls;

    private boolean[] fButtonsEnabled;

    private int fRemoveButtonIndex;

    private int fUpButtonIndex;

    private int fDownButtonIndex;

    private Label fLastSeparator;

    protected Tree fTreeControl;

    private Composite fButtonsControl;

    private ISelection fSelectionWhenEnabled;

    protected ITreeListAdapter fTreeAdapter;

    protected Object fParentElement;

    private int fTreeExpandLevel;

    /**
     * @param adapter
     *            Can be <code>null</code>.
     */
    public TreeListDialogField(final ITreeListAdapter adapter,
            final String[] buttonLabels, final ILabelProvider lprovider) {
        super();
        fTreeAdapter = adapter;

        fLabelProvider = lprovider;
        fTreeViewerAdapter = new TreeViewerAdapter();
        fParentElement = this;

        fElements = new ArrayList<Object>(10);

        fButtonLabels = buttonLabels;
        if (fButtonLabels != null) {
            final int nButtons = fButtonLabels.length;
            fButtonsEnabled = new boolean[nButtons];
            for (int i = 0; i < nButtons; i++) {
                fButtonsEnabled[i] = true;
            }
        }

        fTree = null;
        fTreeControl = null;
        fButtonsControl = null;

        fRemoveButtonIndex = -1;
        fUpButtonIndex = -1;
        fDownButtonIndex = -1;

        fTreeExpandLevel = 0;
    }

    /**
     * Sets the index of the 'remove' button in the button label array passed in
     * the constructor. The behaviour of the button marked as the 'remove'
     * button will then behandled internally. (enable state, button invocation
     * behaviour)
     */
    public void setRemoveButtonIndex(final int removeButtonIndex) {
        Assert.isTrue(removeButtonIndex < fButtonLabels.length);
        fRemoveButtonIndex = removeButtonIndex;
    }

    /**
     * Sets the index of the 'up' button in the button label array passed in the
     * constructor. The behaviour of the button marked as the 'up' button will
     * then behandled internally. (enable state, button invocation behaviour)
     */
    public void setUpButtonIndex(final int upButtonIndex) {
        Assert.isTrue(upButtonIndex < fButtonLabels.length);
        fUpButtonIndex = upButtonIndex;
    }

    /**
     * Sets the index of the 'down' button in the button label array passed in
     * the constructor. The behaviour of the button marked as the 'down' button
     * will then be handled internally. (enable state, button invocation
     * behaviour)
     */
    public void setDownButtonIndex(final int downButtonIndex) {
        Assert.isTrue(downButtonIndex < fButtonLabels.length);
        fDownButtonIndex = downButtonIndex;
    }

    /**
     * Sets the viewerSorter.
     * 
     * @param viewerSorter
     *            The viewerSorter to set
     */
    public void setViewerSorter(final ViewerSorter viewerSorter) {
        fViewerSorter = viewerSorter;
    }

    public void setTreeExpansionLevel(final int level) {
        fTreeExpandLevel = level;
        if (isOkToUse(fTreeControl) && fTreeExpandLevel > 0) {
            fTree.expandToLevel(level);
        }
    }

    // ------ adapter communication

    private void buttonPressed(final int index) {
        if (!managedButtonPressed(index) && fTreeAdapter != null) {
            fTreeAdapter.customButtonPressed(this, index);
        }
    }

    /**
     * Checks if the button pressed is handled internally
     * 
     * @return Returns true if button has been handled.
     */
    protected boolean managedButtonPressed(final int index) {
        if (index == fRemoveButtonIndex) {
            remove();
        } else if (index == fUpButtonIndex) {
            up();
        } else if (index == fDownButtonIndex) {
            down();
        } else {
            return false;
        }
        return true;
    }

    // ------ layout helpers

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
        final PixelConverter converter = new PixelConverter(parent);

        assertEnoughColumns(nColumns);

        final Label label = getLabelControl(parent);
        GridData gd = gridDataForLabel(1);
        gd.verticalAlignment = GridData.BEGINNING;
        label.setLayoutData(gd);

        final Control list = getTreeControl(parent);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = false;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalSpan = nColumns - 2;
        gd.widthHint = converter.convertWidthInCharsToPixels(50);
        gd.heightHint = converter.convertHeightInCharsToPixels(6);

        list.setLayoutData(gd);

        final Composite buttons = getButtonBox(parent);
        gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = false;
        gd.verticalAlignment = GridData.FILL;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalSpan = 1;
        buttons.setLayoutData(gd);

        return new Control[] { label, list, buttons };
    }

    /*
     * @see DialogField#getNumberOfControls
     */
    @Override
    public int getNumberOfControls() {
        return 3;
    }

    /**
     * Sets the minimal width of the buttons. Must be called after widget
     * creation.
     */
    public void setButtonsMinWidth(final int minWidth) {
        if (fLastSeparator != null) {
            ((GridData) fLastSeparator.getLayoutData()).widthHint = minWidth;
        }
    }

    // ------ ui creation

    /**
     * Returns the tree control. When called the first time, the control will be
     * created.
     * 
     * @param parent
     *            The parent composite when called the first time, or
     *            <code>null</code> after.
     */
    public Control getTreeControl(final Composite parent) {
        if (fTreeControl == null) {
            assertCompositeNotNull(parent);

            fTree = createTreeViewer(parent);

            fTreeControl = (Tree) fTree.getControl();
            fTreeControl.addKeyListener(new KeyAdapter() {

                @Override
                public void keyPressed(final KeyEvent e) {
                    handleKeyPressed(e);
                }
            });
            fTree.setContentProvider(fTreeViewerAdapter);
            fTree.setLabelProvider(fLabelProvider);
            fTree.addSelectionChangedListener(fTreeViewerAdapter);
            fTree.addDoubleClickListener(fTreeViewerAdapter);

            fTree.setInput(fParentElement);
            fTree.expandToLevel(fTreeExpandLevel);

            if (fViewerSorter != null) {
                fTree.setSorter(fViewerSorter);
            }

            fTreeControl.setEnabled(isEnabled());
            if (fSelectionWhenEnabled != null) {
                postSetSelection(fSelectionWhenEnabled);
            }
        }
        return fTreeControl;
    }

    /**
     * Returns the internally used table viewer.
     */
    public TreeViewer getTreeViewer() {
        return fTree;
    }

    /*
     * Subclasses may override to specify a different style.
     */
    protected int getTreeStyle() {
        final int style = SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL;
        return style;
    }

    protected TreeViewer createTreeViewer(final Composite parent) {
        final Tree tree = new Tree(parent, getTreeStyle());
        return new TreeViewer(tree);
    }

    protected Button createButton(final Composite parent, final String label,
            final SelectionListener listener) {
        final Button button = new Button(parent, SWT.PUSH);
        button.setText(label);
        button.addSelectionListener(listener);
        final GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = true;
        gd.verticalAlignment = GridData.BEGINNING;
        gd.heightHint = SWTUtil.getButtonHeightHint(button);
        gd.widthHint = SWTUtil.getButtonWidthHint(button);

        button.setLayoutData(gd);
        return button;
    }

    private Label createSeparator(final Composite parent) {
        final Label separator = new Label(parent, SWT.NONE);
        separator.setVisible(false);
        final GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.BEGINNING;
        gd.heightHint = 4;
        separator.setLayoutData(gd);
        return separator;
    }

    /**
     * Returns the composite containing the buttons. When called the first time,
     * the control will be created.
     * 
     * @param parent
     *            The parent composite when called the first time, or
     *            <code>null</code> after.
     */
    public Composite getButtonBox(final Composite parent) {
        if (fButtonsControl == null) {
            assertCompositeNotNull(parent);

            final SelectionListener listener = new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                    doButtonSelected(e);
                }

                @Override
                public void widgetSelected(final SelectionEvent e) {
                    doButtonSelected(e);
                }
            };

            final Composite contents = new Composite(parent, SWT.NULL);
            final GridLayout layout = new GridLayout();
            layout.marginWidth = 0;
            layout.marginHeight = 0;
            contents.setLayout(layout);

            if (fButtonLabels != null) {
                fButtonControls = new Button[fButtonLabels.length];
                for (int i = 0; i < fButtonLabels.length; i++) {
                    final String currLabel = fButtonLabels[i];
                    if (currLabel != null) {
                        fButtonControls[i] = createButton(contents, currLabel,
                                listener);
                        fButtonControls[i].setEnabled(isEnabled()
                                && fButtonsEnabled[i]);
                    } else {
                        fButtonControls[i] = null;
                        createSeparator(contents);
                    }
                }
            }

            fLastSeparator = createSeparator(contents);

            updateButtonState();
            fButtonsControl = contents;
        }

        return fButtonsControl;
    }

    protected void doButtonSelected(final SelectionEvent e) {
        if (fButtonControls != null) {
            for (int i = 0; i < fButtonControls.length; i++) {
                if (e.widget == fButtonControls[i]) {
                    buttonPressed(i);
                    return;
                }
            }
        }
    }

    /**
     * Handles key events in the table viewer. Specifically when the delete key
     * is pressed.
     */
    protected void handleKeyPressed(final KeyEvent event) {
        if (event.character == SWT.DEL && event.stateMask == 0) {
            if (fRemoveButtonIndex != -1
                    && isButtonEnabled(fTree.getSelection(), fRemoveButtonIndex)) {
                managedButtonPressed(fRemoveButtonIndex);
                return;
            }
        }
        fTreeAdapter.keyPressed(this, event);
    }

    // ------ enable / disable management

    /*
     * @see DialogField#dialogFieldChanged
     */
    @Override
    public void dialogFieldChanged() {
        super.dialogFieldChanged();
        updateButtonState();
    }

    /*
     * Updates the enable state of the all buttons
     */
    protected void updateButtonState() {
        if (fButtonControls != null) {
            final ISelection sel = fTree.getSelection();
            for (int i = 0; i < fButtonControls.length; i++) {
                final Button button = fButtonControls[i];
                if (isOkToUse(button)) {
                    button.setEnabled(isButtonEnabled(sel, i));
                }
            }
        }
    }

    protected boolean containsAttributes(final List<Object> selected) {
        for (int i = 0; i < selected.size(); i++) {
            if (!fElements.contains(selected.get(i))) {
                return true;
            }
        }
        return false;
    }

    protected boolean getManagedButtonState(final ISelection sel,
            final int index) {
        final List<Object> selected = getSelectedElements();
        final boolean hasAttributes = containsAttributes(selected);
        if (index == fRemoveButtonIndex) {
            return !selected.isEmpty() && !hasAttributes;
        } else if (index == fUpButtonIndex) {
            return !sel.isEmpty() && !hasAttributes && canMoveUp(selected);
        } else if (index == fDownButtonIndex) {
            return !sel.isEmpty() && !hasAttributes && canMoveDown(selected);
        }
        return true;
    }

    /*
     * @see DialogField#updateEnableState
     */
    @Override
    protected void updateEnableState() {
        super.updateEnableState();

        final boolean enabled = isEnabled();
        if (isOkToUse(fTreeControl)) {
            if (!enabled) {
                fSelectionWhenEnabled = fTree.getSelection();
                selectElements(null);
            } else {
                selectElements(fSelectionWhenEnabled);
                fSelectionWhenEnabled = null;
            }
            fTreeControl.setEnabled(enabled);
        }
        updateButtonState();
    }

    /**
     * Sets a button enabled or disabled.
     */
    public void enableButton(final int index, final boolean enable) {
        if (fButtonsEnabled != null && index < fButtonsEnabled.length) {
            fButtonsEnabled[index] = enable;
            updateButtonState();
        }
    }

    private boolean isButtonEnabled(final ISelection sel, final int index) {
        final boolean extraState = getManagedButtonState(sel, index);
        return isEnabled() && extraState && fButtonsEnabled[index];
    }

    // ------ model access

    /**
     * Sets the elements shown in the list.
     */
    public void setElements(final List<Object> elements) {
        fElements = new ArrayList<Object>(elements);
        refresh();
        if (isOkToUse(fTreeControl)) {
            fTree.expandToLevel(fTreeExpandLevel);
        }
        dialogFieldChanged();
    }

    /**
     * Gets the elements shown in the list. The list returned is a copy, so it
     * can be modified by the user.
     */
    public List<Object> getElements() {
        return new ArrayList<Object>(fElements);
    }

    /**
     * Gets the element shown at the given index.
     */
    public Object getElement(final int index) {
        return fElements.get(index);
    }

    /**
     * Gets the index of an element in the list or -1 if element is not in list.
     */
    public int getIndexOfElement(final Object elem) {
        return fElements.indexOf(elem);
    }

    /**
     * Replace an element.
     */
    public void replaceElement(final Object oldElement, final Object newElement) {
        final int idx = fElements.indexOf(oldElement);
        if (idx != -1) {
            fElements.set(idx, newElement);
            if (isOkToUse(fTreeControl)) {
                final List<Object> selected = getSelectedElements();
                if (selected.remove(oldElement)) {
                    selected.add(newElement);
                }
                final boolean isExpanded = fTree.getExpandedState(oldElement);
                fTree.remove(oldElement);
                fTree.add(fParentElement, newElement);
                if (isExpanded) {
                    fTree.expandToLevel(newElement, fTreeExpandLevel);
                }
                selectElements(new StructuredSelection(selected));
            }
            dialogFieldChanged();
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Adds an element at the end of the tree list.
     */
    public void addElement(final Object element) {
        if (fElements.contains(element)) {
            return;
        }
        fElements.add(element);
        if (isOkToUse(fTreeControl)) {
            fTree.add(fParentElement, element);
            fTree.expandToLevel(element, fTreeExpandLevel);
        }
        dialogFieldChanged();
    }

    /**
     * Adds elements at the end of the tree list.
     */
    public void addElements(final List<Object> elements) {
        final int nElements = elements.size();

        if (nElements > 0) {
            // filter duplicated
            final ArrayList<Object> elementsToAdd = new ArrayList<Object>(
                    nElements);

            for (int i = 0; i < nElements; i++) {
                final Object elem = elements.get(i);
                if (!fElements.contains(elem)) {
                    elementsToAdd.add(elem);
                }
            }
            fElements.addAll(elementsToAdd);
            if (isOkToUse(fTreeControl)) {
                fTree.add(fParentElement, elementsToAdd.toArray());
                for (int i = 0; i < elementsToAdd.size(); i++) {
                    fTree.expandToLevel(elementsToAdd.get(i), fTreeExpandLevel);
                }
            }
            dialogFieldChanged();
        }
    }

    /**
     * Adds an element at a position.
     */
    public void insertElementAt(final Object element, final int index) {
        if (fElements.contains(element)) {
            return;
        }
        fElements.add(index, element);
        if (isOkToUse(fTreeControl)) {
            fTree.add(fParentElement, element);
            if (fTreeExpandLevel != -1) {
                fTree.expandToLevel(element, fTreeExpandLevel);
            }
        }

        dialogFieldChanged();
    }

    /**
     * Adds an element at a position.
     */
    public void removeAllElements() {
        if (fElements.size() > 0) {
            fElements.clear();
            refresh();
            dialogFieldChanged();
        }
    }

    /**
     * Removes an element from the list.
     */
    public void removeElement(final Object element) {
        if (fElements.remove(element)) {
            if (isOkToUse(fTreeControl)) {
                fTree.remove(element);
            }
            dialogFieldChanged();
        } else {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Removes elements from the list.
     */
    public void removeElements(final List<Object> elements) {
        if (elements.size() > 0) {
            fElements.removeAll(elements);
            if (isOkToUse(fTreeControl)) {
                fTree.remove(elements.toArray());
            }
            dialogFieldChanged();
        }
    }

    /**
     * Gets the number of elements
     */
    public int getSize() {
        return fElements.size();
    }

    public void selectElements(final ISelection selection) {
        fSelectionWhenEnabled = selection;
        if (isOkToUse(fTreeControl)) {
            fTree.setSelection(selection, true);
        }
    }

    public void selectFirstElement() {
        Object element = null;
        if (fViewerSorter != null) {
            final Object[] arr = fElements.toArray();
            fViewerSorter.sort(fTree, arr);
            if (arr.length > 0) {
                element = arr[0];
            }
        } else {
            if (fElements.size() > 0) {
                element = fElements.get(0);
            }
        }
        if (element != null) {
            selectElements(new StructuredSelection(element));
        }
    }

    public void postSetSelection(final ISelection selection) {
        if (isOkToUse(fTreeControl)) {
            final Display d = fTreeControl.getDisplay();
            d.asyncExec(new Runnable() {

                @Override
                public void run() {
                    if (isOkToUse(fTreeControl)) {
                        selectElements(selection);
                    }
                }
            });
        }
    }

    /**
     * Refreshes the tree.
     */
    @Override
    public void refresh() {
        super.refresh();
        if (isOkToUse(fTreeControl)) {
            fTree.refresh();
        }
    }

    /**
     * Refreshes the tree.
     */
    public void refresh(final Object element) {
        if (isOkToUse(fTreeControl)) {
            fTree.refresh(element);
        }
    }

    // ------- list maintenance

    private List<Object> moveUp(final List<Object> elements,
            final List<Object> move) {
        final int nElements = elements.size();
        final List<Object> res = new ArrayList<Object>(nElements);
        Object floating = null;
        for (int i = 0; i < nElements; i++) {
            final Object curr = elements.get(i);
            if (move.contains(curr)) {
                res.add(curr);
            } else {
                if (floating != null) {
                    res.add(floating);
                }
                floating = curr;
            }
        }
        if (floating != null) {
            res.add(floating);
        }
        return res;
    }

    private void moveUp(final List<Object> toMoveUp) {
        if (toMoveUp.size() > 0) {
            setElements(moveUp(fElements, toMoveUp));
            fTree.reveal(toMoveUp.get(0));
        }
    }

    private void moveDown(final List<Object> toMoveDown) {
        if (toMoveDown.size() > 0) {
            setElements(reverse(moveUp(reverse(fElements), toMoveDown)));
            fTree.reveal(toMoveDown.get(toMoveDown.size() - 1));
        }
    }

    private List<Object> reverse(final List<Object> p) {
        final List<Object> reverse = new ArrayList<Object>(p.size());
        for (int i = p.size() - 1; i >= 0; i--) {
            reverse.add(p.get(i));
        }
        return reverse;
    }

    private void remove() {
        removeElements(getSelectedElements());
    }

    private void up() {
        moveUp(getSelectedElements());
    }

    private void down() {
        moveDown(getSelectedElements());
    }

    private boolean canMoveUp(final List<Object> selectedElements) {
        if (isOkToUse(fTreeControl)) {
            int nSelected = selectedElements.size();
            final int nElements = fElements.size();
            for (int i = 0; i < nElements && nSelected > 0; i++) {
                if (!selectedElements.contains(fElements.get(i))) {
                    return true;
                }
                nSelected--;
            }
        }
        return false;
    }

    private boolean canMoveDown(final List<Object> selectedElements) {
        if (isOkToUse(fTreeControl)) {
            int nSelected = selectedElements.size();
            for (int i = fElements.size() - 1; i >= 0 && nSelected > 0; i--) {
                if (!selectedElements.contains(fElements.get(i))) {
                    return true;
                }
                nSelected--;
            }
        }
        return false;
    }

    /**
     * Returns the selected elements.
     */
    public List<Object> getSelectedElements() {
        final ArrayList<Object> result = new ArrayList<Object>();
        if (isOkToUse(fTreeControl)) {
            final ISelection selection = fTree.getSelection();
            if (selection instanceof IStructuredSelection) {
                final Iterator<?> iter = ((IStructuredSelection) selection)
                        .iterator();
                while (iter.hasNext()) {
                    result.add(iter.next());
                }
            }
        }
        return result;
    }

    public void expandElement(final Object element, final int level) {
        if (isOkToUse(fTreeControl)) {
            fTree.expandToLevel(element, level);
        }
    }

    // ------- TreeViewerAdapter

    class TreeViewerAdapter implements ITreeContentProvider,
            ISelectionChangedListener, IDoubleClickListener {

        private final Object[] NO_ELEMENTS = new Object[0];

        // ------- ITreeContentProvider Interface ------------

        @Override
        public void inputChanged(final Viewer viewer, final Object oldInput,
                final Object newInput) {
            // will never happen
        }

        public boolean isDeleted(final Object element) {
            return false;
        }

        @Override
        public void dispose() {
        }

        @Override
        public Object[] getElements(final Object obj) {
            return fElements.toArray();
        }

        @Override
        public Object[] getChildren(final Object element) {
            if (fTreeAdapter != null) {
                return fTreeAdapter.getChildren(TreeListDialogField.this,
                        element);
            }
            return NO_ELEMENTS;
        }

        @Override
        public Object getParent(final Object element) {
            if (!fElements.contains(element) && fTreeAdapter != null) {
                return fTreeAdapter
                        .getParent(TreeListDialogField.this, element);
            }
            return fParentElement;
        }

        @Override
        public boolean hasChildren(final Object element) {
            if (fTreeAdapter != null) {
                return fTreeAdapter.hasChildren(TreeListDialogField.this,
                        element);
            }
            return false;
        }

        // ------- ISelectionChangedListener Interface ------------

        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            doListSelected(event);
        }

        @Override
        public void doubleClick(final DoubleClickEvent event) {
            doDoubleClick(event);
        }

    }

    protected void doListSelected(final SelectionChangedEvent event) {
        updateButtonState();
        if (fTreeAdapter != null) {
            fTreeAdapter.selectionChanged(this);
        }
    }

    protected void doDoubleClick(final DoubleClickEvent event) {
        if (fTreeAdapter != null) {
            fTreeAdapter.doubleClicked(this);
        }
    }

}
