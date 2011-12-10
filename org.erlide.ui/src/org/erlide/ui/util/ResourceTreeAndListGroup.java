package org.erlide.ui.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.commands.common.EventManager;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Tree;

/**
 * Workbench-level composite that combines a CheckboxTreeViewer and
 * CheckboxListViewer. All viewer selection-driven interactions are handled
 * within this object
 */
public class ResourceTreeAndListGroup extends EventManager implements
        ICheckStateListener, ISelectionChangedListener, ITreeViewerListener {
    Object root;

    private Object currentTreeSelection;
    Collection<Object> expandedTreeNodes = new HashSet<Object>();
    Map<Object, List<Object>> checkedStateStore = new HashMap<Object, List<Object>>(
            9);
    Collection<Object> whiteCheckedTreeItems = new HashSet<Object>();
    final ITreeContentProvider treeContentProvider;
    private final IStructuredContentProvider listContentProvider;
    private final ILabelProvider treeLabelProvider;
    private final ILabelProvider listLabelProvider;

    // widgets
    CheckboxTreeViewer treeViewer;
    CheckboxTableViewer listViewer;
    private Composite folderComposite;

    // height hint for viewers
    private static int PREFERRED_HEIGHT = 150;

    /**
     * Create an instance of this class. Use this constructor if you wish to
     * specify the width and/or height of the combined widget (to only hardcode
     * one of the sizing dimensions, specify the other dimension's value as -1)
     * 
     * @param parent
     * @param rootObject
     * @param treeContentProvider
     * @param treeLabelProvider
     * @param listContentProvider
     * @param listLabelProvider
     * @param style
     * @param useHeightHint
     *            If true then use the height hint to make this group big enough
     * 
     */
    public ResourceTreeAndListGroup(final Composite parent,
            final Object rootObject,
            final ITreeContentProvider treeContentProvider,
            final ILabelProvider treeLabelProvider,
            final IStructuredContentProvider listContentProvider,
            final ILabelProvider listLabelProvider, final int style,
            final boolean useHeightHint) {

        root = rootObject;
        this.treeContentProvider = treeContentProvider;
        this.listContentProvider = listContentProvider;
        this.treeLabelProvider = treeLabelProvider;
        this.listLabelProvider = listLabelProvider;
        createContents(parent, style, useHeightHint);

    }

    /**
     * This method must be called just before this window becomes visible.
     */
    public void aboutToOpen() {
        determineWhiteCheckedDescendents(root);
        checkNewTreeElements(treeContentProvider.getElements(root));
        currentTreeSelection = null;

        // select the first element in the list
        final Object[] elements = treeContentProvider.getElements(root);
        final Object primary = elements.length > 0 ? elements[0] : null;
        if (primary != null) {
            treeViewer.setSelection(new StructuredSelection(primary));
        }
        treeViewer.getControl().setFocus();
    }

    /**
     * Add the passed listener to self's collection of clients that listen for
     * changes to element checked states
     * 
     * @param listener
     *            ICheckStateListener
     */
    public void addCheckStateListener(final ICheckStateListener listener) {
        addListenerObject(listener);
    }

    /**
     * Return a boolean indicating whether all children of the passed tree
     * element are currently white-checked
     * 
     * @return boolean
     * @param treeElement
     *            java.lang.Object
     */
    protected boolean areAllChildrenWhiteChecked(final Object treeElement) {
        final Object[] children = treeContentProvider.getChildren(treeElement);
        for (int i = 0; i < children.length; ++i) {
            if (!whiteCheckedTreeItems.contains(children[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Return a boolean indicating whether all list elements associated with the
     * passed tree element are currently checked
     * 
     * @return boolean
     * @param treeElement
     *            java.lang.Object
     */
    protected boolean areAllElementsChecked(final Object treeElement) {
        final List<Object> checkedElements = checkedStateStore.get(treeElement);
        if (checkedElements == null) {
            return false;
        }

        return getListItemsSize(treeElement) == checkedElements.size();
    }

    /**
     * Iterate through the passed elements which are being realized for the
     * first time and check each one in the tree viewer as appropriate
     */
    protected void checkNewTreeElements(final Object[] elements) {
        for (int i = 0; i < elements.length; ++i) {
            final Object currentElement = elements[i];
            final boolean checked = checkedStateStore
                    .containsKey(currentElement);
            treeViewer.setChecked(currentElement, checked);
            treeViewer.setGrayed(currentElement, checked
                    && !whiteCheckedTreeItems.contains(currentElement));
        }
    }

    /**
     * An item was checked in one of self's two views. Determine which view this
     * occurred in and delegate appropriately
     * 
     * @param event
     *            CheckStateChangedEvent
     */
    @Override
    public void checkStateChanged(final CheckStateChangedEvent event) {

        // Potentially long operation - show a busy cursor
        BusyIndicator.showWhile(treeViewer.getControl().getDisplay(),
                new Runnable() {
                    @Override
                    public void run() {
                        if (event.getCheckable().equals(treeViewer)) {
                            treeItemChecked(event.getElement(),
                                    event.getChecked());
                        } else {
                            listItemChecked(event.getElement(),
                                    event.getChecked(), true);
                        }

                        notifyCheckStateChangeListeners(event);
                    }
                });
    }

    /**
     * Lay out and initialize self's visual components.
     * 
     * @param parent
     *            org.eclipse.swt.widgets.Composite
     * @param style
     *            the style flags for the new Composite
     * @param useHeightHint
     *            If true yse the preferredHeight.
     */
    protected void createContents(final Composite parent, final int style,
            final boolean useHeightHint) {
        // group pane
        folderComposite = new Composite(parent, style);
        folderComposite.setFont(parent.getFont());
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        layout.makeColumnsEqualWidth = true;
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        folderComposite.setLayout(layout);
        folderComposite.setLayoutData(new GridData(GridData.FILL_BOTH));

        createTreeViewer(folderComposite, useHeightHint);
        createListViewer(folderComposite, useHeightHint);

        initialize();
    }

    /**
     * Create this group's list viewer.
     */
    protected void createListViewer(final Composite parent,
            final boolean useHeightHint) {
        listViewer = CheckboxTableViewer.newCheckList(parent, SWT.BORDER);
        final GridData data = new GridData(GridData.FILL_BOTH);
        if (useHeightHint) {
            data.heightHint = PREFERRED_HEIGHT;
        }
        listViewer.getTable().setLayoutData(data);
        listViewer.getTable().setFont(parent.getFont());
        listViewer.setContentProvider(listContentProvider);
        listViewer.setLabelProvider(listLabelProvider);
        listViewer.addCheckStateListener(this);
    }

    /**
     * Create this group's tree viewer.
     */
    protected void createTreeViewer(final Composite parent,
            final boolean useHeightHint) {
        final Tree tree = new Tree(parent, SWT.CHECK | SWT.BORDER);
        final GridData data = new GridData(GridData.FILL_BOTH);
        if (useHeightHint) {
            data.heightHint = PREFERRED_HEIGHT;
        }
        tree.setLayoutData(data);
        tree.setFont(parent.getFont());

        treeViewer = new CheckboxTreeViewer(tree);
        treeViewer.setContentProvider(treeContentProvider);
        treeViewer.setLabelProvider(treeLabelProvider);
        treeViewer.addTreeListener(this);
        treeViewer.addCheckStateListener(this);
        treeViewer.addSelectionChangedListener(this);
    }

    /**
     * Returns a boolean indicating whether the passed tree element should be at
     * LEAST gray-checked. Note that this method does not consider whether it
     * should be white-checked, so a specified tree item which should be
     * white-checked will result in a <code>true</code> answer from this method.
     * To determine whether a tree item should be white-checked use method
     * #determineShouldBeWhiteChecked(Object).
     * 
     * @param treeElement
     *            java.lang.Object
     * @return boolean
     * @see #determineShouldBeWhiteChecked(Object)
     */
    protected boolean determineShouldBeAtLeastGrayChecked(
            final Object treeElement) {
        // if any list items associated with treeElement are checked then it
        // retains its gray-checked status regardless of its children
        final List<Object> checked = checkedStateStore.get(treeElement);
        if (checked != null && !checked.isEmpty()) {
            return true;
        }

        // if any children of treeElement are still gray-checked then
        // treeElement
        // must remain gray-checked as well. Only ask expanded nodes
        if (expandedTreeNodes.contains(treeElement)) {
            final Object[] children = treeContentProvider
                    .getChildren(treeElement);
            for (int i = 0; i < children.length; ++i) {
                if (checkedStateStore.containsKey(children[i])) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Returns a boolean indicating whether the passed tree item should be
     * white-checked.
     * 
     * @return boolean
     * @param treeElement
     *            java.lang.Object
     */
    protected boolean determineShouldBeWhiteChecked(final Object treeElement) {
        return areAllChildrenWhiteChecked(treeElement)
                && areAllElementsChecked(treeElement);
    }

    /**
     * Recursively add appropriate tree elements to the collection of known
     * white-checked tree elements.
     * 
     * @param treeElement
     *            java.lang.Object
     */
    protected void determineWhiteCheckedDescendents(final Object treeElement) {
        // always go through all children first since their white-checked
        // statuses will be needed to determine the white-checked status for
        // this tree element
        final Object[] children = treeContentProvider.getElements(treeElement);
        for (int i = 0; i < children.length; ++i) {
            determineWhiteCheckedDescendents(children[i]);
        }

        // now determine the white-checked status for this tree element
        if (determineShouldBeWhiteChecked(treeElement)) {
            setWhiteChecked(treeElement, true);
        }
    }

    /**
     * Cause the tree viewer to expand all its items
     */
    public void expandAll() {
        treeViewer.expandAll();
    }

    /**
     * Expand an element in a tree viewer
     */
    private void expandTreeElement(final Object item) {
        BusyIndicator.showWhile(treeViewer.getControl().getDisplay(),
                new Runnable() {
                    @Override
                    public void run() {

                        // First see if the children need to be given their
                        // checked state at all. If they've
                        // already been realized then this won't be necessary
                        if (expandedTreeNodes.contains(item)) {
                            checkNewTreeElements(treeContentProvider
                                    .getChildren(item));
                        } else {

                            expandedTreeNodes.add(item);
                            if (whiteCheckedTreeItems.contains(item)) {
                                // If this is the first expansion and this is a
                                // white checked node then check the children
                                final Object[] children = treeContentProvider
                                        .getChildren(item);
                                for (int i = 0; i < children.length; ++i) {
                                    if (!whiteCheckedTreeItems
                                            .contains(children[i])) {
                                        final Object child = children[i];
                                        setWhiteChecked(child, true);
                                        treeViewer.setChecked(child, true);
                                        checkedStateStore.put(child,
                                                new ArrayList<Object>());
                                    }
                                }

                                // Now be sure to select the list of items too
                                setListForWhiteSelection(item);
                            }
                        }

                    }
                });
    }

    /**
     * Add all of the selected children of nextEntry to result recursively. This
     * does not set any values in the checked state.
     * 
     * @param The
     *            treeElement being queried
     * @param addAll
     *            a boolean to indicate if the checked state store needs to be
     *            queried
     * @param filter
     *            IElementFilter - the filter being used on the data
     * @param monitor
     *            IProgressMonitor or null that the cancel is polled for
     */
    private void findAllSelectedListElements(final Object treeElement,
            final String parentLabel, final boolean addAll,
            final IElementFilter filter, final IProgressMonitor monitor)
            throws InterruptedException {

        String fullLabel = null;
        if (monitor != null && monitor.isCanceled()) {
            return;
        }
        if (monitor != null) {
            fullLabel = getFullLabel(treeElement, parentLabel);
            monitor.subTask(fullLabel);
        }

        if (addAll) {
            filter.filterElements(listContentProvider.getElements(treeElement),
                    monitor);
        } else { // Add what we have stored
            if (checkedStateStore.containsKey(treeElement)) {
                filter.filterElements(checkedStateStore.get(treeElement),
                        monitor);
            }
        }

        final Object[] treeChildren = treeContentProvider
                .getChildren(treeElement);
        for (int i = 0; i < treeChildren.length; i++) {
            final Object child = treeChildren[i];
            if (addAll) {
                findAllSelectedListElements(child, fullLabel, true, filter,
                        monitor);
            } else { // Only continue for those with checked state
                if (checkedStateStore.containsKey(child)) {
                    findAllSelectedListElements(child, fullLabel,
                            whiteCheckedTreeItems.contains(child), filter,
                            monitor);
                }
            }

        }
    }

    /**
     * Find all of the white checked children of the treeElement and add them to
     * the collection. If the element itself is white select add it. If not then
     * add any selected list elements and recurse down to the children.
     * 
     * @param treeElement
     *            java.lang.Object
     * @param result
     *            java.util.Collection
     */
    private void findAllWhiteCheckedItems(final Object treeElement,
            final Collection<Object> result) {

        if (whiteCheckedTreeItems.contains(treeElement)) {
            result.add(treeElement);
        } else {
            final List<Object> listChildren = checkedStateStore
                    .get(treeElement);
            // if it is not in the store then it and it's children are not
            // interesting
            if (listChildren == null) {
                return;
            }
            result.addAll(listChildren);
            final Object[] children = treeContentProvider
                    .getChildren(treeElement);
            for (int i = 0; i < children.length; ++i) {
                findAllWhiteCheckedItems(children[i], result);
            }
        }
    }

    /**
     * Returns a flat list of all of the leaf elements which are checked. Filter
     * then based on the supplied ElementFilter. If monitor is cancelled then
     * return null
     * 
     * @param filter
     *            - the filter for the data
     * @param monitor
     *            IProgressMonitor or null
     * @throws InterruptedException
     *             If the find is interrupted.
     */
    public void getAllCheckedListItems(final IElementFilter filter,
            final IProgressMonitor monitor) throws InterruptedException {

        // Iterate through the children of the root as the root is not in the
        // store
        final Object[] children = treeContentProvider.getChildren(root);
        for (int i = 0; i < children.length; ++i) {
            findAllSelectedListElements(children[i], null,
                    whiteCheckedTreeItems.contains(children[i]), filter,
                    monitor);
        }
    }

    /**
     * Returns a flat list of all of the leaf elements which are checked.
     * 
     * @return all of the leaf elements which are checked. This API does not
     *         return null in order to keep backwards compatibility.
     */
    public List<Object> getAllCheckedListItems() {

        final ArrayList<Object> returnValue = new ArrayList<Object>();

        final IElementFilter passThroughFilter = new IElementFilter() {

            @Override
            public void filterElements(final Collection<?> elements,
                    final IProgressMonitor monitor) {
                returnValue.addAll(elements);
            }

            @Override
            public void filterElements(final Object[] elements,
                    final IProgressMonitor monitor) {
                for (int i = 0; i < elements.length; i++) {
                    returnValue.add(elements[i]);
                }
            }
        };

        try {
            getAllCheckedListItems(passThroughFilter, null);
        } catch (final InterruptedException exception) {
            return new ArrayList<Object>();
        }
        return returnValue;

    }

    /**
     * Returns a list of all of the items that are white checked. Any folders
     * that are white checked are added and then any files from white checked
     * folders are added.
     * 
     * @return the list of all of the items that are white checked
     */
    public List<Object> getAllWhiteCheckedItems() {

        final List<Object> result = new ArrayList<Object>();

        // Iterate through the children of the root as the root is not in the
        // store
        final Object[] children = treeContentProvider.getChildren(root);
        for (int i = 0; i < children.length; ++i) {
            findAllWhiteCheckedItems(children[i], result);
        }

        return result;
    }

    /**
     * Answer the number of elements that have been checked by the user.
     * 
     * @return int
     */
    public int getCheckedElementCount() {
        return checkedStateStore.size();
    }

    /**
     * Get the full label of the treeElement (its name and its parent's name).
     * 
     * @param treeElement
     *            - the element being exported
     * @param parentLabel
     *            - the label of the parent, can be null
     * @return String
     */
    protected String getFullLabel(final Object treeElement,
            final String parentLabel) {
        String label = parentLabel;
        if (parentLabel == null) {
            label = ""; //$NON-NLS-1$
        }
        final IPath parentName = new Path(label);

        final String elementText = treeLabelProvider.getText(treeElement);
        if (elementText == null) {
            return parentName.toString();
        }
        return parentName.append(elementText).toString();
    }

    /**
     * Return a count of the number of list items associated with a given tree
     * item.
     * 
     * @return int
     * @param treeElement
     *            java.lang.Object
     */
    protected int getListItemsSize(final Object treeElement) {
        final Object[] elements = listContentProvider.getElements(treeElement);
        return elements.length;
    }

    /**
     * Get the table the list viewer uses.
     * 
     * @return org.eclipse.swt.widgets.Table
     */
    public Table getListTable() {
        return listViewer.getTable();
    }

    /**
     * Logically gray-check all ancestors of treeItem by ensuring that they
     * appear in the checked table
     */
    protected void grayCheckHierarchy(final Object treeElement) {

        // expand the element first to make sure we have populated for it
        expandTreeElement(treeElement);

        // if this tree element is already gray then its ancestors all are as
        // well
        if (checkedStateStore.containsKey(treeElement)) {
            return; // no need to proceed upwards from here
        }

        checkedStateStore.put(treeElement, new ArrayList<Object>());
        final Object parent = treeContentProvider.getParent(treeElement);
        if (parent != null) {
            grayCheckHierarchy(parent);
        }
    }

    /**
     * Set the checked state of self and all ancestors appropriately. Do not
     * white check anyone - this is only done down a hierarchy.
     */
    private void grayUpdateHierarchy(final Object treeElement) {

        final boolean shouldBeAtLeastGray = determineShouldBeAtLeastGrayChecked(treeElement);

        treeViewer.setGrayChecked(treeElement, shouldBeAtLeastGray);

        if (whiteCheckedTreeItems.contains(treeElement)) {
            whiteCheckedTreeItems.remove(treeElement);
        }

        // proceed up the tree element hierarchy
        final Object parent = treeContentProvider.getParent(treeElement);
        if (parent != null) {
            grayUpdateHierarchy(parent);
        }
    }

    /**
     * Set the initial checked state of the passed list element to true.
     * 
     * @param element
     */
    public void initialCheckListItem(final Object element) {
        final Object parent = treeContentProvider.getParent(element);
        selectAndReveal(parent);
        // Check the element in the viewer as if it had been manually checked
        listViewer.setChecked(element, true);
        // As this is not done from the UI then set the box for updating from
        // the selection to false
        listItemChecked(element, true, false);
        grayUpdateHierarchy(parent);
    }

    /**
     * Set the initial checked state of the passed element to true, as well as
     * to all of its children and associated list elements
     * 
     * @param element
     */
    public void initialCheckTreeItem(final Object element) {
        treeItemChecked(element, true);
        selectAndReveal(element);
    }

    private void selectAndReveal(final Object treeElement) {
        treeViewer.reveal(treeElement);
        final IStructuredSelection selection = new StructuredSelection(
                treeElement);
        treeViewer.setSelection(selection);
    }

    /**
     * Initialize this group's viewers after they have been laid out.
     */
    protected void initialize() {
        treeViewer.setInput(root);
        expandedTreeNodes = new ArrayList<Object>();
        expandedTreeNodes.add(root);

    }

    /**
     * Call-back that's invoked when the checked status of an item in the list
     * is changed by the user. Do not try and update the hierarchy if we are
     * building the initial list.
     */
    protected void listItemChecked(final Object listElement,
            final boolean state, final boolean updatingFromSelection) {
        List<Object> checkedListItems = checkedStateStore
                .get(currentTreeSelection);
        // If it has not been expanded do so as the selection of list items will
        // affect gray state
        if (!expandedTreeNodes.contains(currentTreeSelection)) {
            expandTreeElement(currentTreeSelection);
        }

        if (state) {
            if (checkedListItems == null) {
                // since the associated tree item has gone from 0 -> 1 checked
                // list items, tree checking may need to be updated
                grayCheckHierarchy(currentTreeSelection);
                checkedListItems = checkedStateStore.get(currentTreeSelection);
            }
            checkedListItems.add(listElement);
        } else {
            checkedListItems.remove(listElement);
            if (checkedListItems.isEmpty()) {
                // since the associated tree item has gone from 1 -> 0 checked
                // list items, tree checking may need to be updated
                ungrayCheckHierarchy(currentTreeSelection);
            }
        }

        // Update the list with the selections if there are any
        if (checkedListItems.size() > 0) {
            checkedStateStore.put(currentTreeSelection, checkedListItems);
        }
        if (updatingFromSelection) {
            grayUpdateHierarchy(currentTreeSelection);
        }
    }

    /**
     * Notify all checked state listeners that the passed element has had its
     * checked state changed to the passed state
     */
    protected void notifyCheckStateChangeListeners(
            final CheckStateChangedEvent event) {
        final Object[] array = getListeners();
        for (int i = 0; i < array.length; i++) {
            final ICheckStateListener l = (ICheckStateListener) array[i];
            SafeRunner.run(new SafeRunnable() {
                @Override
                public void run() {
                    l.checkStateChanged(event);
                }
            });
        }
    }

    /**
     * Set the contents of the list viewer based upon the specified selected
     * tree element. This also includes checking the appropriate list items.
     * 
     * @param treeElement
     *            java.lang.Object
     */
    protected void populateListViewer(final Object treeElement) {
        listViewer.setInput(treeElement);

        // If the element is white checked but not expanded we have not set up
        // all of the children yet
        if (!expandedTreeNodes.contains(treeElement)
                && whiteCheckedTreeItems.contains(treeElement)) {

            // Potentially long operation - show a busy cursor
            BusyIndicator.showWhile(treeViewer.getControl().getDisplay(),
                    new Runnable() {
                        @Override
                        public void run() {
                            setListForWhiteSelection(treeElement);
                            listViewer.setAllChecked(true);
                        }
                    });

        } else {
            final List<Object> listItemsToCheck = checkedStateStore
                    .get(treeElement);

            if (listItemsToCheck != null) {
                final Iterator<Object> listItemsEnum = listItemsToCheck
                        .iterator();
                while (listItemsEnum.hasNext()) {
                    listViewer.setChecked(listItemsEnum.next(), true);
                }
            }
        }
    }

    /**
     * Logically gray-check all ancestors of treeItem by ensuring that they
     * appear in the checked table. Add any elements to the selectedNodes so we
     * can track that has been done.
     */
    private void primeHierarchyForSelection(final Object item,
            final Set<Object> selectedNodes) {

        // Only prime it if we haven't visited yet
        if (selectedNodes.contains(item)) {
            return;
        }

        checkedStateStore.put(item, new ArrayList<Object>());

        // mark as expanded as we are going to populate it after this
        expandedTreeNodes.add(item);
        selectedNodes.add(item);

        final Object parent = treeContentProvider.getParent(item);
        if (parent != null) {
            primeHierarchyForSelection(parent, selectedNodes);
        }
    }

    /**
     * Remove the passed listener from self's collection of clients that listen
     * for changes to element checked states
     * 
     * @param listener
     *            ICheckStateListener
     */
    public void removeCheckStateListener(final ICheckStateListener listener) {
        removeListenerObject(listener);
    }

    /**
     * Handle the selection of an item in the tree viewer
     * 
     * @param event
     *            SelectionChangedEvent
     */
    @Override
    public void selectionChanged(final SelectionChangedEvent event) {
        final IStructuredSelection selection = (IStructuredSelection) event
                .getSelection();
        final Object selectedElement = selection.getFirstElement();
        if (selectedElement == null) {
            currentTreeSelection = null;
            listViewer.setInput(currentTreeSelection);
            return;
        }

        // ie.- if not an item deselection
        if (selectedElement != currentTreeSelection) {
            populateListViewer(selectedElement);
        }

        currentTreeSelection = selectedElement;
    }

    /**
     * Select or deselect all of the elements in the tree depending on the value
     * of the selection boolean. Be sure to update the displayed files as well.
     * 
     * @param selection
     */
    public void setAllSelections(final boolean selection) {

        // If there is no root there is nothing to select
        if (root == null) {
            return;
        }

        // Potentially long operation - show a busy cursor
        BusyIndicator.showWhile(treeViewer.getControl().getDisplay(),
                new Runnable() {
                    @Override
                    public void run() {
                        setTreeChecked(root, selection);
                        listViewer.setAllChecked(selection);
                    }
                });
    }

    /**
     * The treeElement has been white selected. Get the list for the element and
     * set it in the checked state store.
     * 
     * @param treeElement
     *            the element being updated
     */
    void setListForWhiteSelection(final Object treeElement) {

        final Object[] listItems = listContentProvider.getElements(treeElement);
        final List<Object> listItemsChecked = new ArrayList<Object>();
        for (int i = 0; i < listItems.length; ++i) {
            listItemsChecked.add(listItems[i]);
        }

        checkedStateStore.put(treeElement, listItemsChecked);
    }

    /**
     * Set the list viewer's providers to those passed
     * 
     * @param contentProvider
     *            ITreeContentProvider
     * @param labelProvider
     *            ILabelProvider
     */
    public void setListProviders(
            final IStructuredContentProvider contentProvider,
            final ILabelProvider labelProvider) {
        listViewer.setContentProvider(contentProvider);
        listViewer.setLabelProvider(labelProvider);
    }

    /**
     * Set the comparator that is to be applied to self's list viewer
     * 
     * @param comparator
     */
    public void setListComparator(final ViewerComparator comparator) {
        listViewer.setComparator(comparator);
    }

    /**
     * Set the root of the widget to be new Root. Regenerate all of the tables
     * and lists from this value.
     * 
     * @param newRoot
     */
    public void setRoot(final Object newRoot) {
        root = newRoot;
        initialize();
    }

    /**
     * Set the checked state of the passed tree element appropriately, and do so
     * recursively to all of its child tree elements as well
     */
    protected void setTreeChecked(final Object treeElement, final boolean state) {

        if (treeElement.equals(currentTreeSelection)) {
            listViewer.setAllChecked(state);
        }

        if (state) {
            setListForWhiteSelection(treeElement);
        } else {
            checkedStateStore.remove(treeElement);
        }

        setWhiteChecked(treeElement, state);
        treeViewer.setChecked(treeElement, state);
        treeViewer.setGrayed(treeElement, false);

        // now logically check/uncheck all children as well if it has been
        // expanded
        if (expandedTreeNodes.contains(treeElement)) {
            final Object[] children = treeContentProvider
                    .getChildren(treeElement);
            for (int i = 0; i < children.length; ++i) {
                setTreeChecked(children[i], state);
            }
        }
    }

    /**
     * Set the tree viewer's providers to those passed
     * 
     * @param contentProvider
     *            ITreeContentProvider
     * @param labelProvider
     *            ILabelProvider
     */
    public void setTreeProviders(final ITreeContentProvider contentProvider,
            final ILabelProvider labelProvider) {
        treeViewer.setContentProvider(contentProvider);
        treeViewer.setLabelProvider(labelProvider);
    }

    /**
     * Set the sorter that is to be applied to self's tree viewer
     * 
     * @param sorter
     */
    public void setTreeComparator(final ViewerComparator comparator) {
        treeViewer.setComparator(comparator);
    }

    /**
     * Adjust the collection of references to white-checked tree elements
     * appropriately.
     * 
     * @param treeElement
     *            java.lang.Object
     * @param isWhiteChecked
     *            boolean
     */
    protected void setWhiteChecked(final Object treeElement,
            final boolean isWhiteChecked) {
        if (isWhiteChecked) {
            if (!whiteCheckedTreeItems.contains(treeElement)) {
                whiteCheckedTreeItems.add(treeElement);
            }
        } else {
            whiteCheckedTreeItems.remove(treeElement);
        }
    }

    /**
     * Handle the collapsing of an element in a tree viewer
     */
    @Override
    public void treeCollapsed(final TreeExpansionEvent event) {
        // We don't need to do anything with this
    }

    /**
     * Handle the expansionsion of an element in a tree viewer
     */
    @Override
    public void treeExpanded(final TreeExpansionEvent event) {
        expandTreeElement(event.getElement());
    }

    /**
     * Callback that's invoked when the checked status of an item in the tree is
     * changed by the user.
     */
    protected void treeItemChecked(final Object treeElement, final boolean state) {

        // recursively adjust all child tree elements appropriately
        setTreeChecked(treeElement, state);

        final Object parent = treeContentProvider.getParent(treeElement);
        if (parent == null) {
            return;
        }

        // now update upwards in the tree hierarchy
        if (state) {
            grayCheckHierarchy(parent);
        } else {
            ungrayCheckHierarchy(parent);
        }

        // Update the hierarchy but do not white select the parent
        grayUpdateHierarchy(parent);
    }

    /**
     * Logically un-gray-check all ancestors of treeItem iff appropriate.
     */
    protected void ungrayCheckHierarchy(final Object treeElement) {
        if (!determineShouldBeAtLeastGrayChecked(treeElement)) {
            checkedStateStore.remove(treeElement);
        }

        final Object parent = treeContentProvider.getParent(treeElement);
        if (parent != null) {
            ungrayCheckHierarchy(parent);
        }
    }

    /**
     * Set the checked state of self and all ancestors appropriately
     */
    protected void updateHierarchy(final Object treeElement) {

        final boolean whiteChecked = determineShouldBeWhiteChecked(treeElement);
        final boolean shouldBeAtLeastGray = determineShouldBeAtLeastGrayChecked(treeElement);

        treeViewer.setChecked(treeElement, shouldBeAtLeastGray);
        setWhiteChecked(treeElement, whiteChecked);
        if (whiteChecked) {
            treeViewer.setGrayed(treeElement, false);
        } else {
            treeViewer.setGrayed(treeElement, shouldBeAtLeastGray);
        }

        // proceed up the tree element hierarchy but gray select all of them
        final Object parent = treeContentProvider.getParent(treeElement);
        if (parent != null) {
            grayUpdateHierarchy(parent);
        }
    }

    /**
     * Update the selections of the tree elements in items to reflect the new
     * selections provided.
     * 
     * @param items
     *            Map with keys of Object (the tree element) and values of List
     *            (the selected list elements). NOTE: This method does not
     *            special case keys with no values (i.e., a tree element with an
     *            empty list). If a tree element does not have any selected
     *            items, do not include the element in the Map.
     */
    public void updateSelections(final Map<Object, List<Object>> items) {
        // We are replacing all selected items with the given selected items,
        // so reinitialize everything.
        listViewer.setAllChecked(false);
        treeViewer.setCheckedElements(new Object[0]);
        whiteCheckedTreeItems = new HashSet<Object>();
        final Set<Object> selectedNodes = new HashSet<Object>();
        checkedStateStore = new HashMap<Object, List<Object>>();

        // Update the store before the hierarchy to prevent updating parents
        // before all of the children are done
        for (final Entry<Object, List<Object>> entry : items.entrySet()) {
            // Replace the items in the checked state store with those from the
            // supplied items
            checkedStateStore.put(entry, entry.getValue());
            selectedNodes.add(entry);
            // proceed up the tree element hierarchy
            final Object parent = treeContentProvider.getParent(entry);
            if (parent != null) {
                // proceed up the tree element hierarchy and make sure
                // everything is in the table
                primeHierarchyForSelection(parent, selectedNodes);
            }
        }

        // Update the checked tree items. Since each tree item has a selected
        // item, all the tree items will be gray checked.
        treeViewer.setCheckedElements(checkedStateStore.keySet().toArray());
        treeViewer.setGrayedElements(checkedStateStore.keySet().toArray());

        // Update the listView of the currently selected tree item.
        if (currentTreeSelection != null) {
            final Object displayItems = items.get(currentTreeSelection);
            if (displayItems != null) {
                listViewer.setCheckedElements(((List<?>) displayItems)
                        .toArray());
            }
        }
    }

    /**
	 * 
	 */
    public void enableFolderComposite(final boolean enabled) {
        folderComposite.setEnabled(enabled);
    }

    /**
     * Set the focus on to the list widget.
     */
    public void setFocus() {

        treeViewer.getTree().setFocus();
    }

}
