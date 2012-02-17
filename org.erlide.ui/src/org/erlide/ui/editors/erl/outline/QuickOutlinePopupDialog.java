/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.ui.editors.erl.outline;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.PopupDialog;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlExtension;
import org.eclipse.jface.text.IInformationControlExtension2;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.erlide.core.model.root.IErlElement;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.ErlideUIMessages;
import org.erlide.ui.actions.SortAction;
import org.erlide.ui.editors.erl.ErlangEditor;
import org.erlide.ui.editors.erl.outline.filters.FilterDescriptor;
import org.erlide.ui.editors.util.EditorUtility;
import org.erlide.ui.navigator.ErlElementSorter;
import org.erlide.ui.util.StringMatcher;

/**
 * AbstractInfoPopupDialog
 * 
 */
public class QuickOutlinePopupDialog extends PopupDialog implements
        IInformationControl, IInformationControlExtension,
        IInformationControlExtension2, DisposeListener {

    TreeViewer fTreeViewer;
    private final IOutlineContentCreator fOutlineContentCreator;
    private final IOutlineSelectionHandler fOutlineSelectionHandler;
    private Text fFilterText;
    private StringMatcher fStringMatcher;
    private QuickOutlineNamePatternFilter fNamePatternFilter;
    private SortAction fSortAction;
    private ITreeContentProvider fTreeContentProvider;
    private ILabelProvider fTreeLabelProvider;
    private final ErlangEditor fEditor;

    // private ViewerComparator fTreeViewerComparator;
    // private ViewerComparator fTreeViewerDefaultComparator;

    public QuickOutlinePopupDialog(final Shell parent, final int shellStyle,
            final ErlangEditor editor, final IOutlineContentCreator creator,
            final IOutlineSelectionHandler handler) {
        super(parent, shellStyle, true, true, true, true, true, null, null);
        // Set outline creator
        fOutlineContentCreator = creator;
        // Set outline handler
        fOutlineSelectionHandler = handler;
        // Initialize the other fields
        fEditor = editor;
        initialize();
        // Create all controls early to preserve the life cycle of the original
        // implementation.
        create();
    }

    /**
	 *
	 */
    private void initialize() {
        setInfoText(ErlideUIMessages.QuickOutlinePopupDialog_infoTextPressEscToExit);

        fFilterText = null;
        fTreeViewer = null;
        fStringMatcher = null;
        fNamePatternFilter = null;
        fSortAction = null;
        fTreeContentProvider = null;
        fTreeLabelProvider = null;
        // fTreeViewerComparator = null;
        // fTreeViewerDefaultComparator = null;
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        // Applies only to dialog body - not title. See createTitleControl
        // Create an empty dialog area, if the source page is not defined
        if (fOutlineContentCreator == null || fOutlineSelectionHandler == null) {
            return super.createDialogArea(parent);
        }
        // Create the tree viewer
        createUIWidgetTreeViewer(parent);
        // Add listeners to the tree viewer
        createUIListenersTreeViewer();
        // Create the actions
        createUIActions();
        // Add a dispose listner
        addDisposeListener(this);
        // Return the tree
        return fTreeViewer.getControl();
    }

    /**
	 *
	 */
    private void createUIActions() {
        // Add sort action to dialog menu
        fSortAction = new SortAction(
                fTreeViewer,
                ErlideUIMessages.PDEMultiPageContentOutline_SortingAction_tooltip,
                new ErlElementSorter(ErlElementSorter.SORT_ON_NAME),
                new ErlElementSorter(ErlElementSorter.SORT_ON_EXPORT), null,
                false, null);
    }

    @Override
    protected void fillDialogMenu(final IMenuManager dialogMenu) {
        // Add the sort action
        dialogMenu.add(fSortAction);
        // Separator
        dialogMenu.add(new Separator());
        // Add the default actions
        super.fillDialogMenu(dialogMenu);
    }

    /**
     * @param parent
     */
    private void createUIWidgetTreeViewer(final Composite parent) {

        final int style = SWT.H_SCROLL | SWT.V_SCROLL;
        // Create the tree
        final Tree widget = new Tree(parent, style);
        // Configure the layout
        final GridData data = new GridData(GridData.FILL_BOTH);
        data.heightHint = widget.getItemHeight() * 12;
        widget.setLayoutData(data);
        // Create the tree viewer
        fTreeViewer = new TreeViewer(widget);
        // Add member filter, don't show attributes
        final FilterDescriptor filterDescriptor = FilterDescriptor
                .getFilterDescriptor("attributesFilter");
        fTreeViewer.addFilter(filterDescriptor.getViewerFilter());
        // Add the name pattern filter
        fNamePatternFilter = new QuickOutlineNamePatternFilter();
        fTreeViewer.addFilter(fNamePatternFilter);
        // Set the content provider
        fTreeContentProvider = fOutlineContentCreator
                .createOutlineContentProvider();
        fTreeViewer.setContentProvider(fTreeContentProvider);
        // Set the label provider
        fTreeLabelProvider = fOutlineContentCreator
                .createOutlineLabelProvider();
        fTreeViewer.setLabelProvider(fTreeLabelProvider);
        // Create the outline sorter (to be set on the sort action)
        // fTreeViewerComparator = fOutlineContentCreator
        // .createOutlineComparator();
        // Set the comparator to null (sort action will be disabled initially
        // because of this)
        // Create the default outline sorter (Most like this will just return
        // null to indicate sorting disabled
        // fTreeViewerDefaultComparator = fOutlineContentCreator
        // .createDefaultOutlineComparator();
        // fTreeViewer.setComparator(fTreeViewerDefaultComparator);
        fTreeViewer.setAutoExpandLevel(1);
        fTreeViewer.setUseHashlookup(true);
        fTreeViewer.setInput(fOutlineContentCreator.getOutlineInput());
    }

    /**
	 *
	 */
    private void createUIListenersTreeViewer() {
        // Get the underlying tree widget
        final Tree tree = fTreeViewer.getTree();
        // Handle key events
        tree.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.character == 0x1B) {
                    // Dispose on ESC key press
                    dispose();
                }
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                // NO-OP
            }
        });
        // Handle mouse clicks
        tree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(final MouseEvent e) {
                handleTreeViewerMouseUp(tree, e);
            }
        });
        // Handle mouse move events
        tree.addMouseMoveListener(new QuickOutlineMouseMoveListener(fTreeViewer));
        // Handle widget selection events
        tree.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                // NO-OP
            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                gotoSelectedElement();
            }
        });
    }

    /**
     * @param tree
     * @param e
     */
    void handleTreeViewerMouseUp(final Tree tree, final MouseEvent e) {
        // Ensure a selection was made, the first mouse button was
        // used and the event happened in the tree
        if (tree.getSelectionCount() < 1 || e.button != 1
                || !tree.equals(e.getSource())) {
            return;
        }
        // Selection is made in the selection changed listener
        final Object object = tree.getItem(new Point(e.x, e.y));
        final TreeItem selection = tree.getSelection()[0];
        if (selection.equals(object)) {
            gotoSelectedElement();
        }
    }

    /**
     * @return
     */
    private Object getSelectedElement() {
        if (fTreeViewer == null) {
            return null;
        }
        return ((IStructuredSelection) fTreeViewer.getSelection())
                .getFirstElement();
    }

    @Override
    public void addDisposeListener(final DisposeListener listener) {
        getShell().addDisposeListener(listener);
    }

    @Override
    public void addFocusListener(final FocusListener listener) {
        getShell().addFocusListener(listener);
    }

    @Override
    public Point computeSizeHint() {
        // Return the shell's size
        // Note that it already has the persisted size if persisting is enabled.
        return getShell().getSize();
    }

    @Override
    public void dispose() {
        close();
    }

    @Override
    public boolean isFocusControl() {
        if (fTreeViewer.getControl().isFocusControl()
                || fFilterText.isFocusControl()) {
            return true;
        }
        return false;
    }

    @Override
    public void removeDisposeListener(final DisposeListener listener) {
        getShell().removeDisposeListener(listener);
    }

    @Override
    public void removeFocusListener(final FocusListener listener) {
        getShell().removeFocusListener(listener);
    }

    @Override
    public void setBackgroundColor(final Color background) {
        applyBackgroundColor(background, getContents());
    }

    @Override
    public void setFocus() {
        getShell().forceFocus();
        fFilterText.setFocus();
    }

    @Override
    protected Control getFocusControl() {
        return fFilterText;
    }

    @Override
    public void setForegroundColor(final Color foreground) {
        applyForegroundColor(foreground, getContents());
    }

    @Override
    public void setInformation(final String information) {
        // Ignore
        // See IInformationControlExtension2
    }

    @Override
    public void setLocation(final Point location) {
        /*
         * If the location is persisted, it gets managed by PopupDialog - fine.
         * Otherwise, the location is computed in Window#getInitialLocation,
         * which will center it in the parent shell / main monitor, which is
         * wrong for two reasons: - we want to center over the editor / subject
         * control, not the parent shell - the center is computed via the
         * initalSize, which may be also wrong since the size may have been
         * updated since via min/max sizing of
         * AbstractInformationControlManager. In that case, override the
         * location with the one computed by the manager. Note that the call to
         * constrainShellSize in PopupDialog.open will still ensure that the
         * shell is entirely visible.
         */
        if (!getPersistLocation() || !getPersistSize()
                || getDialogSettings() == null) {
            getShell().setLocation(location);
        }
    }

    @Override
    public void setSize(final int width, final int height) {
        getShell().setSize(width, height);
    }

    @Override
    public void setSizeConstraints(final int maxWidth, final int maxHeight) {
        // Ignore
    }

    @Override
    public void setVisible(final boolean visible) {
        if (visible) {
            open();
        } else {
            saveDialogBounds(getShell());
            getShell().setVisible(false);
        }
    }

    @Override
    public boolean hasContents() {
        if (fTreeViewer == null || fTreeViewer.getInput() == null) {
            return false;
        }
        return true;
    }

    @Override
    public void setInput(final Object input) {
        // Input comes from ErlangSourceInfoProvider.getInformation2()
        // The input should be a model object of some sort
        // Turn it into a structured selection and set the selection in the tree
        if (input != null) {
            fTreeViewer.setSelection(new StructuredSelection(input));
        }
    }

    @Override
    public void widgetDisposed(final DisposeEvent e) {
        // Note: We do not reuse the dialog
        fTreeViewer = null;
        fFilterText = null;
    }

    @Override
    protected Control createTitleControl(final Composite parent) {
        // Applies only to dialog title - not body. See createDialogArea
        // Create the text widget
        createUIWidgetFilterText(parent);
        // Add listeners to the text widget
        createUIListenersFilterText();
        // Return the text widget
        return fFilterText;
    }

    /**
     * @param parent
     * @return
     */
    private void createUIWidgetFilterText(final Composite parent) {
        // Create the widget
        fFilterText = new Text(parent, SWT.NONE);
        // Set the font
        final GC gc = new GC(parent);
        gc.setFont(parent.getFont());
        final FontMetrics fontMetrics = gc.getFontMetrics();
        gc.dispose();
        // Create the layout
        final GridData data = new GridData(GridData.FILL_HORIZONTAL);
        data.heightHint = Dialog.convertHeightInCharsToPixels(fontMetrics, 1);
        data.horizontalAlignment = GridData.FILL;
        data.verticalAlignment = GridData.CENTER;
        fFilterText.setLayoutData(data);
    }

    /**
	 *
	 */
    protected void gotoSelectedElement() {
        final Object selectedElement = getSelectedElement();
        ErlLogger.debug("&&>> " + selectedElement);
        if (selectedElement == null) {
            return;
        }
        dispose();
        if (fEditor != null && selectedElement instanceof IErlElement) {
            EditorUtility
                    .revealInEditor(fEditor, (IErlElement) selectedElement);
        }
    }

    /**
	 *
	 */
    private void createUIListenersFilterText() {
        // Handle key events
        fFilterText.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.keyCode == 0x0D) {
                    // Return key was pressed
                    gotoSelectedElement();
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    // Down key was pressed
                    fTreeViewer.getTree().setFocus();
                } else if (e.keyCode == SWT.ARROW_UP) {
                    // Up key was pressed
                    fTreeViewer.getTree().setFocus();
                } else if (e.character == 0x1B) {
                    // Escape key was pressed
                    dispose();
                }
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                // NO-OP
            }
        });
        // Handle text modify events
        fFilterText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                String text = ((Text) e.widget).getText();
                final int length = text.length();
                if (length > 0) {
                    // Append a '*' pattern to the end of the text value if it
                    // does not have one already
                    if (text.charAt(length - 1) != '*') {
                        text = text + '*';
                    }
                    // Prepend a '*' pattern to the beginning of the text value
                    // if it does not have one already
                    if (text.charAt(0) != '*') {
                        text = '*' + text;
                    }
                }
                // Set and update the pattern
                setMatcherString(text, true);
            }
        });
    }

    /**
     * Sets the patterns to filter out for the receiver.
     * <p>
     * The following characters have special meaning: ? => any character * =>
     * any string
     * </p>
     * 
     * @param pattern
     *            the pattern
     * @param update
     *            <code>true</code> if the viewer should be updated
     */
    void setMatcherString(final String pattern, final boolean update) {
        if (pattern.length() == 0) {
            fStringMatcher = null;
        } else {
            fStringMatcher = new StringMatcher(pattern, true, false);
        }
        // Update the name pattern filter on the tree viewer
        fNamePatternFilter.setStringMatcher(fStringMatcher);
        // Update the tree viewer according to the pattern
        if (update) {
            stringMatcherUpdated();
        }
    }

    /**
     * The string matcher has been modified. The default implementation
     * refreshes the view and selects the first matched element
     */
    private void stringMatcherUpdated() {
        // Refresh the tree viewer to re-filter
        fTreeViewer.getControl().setRedraw(false);
        fTreeViewer.refresh();
        fTreeViewer.expandAll();
        selectFirstMatch();
        fTreeViewer.getControl().setRedraw(true);
    }

    /**
     * Selects the first element in the tree which matches the current filter
     * pattern.
     */
    private void selectFirstMatch() {
        final Tree tree = fTreeViewer.getTree();
        final Object element = findFirstMatchToPattern(tree.getItems());
        if (element != null) {
            fTreeViewer.setSelection(new StructuredSelection(element), true);
        } else {
            fTreeViewer.setSelection(StructuredSelection.EMPTY);
        }
    }

    /**
     * @param items
     * @return
     */
    private Object findFirstMatchToPattern(final TreeItem[] items) {
        // Match the string pattern against labels
        final ILabelProvider labelProvider = (ILabelProvider) fTreeViewer
                .getLabelProvider();
        // Process each item in the tree
        for (int i = 0; i < items.length; i++) {
            Object element = items[i].getData();
            // Return the first element if no pattern is set
            if (fStringMatcher == null) {
                return element;
            }
            // Return the element if it matches the pattern
            if (element != null) {
                final String label = labelProvider.getText(element);
                if (fStringMatcher.match(label)) {
                    return element;
                }
            }
            // Recursively check the elements children for a match
            element = findFirstMatchToPattern(items[i].getItems());
            // Return the child element match if found
            if (element != null) {
                return element;
            }
        }
        // No match found
        return null;
    }

    @Override
    protected Point getInitialSize() {
        return new Point(400, 250);
    }

}
