package org.erlide.ui.editors.erl.outline.filters;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.SelectionDialog;
import org.erlide.ui.util.SWTUtil;
import org.erlide.utils.ListsUtils;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class CustomOutlineFiltersDialog extends SelectionDialog {

    private static final String SEPARATOR = ","; //$NON-NLS-1$

    private final String fViewId;
    private boolean fEnablePatterns;
    private List<String> fPatterns;
    private final Set<String> fEnabledFilterIds;

    private final List<FilterDescriptor> fBuiltInFilters;

    private CheckboxTableViewer fCheckBoxList;
    private Button fEnableUserDefinedPatterns;
    private Text fUserDefinedPatterns;

    private final Stack<FilterDescriptor> fFilterDescriptorChangeHistory;

    /**
     * Creates a dialog to customize Erlang element filters.
     * 
     * @param shell
     *            the parent shell
     * @param viewId
     *            the id of the view
     * @param enablePatterns
     *            <code>true</code> if pattern filters are enabled
     * @param patterns
     *            the filter patterns
     * @param enabledFilterIds
     *            the Ids of the enabled filters
     */
    public CustomOutlineFiltersDialog(final Shell shell, final String viewId,
            final boolean enablePatterns, final Collection<String> patterns,
            final Collection<String> enabledFilterIds) {

        super(shell);
        Assert.isNotNull(viewId);
        Assert.isNotNull(patterns);
        Assert.isNotNull(enabledFilterIds);

        fViewId = viewId;
        fPatterns = Lists.newArrayList(patterns);
        fEnablePatterns = enablePatterns;
        fEnabledFilterIds = Sets.newHashSet(enabledFilterIds);

        fBuiltInFilters = FilterDescriptor.getFilterDescriptors(fViewId);
        fFilterDescriptorChangeHistory = new Stack<FilterDescriptor>();
    }

    @Override
    protected void configureShell(final Shell shell) {
        setTitle("Erlang Element Filters");
        setMessage("S&elect the elements to exclude from the view:");
        super.configureShell(shell);
        // TODO help id
        // PlatformUI.getWorkbench().getHelpSystem().setHelp(shell, -1);
    }

    /**
     * Overrides method in Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        initializeDialogUnits(parent);
        // create a composite with standard margins and spacing
        final Composite composite = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
        layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
        layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
        layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(GridData.FILL_BOTH));
        composite.setFont(parent.getFont());
        final Composite group = composite;

        // Checkbox
        fEnableUserDefinedPatterns = new Button(group, SWT.CHECK);
        fEnableUserDefinedPatterns.setFocus();
        fEnableUserDefinedPatterns
                .setText("&Name filter patterns (matching names will be hidden):");

        // Pattern field
        fUserDefinedPatterns = new Text(group, SWT.SINGLE | SWT.BORDER);
        final GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL);
        data.widthHint = convertWidthInCharsToPixels(59);
        fUserDefinedPatterns.setLayoutData(data);
        final String patterns = convertToString(fPatterns, SEPARATOR);
        fUserDefinedPatterns.setText(patterns);
        // TODO SWTUtil.setAccessibilityText(
        // fUserDefinedPatterns,
        // "Name filter patterns. The patterns are separated by comma, where star is any string");

        // Info text
        final Label info = new Label(group, SWT.LEFT);
        info.setText("The patterns are separated by comma, where\n* = any string, ? = any character, ,, = ,");

        // Enabling / disabling of pattern group
        fEnableUserDefinedPatterns.setSelection(fEnablePatterns);
        fUserDefinedPatterns.setEnabled(fEnablePatterns);
        info.setEnabled(fEnablePatterns);
        fEnableUserDefinedPatterns.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final boolean state = fEnableUserDefinedPatterns.getSelection();
                fUserDefinedPatterns.setEnabled(state);
                info.setEnabled(fEnableUserDefinedPatterns.getSelection());
                if (state) {
                    fUserDefinedPatterns.setFocus();
                }
            }
        });

        // Filters provided by extension point
        if (fBuiltInFilters.size() > 0) {
            createCheckBoxList(group);
        }

        applyDialogFont(parent);
        return parent;
    }

    private String convertToString(final List<String> patterns,
            final String separator) {
        return ListsUtils.packList(patterns, separator);
    }

    private void createCheckBoxList(final Composite parent) {
        // Filler
        new Label(parent, SWT.NONE);

        Label info = new Label(parent, SWT.LEFT);
        info.setText("S&elect the elements to exclude from the view:");

        fCheckBoxList = CheckboxTableViewer.newCheckList(parent, SWT.BORDER);
        GridData data = new GridData(GridData.FILL_BOTH);
        data.heightHint = fCheckBoxList.getTable().getItemHeight() * 10;
        fCheckBoxList.getTable().setLayoutData(data);

        fCheckBoxList.setLabelProvider(createLabelPrivder());
        fCheckBoxList.setContentProvider(new ArrayContentProvider());
        Collections.sort(fBuiltInFilters);
        fCheckBoxList.setInput(fBuiltInFilters);
        setInitialSelections(getEnabledFilterDescriptors().toArray());

        final List<?> initialSelection = getInitialElementSelections();
        if (initialSelection != null && !initialSelection.isEmpty()) {
            checkInitialSelections();
        }

        // Description
        info = new Label(parent, SWT.LEFT);
        info.setText("Filter description:");
        final Text description = new Text(parent, SWT.LEFT | SWT.WRAP
                | SWT.MULTI | SWT.READ_ONLY | SWT.BORDER | SWT.V_SCROLL);
        data = new GridData(GridData.FILL_HORIZONTAL);
        data.heightHint = convertHeightInCharsToPixels(3);
        description.setLayoutData(data);
        fCheckBoxList
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    @Override
                    public void selectionChanged(
                            final SelectionChangedEvent event) {
                        final ISelection selection = event.getSelection();
                        if (selection instanceof IStructuredSelection) {
                            final Object selectedElement = ((IStructuredSelection) selection)
                                    .getFirstElement();
                            if (selectedElement instanceof FilterDescriptor) {
                                description
                                        .setText(((FilterDescriptor) selectedElement)
                                                .getDescription());
                            }
                        }
                    }
                });
        fCheckBoxList.addCheckStateListener(new ICheckStateListener() {
            /*
             * @see
             * org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged
             * (org.eclipse.jface.viewers.CheckStateChangedEvent)
             */
            @Override
            public void checkStateChanged(final CheckStateChangedEvent event) {
                final Object element = event.getElement();
                if (element instanceof FilterDescriptor) {
                    // renew if already touched
                    if (fFilterDescriptorChangeHistory.contains(element)) {
                        fFilterDescriptorChangeHistory.remove(element);
                    }
                    fFilterDescriptorChangeHistory
                            .push((FilterDescriptor) element);
                }
            }
        });

        addSelectionButtons(parent);
    }

    private void addSelectionButtons(final Composite composite) {
        final Composite buttonComposite = new Composite(composite, SWT.RIGHT);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        buttonComposite.setLayout(layout);
        final GridData data = new GridData(GridData.HORIZONTAL_ALIGN_END
                | GridData.GRAB_HORIZONTAL);
        data.grabExcessHorizontalSpace = true;
        composite.setData(data);

        // Select All button
        final Button selectButton = createButton(buttonComposite,
                IDialogConstants.SELECT_ALL_ID, "&Select All", false);
        SWTUtil.setButtonDimensionHint(selectButton);
        SelectionListener listener = new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                fCheckBoxList.setAllChecked(true);
                fFilterDescriptorChangeHistory.clear();
                for (final FilterDescriptor desc : fBuiltInFilters) {
                    fFilterDescriptorChangeHistory.push(desc);
                }
            }
        };
        selectButton.addSelectionListener(listener);

        final Button deselectButton = createButton(buttonComposite,
                IDialogConstants.DESELECT_ALL_ID, "&Deselect All", false);
        SWTUtil.setButtonDimensionHint(deselectButton);
        listener = new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                fCheckBoxList.setAllChecked(false);
                fFilterDescriptorChangeHistory.clear();
                for (final FilterDescriptor desc : fBuiltInFilters) {
                    fFilterDescriptorChangeHistory.push(desc);
                }
            }
        };
        deselectButton.addSelectionListener(listener);
    }

    private void checkInitialSelections() {
        for (final Object item : getInitialElementSelections()) {
            fCheckBoxList.setChecked(item, true);
        }
    }

    @Override
    protected void okPressed() {
        if (fBuiltInFilters != null) {
            final List<FilterDescriptor> result = Lists.newArrayList();
            for (final FilterDescriptor desc : fBuiltInFilters) {
                if (fCheckBoxList.getChecked(desc)) {
                    result.add(desc);
                }
            }
            setResult(result);
        }
        super.okPressed();
    }

    private ILabelProvider createLabelPrivder() {
        return new LabelProvider() {
            @Override
            public Image getImage(final Object element) {
                return null;
            }

            @Override
            public String getText(final Object element) {
                if (element instanceof FilterDescriptor) {
                    return ((FilterDescriptor) element).getName();
                } else {
                    return null;
                }
            }
        };
    }

    // ---------- result handling ----------

    @Override
    @SuppressWarnings("rawtypes")
    protected void setResult(final List newResult) {
        super.setResult(newResult);
        if (fUserDefinedPatterns.getText().length() > 0) {
            fEnablePatterns = fEnableUserDefinedPatterns.getSelection();
            fPatterns = convertFromString(fUserDefinedPatterns.getText(),
                    SEPARATOR);
        } else {
            fEnablePatterns = false;
            fPatterns = Lists.newArrayList();
        }
    }

    private List<String> convertFromString(final String text,
            final String separator) {
        return ListsUtils.unpackList(text, separator);
    }

    /**
     * @return the patterns which have been entered by the user
     */
    public List<String> getUserDefinedPatterns() {
        return fPatterns;
    }

    /**
     * @return the Ids of the enabled built-in filters
     */
    public Set<String> getEnabledFilterIds() {
        final Set<String> result = Sets.newHashSet();
        for (final Object item : getResult()) {
            final FilterDescriptor desc = (FilterDescriptor) item;
            result.add(desc.getId());
        }
        return result;
    }

    /**
     * @return <code>true</code> if the user-defined patterns are disabled
     */
    public boolean areUserDefinedPatternsEnabled() {
        return fEnablePatterns;
    }

    /**
     * @return a stack with the filter descriptor check history
     * @since 3.0
     */
    public Stack<FilterDescriptor> getFilterDescriptorChangeHistory() {
        return fFilterDescriptorChangeHistory;
    }

    private List<FilterDescriptor> getEnabledFilterDescriptors() {
        final List<FilterDescriptor> filterDescs = fBuiltInFilters;
        final List<FilterDescriptor> result = Lists
                .newArrayListWithCapacity(filterDescs.size());
        for (final FilterDescriptor desc : filterDescs) {
            if (fEnabledFilterIds.contains(desc.getId())) {
                result.add(desc);
            }
        }
        return result;
    }
}
