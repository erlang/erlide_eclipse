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
package org.erlide.ui.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.FileSystemElement;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.wizards.datatransfer.FileSystemStructureProvider;
import org.eclipse.ui.wizards.datatransfer.IImportStructureProvider;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;

public class ErlangProjectImportWizardPage extends
        ErlangWizardResourceImportPage implements Listener, IOverwriteQuery {
    // widgets
    protected Combo sourceNameField;
    protected Button overwriteExistingResourcesCheckbox;
    protected Button copyProjectsIntoWorkspaceCheckbox;
    // protected Button createContainerStructureButton;
    // protected Button createOnlySelectedButton;
    protected Button sourceBrowseButton;
    protected Button selectTypesButton;
    // protected Button selectAllButton;
    // protected Button deselectAllButton;

    // A boolean to indicate if the user has typed anything
    boolean entryChanged = false;
    boolean copyFiles = ErlangProjectImportWizard.COPY_ONLY;

    // dialog store id constants
    private final static String STORE_SOURCE_NAMES_ID = "WizardFileSystemResourceImportPage1.STORE_SOURCE_NAMES_ID";//$NON-NLS-1$
    private final static String STORE_OVERWRITE_EXISTING_RESOURCES_ID = "WizardFileSystemResourceImportPage1.STORE_OVERWRITE_EXISTING_RESOURCES_ID";//$NON-NLS-1$
    private static final String SELECT_TYPES_TITLE = WizardMessages.DataTransfer_selectTypes;

    // private static final String SELECT_ALL_TITLE =
    // ErlangDataTransferMessages.DataTransfer_selectAll;
    //
    // private static final String DESELECT_ALL_TITLE =
    // ErlangDataTransferMessages.DataTransfer_deselectAll;

    private static final String SELECT_SOURCE_TITLE = WizardMessages.FileImport_selectSourceTitle;
    private static final String SELECT_SOURCE_MESSAGE = WizardMessages.FileImport_selectSource;
    protected static final String SOURCE_EMPTY_MESSAGE = WizardMessages.FileImport_sourceEmpty;
    private String projectName;
    protected Path projectPath;
    private Composite sourceContainerGroup;

    /**
     * Creates an instance of this class
     */
    protected ErlangProjectImportWizardPage(final String name,
            final IStructuredSelection selection) {
        super(name, selection);
    }

    /**
     * Creates an instance of this class
     * 
     * @param aWorkbench
     *            IWorkbench
     * @param selection
     *            IStructuredSelection
     */
    public ErlangProjectImportWizardPage(final IStructuredSelection selection) {
        this("ErlangProjectImportWizardPage", selection); // $NON-NLS-1$
        setTitle(WizardMessages.DataTransfer_fileSystemTitle);
        setDescription(WizardMessages.FileImport_importFileSystem);
    }

    /**
     * Creates the buttons for selecting specific types or selecting all or none
     * of the elements.
     * 
     * @param parent
     *            the parent control
     */
    protected final void createButtonsGroup(final Composite parent) {
        // top level group
        final Composite buttonComposite = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        layout.makeColumnsEqualWidth = true;
        buttonComposite.setLayout(layout);
        buttonComposite.setFont(parent.getFont());
        final GridData buttonData = new GridData(GridData.VERTICAL_ALIGN_FILL
                | GridData.HORIZONTAL_ALIGN_FILL);
        buttonData.horizontalSpan = 2;
        buttonComposite.setLayoutData(buttonData);

        final Button button = new Button(buttonComposite, SWT.PUSH);
        button.setFont(buttonComposite.getFont());

        final GridData buttonData1 = new GridData(GridData.FILL_HORIZONTAL);
        button.setLayoutData(buttonData1);

        button.setData(Integer.valueOf(IDialogConstants.SELECT_TYPES_ID));
        button.setText(SELECT_TYPES_TITLE);

        // types edit button
        selectTypesButton = button;

        final SelectionListener listener = new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                handleTypesEditButtonPressed();
            }
        };
        selectTypesButton.addSelectionListener(listener);
        setButtonLayoutData(selectTypesButton);
    }

    @Override
    public void createControl(final Composite parent) {
        super.createControl(parent);
        validateSourceGroup();
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(getControl(), "file_system_import_wizard_page");
    }

    /**
     * Create the import options specification widgets.
     */
    @Override
    protected void createOptionsGroupButtons(final Group optionsGroup) {

        // // overwrite... checkbox
        overwriteExistingResourcesCheckbox = new Button(optionsGroup, SWT.CHECK);
        overwriteExistingResourcesCheckbox.setFont(optionsGroup.getFont());
        overwriteExistingResourcesCheckbox
                .setText(WizardMessages.FileImport_overwriteExisting);
        overwriteExistingResourcesCheckbox.setEnabled(false);

        if (!ErlangProjectImportWizard.COPY_ONLY) {
            // copy projects into workspace
            copyProjectsIntoWorkspaceCheckbox = new Button(optionsGroup,
                    SWT.CHECK);
            copyProjectsIntoWorkspaceCheckbox.setFont(optionsGroup.getFont());
            copyProjectsIntoWorkspaceCheckbox
                    .setText(WizardMessages.FileImport_copyProjectIntoWorkspace);
            copyProjectsIntoWorkspaceCheckbox
                    .addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(final SelectionEvent e) {
                            copyFiles = copyProjectsIntoWorkspaceCheckbox
                                    .getSelection();
                            if (copyFiles) {
                                enableButtonGroup(false);
                                enableSourceGroup(true);
                                enableResourceTreeGroup(true);
                                // setAllSelections(false);
                            } else {
                                // setAllSelections(tr);
                                enableButtonGroup(false);
                                enableResourceTreeGroup(true);
                            }
                        }
                    });
            // copyProjectsIntoWorkspaceCheckbox.setEnabled(false);
        } else {
            enableButtonGroup(false);
            enableSourceGroup(true);
            enableResourceTreeGroup(true);
            // setAllSelections(false);
        }
    }

    /**
     * Create the group for creating the root directory
     */
    protected void createRootDirectoryGroup(final Composite parent) {
        sourceContainerGroup = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        sourceContainerGroup.setLayout(layout);
        sourceContainerGroup.setFont(parent.getFont());
        sourceContainerGroup.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL));

        final Label groupLabel = new Label(sourceContainerGroup, SWT.NONE);
        groupLabel.setText(getSourceLabel());
        groupLabel.setFont(parent.getFont());

        // source name entry field
        sourceNameField = new Combo(sourceContainerGroup, SWT.BORDER);
        final GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL);
        data.widthHint = SIZING_TEXT_FIELD_WIDTH;
        sourceNameField.setLayoutData(data);
        sourceNameField.setFont(parent.getFont());

        sourceNameField.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                updateFromSourceField();
                setAllSelections(true);
            }
        });

        sourceNameField.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }

            private void dialogChanged() {
                projectPath = new Path(sourceNameField.getText());

                final String filePath = projectPath.lastSegment();

                // xPath.substring(xPath.lastIndexOf("{") + 1,
                // xPath.lastIndexOf("}"));

                if (filePath != null) {

                    final File file = new File(filePath);
                    setProjectName(file.getName());

                    // IFolder projectFolder = projectPath.getFolder(file
                    // .getName());
                    //
                    // if (projectFolder.exists()) {
                    // MessageDialog dialog = new MessageDialog(new Shell(
                    // Display.getCurrent()),
                    // "Erlang project import", null,
                    // "Target already exists.",
                    // MessageDialog.ERROR, new String[] { "OK" },
                    // 0);
                    // dialog.open();
                    // // errMessage = Messages
                    // // .getString("Target already exists.");
                    // // //$NON-NLS-1$
                    // complete = false;
                    // }
                }

            }

        });

        sourceNameField.addKeyListener(new KeyListener() {
            /*
             * @see KeyListener.keyPressed
             */
            @Override
            public void keyPressed(final KeyEvent e) {
                // If there has been a key pressed then mark as dirty
                entryChanged = true;
            }

            /*
             * @see KeyListener.keyReleased
             */
            @Override
            public void keyReleased(final KeyEvent e) {
            }
        });

        sourceNameField.addFocusListener(new FocusListener() {
            /*
             * @see FocusListener.focusGained(FocusEvent)
             */
            @Override
            public void focusGained(final FocusEvent e) {
                // Do nothing when getting focus
            }

            /*
             * @see FocusListener.focusLost(FocusEvent)
             */
            @Override
            public void focusLost(final FocusEvent e) {
                // Clear the flag to prevent constant update
                if (entryChanged) {
                    entryChanged = false;
                    updateFromSourceField();
                }

            }
        });

        // source browse button
        sourceBrowseButton = new Button(sourceContainerGroup, SWT.PUSH);
        sourceBrowseButton.setText(WizardMessages.DataTransfer_browse);
        sourceBrowseButton.addListener(SWT.Selection, this);
        sourceBrowseButton.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_FILL));
        sourceBrowseButton.setFont(parent.getFont());
        setButtonLayoutData(sourceBrowseButton);
    }

    protected void setProjectName(final String name) {
        projectName = name;
        if (name.length() > 0) {
            setContainerFieldValue(name);
        }

    }

    /**
     * Update the receiver from the source name field.
     */

    private void updateFromSourceField() {

        setSourceName(sourceNameField.getText());
        // Update enablements when this is selected
        updateWidgetEnablements();
    }

    /**
     * Creates and returns a <code>FileSystemElement</code> if the specified
     * file system object merits one. The criteria for this are: Also create the
     * children.
     */
    protected MinimizedFileSystemElement createRootElement(
            final Object fileSystemObject,
            final IImportStructureProvider provider) {
        final boolean isContainer = provider.isFolder(fileSystemObject);
        final String elementLabel = provider.getLabel(fileSystemObject);

        // Use an empty label so that display of the element's full name
        // doesn't include a confusing label
        final MinimizedFileSystemElement dummyParent = new MinimizedFileSystemElement(
                "", null, true);//$NON-NLS-1$
        dummyParent.setPopulated();
        final MinimizedFileSystemElement result = new MinimizedFileSystemElement(
                elementLabel, dummyParent, isContainer);
        result.setFileSystemObject(fileSystemObject);

        // Get the files for the element so as to build the first level
        result.getFiles(provider);

        return dummyParent;
    }

    /**
     * Create the import source specification widgets
     */
    @Override
    protected void createSourceGroup(final Composite parent) {

        createRootDirectoryGroup(parent);
        createFileSelectionGroup(parent);
        createButtonsGroup(parent);
    }

    /**
     * Enable or disable the button group.
     */
    protected void enableButtonGroup(final boolean enable) {
        selectTypesButton.setEnabled(enable);
        // selectAllButton.setEnabled(false);
        // deselectAllButton.setEnabled(false);
    }

    /**
     * @param enable
     */
    protected void enableResourceTreeGroup(final boolean enable) {
        selectionGroup.enableFolderComposite(enable);
    }

    /**
     * Enable or disable the group.
     */
    protected void enableSourceGroup(final boolean enable) {
        sourceContainerGroup.setEnabled(enable);
    }

    /**
     * Answer a boolean indicating whether the specified source currently exists
     * and is valid
     */
    protected boolean ensureSourceIsValid() {
        if (new File(getSourceDirectoryName()).isDirectory()) {
            return true;
        }

        displayErrorDialog(WizardMessages.FileImport_invalidSource);
        sourceNameField.setFocus();
        return false;
    }

    /**
     * Execute the passed import operation. Answer a boolean indicating success.
     */
    protected boolean executeImportOperation(final ImportOperation op) {
        initializeOperation(op);
        try {
            getContainer().run(true, true, op);
        } catch (final InterruptedException e) {
            return false;
        } catch (final InvocationTargetException e) {
            displayErrorDialog(e.getTargetException());
            return false;
        }

        final IStatus status = op.getStatus();
        if (!status.isOK()) {
            ErrorDialog.openError(getContainer().getShell(),
                    WizardMessages.FileImport_importProblems, null, // no
                    // special
                    // message
                    status);
            return false;
        }

        return true;
    }

    /**
     * Returns a content provider for <code>FileSystemElement</code>s that
     * returns only files as children.
     */
    @Override
    protected ITreeContentProvider getFileProvider() {
        return new WorkbenchContentProvider() {
            @Override
            public Object[] getChildren(final Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    final MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    return element.getFiles(
                            FileSystemStructureProvider.INSTANCE).getChildren(
                            element);
                }
                return new Object[0];
            }
        };
    }

    /**
     * Answer the root FileSystemElement that represents the contents of the
     * currently-specified source. If this FileSystemElement is not currently
     * defined then create and return it.
     */
    protected MinimizedFileSystemElement getFileSystemTree() {

        final File sourceDirectory = getSourceDirectory();
        if (sourceDirectory == null) {
            return null;
        }

        return selectFiles(sourceDirectory,
                FileSystemStructureProvider.INSTANCE);
    }

    /**
     * Returns a content provider for <code>FileSystemElement</code>s that
     * returns only folders as children.
     */
    @Override
    protected ITreeContentProvider getFolderProvider() {
        return new WorkbenchContentProvider() {
            @Override
            public Object[] getChildren(final Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    final MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    return element.getFolders(
                            FileSystemStructureProvider.INSTANCE).getChildren(
                            element);
                }
                return new Object[0];
            }

            @Override
            public boolean hasChildren(final Object o) {
                if (o instanceof MinimizedFileSystemElement) {
                    final MinimizedFileSystemElement element = (MinimizedFileSystemElement) o;
                    return getChildren(element).length > 0;
                }
                return false;
            }
        };
    }

    /**
     * Returns a File object representing the currently-named source directory
     * if it exists as a valid directory, or <code>null</code> otherwise.
     */
    protected File getSourceDirectory() {
        return getSourceDirectory(sourceNameField.getText());
    }

    /**
     * Returns a File object representing the currently-named source directory
     * if it exists as a valid directory, or <code>null</code> otherwise.
     * 
     * @param path
     *            a String not yet formatted for java.io.File compatability
     */
    private File getSourceDirectory(final String path) {
        final File sourceDirectory = new File(getSourceDirectoryName(path));
        if (!sourceDirectory.exists() || !sourceDirectory.isDirectory()) {
            return null;
        }

        return sourceDirectory;
    }

    /**
     * Answer the directory name specified as being the import source. Note that
     * if it ends with a separator then the separator is first removed so that
     * java treats it as a proper directory
     */
    private String getSourceDirectoryName() {
        return getSourceDirectoryName(sourceNameField.getText());
    }

    /**
     * Answer the directory name specified as being the import source. Note that
     * if it ends with a separator then the separator is first removed so that
     * java treats it as a proper directory
     */
    private String getSourceDirectoryName(final String sourceName) {
        IPath result = new Path(sourceName.trim());

        if (result.getDevice() != null && result.segmentCount() == 0) {
            result = result.addTrailingSeparator();
        } else {
            result = result.removeTrailingSeparator();
        }

        return result.toOSString();
    }

    /**
     * Answer the string to display as the label for the source specification
     * field
     */
    protected String getSourceLabel() {
        return WizardMessages.FileImport_fromDirectory;
    }

    /**
     * Handle all events and enablements for widgets in this dialog
     * 
     * @param event
     *            Event
     */
    @Override
    public void handleEvent(final Event event) {
        if (event.widget == sourceBrowseButton) {
            handleSourceBrowseButtonPressed();
        }

        // super.handleEvent(event);
    }

    /**
     * Open an appropriate source browser so that the user can specify a source
     * to import from
     */
    protected void handleSourceBrowseButtonPressed() {

        final String currentSource = sourceNameField.getText();
        final DirectoryDialog dialog = new DirectoryDialog(
                sourceNameField.getShell(), SWT.SAVE);
        dialog.setText(SELECT_SOURCE_TITLE);
        dialog.setMessage(SELECT_SOURCE_MESSAGE);
        dialog.setFilterPath(getSourceDirectoryName(currentSource));

        final String selectedDirectory = dialog.open();
        if (selectedDirectory != null) {
            // Just quit if the directory is not valid
            if (getSourceDirectory(selectedDirectory) == null
                    || selectedDirectory.equals(currentSource)) {
                return;
            }
            // If it is valid then proceed to populate
            setErrorMessage(null);
            setSourceName(selectedDirectory);
            selectionGroup.setFocus();
        }
    }

    /**
     * Open a registered type selection dialog and note the selections in the
     * receivers types-to-export field., Added here so that inner classes can
     * have access
     */
    @Override
    protected void handleTypesEditButtonPressed() {

        super.handleTypesEditButtonPressed();
    }

    /**
     * Import the resources with extensions as specified by the user
     */
    protected boolean importResources(final List<Object> fileSystemObjects) {

        final ImportOperation operation = new ImportOperation(
                getContainerFullPath(), getSourceDirectory(),
                FileSystemStructureProvider.INSTANCE, this, fileSystemObjects);

        operation.setContext(getShell());
        return executeImportOperation(operation);
    }

    /**
     * Initializes the specified operation appropriately.
     */
    protected void initializeOperation(final ImportOperation op) {

        // op.setCreateContainerStructure(createContainerStructureButton
        // .getSelection());
        op.setCreateContainerStructure(false);
        op.setOverwriteResources(true);

    }

    /**
     * Returns whether the extension provided is an extension that has been
     * specified for export by the user.
     * 
     * @param extension
     *            the resource name
     * @return <code>true</code> if the resource name is suitable for export
     *         based upon its extension
     */
    protected boolean isExportableExtension(final String extension) {
        if (selectedTypes == null) {
            return true;
        }

        final Iterator<?> itr = selectedTypes.iterator();
        while (itr.hasNext()) {
            if (extension.equalsIgnoreCase((String) itr.next())) {
                return true;
            }
        }

        return false;
    }

    /**
     * Repopulate the view based on the currently entered directory.
     */
    protected void resetSelection() {

        final MinimizedFileSystemElement currentRoot = getFileSystemTree();
        selectionGroup.setRoot(currentRoot);

    }

    /**
     * Use the dialog store to restore widget values to the values that they
     * held last time this wizard was used to completion
     */
    @Override
    protected void restoreWidgetValues() {
        final IDialogSettings settings = getDialogSettings();
        if (settings != null) {
            final String[] sourceNames = settings
                    .getArray(STORE_SOURCE_NAMES_ID);
            if (sourceNames == null) {
                return; // ie.- no values stored, so stop
            }

            // set filenames history
            for (int i = 0; i < sourceNames.length; i++) {
                sourceNameField.add(sourceNames[i]);
            }

            // radio buttons and checkboxes
            overwriteExistingResourcesCheckbox.setSelection(settings
                    .getBoolean(STORE_OVERWRITE_EXISTING_RESOURCES_ID));
            //
            // boolean createStructure = settings
            // .getBoolean(STORE_CREATE_CONTAINER_STRUCTURE_ID);
            // createContainerStructureButton.setSelection(createStructure);
            // createOnlySelectedButton.setSelection(!createStructure);
            if (!ErlangProjectImportWizard.COPY_ONLY) {
                copyProjectsIntoWorkspaceCheckbox.setSelection(copyFiles);
            }

        }
    }

    /**
     * Since Finish was pressed, write widget values to the dialog store so that
     * they will persist into the next invocation of this wizard page
     */
    protected void saveWidgetValues() {
        final IDialogSettings settings = getDialogSettings();
        if (settings != null) {
            // update source names history
            String[] sourceNames = settings.getArray(STORE_SOURCE_NAMES_ID);
            if (sourceNames == null) {
                sourceNames = new String[0];
            }

            sourceNames = addToHistory(sourceNames, getSourceDirectoryName());
            settings.put(STORE_SOURCE_NAMES_ID, sourceNames);

            // radio buttons and checkboxes
            settings.put(STORE_OVERWRITE_EXISTING_RESOURCES_ID,
                    overwriteExistingResourcesCheckbox.getSelection());

            // settings.put(STORE_CREATE_CONTAINER_STRUCTURE_ID,
            // createContainerStructureButton.getSelection());

        }
    }

    /**
     * Invokes a file selection operation using the specified file system and
     * structure provider. If the user specifies files to be imported then this
     * selection is cached for later retrieval and is returned.
     */
    protected MinimizedFileSystemElement selectFiles(
            final Object rootFileSystemObject,
            final IImportStructureProvider structureProvider) {

        final MinimizedFileSystemElement[] results = new MinimizedFileSystemElement[1];

        BusyIndicator.showWhile(getShell().getDisplay(), new Runnable() {
            @Override
            public void run() {
                // Create the root element from the supplied file system object
                results[0] = createRootElement(rootFileSystemObject,
                        structureProvider);
            }
        });

        return results[0];
    }

    /**
     * Set all of the selections in the selection group to value. Implemented
     * here to provide access for inner classes.
     * 
     * @param value
     *            boolean
     */
    @Override
    protected void setAllSelections(final boolean value) {
        super.setAllSelections(value);
    }

    /**
     * Sets the source name of the import to be the supplied path. Adds the name
     * of the path to the list of items in the source combo and selects it.
     * 
     * @param path
     *            the path to be added
     */
    protected void setSourceName(final String path) {

        if (path.length() > 0) {

            final String[] currentItems = sourceNameField.getItems();
            int selectionIndex = -1;
            for (int i = 0; i < currentItems.length; i++) {
                if (currentItems[i].equals(path)) {
                    selectionIndex = i;
                }
            }
            if (selectionIndex < 0) {
                final int oldLength = currentItems.length;
                final String[] newItems = new String[oldLength + 1];
                System.arraycopy(currentItems, 0, newItems, 0, oldLength);
                newItems[oldLength] = path;
                sourceNameField.setItems(newItems);
                selectionIndex = oldLength;
            }
            sourceNameField.select(selectionIndex);

            resetSelection();
        }
    }

    /**
     * Update the tree to only select those elements that match the selected
     * types
     */
    @Override
    protected void setupSelectionsBasedOnSelectedTypes() {
        // ProgressMonitorDialog dialog = new ProgressMonitorJobsDialog(
        // getContainer().getShell());
        final Map<FileSystemElement, List<MinimizedFileSystemElement>> selectionMap = new Hashtable<FileSystemElement, List<MinimizedFileSystemElement>>();

        // final IElementFilter filter = new IElementFilter() {
        //
        // @SuppressWarnings("unchecked")
        // public void filterElements(Collection files,
        // IProgressMonitor monitor) throws InterruptedException {
        // if (files == null) {
        // throw new InterruptedException();
        // }
        // Iterator<?> filesList = files.iterator();
        // while (filesList.hasNext()) {
        // if (monitor.isCanceled()) {
        // throw new InterruptedException();
        // }
        // checkFile(filesList.next());
        // }
        // }
        //
        // public void filterElements(Object[] files, IProgressMonitor monitor)
        // throws InterruptedException {
        // if (files == null) {
        // throw new InterruptedException();
        // }
        // for (int i = 0; i < files.length; i++) {
        // if (monitor.isCanceled()) {
        // throw new InterruptedException();
        // }
        // checkFile(files[i]);
        // }
        // }
        //
        // private void checkFile(Object fileElement) {
        // MinimizedFileSystemElement file = (MinimizedFileSystemElement)
        // fileElement;
        // if (isExportableExtension(file.getFileNameExtension())) {
        // List<MinimizedFileSystemElement> elements = new
        // ArrayList<MinimizedFileSystemElement>();
        // FileSystemElement parent = file.getParent();
        // if (selectionMap.containsKey(parent)) {
        // elements = selectionMap.get(parent);
        // }
        // elements.add(file);
        // selectionMap.put(parent, elements);
        // }
        // }
        //
        // };

        // final IRunnableWithProgress runnable = new IRunnableWithProgress() {
        // public void run(final IProgressMonitor monitor)
        // throws InterruptedException {
        // monitor.beginTask(
        // ErlangDataTransferMessages.ImportPage_filterSelections,
        // IProgressMonitor.UNKNOWN);
        // getSelectedResources(filter, monitor);
        // }
        // };

        // try {
        // dialog.run(true, true, runnable);
        // } catch (InvocationTargetException exception) {
        // // Couldn't start. Do nothing.
        // return;
        // } catch (InterruptedException exception) {
        // // Got interrupted. Do nothing.
        // return;
        // }
        // make sure that all paint operations caused by closing the progress
        // dialog get flushed, otherwise extra pixels will remain on the screen
        // until
        // updateSelections is completed
        getShell().update();
        // The updateSelections method accesses SWT widgets so cannot be
        // executed
        // as part of the above progress dialog operation since the operation
        // forks
        // a new process.
        // if (selectionMap != null) {
        updateSelections(selectionMap);
        // }
    }

    @Override
    public void setVisible(final boolean visible) {
        super.setVisible(visible);
        // resetSelection();
        if (visible) {
            sourceNameField.setFocus();
        }
    }

    /**
     * Update the selections with those in map. Implemented here to give inner
     * class visibility
     * 
     * @param map
     *            Map - key tree elements, values Lists of list elements
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    protected void updateSelections(final Map map) {
        super.updateSelections(map);
    }

    /**
     * Check if widgets are enabled or disabled by a change in the dialog.
     * Provided here to give access to inner classes.
     */
    @Override
    protected void updateWidgetEnablements() {

        super.updateWidgetEnablements();
    }

    /**
     * Answer a boolean indicating whether self's source specification widgets
     * currently all contain valid values.
     */
    @Override
    protected boolean validateSourceGroup() {
        final File sourceDirectory = getSourceDirectory();
        if (sourceDirectory == null) {
            setMessage(SOURCE_EMPTY_MESSAGE);
            enableButtonGroup(false);
            return false;
        }

        if (sourceConflictsWithDestination(new Path(sourceDirectory.getPath()))) {
            setErrorMessage(getSourceConflictMessage());
            enableButtonGroup(false);
            return false;
        }
        // if (!isCopyFiles()) {
        // selectionGroup.setAllSelections(true);
        // }
        final List<?> resourcesToExport = selectionGroup
                .getAllWhiteCheckedItems();
        if (resourcesToExport.size() == 0) {
            setErrorMessage(WizardMessages.FileImport_noneSelected);
            return false;
        }

        // if (copyFiles) {
        // enableButtonGroup(true);
        // }
        setErrorMessage(null);
        return true;
    }

    /**
     * 
     * @return
     */
    public String getProjectName() {
        // final Iterator<?> resourcesEnum = getSelectedResources().iterator();
        // final List<Object> fileSystemObjects = new ArrayList<Object>();
        // while (resourcesEnum.hasNext()) {
        // fileSystemObjects.add(((FileSystemElement) resourcesEnum.next())
        // .getFileSystemObject());
        // }
        return projectName;
    }

    public boolean isCopyFiles() {
        return copyFiles;
    }

    public Path getProjectPath() {
        return projectPath;
    }

    // public boolean createLinkedProject (final ProjectRecord record,
    // IProgressMonitor monitor) throws InvocationTargetException,
    // InterruptedException {
    // String projectName = record.getProjectName();
    // final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    // final IProject project = workspace.getRoot().getProject(projectName);
    // if (record.description == null) {
    // // error case
    // record.description = workspace.newProjectDescription(projectName);
    // IPath locationPath = new Path(record.projectSystemFile
    // .getAbsolutePath());
    //
    // // If it is under the root use the default location
    // if (Platform.getLocation().isPrefixOf(locationPath)) {
    // record.description.setLocation(null);
    // } else {
    // record.description.setLocation(locationPath);
    // }
    // } else {
    // record.description.setName(projectName);
    // }
    // // import from file system
    // File importSource = null;
    // try {
    // monitor
    // .beginTask(
    // DataTransferMessages.WizardProjectsImportPage_CreateProjectsTask,
    // 100);
    // project.create(record.description, new SubProgressMonitor(monitor,
    // 30));
    // project.open(IResource.BACKGROUND_REFRESH, new SubProgressMonitor(
    // monitor, 70));
    // } catch (CoreException e) {
    // throw new InvocationTargetException(e);
    // } finally {
    // monitor.done();
    // }
    //
    // // // import operation to import project files if copy checkbox is
    // selected
    // // if (copyFiles && importSource != null) {
    // // List filesToImport = FileSystemStructureProvider.INSTANCE
    // // .getChildren(importSource);
    // // ImportOperation operation = new ImportOperation(project
    // // .getFullPath(), importSource,
    // // FileSystemStructureProvider.INSTANCE, this, filesToImport);
    // // operation.setContext(getShell());
    // // operation.setOverwriteResources(true); // need to overwrite
    // // // .project, .classpath
    // // // files
    // // operation.setCreateContainerStructure(false);
    // // operation.run(monitor);
    // // }
    //
    // return true;
    //
    // }

    /**
     * Returns whether the source location conflicts with the destination
     * resource. This will occur if the source is already under the destination.
     * 
     * @param sourcePath
     *            the path to check
     * @return <code>true</code> if there is a conflict, <code>false</code> if
     *         not
     */
    // protected boolean sourceConflictsWithDestination(IPath sourcePath) {
    //
    // IContainer container = getSpecifiedContainer();
    // if (container == null) {
    // return false;
    // }
    //
    // IPath destinationLocation = getSpecifiedContainer().getLocation();
    // if (destinationLocation != null) {
    // return destinationLocation.isPrefixOf(sourcePath);
    // }
    // // null destination location is handled in
    // // WizardResourceImportPage
    // return false;
    // }
    /**
     * Display an error dialog with the specified message.
     * 
     * @param message
     *            the error message
     */
    protected void displayErrorDialog(final String message) {
        MessageDialog.openError(getContainer().getShell(),
                getErrorDialogTitle(), message);
    }

    /**
     * Display an error dislog with the information from the supplied exception.
     * 
     * @param exception
     *            Throwable
     */
    protected void displayErrorDialog(final Throwable exception) {
        String message = exception.getMessage();
        // Some system exceptions have no message
        if (message == null) {
            message = NLS.bind(
                    WizardMessages.WizardDataTransfer_exceptionMessage,
                    exception);
        }
        displayErrorDialog(message);
    }

    /**
     * Get the title for an error dialog. Subclasses should override.
     */
    @Override
    protected String getErrorDialogTitle() {
        return WizardMessages.WizardExportPage_internalErrorTitle;
    }

    /**
     * The <code>WizardDataTransfer</code> implementation of this
     * <code>IOverwriteQuery</code> method asks the user whether the existing
     * resource at the given path should be overwritten.
     * 
     * @param pathString
     * @return the user's reply: one of <code>"YES"</code>, <code>"NO"</code>,
     *         <code>"ALL"</code>, or <code>"CANCEL"</code>
     */
    @Override
    public String queryOverwrite(final String pathString) {

        final Path path = new Path(pathString);

        String messageString;
        // Break the message up if there is a file name and a directory
        // and there are at least 2 segments.
        if (path.getFileExtension() == null || path.segmentCount() < 2) {
            messageString = NLS.bind(
                    WizardMessages.WizardDataTransfer_existsQuestion,
                    pathString);
        } else {
            messageString = NLS
                    .bind(WizardMessages.WizardDataTransfer_overwriteNameAndPathQuestion,
                            path.lastSegment(), path.removeLastSegments(1)
                                    .toOSString());
        }

        final MessageDialog dialog = new MessageDialog(getContainer()
                .getShell(), WizardMessages.Question, null, messageString,
                MessageDialog.QUESTION, new String[] {
                        IDialogConstants.YES_LABEL,
                        IDialogConstants.YES_TO_ALL_LABEL,
                        IDialogConstants.NO_LABEL,
                        IDialogConstants.NO_TO_ALL_LABEL,
                        IDialogConstants.CANCEL_LABEL }, 0) {
        };
        final String[] response = new String[] { YES, ALL, NO, NO_ALL, CANCEL };
        // run in syncExec because callback is from an operation,
        // which is probably not running in the UI thread.
        getControl().getDisplay().syncExec(new Runnable() {
            @Override
            public void run() {
                dialog.open();
            }
        });
        return dialog.getReturnCode() < 0 ? CANCEL : response[dialog
                .getReturnCode()];
    }

    /**
     * Displays a Yes/No question to the user with the specified message and
     * returns the user's response.
     * 
     * @param message
     *            the question to ask
     * @return <code>true</code> for Yes, and <code>false</code> for No
     */
    protected boolean queryYesNoQuestion(final String message) {
        final MessageDialog dialog = new MessageDialog(getContainer()
                .getShell(), WizardMessages.Question, null, message,
                MessageDialog.NONE, new String[] { IDialogConstants.YES_LABEL,
                        IDialogConstants.NO_LABEL }, 0) {
        };
        // ensure yes is the default

        return dialog.open() == 0;
    }

}
