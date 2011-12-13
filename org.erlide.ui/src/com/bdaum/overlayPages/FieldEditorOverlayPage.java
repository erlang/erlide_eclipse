/*******************************************************************************
 * Copyright (c) 2003 Berthold Daum.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 *
 * Contributors:
 *     Berthold Daum
 *******************************************************************************/
package com.bdaum.overlayPages;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.erlide.jinterface.ErlLogger;

/**
 * @author Berthold Daum
 */
public abstract class FieldEditorOverlayPage extends FieldEditorPreferencePage
        implements IWorkbenchPropertyPage, IWorkbenchPreferencePage {

    /**
     * * Name of resource property for the selection of workbench or project
     * settings **
     */
    public static final String USEPROJECTSETTINGS = "useProjectSettings"; //$NON-NLS-1$

    private static final String FALSE = "false"; //$NON-NLS-1$
    private static final String TRUE = "true"; //$NON-NLS-1$

    // Stores all created field editors
    private final List<FieldEditor> editors = new ArrayList<FieldEditor>();

    // Stores owning element of properties
    private IAdaptable element;

    // Additional buttons for property pages
    private Button useWorkspaceSettingsButton;
    private Button useProjectSettingsButton;

    Button configureButton;

    // Overlay preference store for property pages
    private IPreferenceStore overlayStore;

    // The image descriptor of this pages title image
    private ImageDescriptor image;

    // Cache for page id
    private String pageId;

    private boolean propertiesOnly = false;

    /**
     * Constructor
     * 
     * @param style
     *            - layout style
     */
    public FieldEditorOverlayPage(final int style) {
        super(style);
    }

    /**
     * Constructor
     * 
     * @param title
     *            - title string
     * @param style
     *            - layout style
     */
    public FieldEditorOverlayPage(final String title, final int style) {
        super(title, style);
    }

    /**
     * Constructor
     * 
     * @param title
     *            - title string
     * @param image
     *            - title image
     * @param style
     *            - layout style
     */
    public FieldEditorOverlayPage(final String title,
            final ImageDescriptor image, final int style) {
        super(title, image, style);
        this.image = image;
    }

    /**
     * Returns the id of the current preference page as defined in plugin.xml
     * Subclasses must implement.
     * 
     * @return - the qualifier
     */
    protected abstract String getPageId();

    /**
     * Receives the object that owns the properties shown in this property page.
     * 
     * @see org.eclipse.ui.IWorkbenchPropertyPage#setElement(org.eclipse.core.runtime.IAdaptable)
     */
    @Override
    public void setElement(final IAdaptable element) {
        this.element = element;
    }

    /**
     * Delivers the object that owns the properties shown in this property page.
     * 
     * @see org.eclipse.ui.IWorkbenchPropertyPage#getElement()
     */
    @Override
    public IAdaptable getElement() {
        return element;
    }

    /**
     * Returns true if this instance represents a property page
     * 
     * @return - true for property pages, false for preference pages
     */
    public boolean isPropertyPage() {
        return getElement() != null;
    }

    /**
     * We override the addField method. This allows us to store each field
     * editor added by subclasses in a list for later processing.
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#addField(org.eclipse.jface.preference.FieldEditor)
     */
    @Override
    protected void addField(final FieldEditor editor) {
        editors.add(editor);
        super.addField(editor);
    }

    /**
     * We override the createControl method. In case of property pages we create
     * a new PropertyStore as local preference store. After all control have
     * been create, we enable/disable these controls.
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createControl()
     */
    @Override
    public void createControl(final Composite parent) {
        // Special treatment for property pages
        if (isPropertyPage()) {
            // Cache the page id
            pageId = getPageId();
            // Create an overlay preference store and fill it with properties
            overlayStore = new ScopedPreferenceStore(new ProjectScope(
                    (IProject) getElement().getAdapter(IProject.class)), pageId);
            // Set overlay store as current preference store
        }
        super.createControl(parent);
        // Update state of all subclass controls
        if (isPropertyPage() && !propertiesOnly) {
            updateFieldEditors();
        }
    }

    /**
     * We override the createContents method. In case of property pages we
     * insert two radio buttons at the top of the page.
     * 
     * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createContents(final Composite parent) {
        if (isPropertyPage() && !propertiesOnly) {
            createSelectionGroup(parent);
        }
        return super.createContents(parent);
    }

    /**
     * Creates and initializes a selection group with two choice buttons and one
     * push button.
     * 
     * @param parent
     *            - the parent composite
     */
    private void createSelectionGroup(final Composite parent) {
        final Composite comp = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        comp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        final Composite radioGroup = new Composite(comp, SWT.NONE);
        radioGroup.setLayout(new GridLayout());
        radioGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        useWorkspaceSettingsButton = createRadioButton(radioGroup,
                Messages.getString("OverlayPage.Use_Workspace_Settings")); //$NON-NLS-1$
        useProjectSettingsButton = createRadioButton(radioGroup,
                Messages.getString("OverlayPage.Use_Project_Settings")); //$NON-NLS-1$
        configureButton = new Button(comp, SWT.PUSH);
        configureButton.setText(Messages
                .getString("OverlayPage.Configure_Workspace_Settings")); //$NON-NLS-1$
        configureButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                configureWorkspaceSettings();
            }
        });
        // Set workspace/project radio buttons
        try {
            final String use = ((IResource) getElement())
                    .getPersistentProperty(new QualifiedName(pageId,
                            USEPROJECTSETTINGS));
            if (TRUE.equals(use)) {
                useProjectSettingsButton.setSelection(true);
                configureButton.setEnabled(false);
            } else {
                useWorkspaceSettingsButton.setSelection(true);
            }
        } catch (final CoreException e) {
            useWorkspaceSettingsButton.setSelection(true);
        }
    }

    /**
     * Convenience method creating a radio button
     * 
     * @param parent
     *            - the parent composite
     * @param label
     *            - the button label
     * @return - the new button
     */
    private Button createRadioButton(final Composite parent, final String label) {
        final Button button = new Button(parent, SWT.RADIO);
        button.setText(label);
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                configureButton
                        .setEnabled(button == useWorkspaceSettingsButton);
                updateFieldEditors();
            }
        });
        return button;
    }

    /**
     * Returns in case of property pages the overlay store, in case of
     * preference pages the standard preference store
     * 
     * @see org.eclipse.jface.preference.PreferencePage#getPreferenceStore()
     */
    @Override
    public IPreferenceStore getPreferenceStore() {
        if (isPropertyPage()) {
            return overlayStore;
        }
        return super.getPreferenceStore();
    }

    /*
     * Enables or disables the field editors and buttons of this page
     */
    void updateFieldEditors() {
        // We iterate through all field editors
        final boolean enabled = useProjectSettingsButton.getSelection();
        updateFieldEditors(enabled);
    }

    /**
     * Enables or disables the field editors and buttons of this page Subclasses
     * may override.
     * 
     * @param enabled
     *            - true if enabled
     */
    protected void updateFieldEditors(final boolean enabled) {
        final Composite parent = getFieldEditorParent();
        final Iterator<FieldEditor> it = editors.iterator();
        while (it.hasNext()) {
            final FieldEditor editor = it.next();
            editor.setEnabled(enabled, parent);
        }
    }

    /**
     * We override the performOk method. In case of property pages we copy the
     * values in the overlay store into the property values of the selected
     * project. We also save the state of the radio buttons.
     * 
     * @see org.eclipse.jface.preference.IPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        final boolean result = super.performOk();
        if (result && isPropertyPage() && !propertiesOnly) {
            // Save state of radiobuttons in project properties
            final IResource resource = (IResource) getElement();
            try {
                final String value = useProjectSettingsButton.getSelection() ? TRUE
                        : FALSE;
                resource.setPersistentProperty(new QualifiedName(pageId,
                        USEPROJECTSETTINGS), value);
            } catch (final CoreException e) {
            }
        }
        return result;
    }

    /**
     * We override the performDefaults method. In case of property pages we
     * switch back to the workspace settings and disable the field editors.
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        if (isPropertyPage() && !propertiesOnly) {
            useWorkspaceSettingsButton.setSelection(true);
            useProjectSettingsButton.setSelection(false);
            configureButton.setEnabled(true);
            updateFieldEditors();
        }
        super.performDefaults();
    }

    /**
     * Creates a new preferences page and opens it
     * 
     * @see com.bdaum.SpellChecker.preferences.SpellCheckerPreferencePage#configureWorkspaceSettings()
     */
    protected void configureWorkspaceSettings() {
        try {
            // create a new instance of the current class
            final IPreferencePage page = this.getClass().newInstance();
            page.setTitle(getTitle());
            page.setImageDescriptor(image);
            // and show it
            showPreferencePage(pageId, page);
        } catch (final InstantiationException e) {
            ErlLogger.warn(e);
        } catch (final IllegalAccessException e) {
            ErlLogger.warn(e);
        }
    }

    /**
     * Show a single preference pages
     * 
     * @param id
     *            - the preference page identification
     * @param page
     *            - the preference page
     */
    protected void showPreferencePage(final String id,
            final IPreferencePage page) {
        final IPreferenceNode targetNode = new PreferenceNode(id, page);
        final PreferenceManager manager = new PreferenceManager();
        manager.addToRoot(targetNode);
        final PreferenceDialog dialog = new PreferenceDialog(getControl()
                .getShell(), manager);
        BusyIndicator.showWhile(getControl().getDisplay(), new Runnable() {
            @Override
            public void run() {
                dialog.create();
                dialog.setMessage(targetNode.getLabelText());
                dialog.open();
            }
        });
    }

    /**
     * If called, the page will not display "workspace/project" settings choice
     */
    protected void setPropertiesOnly() {
        propertiesOnly = true;
    }

}
