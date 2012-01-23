/*******************************************************************************
 * Copyright (c) 2004 Lukas Larsson and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Lukas Larsson
 *******************************************************************************/

package org.erlide.ui.wizards;

import java.util.Collection;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateBuffer;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.templates.ErlangSourceContextTypeModule;
import org.erlide.ui.templates.ModuleVariableResolver;
import org.erlide.ui.wizards.templates.ExportedFunctionsVariableResolver;
import org.erlide.ui.wizards.templates.LocalFunctionsVariableResolver;
import org.erlide.utils.SystemUtils;

/**
 * The "New" wizard page allows setting the container for the new file as well
 * as the file name. The page will only accept file name without the extension
 * OR with the extension that matches the expected one (erl).
 */

public class ErlangFileWizardPage extends WizardPage {

    public boolean gettingInput = false;
    private Text containerText;
    private Text fileText;
    private Combo applications;
    private Combo skeleton;
    private FunctionGroup functionGroup;
    private final ISelection fSelection;
    private final Template[] moduleTemplates;
    private final ModifyListener fModifyListener;
    private TemplateContextType fContextType = null;

    /**
     * Constructor for SampleNewWizardPage.
     * 
     * @param pageName
     */
    public ErlangFileWizardPage(final ISelection selection) {
        super("wizardPage");
        setTitle("Erlang Source File");
        setDescription("This wizard creates a new erlang source file.");
        fSelection = selection;

        moduleTemplates = ErlideUIPlugin
                .getDefault()
                .getTemplateStore()
                .getTemplates(
                        ErlangSourceContextTypeModule.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ID);
        fModifyListener = new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        };
    }

    /**
     * @see IDialogPage#createControl(Composite)
     */
    @Override
    public void createControl(final Composite parent) {

        final Composite container = new Composite(parent, SWT.NULL);

        final GridLayout grid = new GridLayout(1, true);
        container.setLayout(grid);

        final Composite filePanel = new Composite(container, SWT.NULL);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        filePanel.setLayoutData(gd);
        final GridLayout layout = new GridLayout();
        filePanel.setLayout(layout);
        layout.numColumns = 3;
        layout.verticalSpacing = 9;

        Label label = new Label(filePanel, SWT.NULL);
        label.setText("&Module name:");

        fileText = new Text(filePanel, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        fileText.setLayoutData(gd);
        fileText.addModifyListener(fModifyListener);

        label = new Label(filePanel, SWT.NULL);

        label = new Label(filePanel, SWT.NULL);
        label.setText("&Container:");

        containerText = new Text(filePanel, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        containerText.setLayoutData(gd);
        containerText.addModifyListener(fModifyListener);

        final Button button = new Button(filePanel, SWT.PUSH);
        button.setText("Browse...");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                handleBrowse();
            }
        });

        label = new Label(filePanel, SWT.NULL);
        label.setText("&Application name:");

        applications = new Combo(filePanel, SWT.BORDER | SWT.DROP_DOWN
                | SWT.READ_ONLY);
        applications.add("None");
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        applications.setLayoutData(gd);
        applications.select(0);
        applications.addModifyListener(fModifyListener);

        new Label(filePanel, SWT.NULL);

        label = new Label(filePanel, SWT.NULL);
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        label.setLayoutData(gd);
        label.setText("&Skeleton");

        skeleton = new Combo(filePanel, SWT.BORDER | SWT.DROP_DOWN
                | SWT.READ_ONLY);
        // skeleton.add("None");

        for (final Template element : moduleTemplates) {
            skeleton.add(element.getName());
        }
        skeleton.select(0);

        functionGroup = new FunctionGroup(container, this);

        initialize();
        dialogChanged();
        setControl(container);
    }

    /**
     * Tests if the current workbench selection is a suitable container to use.
     */

    private void initialize() {
        if (fSelection != null && !fSelection.isEmpty()
                && fSelection instanceof IStructuredSelection) {
            final IStructuredSelection ssel = (IStructuredSelection) fSelection;
            if (ssel.size() > 1) {
                return;
            }
            final Object obj = ssel.getFirstElement();
            if (obj instanceof IResource) {
                final IResource resource = (IResource) obj;
                IContainer container;
                if (resource instanceof IContainer) {
                    container = (IContainer) resource;
                } else {
                    container = resource.getParent();
                }
                final IProject project = resource.getProject();
                final IErlProject erlProject = ErlModelManager.getErlangModel()
                        .getErlangProject(project);
                String txt;
                final Collection<IPath> sourceDirs = erlProject.getSourceDirs();
                if (sourceDirs.size() > 0) {
                    final IPath sourceDirWithinContainer = sourceDirWithinContainer(
                            sourceDirs, container);
                    if (sourceDirWithinContainer != null) {
                        txt = sourceDirWithinContainer.toString();
                    } else {
                        final IPath path = project.getFullPath().append(
                                sourceDirs.iterator().next());
                        txt = path.toString();
                    }
                } else {
                    txt = container.getFullPath().toString();
                }
                containerText.setText(txt);

            }
        }

        fileText.setText("new_file");
    }

    private IPath sourceDirWithinContainer(final Collection<IPath> sourceDirs,
            final IContainer container) {
        final IPath containerPath = container.getFullPath();
        for (final IPath sourceDir : sourceDirs) {
            if (containerPath.equals(sourceDir)
                    || containerPath.isPrefixOf(sourceDir)) {
                return sourceDir;
            }
        }
        return null;
    }

    /**
     * Uses the standard container selection dialog to choose the new value for
     * the container field.
     */
    void handleBrowse() {
        final ContainerSelectionDialog dialog = new ContainerSelectionDialog(
                getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
                "Select new file container");
        if (dialog.open() == Window.OK) {
            final Object[] result = dialog.getResult();
            if (result.length == 1) {
                containerText.setText(((Path) result[0]).toString());
            }
        }
    }

    /**
     * Ensures that both text fields are set.
     */
    void dialogChanged() {
        final IResource container = ResourcesPlugin.getWorkspace().getRoot()
                .findMember(new Path(getContainerName()));
        final String fileName = getFileName();

        if (getContainerName().length() == 0) {
            updateStatus("File container must be specified");
            return;
        }
        if (container == null
                || (container.getType() & (IResource.PROJECT | IResource.FOLDER)) == 0) {
            updateStatus("File container must exist");
            return;
        }
        if (!container.isAccessible()) {
            updateStatus("Project must be writable");
            return;
        }
        if (fileName.length() == 0) {
            updateStatus("File name must be specified");
            return;
        }
        if (fileName.replace('\\', '/').indexOf('/', 1) > 0) {
            updateStatus("File name must be valid");
            return;
        }
        final int dotLoc = fileName.lastIndexOf('.');
        if (dotLoc != -1) {
            final String ext = fileName.substring(dotLoc + 1);
            if (!ext.equalsIgnoreCase("erl")) {
                updateStatus("File extension must be \"erl\"");
                return;
            }
        }
        updateStatus(null);
    }

    private void updateStatus(final String message) {
        setErrorMessage(message);
        setPageComplete(message == null);
    }

    public String getContainerName() {
        return containerText.getText();
    }

    public String getFileName() {
        return fileText.getText();
    }

    /**
     * Get the skeleton that is to be generated.
     * 
     * @return The skeleton that the new file is to consist of.
     */
    public String getSkeleton() {
        final int index = skeleton.getSelectionIndex();
        if (index < 0 || index >= moduleTemplates.length) {
            return "";
        }
        return parse(moduleTemplates[index], getContextType());
    }

    private TemplateContextType getContextType() {
        if (fContextType == null) {
            fContextType = ErlideUIPlugin
                    .getDefault()
                    .getContextTypeRegistry()
                    .getContextType(
                            ErlangSourceContextTypeModule.ERLANG_SOURCE_CONTEXT_TYPE_MODULE_ID);
        }
        return fContextType;
    }

    private String parse(final Template template,
            final TemplateContextType contextType) {
        String s = getFileName();
        if (ModuleKind.hasModuleExtension(s)) {
            s = SystemUtils.withoutExtension(s);
        }
        ModuleVariableResolver.getDefault().setModule(s);

        ExportedFunctionsVariableResolver.getDefault().clearFunctions();
        LocalFunctionsVariableResolver.getDefault().clearFunctions();

        for (int i = 0; i < functionGroup.getFunctionData().length; i++) {
            final Function fun = functionGroup.getFunctionData()[i];
            if (fun.isExported) {
                ExportedFunctionsVariableResolver.getDefault().addFunction(
                        fun.name, fun.arity);
            } else {
                LocalFunctionsVariableResolver.getDefault().addFunction(
                        fun.name, fun.arity);
            }
        }

        TemplateBuffer tb = null;

        try {
            final DocumentTemplateContext context = new DocumentTemplateContext(
                    contextType, new Document(template.getPattern()), 0,
                    template.getPattern().length());
            tb = context.evaluate(template);
        } catch (final BadLocationException e) {
            ErlLogger.warn(e);
        } catch (final TemplateException e) {
            ErlLogger.warn(e);
        }

        if (tb == null) {
            return null;
        }
        return tb.getString();
    }
}
