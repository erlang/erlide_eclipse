/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideEdocExport;

public class EdocExportWizard extends Wizard implements IExportWizard {

    private EdocExportWizardPage page;
    private IStructuredSelection selection;

    @Override
    public boolean performFinish() {
        if (!validateFinish()) {
            return false;
        }

        final Collection<IProject> projects = page.getSelectedResources();
        final Map<String, OtpErlangObject> options = page.getOptions();
        for (final IProject prj : projects) {
            if (!prj.isAccessible()) {
                ErlLogger.debug("EDOC: " + prj.getName()
                        + " is not accessible, skipping.");
                continue;
            }
            ErlLogger.debug("EDOC: " + prj.getName());
            try {
                final IFolder dest = prj.getFolder(page.getDestination());
                if (!dest.exists()) {
                    dest.create(true, true, null);
                }
                options.put("dir", new OtpErlangString(dest.getLocation()
                        .toString()));
                final List<String> files = new ArrayList<String>();
                final IErlProject eprj = ErlangCore.getModel().findProject(prj);
                final IOldErlangProjectProperties props = eprj.getProperties();
                for (final IPath dir : props.getSourceDirs()) {
                    final IFolder folder = prj.getFolder(dir);
                    if (folder.isAccessible()) {
                        folder.accept(new IResourceVisitor() {
                            public boolean visit(final IResource resource)
                                    throws CoreException {
                                if ("erl".equals(resource.getFileExtension())) {
                                    if (resource.isAccessible()) {
                                        files.add(resource.getLocation()
                                                .toString());
                                    }
                                }
                                return true;
                            }
                        });
                    }
                }
                try {
                    ErlideEdocExport.files(files, options);
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
        }

        return true;
    }

    public void init(final IWorkbench workbench,
            final IStructuredSelection aSelection) {
        selection = aSelection;

        final List<?> selectedResources = IDE
                .computeSelectedResources(aSelection);
        if (!selectedResources.isEmpty()) {
            selection = new StructuredSelection(selectedResources);
        }
        setWindowTitle("eDoc Export Wizard"); // NON-NLS-1
        setNeedsProgressMonitor(true);
    }

    @Override
    public void addPages() {
        super.addPages();
        page = new EdocExportWizardPage("edoc", selection);
        addPage(page);
    }

    private boolean validateFinish() {
        return true;
    }
}
