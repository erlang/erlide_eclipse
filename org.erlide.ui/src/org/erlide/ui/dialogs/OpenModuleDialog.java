/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.dialogs;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

/**
 * Shows a list of resources to the user with a text entry field for a string
 * pattern used to filter the list of resources.
 * 
 */
public class OpenModuleDialog extends FilteredModulesSelectionDialog {

    /**
     * Creates a new instance of the class.
     * 
     * @param parentShell
     *            the parent shell
     * @param container
     *            the container
     * @param typesMask
     *            the types mask
     */
    public OpenModuleDialog(final Shell parentShell, final IContainer container) {
        super(parentShell, true, container, IResource.FILE);
        setTitle("Open module");
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(parentShell, IErlangHelpContextIds.OPEN_MODULE_DIALOG);
    }
}
