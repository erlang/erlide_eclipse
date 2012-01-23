/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.widgets.Composite;
import org.erlide.backend.runtimeinfo.RuntimeInfo;

public class OtpDirectoryFieldEditor extends DirectoryFieldEditor {

    public OtpDirectoryFieldEditor(final String name, final String label,
            final Composite parent) {
        init(name, label);
        setErrorMessage("\"Erlang Home\" location isn't an Erlang root directory");
        setChangeButtonText(JFaceResources.getString("openBrowse")); //$NON-NLS-1$
        setValidateStrategy(VALIDATE_ON_KEY_STROKE);
        // setValidateStrategy(VALIDATE_ON_FOCUS_LOST);
        createControl(parent);
    }

    @Override
    protected boolean doCheckState() {
        String fileName = getTextControl().getText();
        fileName = fileName.trim();
        if (RuntimeInfo.hasCompiler(fileName)) {
            showMessage("No Erlang compiler was found."
                    + " This runtime can't be used for a compiler backend.");
        }
        return RuntimeInfo.isValidOtpHome(fileName);
    }
}
