/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.erlide.engine.model.root.IErlangProjectProperties;

/**
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public abstract class ProjectPreferencesWizardPage extends WizardPage {

    protected final IErlangProjectProperties info;

    /**
     * Constructor inherited from parent
     * 
     * @param pageName
     * @param builder
     * @param info
     * @wbp.parser.constructor
     */
    public ProjectPreferencesWizardPage(final String pageName,
            final IErlangProjectProperties info) {
        super(pageName);
        this.info = info;
    }
}
