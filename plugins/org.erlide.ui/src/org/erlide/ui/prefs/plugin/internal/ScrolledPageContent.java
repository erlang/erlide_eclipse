/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.prefs.plugin.internal;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.SharedScrolledComposite;

public class ScrolledPageContent extends SharedScrolledComposite {

    private final FormToolkit fToolkit;

    public ScrolledPageContent(final Composite parent) {
        this(parent, SWT.V_SCROLL | SWT.H_SCROLL);
    }

    public ScrolledPageContent(final Composite parent, final int style) {
        super(parent, style);

        setFont(parent.getFont());
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final Display display = workbench.getDisplay();

        fToolkit = new FormToolkit(display);

        setExpandHorizontal(true);
        setExpandVertical(true);

        final Composite body = new Composite(this, SWT.NONE);
        body.setFont(parent.getFont());
        setContent(body);
    }

    public void adaptChild(final Control childControl) {
        fToolkit.adapt(childControl, true, true);
    }

    public Composite getBody() {
        return (Composite) getContent();
    }

}
