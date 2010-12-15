/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.views;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;

class GotoInputAction extends Action {

    // private AbstractInfoView fInfoView;

    public GotoInputAction(final AbstractInfoView infoView) {
        Assert.isNotNull(infoView);
        // fInfoView = infoView;

        // TODO JavaPluginImages.setLocalImageDescriptors(this,
        // "goto_input.gif");
        // //$NON-NLS-1$
        setText("Goto Input");
        setToolTipText("Go to input");
        setDescription("Go to input");

        // TODO PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
        // IJavaHelpContextIds.OPEN_INPUT_ACTION);
    }

    @Override
    public void run() {
        // TODO not yet implemented
        // IErlElement inputElement= fInfoView.getInput();
        // new OpenAction(fInfoView.getViewSite()).run(new Object[] {
        // inputElement });
    }
}
