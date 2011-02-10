/*******************************************************************************
 * Copyright (c) 2005, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package shade.bterl.browser;

import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.internal.browser.DefaultWorkbenchBrowserSupport;

@SuppressWarnings("restriction")
public class BterlBrowserSupport extends DefaultWorkbenchBrowserSupport {

    @Override
    protected IWebBrowser doCreateBrowser(final int style,
            final String browserId, final String name, final String tooltip) {
        return new BterlBrowserEditorInstance(browserId, style, name, null);
    }

}
