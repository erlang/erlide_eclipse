/*******************************************************************************
 * Copyright (c) 2004, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.erl.outline;

import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * IConentOutlinePage with externally enabled/disabled element sorting
 */
public interface ISortableContentOutlinePage extends IContentOutlinePage {
    /**
     * Turns sorting on or off
     * 
     * @param sorting
     *            - boolean value indicating if sorting should be enabled
     */
    public void sort(boolean sorting);

}
