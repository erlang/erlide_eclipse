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
package org.erlide.ui.util;

import org.eclipse.ui.IWorkbenchActionConstants;

/**
 * Constants for menu groups used in context menus for Erlang views and editors.
 * <p>
 * This interface declares constants only; it is not intended to be implemented.
 * </p>
 */
public interface IContextMenuConstants {

    /**
     * Pop-up menu: name of group for goto actions (value
     * <code>"group.open"</code>).
     * <p>
     * Examples for open actions are:
     * <ul>
     * <li>Go Into</li>
     * <li>Go To</li>
     * </ul>
     * </p>
     */
    String GROUP_GOTO = "group.goto"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for open actions (value
     * <code>"group.open"</code>).
     * <p>
     * Examples for open actions are:
     * <ul>
     * <li>Open To</li>
     * <li>Open With</li>
     * </ul>
     * </p>
     */
    String GROUP_OPEN = "group.open"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for show actions (value
     * <code>"group.show"</code>).
     * <p>
     * Examples for show actions are:
     * <ul>
     * <li>Show in Navigator</li>
     * <li>Show in Type Hierarchy</li>
     * </ul>
     * </p>
     */
    String GROUP_SHOW = "group.show"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for new actions (value
     * <code>"group.new"</code>).
     * <p>
     * Examples for new actions are:
     * <ul>
     * <li>Create new class</li>
     * <li>Create new interface</li>
     * </ul>
     * </p>
     */
    String GROUP_NEW = "group.new"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for build actions (value
     * <code>"group.build"</code>).
     */
    String GROUP_BUILD = "group.build"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for reorganize actions (value
     * <code>"group.reorganize"</code>).
     */
    String GROUP_REORGANIZE = IWorkbenchActionConstants.GROUP_REORGANIZE;

    /**
     * Pop-up menu: name of group for code generation actions ( value
     * <code>"group.generate"</code>).
     */
    String GROUP_GENERATE = "group.generate"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for source actions. This is an alias for
     * <code>GROUP_GENERATE</code> to be more consistent with main menu bar
     * structure.
     * 
     * @since 2.0
     */
    String GROUP_SOURCE = GROUP_GENERATE;

    /**
     * Pop-up menu: name of group for search actions (value
     * <code>"group.search"</code>).
     */
    String GROUP_SEARCH = "group.search"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for additional actions (value
     * <code>"additions"</code>).
     */
    String GROUP_ADDITIONS = "additions"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for viewer setup actions (value
     * <code>"group.viewerSetup"</code>).
     */
    String GROUP_VIEWER_SETUP = "group.viewerSetup"; //$NON-NLS-1$

    /**
     * Pop-up menu: name of group for properties actions (value
     * <code>"group.properties"</code>).
     */
    String GROUP_PROPERTIES = "group.properties"; //$NON-NLS-1$
}
