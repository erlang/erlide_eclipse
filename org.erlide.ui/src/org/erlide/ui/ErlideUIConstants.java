/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

/**
 * Simple ui constants.
 * 
 * 
 * @author Vlad Dumitrescu
 */
public final class ErlideUIConstants {

    /**
     * The icon path
     */
    public static final String ICON_PATH = "/icons/full/obj16/";

    /**
     * The console image
     */
    public static final String IMG_CONSOLE = "console.gif";

    /**
     * The new project wizard
     */
    public static final String IMG_NEW_PROJECT_WIZARD = "erlang058.gif";

    /**
     * The project tree label
     */
    public static final String IMG_PROJECT_LABEL = "projects.gif";

    /**
     * The closed project tree label
     */
    public static final String IMG_PROJECT_CLOSED_LABEL = "closedproject.gif";

    /**
     * The package folder (source folders)
     */
    public static final String IMG_PACKAGE_FOLDER_LABEL = "packagefolder.gif";

    /**
     * The package tree label
     */
    public static final String IMG_PACKAGE_LABEL = "package.gif";

    /**
     * The file tree label
     */
    public static final String IMG_FILE_LABEL = "file.gif";

    /**
     * the folder tree label
     */
    public static final String IMG_FOLDER_LABEL = "folder.gif";

    /**
     * disabled refresh
     */
    public static final String IMG_DISABLED_REFRESH = "drefresh_nav.gif";

    /**
     * Refresh id
     */
    public static final String IMG_REFRESH = "refresh_nav.gif";

    /**
     * disabled import
     */
    public static final String IMG_DISABLED_IMPORT = "dimport_wiz.gif";

    /**
     * the import wizard
     */
    public static final String IMG_IMPORT = "import_wiz.gif";

    /**
     * The disabled export
     */
    public static final String IMG_DISABLED_EXPORT = "dexport_wiz.gif";

    /**
     * The export wizard
     */
    public static final String IMG_EXPORT = "export_wiz.gif";

    /**
     * collapse all
     */
    public static final String IMG_COLLAPSEALL = "collapseall.gif";

    /**
     * the action group
     */
    public static final String CONSOLE_ACTION_ERLANG_GROUP = "erlang_group";

    public static final String EVALUATION_GROUP = "erlang_evaluation_group";

    public static final String IMG_ERLANG_LOGO = "erlang058.gif";

    // The Erlang Navigator View ID
    public static final String NAVIGATOR_VIEW_ID = "org.erlide.ui.views.navigator.view"; //$NON-NLS-1$

    // Search Menu (action ID)
    public static final String FIND_REFERENCES_IN_WORKSPACE = "org.erlide.ui.search.actions.ReferencesInWorkspace"; //$NON-NLS-1$
    public static final String FIND_REFERENCES_IN_PROJECT = "org.erlide.ui.actions.ReferencesInProject"; //$NON-NLS-1$
    public static final String FIND_REFERENCES_IN_WORKING_SET = "org.erlide.ui.search.actions.ReferencesInWorkingSet"; //$NON-NLS-1$;

    public static final String FIND_IMPLEMENTORS_IN_WORKSPACE = "org.erlide.ui.search.actions.ImplementorsInWorkspace"; //$NON-NLS-1$
    public static final String FIND_IMPLEMENTORS_IN_PROJECT = "org.erlide.ui.search.actions.ImplementorsInProject"; //$NON-NLS-1$
    public static final String FIND_IMPLEMENTORS_IN_WORKING_SET = "org.erlide.ui.search.actions.ImplementorsInWorkingSet"; //$NON-NLS-1$;

    private ErlideUIConstants() {
    }

}
