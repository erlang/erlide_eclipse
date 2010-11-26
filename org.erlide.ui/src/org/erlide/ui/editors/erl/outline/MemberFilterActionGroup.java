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
package org.erlide.ui.editors.erl.outline;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionGroup;
import org.erlide.core.erlang.IErlAttribute;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImportExport;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.ErlideUIPluginImages;
import org.erlide.ui.actions.ActionMessages;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

/**
 * Action Group that contributes filter buttons for a view parts showing methods
 * and fields. Contributed filters are: hide fields, hide static members hide
 * non-public members and hide local types.
 * <p>
 * The action group installs a filter on a structured viewer. The filter is
 * connected to the actions installed in the view part's toolbar menu and is
 * updated when the state of the buttons changes.
 * 
 * <p>
 * This class may be instantiated; it is not intended to be subclassed.
 * </p>
 * 
 * @since 2.0
 */
public class MemberFilterActionGroup extends ActionGroup {

    public static final int ALL_FILTERS = MemberFilter.FILTER_LOCAL_FUNCTIONS
            | MemberFilter.FILTER_ATTRIBUTES
            | MemberFilter.FILTER_MACRO_RECORD_DEFS_TYPESPECS;

    private static final String TAG_HIDEATTRIBUTES = "hideattributes"; //$NON-NLS-1$
    private static final String TAG_HIDELOCALFUNCTIONS = "hidelocalfunctions"; //$NON-NLS-1$
    private static final String TAG_HIDEMACRORECORDDEFSTYPESPECS = "hidemacrorecorddefstypespecs"; //$NON-NLS-1$

    private final List<MemberFilterAction> fFilterActions;
    private final MemberFilter fFilter;

    final StructuredViewer fViewer;
    private final String fViewerId;
    private final boolean fInViewMenu;

    /**
     * Creates a new <code>MemberFilterActionGroup</code>.
     * 
     * @param viewer
     *            the viewer to be filtered
     * @param viewerId
     *            a unique id of the viewer. Used as a key to to store the last
     *            used filter settings in the preference store
     */
    public MemberFilterActionGroup(final StructuredViewer viewer,
            final String viewerId) {
        this(viewer, viewerId, false);
    }

    /**
     * Creates a new <code>MemberFilterActionGroup</code>.
     * 
     * @param viewer
     *            the viewer to be filtered
     * @param viewerId
     *            a unique id of the viewer. Used as a key to to store the last
     *            used filter settings in the preference store
     * @param inViewMenu
     *            if <code>true</code> the actions are added to the view menu.
     *            If <code>false</code> they are added to the toolbar.
     * 
     * @since 2.1
     */
    public MemberFilterActionGroup(final StructuredViewer viewer,
            final String viewerId, final boolean inViewMenu) {
        this(viewer, viewerId, inViewMenu, ALL_FILTERS);
    }

    /**
     * Creates a new <code>MemberFilterActionGroup</code>.
     * 
     * @param viewer
     *            the viewer to be filtered
     * @param viewerId
     *            a unique id of the viewer. Used as a key to to store the last
     *            used filter settings in the preference store
     * @param inViewMenu
     *            if <code>true</code> the actions are added to the view menu.
     *            If <code>false</code> they are added to the toolbar.
     * @param availableFilters
     *            Specifies which filter action should be contained.
     *            <code>FILTER_NONPUBLIC</code>, <code>FILTER_STATIC</code>,
     *            <code>FILTER_FIELDS</code> and <code>FILTER_LOCALTYPES</code>
     *            or a combination of these constants are possible values. Use
     *            <code>ALL_FILTERS</code> to select all available filters.
     * 
     * @since 3.0
     */
    public MemberFilterActionGroup(final StructuredViewer viewer,
            final String viewerId, final boolean inViewMenu,
            final int availableFilters) {

        fViewer = viewer;
        fViewerId = viewerId;
        fInViewMenu = inViewMenu;

        fFilter = new MemberFilter();

        String title, helpContext;
        fFilterActions = new ArrayList<MemberFilterAction>();

        // export directives
        int filterProperty = MemberFilter.FILTER_ATTRIBUTES;
        if (isSet(filterProperty, availableFilters)) {
            final boolean filterEnabled = getPrefsNode().getBoolean(
                    getPreferenceKey(filterProperty), false);
            if (filterEnabled) {
                fFilter.addFilter(filterProperty);
            }
            title = ActionMessages.MemberFilterActionGroup_hide_attributes_label;
            helpContext = IErlangHelpContextIds.FILTER_ATTRIBUTES_ACTION;
            final MemberFilterAction hideAttributes = new MemberFilterAction(
                    this, title, MemberFilter.FILTER_ATTRIBUTES, helpContext,
                    filterEnabled);
            hideAttributes
                    .setDescription(ActionMessages.MemberFilterActionGroup_hide_attributes_description);
            hideAttributes
                    .setToolTipText(ActionMessages.MemberFilterActionGroup_hide_attributes_tooltip);
            hideAttributes
                    .setImageDescriptor(ErlideUIPluginImages.DESC_HIDE_ATTRIBUTES);
            fFilterActions.add(hideAttributes);
        }

        // non-exported
        filterProperty = MemberFilter.FILTER_LOCAL_FUNCTIONS;
        if (isSet(filterProperty, availableFilters)) {
            final boolean filterEnabled = getPrefsNode().getBoolean(
                    getPreferenceKey(filterProperty), false);
            if (filterEnabled) {
                fFilter.addFilter(filterProperty);
            }
            title = ActionMessages.MemberFilterActionGroup_hide_local_functions_label;
            helpContext = IErlangHelpContextIds.FILTER_LOCAL_FUNCTIONS_ACTION;
            final MemberFilterAction hideLocalFunctions = new MemberFilterAction(
                    this, title, filterProperty, helpContext, filterEnabled);
            hideLocalFunctions
                    .setDescription(ActionMessages.MemberFilterActionGroup_hide_local_functions_description);
            hideLocalFunctions
                    .setToolTipText(ActionMessages.MemberFilterActionGroup_hide_local_functions_tooltip);
            hideLocalFunctions
                    .setImageDescriptor(ErlideUIPluginImages.DESC_HIDE_LOCAL_FUNCTIONS);
            fFilterActions.add(hideLocalFunctions);
        }

        // macros and record definitions
        filterProperty = MemberFilter.FILTER_MACRO_RECORD_DEFS_TYPESPECS;
        if (isSet(filterProperty, availableFilters)) {
            final boolean filterEnabled = getPrefsNode().getBoolean(
                    getPreferenceKey(filterProperty), false);
            if (filterEnabled) {
                fFilter.addFilter(filterProperty);
            }
            title = ActionMessages.MemberFilterActionGroup_hide_macro_record_defs_typespecs_label;
            helpContext = IErlangHelpContextIds.FILTER_MACRO_RECORD_DEFS_ACTION;
            final MemberFilterAction hideMacroRecordDefs = new MemberFilterAction(
                    this, title, filterProperty, helpContext, filterEnabled);
            hideMacroRecordDefs
                    .setDescription(ActionMessages.MemberFilterActionGroup_hide_macro_record_defs_typespecs_description);
            hideMacroRecordDefs
                    .setToolTipText(ActionMessages.MemberFilterActionGroup_hide_macro_record_defs_typespecs_tooltip);
            hideMacroRecordDefs
                    .setImageDescriptor(ErlideUIPluginImages.DESC_HIDE_MACRO_RECORD_DEFS);
            fFilterActions.add(hideMacroRecordDefs);
        }

        // order corresponds to order in toolbar

        fViewer.addFilter(fFilter);
    }

    private String getPreferenceKey(final int filterProperty) {
        return "MemberFilterActionGroup." + fViewerId + '.' + String.valueOf(filterProperty); //$NON-NLS-1$
    }

    public static IEclipsePreferences getPrefsNode() {
        final String qualifier = ErlideUIPlugin.PLUGIN_ID;
        final IScopeContext context = new InstanceScope();
        final IEclipsePreferences eclipsePreferences = context
                .getNode(qualifier);
        return eclipsePreferences;
    }

    /**
     * Sets the member filters.
     * 
     * @param filterProperty
     *            the filter to be manipulated. Valid values are defined by this
     *            action group
     * @param set
     *            if <code>true</code> the given filter is installed. If
     *            <code>false</code> the given filter is removed .
     */
    public void setMemberFilter(final int filterProperty, final boolean set) {
        setMemberFilters(new int[] { filterProperty }, new boolean[] { set },
                true);
    }

    private void setMemberFilters(final int[] propertyKeys,
            final boolean[] propertyValues, final boolean refresh) {
        if (propertyKeys.length == 0) {
            return;
        }
        Assert.isTrue(propertyKeys.length == propertyValues.length);

        for (int i = 0; i < propertyKeys.length; i++) {
            final int filterProperty = propertyKeys[i];
            final boolean set = propertyValues[i];

            final IEclipsePreferences node = getPrefsNode();
            boolean found = false;
            for (final MemberFilterAction j : fFilterActions) {
                final int currProperty = j.getFilterProperty();
                if (currProperty == filterProperty) {
                    j.setChecked(set);
                    found = true;
                    node.putBoolean(getPreferenceKey(filterProperty), set);
                }
            }
            if (found) {
                if (set) {
                    fFilter.addFilter(filterProperty);
                } else {
                    fFilter.removeFilter(filterProperty);
                }
            }
        }
        if (refresh) {
            fViewer.getControl().setRedraw(false);
            BusyIndicator.showWhile(fViewer.getControl().getDisplay(),
                    new Runnable() {
                        public void run() {
                            fViewer.refresh();
                        }
                    });
            fViewer.getControl().setRedraw(true);
        }
    }

    private boolean isSet(final int flag, final int set) {
        return (flag & set) != 0;
    }

    /**
     * Returns <code>true</code> if the given filter is installed.
     * 
     * @param filterProperty
     *            the filter to be tested. Valid values are
     *            <code>FILTER_FIELDS</code>, <code>FILTER_PUBLIC</code>,
     *            <code>FILTER_PRIVATE</code> and <code>FILTER_LOCALTYPES</code>
     *            as defined by this action group
     */
    public boolean hasMemberFilter(final int filterProperty) {
        return fFilter.hasFilter(filterProperty);
    }

    /**
     * Saves the state of the filter actions in a memento.
     * 
     * @param memento
     *            the memento to which the state is saved
     */
    public void saveState(final IMemento memento) {
        memento.putString(TAG_HIDEATTRIBUTES,
                String.valueOf(hasMemberFilter(MemberFilter.FILTER_ATTRIBUTES)));
        memento.putString(TAG_HIDELOCALFUNCTIONS, String
                .valueOf(hasMemberFilter(MemberFilter.FILTER_LOCAL_FUNCTIONS)));
        memento.putString(
                TAG_HIDEMACRORECORDDEFSTYPESPECS,
                String.valueOf(hasMemberFilter(MemberFilter.FILTER_MACRO_RECORD_DEFS_TYPESPECS)));
    }

    /**
     * Restores the state of the filter actions from a memento.
     * <p>
     * Note: This method does not refresh the viewer.
     * </p>
     * 
     * @param memento
     *            the memento from which the state is restored
     */
    public void restoreState(final IMemento memento) {
        setMemberFilters(
                new int[] { MemberFilter.FILTER_ATTRIBUTES,
                        MemberFilter.FILTER_LOCAL_FUNCTIONS,
                        MemberFilter.FILTER_MACRO_RECORD_DEFS_TYPESPECS },
                new boolean[] {
                        Boolean.valueOf(memento.getString(TAG_HIDEATTRIBUTES))
                                .booleanValue(),
                        Boolean.valueOf(
                                memento.getString(TAG_HIDELOCALFUNCTIONS))
                                .booleanValue(),
                        Boolean.valueOf(
                                memento.getString(TAG_HIDEMACRORECORDDEFSTYPESPECS))
                                .booleanValue() }, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see ActionGroup#fillActionBars(IActionBars)
     */
    @Override
    public void fillActionBars(final IActionBars actionBars) {
        contributeToToolBar(actionBars.getToolBarManager());
    }

    /**
     * Adds the filter actions to the given tool bar
     * 
     * @param tbm
     *            the tool bar to which the actions are added
     */
    public void contributeToToolBar(final IToolBarManager tbm) {
        if (fInViewMenu) {
            return;
        }
        for (final MemberFilterAction i : fFilterActions) {
            tbm.add(i);
        }
    }

    /**
     * Adds the filter actions to the given menu manager.
     * 
     * @param menu
     *            the menu manager to which the actions are added
     * @since 2.1
     */
    public void contributeToViewMenu(final IMenuManager menu) {
        if (!fInViewMenu) {
            return;
        }
        final String filters = "filters"; //$NON-NLS-1$
        if (menu.find(filters) != null) {
            for (final MemberFilterAction i : fFilterActions) {
                menu.prependToGroup(filters, i);
            }
        } else {
            for (final MemberFilterAction i : fFilterActions) {
                menu.add(i);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see ActionGroup#dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
    }

    /**
     * Action used to enable / disable method filter properties
     */
    public static class MemberFilterAction extends Action {

        private final int fFilterProperty;
        private final MemberFilterActionGroup fFilterActionGroup;

        public MemberFilterAction(final MemberFilterActionGroup actionGroup,
                final String title, final int property,
                final String contextHelpId, final boolean initValue) {
            super(title);
            fFilterActionGroup = actionGroup;
            fFilterProperty = property;

            PlatformUI.getWorkbench().getHelpSystem()
                    .setHelp(this, contextHelpId);

            setChecked(initValue);
        }

        /**
         * Returns this action's filter property.
         */
        public int getFilterProperty() {
            return fFilterProperty;
        }

        /*
         * @see Action#actionPerformed
         */
        @Override
        public void run() {
            fFilterActionGroup.setMemberFilter(fFilterProperty, isChecked());
        }

    }

    /**
     * Filter for the methods viewer. Changing a filter property does not
     * trigger a refiltering of the viewer
     */
    public static class MemberFilter extends ViewerFilter {

        public static final int FILTER_LOCAL_FUNCTIONS = 1;
        public static final int FILTER_ATTRIBUTES = 2;
        public static final int FILTER_MACRO_RECORD_DEFS_TYPESPECS = 4;
        // public static final int FILTER_LOCALTYPES = 8;

        private int fFilterProperties = 0;

        /**
         * Modifies filter and add a property to filter for
         */
        public final void addFilter(final int filter) {
            fFilterProperties |= filter;
        }

        /**
         * Modifies filter and remove a property to filter for
         */
        public final void removeFilter(final int filter) {
            fFilterProperties &= -1 ^ filter;
        }

        /**
         * Tests if a property is filtered
         */
        public final boolean hasFilter(final int filter) {
            return (fFilterProperties & filter) != 0;
        }

        /*
         * @see ViewerFilter#isFilterProperty(java.lang.Object,
         * java.lang.String)
         */
        public boolean isFilterProperty(final Object element,
                final Object property) {
            return false;
        }

        /*
         * @see ViewerFilter#select(org.eclipse.jface.viewers.Viewer,
         * java.lang.Object, java.lang.Object)
         */
        @Override
        public boolean select(final Viewer viewer, final Object parentElement,
                final Object element) {
            if (element instanceof IErlElement) {
                final IErlElement e = (IErlElement) element;
                if (hasFilter(FILTER_LOCAL_FUNCTIONS)) {
                    if (e instanceof IErlFunction) {
                        final IErlFunction f = (IErlFunction) e;
                        if (!f.isExported()) {
                            return false;
                        }
                    }
                }
                if (hasFilter(FILTER_ATTRIBUTES)) {
                    if (e instanceof IErlAttribute
                            || e instanceof IErlImportExport) {
                        return false;
                    }
                }
                if (hasFilter(FILTER_MACRO_RECORD_DEFS_TYPESPECS)) {
                    if (e instanceof IErlPreprocessorDef) {
                        return false;
                    }
                    if (e instanceof IErlTypespec) {
                        return false;
                    }
                }
            }
            return true;
        }
    }
}
