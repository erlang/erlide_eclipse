package org.erlide.ui.editors.erl.outline.filters;

import java.text.Collator;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.IPluginContribution;
import org.eclipse.ui.activities.WorkbenchActivityHelper;
import org.erlide.ui.internal.ErlideUIPlugin;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class FilterDescriptor implements Comparable<Object>,
        IPluginContribution {

    private static String PATTERN_FILTER_ID_PREFIX = "_patternFilterId_"; //$NON-NLS-1$

    private static final String EXTENSION_POINT_NAME = "erlangElementFilters"; //$NON-NLS-1$

    private static final String FILTER_TAG = "filter"; //$NON-NLS-1$

    private static final String PATTERN_ATTRIBUTE = "pattern"; //$NON-NLS-1$
    private static final String ID_ATTRIBUTE = "id"; //$NON-NLS-1$
    private static final String TARGET_ID_ATTRIBUTE = "targetId"; //$NON-NLS-1$
    private static final String CLASS_ATTRIBUTE = "class"; //$NON-NLS-1$
    private static final String NAME_ATTRIBUTE = "name"; //$NON-NLS-1$
    private static final String ENABLED_ATTRIBUTE = "enabled"; //$NON-NLS-1$
    private static final String DESCRIPTION_ATTRIBUTE = "description"; //$NON-NLS-1$

    private static Collection<FilterDescriptor> fgFilterDescriptors;

    private final IConfigurationElement fElement;

    private ViewerFilter fCachedInstance = null;

    /**
     * Returns all contributed Erlang element filters.
     * 
     * @return all contributed Erlang element filters
     */
    public static Collection<FilterDescriptor> getFilterDescriptors() {
        if (fgFilterDescriptors == null) {
            final IExtensionRegistry registry = Platform.getExtensionRegistry();
            final IConfigurationElement[] elements = registry
                    .getConfigurationElementsFor(ErlideUIPlugin.PLUGIN_ID,
                            EXTENSION_POINT_NAME);
            final String extensionPointID = ErlideUIPlugin.PLUGIN_ID + "."
                    + EXTENSION_POINT_NAME;
            fgFilterDescriptors = createFilterDescriptors(elements,
                    extensionPointID);
        }
        return fgFilterDescriptors;
    }

    /**
     * Returns all Erlang element filters which are contributed to the given
     * view.
     * 
     * @param targetId
     *            the target id
     * @return all contributed Erlang element filters for the given view
     */
    public static List<FilterDescriptor> getFilterDescriptors(
            final String targetId) {
        final Collection<FilterDescriptor> descs = FilterDescriptor
                .getFilterDescriptors();
        final List<FilterDescriptor> result = Lists.newArrayList();
        for (final FilterDescriptor desc : descs) {
            final String tid = desc.getTargetId();
            if (WorkbenchActivityHelper.filterItem(desc)) {
                continue;
            }
            if (tid == null || tid.equals(targetId)) {
                result.add(desc);
            }
        }
        return result;
    }

    /**
     * Creates a new filter descriptor for the given configuration element.
     * 
     * @param element
     *            configuration element
     */
    private FilterDescriptor(final IConfigurationElement element) {
        fElement = element;
        // it is either a pattern filter or a custom filter
        Assert.isTrue(
                isPatternFilter() ^ isClassFilter(),
                "An extension for extension-point org.erlide.ui.erlangElementFilters does not specify a correct filter"); //$NON-NLS-1$
        Assert.isNotNull(
                getId(),
                "An extension for extension-point org.erlide.ui.erlangElementFilters does not provide a valid ID"); //$NON-NLS-1$
        Assert.isNotNull(
                getName(),
                "An extension for extension-point org.erlide.ui.erlangElementFilters does not provide a valid name"); //$NON-NLS-1$
    }

    /**
     * Creates a new <code>ViewerFilter</code>. This method is only valid for
     * viewer filters.
     * 
     * @return a new <code>ViewerFilter</code>
     */
    public ViewerFilter getViewerFilter() {
        if (!isClassFilter()) {
            return null;
        }

        if (fCachedInstance == null) {

            final ViewerFilter[] result = new ViewerFilter[1];
            final String message = String
                    .format("The org.erlide.ui.erlangElementFilters plug-in extension \"%s\" specifies a viewer filter class which does not exist.",
                            getId());
            final ISafeRunnable code = new SafeRunnable(message) {
                /*
                 * @see org.eclipse.core.runtime.ISafeRunnable#run()
                 */
                @Override
                public void run() throws Exception {
                    result[0] = (ViewerFilter) fElement
                            .createExecutableExtension(CLASS_ATTRIBUTE);
                }

            };
            SafeRunner.run(code);
            fCachedInstance = result[0];
        }
        return fCachedInstance;
    }

    // ---- XML Attribute accessors
    // ---------------------------------------------

    /**
     * Returns the filter's id.
     * <p>
     * This attribute is mandatory for custom filters. The ID for pattern
     * filters is PATTERN_FILTER_ID_PREFIX plus the pattern itself.
     * </p>
     * 
     * @return the filter id
     */
    public String getId() {
        return fElement.getAttribute(ID_ATTRIBUTE);
    }

    /**
     * Returns the filter's name.
     * <p>
     * If the name of a pattern filter is missing then the pattern is used as
     * its name.
     * </p>
     * 
     * @return the filter's name
     */
    public String getName() {
        String name = fElement.getAttribute(NAME_ATTRIBUTE);
        if (name == null && isPatternFilter()) {
            name = getPattern();
        }
        return name;
    }

    /**
     * Returns the filter's pattern.
     * 
     * @return the pattern string or <code>null</code> if it's not a pattern
     *         filter
     */
    public String getPattern() {
        return fElement.getAttribute(PATTERN_ATTRIBUTE);
    }

    /**
     * Returns the filter's viewId.
     * 
     * @return the view ID or <code>null</code> if the filter is for all views
     * @since 3.0
     */
    public String getTargetId() {
        final String tid = fElement.getAttribute(TARGET_ID_ATTRIBUTE);
        if (tid != null) {
            return tid;
        }
        return "";
    }

    /**
     * Returns the filter's description.
     * 
     * @return the description or <code>null</code> if no description is
     *         provided
     */
    public String getDescription() {
        String description = fElement.getAttribute(DESCRIPTION_ATTRIBUTE);
        if (description == null) {
            description = ""; //$NON-NLS-1$
        }
        return description;
    }

    /**
     * @return <code>true</code> if this filter is a pattern filter.
     */
    public boolean isPatternFilter() {
        return getPattern() != null;
    }

    /**
     * @return <code>true</code> if this filter is a class filter.
     */
    public boolean isClassFilter() {
        return fElement.getAttribute(CLASS_ATTRIBUTE) != null;
    }

    /**
     * Returns <code>true</code> if the filter is initially enabled.
     * 
     * This attribute is optional and defaults to <code>true</code>.
     * 
     * @return returns <code>true</code> if the filter is initially enabled
     */
    public boolean isEnabled() {
        final String strVal = fElement.getAttribute(ENABLED_ATTRIBUTE);
        return strVal == null || Boolean.valueOf(strVal).booleanValue();
    }

    /*
     * Implements a method from Comparable<Object>
     */
    @Override
    public int compareTo(final Object o) {
        if (o instanceof FilterDescriptor) {
            return Collator.getInstance().compare(getName(),
                    ((FilterDescriptor) o).getName());
        } else {
            return -1;
        }
    }

    @Override
    public boolean equals(final Object obj) {
        return compareTo(obj) == 0;
    }

    @Override
    public int hashCode() {
        return getName().hashCode();
    }

    // ---- initialization ---------------------------------------------------

    /**
     * Creates the filter descriptors.
     * 
     * @param elements
     *            the configuration elements
     * @param extensionPointID
     * @return new filter descriptors
     */
    private static Collection<FilterDescriptor> createFilterDescriptors(
            final IConfigurationElement[] elements,
            final String extensionPointID) {
        final List<FilterDescriptor> result = Lists.newArrayList();
        final Set<String> descIds = Sets.newHashSet();
        for (final IConfigurationElement element : elements) {
            if (FILTER_TAG.equals(element.getName())) {

                final FilterDescriptor[] desc = new FilterDescriptor[1];
                SafeRunner.run(new SafeRunnable(
                        " One of the extensions for extension-point "
                                + extensionPointID + " is incorrect.") {
                    @Override
                    public void run() throws Exception {
                        desc[0] = new FilterDescriptor(element);
                    }
                });

                if (desc[0] != null && !descIds.contains(desc[0].getId())) {
                    result.add(desc[0]);
                    descIds.add(desc[0].getId());
                }
            }
        }
        return result;
    }

    @Override
    public String getLocalId() {
        return fElement.getAttribute(ID_ATTRIBUTE);
    }

    @Override
    public String getPluginId() {
        return fElement.getContributor().getName();
    }

    public static FilterDescriptor getFilterDescriptor(final String id) {
        for (final FilterDescriptor desc : getFilterDescriptors()) {
            if (desc.getId().equals(id)) {
                return desc;
            }
        }
        return null;
    }
}
