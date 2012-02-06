/**
 * 
 */
package org.erlide.ui.internal.folding;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.erlide.ui.editors.erl.folding.IErlangFoldingPreferenceBlock;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;

/**
 * @author jakob
 * 
 */
public final class ErlangFoldingStructureProviderDescriptor {

    /* extension point attribute names */

    private static final String PREFERENCES_CLASS = "preferencesClass"; //$NON-NLS-1$

    private static final String CLASS = "class"; //$NON-NLS-1$

    private static final String NAME = "name"; //$NON-NLS-1$

    private static final String ID = "id"; //$NON-NLS-1$

    /** The identifier of the extension. */
    private final String fId;

    /** The name of the extension. */
    private String fName;

    /**
     * The class name of the provided
     * <code>IErlangFoldingStructureProvider</code> .
     */
    private final String fClass;

    /**
     * <code>true</code> if the extension specifies a custom
     * <code>IErlangFoldingPreferenceBlock</code>.
     */
    private boolean fHasPreferences;

    /** The configuration element of this extension. */
    private final IConfigurationElement fElement;

    /**
     * Creates a new descriptor.
     * 
     * @param element
     *            the configuration element to read
     */
    public ErlangFoldingStructureProviderDescriptor(
            final IConfigurationElement element) {
        fElement = element;
        fId = element.getAttribute(ID);
        Assert.isLegal(fId != null);

        fName = element.getAttribute(NAME);
        if (fName == null) {
            fName = fId;
        }

        fClass = element.getAttribute(CLASS);
        Assert.isLegal(fClass != null);

        if (element.getAttribute(PREFERENCES_CLASS) == null) {
            fHasPreferences = false;
        } else {
            fHasPreferences = true;
        }
    }

    /**
     * Creates a folding provider as described in the extension's xml.
     * 
     * @return a new instance of the folding provider described by this
     *         descriptor
     * @throws CoreException
     *             if creation fails
     */
    public IErlangFoldingStructureProvider createProvider()
            throws CoreException {
        final IErlangFoldingStructureProvider prov = (IErlangFoldingStructureProvider) fElement
                .createExecutableExtension(CLASS);
        return prov;
    }

    /**
     * Creates a preferences object as described in the extension's xml.
     * 
     * @return a new instance of the reference provider described by this
     *         descriptor
     * @throws CoreException
     *             if creation fails
     */
    public IErlangFoldingPreferenceBlock createPreferences()
            throws CoreException {
        if (fHasPreferences) {
            final IErlangFoldingPreferenceBlock prefs = (IErlangFoldingPreferenceBlock) fElement
                    .createExecutableExtension(PREFERENCES_CLASS);
            return prefs;
        }
        return new EmptyErlangFoldingPreferenceBlock();
    }

    /**
     * Returns the identifier of the described extension.
     * 
     * @return Returns the id
     */
    public String getId() {
        return fId;
    }

    /**
     * Returns the name of the described extension.
     * 
     * @return Returns the name
     */
    public String getName() {
        return fName;
    }
}
