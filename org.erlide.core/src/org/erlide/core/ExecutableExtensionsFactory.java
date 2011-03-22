package org.erlide.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * Use together with declared handlers like for example
 * 
 * <pre>
 * <code>
 * &lt;extension point="org.eclipse.ui.handlers">
 *   &lt;handler 
 *     class="org.erlide.core.ExecutableExtensionFactory:org.erlide.ui.my.MyHandler" 
 *     commandId="org.erlide.ui.mycommand">
 *   &lt;/handler>
 * &lt;/extension>
 * </code>
 * </pre>
 */
public class ExecutableExtensionsFactory implements
        IExecutableExtensionFactory, IExecutableExtension {

    private String className;

    public void setInitializationData(final IConfigurationElement config,
            final String propertyName, final Object data) throws CoreException {
        if (data instanceof String) {
            className = (String) data;
        }

    }

    public Object create() throws CoreException {
        Class<?> class1;
        try {
            class1 = ErlangPlugin.getDefault().getBundle().loadClass(className);
            return ErlangPlugin.getDefault().getInjector().getInstance(class1);
        } catch (final ClassNotFoundException e) {
            throw new CoreException(
                    new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID,
                            "Could not load class " + className));
        }
    }

}
