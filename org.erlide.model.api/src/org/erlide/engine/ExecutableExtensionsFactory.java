package org.erlide.engine;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import javax.inject.Inject;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.erlide.engine.services.ErlangService;
import org.osgi.framework.Bundle;

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
public class ExecutableExtensionsFactory implements IExecutableExtensionFactory,
        IExecutableExtension {

    private String className;
    private Bundle bundle;

    public ExecutableExtensionsFactory() {
    }

    @Override
    public void setInitializationData(final IConfigurationElement config,
            final String propertyName, final Object data) throws CoreException {
        if (data instanceof String) {
            className = (String) data;
        }
        final String contributor = config.getDeclaringExtension().getContributor()
                .getName();
        bundle = Platform.getBundle(contributor);
    }

    @Override
    public Object create() throws CoreException {
        final String bundleName = bundle.getSymbolicName();
        try {
            final Class<?> clazz = bundle.loadClass(className);

            final Constructor<?> constructor = getDefaultConstructor(clazz);
            final Class<?>[] parameterTypes = constructor.getParameterTypes();
            final Object[] initargs = new Object[parameterTypes.length];
            assignParameters(parameterTypes, initargs);
            return constructor.newInstance(initargs);
        } catch (final ClassNotFoundException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Could not load class " + className));
        } catch (final InjectionException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Injection error for class " + className, e));
        } catch (final IllegalArgumentException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Injection error for class " + className, e));
        } catch (final InstantiationException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Injection error for class " + className, e));
        } catch (final IllegalAccessException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Injection error for class " + className, e));
        } catch (final InvocationTargetException e) {
            throw new CoreException(new Status(IStatus.ERROR, bundleName,
                    "Injection error for class " + className, e));
        }
    }

    private void assignParameters(final Class<?>[] parameterTypes, final Object[] initargs) {
        for (int i = 0; i < parameterTypes.length; i++) {
            final Class<?> paramType = parameterTypes[i];
            if (ErlangService.class.isAssignableFrom(paramType)) {
                @SuppressWarnings("unchecked")
                final Class<? extends ErlangService> serviceClass = (Class<? extends ErlangService>) paramType;
                initargs[i] = injectParameter(serviceClass);
            } else {
                throw new InjectionException(
                        "Constructor parameters are not injectable (ErlangService)");
            }
        }
    }

    private Object injectParameter(final Class<? extends ErlangService> serviceClass) {
        final Object parameter = ErlangEngine.getInstance().getService(serviceClass);
        if (parameter != null) {
            return parameter;
        }
        throw new InjectionException("Constructor parameter " + serviceClass.getName()
                + " could not be instantiated");

    }

    private Constructor<?> getDefaultConstructor(final Class<?> clazz) {
        final Constructor<?>[] constructorArgs = clazz.getConstructors();
        if (constructorArgs.length == 1) {
            return constructorArgs[0];
        }
        for (final Constructor<?> constructor : constructorArgs) {
            if (constructor.isAnnotationPresent(Inject.class)) {
                return constructor;
            }
        }
        throw new InjectionException("Could not find injectable constructor");
    }

}
