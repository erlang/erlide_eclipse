package org.erlide.core.model.root;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.core.model.erlang.FunctionRef;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;

public interface IErlElementLocator {

    enum Scope {
        PROJECT_ONLY, REFERENCED_PROJECTS, ALL_PROJECTS
    }

    IErlElement findElement(IResource resource);

    IErlElement findElement(IResource resource, boolean openElements);

    IErlProject findProject(IProject project);

    IErlModule findModule(IFile file);

    IErlModule findModule(String name) throws ErlModelException;

    IErlModule findModuleIgnoreCase(String name) throws ErlModelException;

    IErlModule findModule(String moduleName, String modulePath)
            throws ErlModelException;

    IErlModule findInclude(final String includeName, final String includePath)
            throws ErlModelException;

    /**
     * Locates definitions of functions matching the given signature. Function
     * name and module can be regexps.
     * 
     * @throws ErlModelException
     */
    IErlFunction findFunction(FunctionRef r) throws ErlModelException;

    IErlModule findModuleFromProject(final IErlProject project,
            final String moduleName, final String modulePath,
            final IErlElementLocator.Scope scope) throws ErlModelException;

    IErlModule findIncludeFromProject(final IErlProject project,
            final String includeName, final String includePath,
            final IErlElementLocator.Scope scope) throws ErlModelException;

    IErlModule findIncludeFromModule(final IErlModule module,
            final String includeName, final String includePath,
            final IErlElementLocator.Scope scope) throws ErlModelException;

    IErlModule findModuleFromProject(IErlProject erlProject, String name,
            String object, boolean b, boolean c,
            IErlElementLocator.Scope projectOnly) throws ErlModelException;

}
